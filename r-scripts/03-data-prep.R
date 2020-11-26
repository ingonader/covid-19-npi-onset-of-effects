## ######################################################################### ##
## Data Preparation
## ######################################################################### ##

# rm(list = ls(), inherits = TRUE); rstudioapi::restartSession()
# rm(list = ls(), inherits = TRUE)

## ========================================================================= ##
## do basic setup
## ========================================================================= ##

source("./r-scripts/setup.R")

## ========================================================================= ##
## load additional packages
## ========================================================================= ##

## ========================================================================= ##
## read data
## ========================================================================= ##

## use current date, if no date is set:
if (!exists("wch_date")) wch_date <- Sys.Date()

filename_dat_confirmed <- paste0(
    "dat_confirmed_", wch_date, ".csv"
)
filename_dat_measures_coronanet_core <- paste0(
    "dat_measures_coronanet_core_", wch_date, ".csv"
)
filename_dat_wb <- paste0(
    "dat_wb_", wch_date, ".rds"
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Novel Coronavirus (COVID-19) Cases Data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

col_names <- suppressMessages(
    read_csv(file.path(
        path_dat, filename_dat_confirmed), 
        n_max = 0
    ) %>% 
        names() %>%
        plyr::revalue(c(
            "Province/State" = "state", 
            "Country/Region" = "country", 
            "Lat" = "lat", 
            "Long" = "lon", 
            "Date" = "date", 
            "Value" = "value", 
            "ISO 3166-1 Alpha 3-Codes" = "countrycode", 
            "Region Code" = "regioncode", 
            "Sub-region Code" = "subregioncode", 
            "Intermediate Region Code" = "intermediateregioncode")
        )
    )

dat_confirmed_raw <- read_csv(
    file.path(path_dat, filename_dat_confirmed),
    skip = 3,
    col_names = col_names,
    col_types = cols(
        state = col_character(),
        country = col_character(),
        lat = col_double(),
        lon = col_double(),
        date = col_date(format = ""),
        value = col_double(),
        countrycode = col_character(),
        regioncode = col_double(),
        subregioncode = col_double(),
        intermediateregioncode = col_double()
    )
)
# head(dat_confirmed_raw)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## CoronaNet data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## core version of the data file:
dat_measures_coronanet_core_raw <- read_csv(
    file.path(path_dat, filename_dat_measures_coronanet_core),
    col_types = cols(
        record_id = col_character(),
        policy_id = col_character(),
        entry_type = col_character(),
        correct_type = col_character(),
        update_type = col_character(),
        update_level = col_character(),
        description = col_character(),
        date_announced = col_date(format = ""),
        date_start = col_date(format = ""),
        date_end = col_date(format = ""),
        country = col_character(),
        ISO_A3 = col_character(),
        ISO_A2 = col_character(),
        init_country_level = col_character(),
        domestic_policy = col_integer(),
        province = col_character(),
        city = col_character(),
        type = col_character(),
        type_sub_cat = col_character(),
        type_text = col_character(),
        school_status = col_character(),
        target_country = col_character(),
        target_geog_level = col_character(),
        target_region = col_character(),
        target_province = col_character(),
        target_city = col_character(),
        target_other = col_character(),
        target_who_what = col_character(),
        target_direction = col_character(),
        travel_mechanism = col_character(),
        compliance = col_character(),
        enforcer = col_character(),
        index_high_est = col_double(),
        index_med_est = col_double(),
        index_low_est = col_double(),
        index_country_rank = col_double(),
        link = col_character(),
        date_updated = col_date(format = ""),
        recorded_date = col_datetime(format = "")))
# head(dat_measures_coronanet_core_raw)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## World Bank Data (selected variables)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

dat_wb <- readRDS(file.path(path_dat, filename_dat_wb))

## ========================================================================= ##
## data preparation
## ========================================================================= ##

## create copy of data for data preparation (save raw data):
dat_confirmed <- dat_confirmed_raw

## add reporting unit: (if state != NA, then state, else country_isocode)
dat_confirmed <- dat_confirmed %>% mutate(
    countryunit = ifelse(is.na(state), 
                         countrycode,
                         paste0(countrycode, "-", state))
)

## aggregate cases by country (countrycode):
dat_confirmed <- dat_confirmed %>% group_by(countrycode, date) %>%
    dplyr::summarize(
        country = paste(unique(country), collapse = "+"), #first(country),
        lat = mean(lat),
        lon = mean(lon),
        value = sum(value, na.rm = TRUE),
        n_summarized = n(),
        states = length(unique(state)),
        regions = length(unique(regioncode)),
        subregions = length(unique(subregioncode)),
        intermediateregions = length(unique(intermediateregioncode)),
        .groups = "drop_last"
    ) %>% 
    ungroup()

# ## check if any countries were combined (same `countrycode``, different `country`):
# dat_confirmed %>% filter(grepl("\\+", country)) %>% group_by(countrycode, country) %>% count()

## add growth rate (value_rel):
dat_confirmed <- dat_confirmed %>% 
    arrange(countrycode, date) %>%
    group_by(countrycode) %>% 
    mutate(
        value_rel = ifelse(countrycode == lag(countrycode, 1),
                           ifelse((value != 0) & (lag(value, 1) != 0),
                                  value / lag(value, 1),
                                  1),  ## per definition: both zero means no change (i.e., 1)
                           NA)
    ) %>% 
    ungroup()

## add moving (geometric) average (smoothing) of relative differences:
smooth_window_size <- 7
dat_confirmed <- dat_confirmed %>% 
    arrange(countrycode, date) %>%
    group_by(countrycode) %>% 
    mutate(
        value_rel_smooth = ifelse(countrycode == lag(countrycode, 1),
                           zoo::rollapply(value_rel, 
                                          FUN = psych::geometric.mean, 
                                          width = smooth_window_size, 
                                          fill = NA, 
                                          align = "right"),
                           NA)
    ) %>% 
    ungroup()

# ## check:
# dat_confirmed %>% filter(countrycode == "AUT") %>% select(contains("date"), contains("value")) %>% print(n = 200)

## add time variable across all countries (global, absolute time axis):
#reference_date <- lubridate::date("2020-01-13") ## first case reported outside of China (https://www.who.int/news-room/detail/27-04-2020-who-timeline---covid-19)
reference_date <- lubridate::date("2020-03-11") ## date that COVID-19 was declared a pandemic (https://www.who.int/news-room/detail/27-04-2020-who-timeline---covid-19)
dat_confirmed <- dat_confirmed %>% mutate(
    time_abs = as.numeric(
        difftime(lubridate::date(date), reference_date, units = "days")
    )
)

## add relative time variable within each country (days since x cases; relative time axis):
## define reference case count:
reference_number_of_cases <- 25

## add relative time variable:
## get reference date when reference_number_of_cases have been reached for each countrycode:
dat_refdate <- dat_confirmed %>%
    group_by(countrycode) %>%
    arrange(countrycode, date) %>%
    filter(value >= reference_number_of_cases) %>%
    summarize(reference_date_country = first(date), .groups = "drop_last") %>%
    ungroup()

## add relative time variable:
## join, calculate date, and remove ref date column:
dat_confirmed <- dat_confirmed %>% 
    left_join(dat_refdate, by = "countrycode") %>%
    mutate(
        time_rel = as.numeric(
            difftime(lubridate::date(date), reference_date_country, units = "days")
        )
    ) %>% select(-reference_date_country)


## ========================================================================= ##
## data preparation: measures
## ========================================================================= ##

## when refactoring code into functions, check (warn) for the following:
# * NA in date_start
# * NA in init_country_level
# * NA in compliance (?)
# * types of "Quarantine/Lockdown", "Restriction of Non-Essential Businesses"
# * ISO_A3 vs country: same length?

## reset data (save raw data in copy of variable):
dat_measures_coronanet_core_all <- dat_measures_coronanet_core_raw

## exclude measures that have date_start that's today or in the future,
## compared to the data we have for the countries:
## (only use measures in the past)
dat_measures_coronanet_core_all <- dat_measures_coronanet_core_all %>% 
    filter(lubridate::date(date_start) < max(lubridate::date(dat_confirmed[["date"]])))
dim(dat_measures_coronanet_core_all)

## define measure translation data:
# dat_measures_coronanet_core_all %>% pull(type) %>% unique() %>% sort() %>% paste0('"', ., '", "",\n') %>% cat(sep = "")
trans_measures_dat <- tribble(
    ~original, ~sanitized,
    "Anti-Disinformation Measures", "anti_disinfo",
    "Closure and Regulation of Schools", "schools_regulation",
    "Curfew", "curfew",
    "Declaration of Emergency", "declaration_of_emergeny",
    "External Border Restrictions", "ext_border_restrictions",
    "Health Monitoring", "health_monitoring",
    "Health Resources", "health_resources",
    "Health Testing", "health_testing",
    "Hygiene", "hygiene",
    "Internal Border Restrictions", "int_border_restrictions",
    "Lockdown", "lockdown",
    "New Task Force, Bureau or Administrative Configuration", "task_force",
    "Other Policy Not Listed Above", "other_policy",
    "Public Awareness Measures", "public_awareness",
    "Quarantine", "quarantine",
    "Quarantine/Lockdown", "quarantinelockdown",  ## REMOVE!
    "Restriction and Regulation of Businesses", "restriction_businesses",
    "Restriction and Regulation of Government Services", "restriction_government_services",
    "Restriction of Non-Essential Businesses", "restriction_noness_businesses",   ## REMOVE!
    "Restrictions of Mass Gatherings", "restriction_mass_gatherings",
    "Social Distancing", "social_distancing"
)
#trans_measures_dat %>% print(n = 100)


## exclude measures (these should be changed in the dataset soon):
dat_measures_coronanet_core_all <- dat_measures_coronanet_core_all %>% 
    filter(
        !(type %in% c("Quarantine/Lockdown", "Restriction of Non-Essential Businesses"))
    )

## shorten measure names:
for (i in 1:nrow(trans_measures_dat)) {
    dat_measures_coronanet_core_all <- dat_measures_coronanet_core_all %>%
        mutate(type = type %>% stringr::str_replace_all(
            trans_measures_dat[["original"]][i],
            trans_measures_dat[["sanitized"]][i])
        )
}
#dat_measures_coronanet_core_all$type %>% table()

## modify compliance variable to have only 2 (or 3) levels:
mutate_compliance_levels <- function(x) {
    x <- mutate(x, 
           compliance_grep_man = grepl("man", compliance, ignore.case = TRUE),
           compliance_grep_vol = grepl("vol", compliance, ignore.case = TRUE),
           compliance = if_else(
               is.na(compliance), 
               "miss",
               if_else(compliance_grep_man & !compliance_grep_vol, 
                       "man",
                       if_else(!compliance_grep_man & compliance_grep_vol, 
                               "vol", 
                               "miss"))
           )
    )
    x$compliance_grep_man <- NULL
    x$compliance_grep_vol <- NULL
    return(x)
}
dat_measures_coronanet_core_all <- dat_measures_coronanet_core_all %>%
    mutate_compliance_levels()
#dat_measures_coronanet_core_all %$% table(compliance)
#   man  miss   vol 
# 12045   627  3288  on 2020-07-08
# 13631   636  3763  on 2020-07-17
# 25674   189  5595  on 2020-08-18


## modify init_country_level variable:
mutate_init_country_level_levels <- function(x) {
    mutate(x,
           init_country_level = plyr::revalue(
               init_country_level,
               c("Provincial" = "subnat",
                 "No, it is at the national level" = "nat",
                 "Other (e.g., county)" = "subnat",
                 "Municipal" = "subnat",
                 "National" = "nat")
           )
    )
}
dat_measures_coronanet_core_all <- dat_measures_coronanet_core_all %>%
    mutate_init_country_level_levels()
#dat_measures_coronanet_core_all %$% table(init_country_level)
#   nat subnat 
# 10364   7602   on 2020-07-17
# 15117  16341   on 2020-08-18


## modify target_direction:
mutate_target_direction_levels <- function(x) {
    mutate(x,
           target_direction = plyr::revalue(
               target_direction,
               c("Inbound" = "in",
                 "Outbound" = "out",
                 "Inbound/Outbound" = "inout")
           )
    ) %>% tidyr::replace_na(list(target_direction = "miss"))
}
dat_measures_coronanet_core_all <- dat_measures_coronanet_core_all %>%
    mutate_target_direction_levels()
#dat_measures_coronanet_core_all %$% table(target_direction, useNA = "ifany")
#dat_measures_coronanet_core_all %>% filter(type == "ext_border_restrictions") %$% table(target_direction, useNA = "ifany")
#    in inout  miss   out 
#   605   362   108   218   on 2020-07-17
#   900   498   120   285   on 2020-08-18


## [[?]] what to do with "Northern Cyprus"? (no country code); 
## Is it in the Johns Hopkins Data? Doesn't seem to be the case, when looking at the 
## extended CoronaNet datset...

## excude data that has no country code (northern cyprus):
dat_measures_coronanet_core_all <- dat_measures_coronanet_core_all %>% 
    filter(ISO_A3 != "-")


## define splits for each variable:
## (init_country_level and compliance for all, but target_direction used additionally
## for external border restrictions):
dat_measures_coronanet_core_all <- dat_measures_coronanet_core_all %>% 
    mutate(
        split3 = ifelse(type == "ext_border_restrictions",
                    target_direction,
                    NA)
        )

## get first date when each measure was implemented, split for 
## init_country_level and compliance for all and additionally for target_direction 
## for external border restrictions:
dat_measures_mindate_long_all <- 
    dat_measures_coronanet_core_all %>% 
    group_by(ISO_A3, country, type, init_country_level, compliance, split3) %>%
    dplyr::summarize(
        date_min = min(date_start, na.rm = TRUE),
        .groups = "drop_last"
    ) %>%
    ungroup() %>%
    mutate(measure_name = paste0(type, 
                                 "__", init_country_level, 
                                 "__", compliance),
           measure_name = ifelse(!is.na(split3), 
                                 paste0(measure_name, "__", split3), 
                                 measure_name)
    )
#nrow(dat_measures_mindate_long_all)
#dat_measures_mindate_long_all %>% group_by(measure_name) %>% tally() %>% print(n = 150)
#Viewxl(dat_measures_mindate_long_all)

# sum(is.na(dat_measures_mindate_long_all$date_min))
# sum(is.nan(dat_measures_mindate_long_all$date_min))
# sum(is.infinite(dat_measures_mindate_long_all$date_min))
# (should all be zero)

## additional approach (currently not used): get first date (and the corresponding measure text):
tmp <- 
    dat_measures_coronanet_core_all %>% 
    filter(!is.na(date_start)) %>%
    group_by(ISO_A3, country, type, init_country_level, compliance, split3) %>%
    arrange(ISO_A3, country, type, init_country_level, compliance, split3, date_start) %>%
    dplyr::summarize(date_min = first(date_start),
                     description = first(description),
                     .groups = "drop_last") %>%
    ungroup() %>%
    mutate(measure_name = paste0(type, 
                                 "__", init_country_level, 
                                 "__", compliance),
           measure_name = ifelse(!is.na(split3), 
                                 paste0(measure_name, "__", split3), 
                                 measure_name)
    )
#Viewxl(tmp)


## remember measure names of all measures:
varnames_measures_all <- dat_measures_mindate_long_all %>%
    pull(measure_name) %>%
    unique()

## CHECK: is the mindate before the first data for a country? 
## or before the first date in the ML data, at 25 cases reached?
## --> That's probably the case!

## we used to filter certain types of measures, hence the new
## data frame here (kept for potential filtering and backward
## compatibility of the code):
dat_measures_mindate_long <- dat_measures_mindate_long_all

## remember measure names for analysis:
varnames_measures <- dat_measures_mindate_long %>%
    pull(measure_name) %>%
    unique()

## convert to wide format, to join later:
dat_measures_mindate_wide <- dat_measures_mindate_long %>% 
    tidyr::pivot_wider(
        id_cols = c(ISO_A3, country),
        names_from = measure_name,
        values_from = date_min
    )
#Viewxl(dat_measures_mindate_wide)

## get countries with measures:
countrycode_with_measures <- dat_measures_coronanet_core_all %>% pull(ISO_A3) %>% 
    unique()

## check countries that are dropped:
cat("\ncountries that have no measures reported in the ACAPS dataset:\n")
dat_confirmed %>% filter(
    !(countrycode %in% countrycode_with_measures)
    ) %>%
    group_by(countrycode, country) %>%
    summarize(n = n()) %>%
    print(n = 100)

# ## check Canada without country code:
# dat_confirmed_raw %>% filter(is.na(countrycode), country == "Canada") %>%
#     group_by(state) %>% summarize(value_last = last(value))

## exclude data from dat_confirmed that has no measures:
cat("\nexcluding confirmed cases from countries that have no measures reported in CoronaNet:\n")
rows_before <- nrow(dat_confirmed)
cat("rows before: ", rows_before, "\n")
dat_confirmed <- dat_confirmed %>% 
    filter(countrycode %in% countrycode_with_measures)
nrow(dat_confirmed)
## how many excluded:
cat("\nexcluded data from confirmed cases because no measures were reported in CoronaNet:\n")
cat(rows_before - nrow(dat_confirmed), "\n")
cat("rows after: ", nrow(dat_confirmed), "\n")


## ========================================================================= ##
## data preparation: common dataset for ML
## ========================================================================= ##

## join confirmed cases data to measures dataset:
dat_confirmed <- dat_confirmed %>% 
    left_join(
        dat_measures_mindate_wide %>% select(-country), 
        by = c("countrycode" = "ISO_A3"))
# head(dat_confirmed, n = 2)
#Viewxl(dat_confirmed)

## define function to calculate the days a measure was implemented,
## depending on "current" date:
calc_days_implemented <- function(current_datetime, date_implemented) {
    days_diff <- as.numeric(
        lubridate::date(current_datetime) - lubridate::date(date_implemented),
        unit = "days")
    ## cap days prior -14 days:
    days_implemented <- ifelse(days_diff <= -14,
                               -14,
                               days_diff)
    ## for countries that have not implemented a measure, use -15:
    days_implemented <- ifelse(is.na(days_implemented),
                               -15,
                               days_implemented)
    return(days_implemented)
}

## now, substract the min_implemented_date for each meaure from the current date
## to calculate how long a measure has been in place:
## for each of the measures variables, replace the min date with 
## the number of days that this measure has been in place 
## (or zero if not in place):
dat_all_raw <- dat_confirmed %>% 
    mutate_at(
        vars(varnames_measures),
        ~ calc_days_implemented(date, .x))

#Viewxl(dat_all_raw)
#dat_all_raw$schools_closure_genpop
#sum(is.na(dat_all_raw$schools_closure_genpop))

# ## testing:
# dat_confirmed <- dat_confirmed %>% 
#     mutate(
#         schools_closure_days = calc_days_implemented(date, schools_closure),
#         limit_public_gatherings_days = calc_days_implemented(date, limit_public_gatherings)
#     )
# dat_confirmed %>% filter(countrycode == "AUT") %>%
#     select(date, contains("schools"), contains("gathering")) %>%
#     print(n = 300)
               
## ========================================================================= ##
## add World Bank Data (selected variables)
## ========================================================================= ##

#dat_all_raw %>% select(1:10)
#dat_wb %>% head()

## add same value for each day (within each country):
dat_all_raw <- dat_all_raw %>% 
    left_join(
        dat_wb %>% select(
            iso3c, 
            ctry_pop65perc, 
            ctry_popdensity, 
            ctry_popurbanperc, 
            ctry_popbelowpovertylineperc, 
            ctry_poppollutionperc,
            ctry_gdppcppp),
        by = c("countrycode" = "iso3c")
    )

# dat_all_raw %>% group_by(countrycode, country) %>%
#     summarize(pop65perc = max(pop65perc),
#               popdensity = max(popdensity)) %>%
#     filter(is.na(pop65perc) | is.na(popdensity))

# dat_wb %>% filter(rowSums(is.na(dat_wb)) > 0) %>%
#     select(iso3c, country, contains("pop"))

## replace missings in world bank indicators...

## ...with median value:
dat_all_raw <- dat_all_raw %>% 
    mutate_at(
        c("ctry_pop65perc", 
          "ctry_popdensity", 
          "ctry_popurbanperc", 
          "ctry_popbelowpovertylineperc", 
          "ctry_poppollutionperc",
          "ctry_gdppcppp"),
        ~ ifelse(is.na(.x), median(.x, na.rm = TRUE), .x)
    )

## transform variables:
# dat_all %>% select(one_of(varnames_features)) %>% summarize_all(max)
# dat_all_raw$ctry_popdensity %>% log() %>% hist()
# dat_all_raw$ctry_gdp %>% log() %>% hist()
dat_all_raw <- dat_all_raw %>%
    mutate(
        ctry_popdensity_trans = log(ctry_popdensity),
        ctry_gdppcppp_trans = log(ctry_gdppcppp)
    )
#nrow(dat_all_raw)
# 32567  on 2020-07-17
# 38455  on 2020-08-18

## ========================================================================= ##
## additional data prep for ML (moved here from other file)
## ========================================================================= ##

dat_all <- dat_all_raw
dim(dat_all_raw)
dim(dat_all)

## ------------------------------------------------------------------------- ##
## exclude cases with missing target variable
## ------------------------------------------------------------------------- ##

# dat_all <- dat_all %>% 
#     filter(!is.na(value_rel)) %>%
#     filter(!is.nan(value_rel))

dat_all <- dat_all %>% 
    filter(!is.na(value_rel_smooth)) %>%
    filter(!is.nan(value_rel_smooth))
dim(dat_all)

## check missing values:
# sapply(dat_all, function(i) sum(is.na(i)))

## ------------------------------------------------------------------------- ##
## exclude values that are too low
## ------------------------------------------------------------------------- ##

n_min_cases <- 25

## exclude moving averages that include values that are low:
dat_all <- dat_all %>% group_by(countrycode) %>% 
    arrange(countrycode, date) %>% 
    filter(lag(value, smooth_window_size) >= n_min_cases) %>%
    ungroup()
nrow(dat_all)

## exclude days with less than <n_min_cases> reported cumulative cases:
dat_all <- dat_all %>% filter(value >= n_min_cases)
nrow(dat_all)


## ------------------------------------------------------------------------- ##
## exclude data x months after 25 cases were reached in each country
## ------------------------------------------------------------------------- ##
## (model only suitable for the beginning of the pandemic):

# ## check reference number:
# reference_number_of_cases

## exclude data 3 months after 25 cases were reached in each country
dat_all <- dat_all %>% 
    filter(time_rel <= 90)

## ------------------------------------------------------------------------- ##
## exclude countries that don't have enough data
## ------------------------------------------------------------------------- ##

## not needed; even if country has only one line of data (one date with
## confirmed cases reported), this is still valid (country is not a predictor)

## but: problematic for the number of measures used by at least n countries;
## a country with only one data point can't really be used to estimate the effect

## only use countries with at least 28d of data:
countries_with_enough_data <- dat_all %>% 
    group_by(countrycode) %>% 
    summarize(n = n(),
              .groups = "drop_last") %>% 
    ungroup() %>%
    filter(n >= 28) %>% 
    pull(countrycode)

dat_all <- dat_all %>% filter(countrycode %in% countries_with_enough_data)
nrow(dat_all)

# dat_all <- dat_all %>% filter(region == "Europe")
# #dat_all$region %>% table()

## ------------------------------------------------------------------------- ##
## define measures that have been used by enough countries
## ------------------------------------------------------------------------- ##

## by at least n_countries_min during the complete time frame,
## not necessarily at the same time:
n_countries_min <- 20

tab_measure_freq <- dat_measures_mindate_long %>%
    ## filter: only consider countries that we have confirmed data for after filtering!
    filter(ISO_A3 %in% unique(dat_all$countrycode)) %>%
    group_by(measure_name) %>%
    summarize(n = n(),
              .groups = "drop_last") %>%
    arrange(n)
# tab_measure_freq %>% print(n = 100)

## only use measures that have been used by <n_countries_min> or more countries:
varnames_measures_mincountries <- tab_measure_freq %>%
    filter(n >= n_countries_min) %>% 
    pull(measure_name)

nrow(tab_measure_freq)
length(varnames_measures_mincountries)

## excluded measures:
tab_measure_freq %>% filter(n < n_countries_min) %>% print(n = 20)

# ## ========================================================================= ##
# ## split data: train / eval / test
# ## ========================================================================= ##
# 
# set.seed(448)
# 
# ## define sizes of train, eval, and test sets:
# probs <- c(.45, .1, .45)
# 
# ## split into traineval and test: 
# ## (mlr3 inner/outer resmapling will use train and eval):
# idx_split <- sample(c("traineval", "test"), 
#                     prob = c(sum(probs[1:2]), probs[3]),
#                     size = nrow(dat_all), 
#                     replace = TRUE)
# 
# ## needed for mlr3 holdout later:
# ratio_train_eval <- probs[1] / sum(probs[1:2])
# dat_traineval <- dat_all[idx_split == "traineval", ]
# dat_test <- dat_all[idx_split == "test", ]
# 
# dim(dat_traineval)
# dim(dat_test)

## ========================================================================= ##
## additional data prep (learned from training data)
## ========================================================================= ##

## ------------------------------------------------------------------------- ##
## mitigate outliers in target variable:
## ------------------------------------------------------------------------- ##

## check outliers:
dat_all %>%
    ggplot(aes(value_rel_smooth)) +
    geom_histogram(bins = 100)
dat_all %>% pull(value_rel_smooth) %>%
    quantile(probs = c(.01, .99), na.rm = TRUE)

## get upper quantile for winsorization:
q_upper <- quantile(dat_all[["value_rel_smooth"]], probs = .99, na.rm = TRUE)

## get lower bound for winsorization:
bound_lower <- 1

## windsorize outliers in full dataset:
dat_all <- dat_all %>% mutate(
    ## trim outliers on the upper end of the distribution:
    value_rel_smooth = ifelse(value_rel_smooth > q_upper,
                              q_upper,
                              value_rel_smooth),
    value_rel_smooth = ifelse(value_rel_smooth < bound_lower,
                              bound_lower,
                              value_rel_smooth)
)

# ## also apply in test set:
# dat_test <- dat_test %>% mutate(
#     ## trim outliers on the upper end of the distribution:
#     value_rel_smooth = ifelse(value_rel_smooth > q_upper,
#                               q_upper,
#                               value_rel_smooth)
# )

#nrow(dat_all)
# 13987   on 2020-07-17
# 14559   on 2020-08-18
