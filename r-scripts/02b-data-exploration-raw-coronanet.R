## ######################################################################### ##
## Exploratory Data Analysis
## ######################################################################### ##

# rm(list = ls(), inherits = TRUE)

## ========================================================================= ##
## do basic setup
## ========================================================================= ##

source("./r-scripts/setup.R")

mutate_init_country_level <- function(x) {
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

mutate_compliance <- function(x) {
    mutate(x, 
           compliance2 = plyr::revalue(
               compliance,
               c(
                   "Mandatory (Unspecified/Implied)" = "mandatory",
                   "Mandatory with Legal Penalties (Jail Time)" = "mandatory",
                   "Mandatory with Fines" = "mandatory",
                   "Mandatory with Exceptions (Please list exceptions in the text entry)" = "mandatory",
                   "Mandatory (Other; please list other enforcement mechanisms in the text entry)" = "mandatory",
                   "Voluntary/Recommended but No Penalties" = "voluntary"
               )),
           compliance2 = if_else(is.na(compliance2), "(missing)", compliance2),
           compliance = if_else(grepl(",", compliance2), "(inconsistent)", compliance2)
    )
}

mutate_compliance2 <- function(x) {
    mutate(x, 
           compliance_grep_man = grepl("mandatory", compliance, ignore.case = TRUE),
           compliance_grep_vol = grepl("voluntary", compliance, ignore.case = TRUE),
           compliance = if_else(
               is.na(compliance), 
               "(miss/incon)",
               if_else(compliance_grep_man & !compliance_grep_vol, 
                       "mandatory",
                       if_else(!compliance_grep_man & compliance_grep_vol, 
                               "voluntary", 
                               "(miss/incon)"))
           )
    )
}

summarize_cust <- function(x) {
    x %>% summarize(
        n_rec = n(),
        n_pol = length(unique(policy_id)),
        n_ctry = length(unique(ISO_A3)))
}
s
check_type_sub_cat <- function(x, wch_type) {
    x %>% filter(type == wch_type) %>%
        group_by(type, type_sub_cat) %>%
        summarize_cust() %>%
        print(n = 30)
}
check_init_country_level <- function(x, wch_type) {
    x %>% 
        filter(type == wch_type) %>%
        group_by(type, init_country_level) %>%
        summarize_cust() %>%
        print(n = 30)
}
check_compliance <- function(x, wch_type) {
    x %>% 
        filter(type == wch_type) %>%
        mutate_compliance() %>%
        group_by(type, compliance) %>%
        summarize_cust() %>%
        print(n = 30)
}
check_target_who_what <- function(x, wch_type) {
    x %>% 
        filter(type == wch_type) %>%
        group_by(type, target_who_what) %>%
        summarize_cust() %>%
        print(n = 30)
}
check_target_direction <- function(x, wch_type) {
    x %>% 
        filter(type == wch_type) %>%
        group_by(type, target_direction) %>%
        summarize_cust() %>%
        print(n = 30)
}
check_travel_mechanism <- function(x, wch_type) {
    x %>% 
        filter(type == wch_type) %>%
        group_by(type, travel_mechanism) %>%
        summarize_cust() %>%
        print(n = 30)
}
check_default_split <- function(x, wch_type) {
    x %>% 
        filter(type == wch_type) %>%
        mutate_compliance2() %>%
        mutate_init_country_level() %>%
        group_by(type, init_country_level, compliance) %>%
        summarize_cust() %>%
        print(n = 30)
    
}

## ========================================================================= ##
## load additional packages
## ========================================================================= ##

## ========================================================================= ##
## global variables
## ========================================================================= ##

varnames_check <- c("record_id", "policy_id", "country", "date_announced", "date_start", "date_end", "entry_type", "update_type", "type", "type_sub_cat", "init_country_level", "compliance", "event_description")

filename_dat_measures_coronanet_extended <- paste0(
    "dat_measures_coronanet_extended_", wch_date, ".csv"
)
filename_dat_measures_coronanet_core <- paste0(
    "dat_measures_coronanet_core_", wch_date, ".csv"
)

wch_date <- "2020-07-17"

## ========================================================================= ##
## read data
## ========================================================================= ##

## core version of the data file:
dat_measures_coronanet_core_raw <- read_csv(
    file.path(path_dat, filename_dat_measures_coronanet_core),
    col_types = list("record_id" = col_character(),
                     "policy_id" = col_character(),
                     "recorded_date" = col_datetime(format = ""),
                     "date_updated" = col_date(format = ""),
                     "date_announced" = col_date(format = ""),
                     "date_start" = col_date(format = ""),
                     "date_end" = col_date(format = ""),
                     "entry_type" = col_character(),
                     "update_type" = col_character(),
                     "event_description" = col_character(),
                     "domestic_policy" = col_integer(),
                     "type" = col_character(),
                     "type_sub_cat" = col_character(),
                     "type_text" = col_integer(),
                     "index_high_est" = col_double(),
                     "index_med_est" = col_double(),
                     "index_low_est" = col_double(),
                     "index_country_rank" = col_double(),
                     "correct_type" = col_character(),
                     "country" = col_character(),
                     "init_country_level" = col_character(),
                     "province" = col_character(),
                     "city" = col_character(),
                     "source_corr_type" = col_character(),
                     "target_country" = col_character(),
                     "target_geog_level" = col_character(),
                     "target_region" = col_character(),
                     "target_province" = col_character(),
                     "target_city" = col_character(),
                     "target_other" = col_character(),
                     "target_who_what" = col_character(),
                     "target_direction" = col_character(),
                     "travel_mechanism" = col_character(),
                     "compliance" = col_character(),
                     "enforcer" = col_character(),
                     "link" = col_character(),
                     "ISO_A3" = col_character(),
                     "ISO_A2" = col_character()))
names(dat_measures_coronanet_core_raw) 
head(dat_measures_coronanet_core_raw)
# 
dim(dat_measures_coronanet_core_raw)

## check latest dates:
dat_measures_coronanet_core_raw %>% 
    summarize_at(vars(contains("date")), max, na.rm = TRUE)


## load extended data set:
dat_measures_coronanet_extended_raw <- read_csv(
    file.path(path_dat, filename_dat_measures_coronanet_extended),
    col_types = cols(
        date_start = col_date(format = ""),
        country = col_character(),
        confirmed_cases = col_double(),
        deaths = col_double(),
        recovered = col_double(),
        record_id = col_character(),
        policy_id = col_double(),
        recorded_date = col_datetime(format = ""),
        date_updated = col_date(format = ""),
        date_announced = col_date(format = ""),
        date_end = col_date(format = ""),
        entry_type = col_character(),
        update_type = col_character(),
        event_description = col_character(),
        domestic_policy = col_double(),
        type = col_character(),
        type_sub_cat = col_character(),
        type_text = col_integer(),
        index_high_est = col_double(),
        index_med_est = col_double(),
        index_low_est = col_double(),
        index_country_rank = col_double(),
        correct_type = col_character(),
        init_country_level = col_character(),
        province = col_character(),
        city = col_character(),
        source_corr_type = col_character(),
        target_country = col_character(),
        target_geog_level = col_character(),
        target_region = col_character(),
        target_province = col_character(),
        target_city = col_character(),
        target_other = col_logical(),
        target_who_what = col_character(),
        target_direction = col_character(),
        travel_mechanism = col_character(),
        compliance = col_character(),
        enforcer = col_character(),
        link = col_character(),
        ISO_A3 = col_character(),
        ISO_A2 = col_character(),
        tests_daily_or_total = col_character(),
        tests_raw = col_double(),
        ccode = col_double(),
        ifs = col_character(),
        pop_WDI_PW = col_double(),
        gdp_WDI_PW = col_double(),
        gdppc_WDI_PW = col_double(),
        growth_WDI_PW = col_double(),
        lnpop_WDI_PW = col_double(),
        lngdp_WDI_PW = col_double(),
        lngdppc_WDI_PW = col_double(),
        FarRight_IO = col_double(),
        ExternalLaborOpenness_IO = col_double(),
        eco_glob_KOF = col_double(),
        soc_glob_KOF = col_double(),
        cult_prox_KOF = col_double(),
        poli_glob_KOF = col_double(),
        overallGlob_index_KOF = col_double(),
        news_WB = col_logical(),
        disap_FA = col_double(),
        polpris_FA = col_double(),
        latentmean_FA = col_double(),
        transparencyindex_HR = col_double(),
        state_IDC = col_double(),
        muni_IDC = col_double(),
        dispersive_IDC = col_double(),
        constraining_IDC = col_double(),
        inclusive_IDC = col_double(),
        Rank_FP = col_double(),
        Score_FP = col_double(),
        sfi_SFI = col_double(),
        ti_cpi_TI = col_double(),
        v2x_polyarchy_VDEM = col_double(),
        EmigrantStock_EMS = col_double()
    ))
    

## ========================================================================= ##
## explore CoronaNet measures
## ========================================================================= ##

names(dat_measures_coronanet_core_raw)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## policy_id
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## check policy_id:
dat_measures_coronanet_core_raw %>% 
    group_by(policy_id) %>%
    summarize(n = n())
dat_measures_coronanet_core_raw %>% 
    group_by(policy_id) %>%
    summarize(n = n()) %$%
    table(n)

## check policy_id with many entries: same countries?
dat_measures_coronanet_core_raw %>% 
    group_by(policy_id, country) %>%
    summarize(n = n()) %$%
    table(n)
# the same policy_id is not used in different countries

## check policies that have more than one entry:
dat_measures_coronanet_core_raw %>% 
    left_join(
        dat_measures_coronanet_core_raw %>% 
            group_by(policy_id) %>%
            summarize(n = n()),
        by = "policy_id"
    ) %>%
    filter(n > 1) %>%
    arrange(recorded_date) %>%
    select(record_id, policy_id, date_start, date_end, entry_type, type, type_sub_cat) %>%
    Viewxl()
# NOTE: if one measure (policy) affects different categories, the `policy_id` is the same for them.

## check policy_id and categories:
dat_measures_coronanet_core_raw %>%
    group_by(policy_id, type, type_sub_cat) %>%
    summarize(n = n()) %$%
    table(n)

## check policies that have more than one entry for the same subcategory:
dat_measures_coronanet_core_raw %>% 
    left_join(
        dat_measures_coronanet_core_raw %>%
            group_by(policy_id, type, type_sub_cat) %>%
            summarize(n = n()) %>%
            ungroup(),
        by = c("policy_id", "type", "type_sub_cat")
    ) %>%
    filter(n > 1) %>%
    arrange(policy_id, type, type_sub_cat, date_start) %>%
    select(record_id, policy_id, n, country, type, type_sub_cat, date_start, date_end, entry_type, update_type, init_country_level, event_description, everything()) %>%
    Viewxl()
# [[?]] why are there policy_id's that don't have a "new entry" as entry_type?
# REPORT
# NOTE: if one policy in a category/subcategory is updated, then the `policy_id` is the same for those updates. The policy (measure) might have new (additional) start- and end-dates
# [[?]] what does `update_type` mean? 
#   Can all ends of policies reliably be inferred from the entry `End of Policiy`? Why, then isn't there an `end_date` reported?
#   What do the entries `Change of Policy` mean?
#   What do `entry_type=="update"` mean when `update_type` is empty (contains no `Change of Policy`?)

## check if each policy_id has a "new_entry":
dat_measures_coronanet_core_raw %>%
    group_by(policy_id, entry_type) %>% 
    summarize(n = n()) %>%
    filter(policy_id == "1135464")
dat_measures_coronanet_core_raw %>%
    group_by(policy_id) %>%
    summarize(
        has_new_entry = as.numeric("new_entry" %in% tolower(entry_type))
    ) %>%
    filter(has_new_entry == 0) #%>%
    #pull(policy_id) %>% paste(collapse = "\", \"") %>% cat("\"", ., "\"", sep = "")
# 1242 rows w/o "new_entry" for date from 29 Jun 2020

## similar check, but with more info:
dat_measures_coronanet_core_raw %>%
    group_by(policy_id) %>%
    summarize(
        n_records = n(),
        n_type = length(unique(type)),
        n_type_sub_cat = length(unique(type_sub_cat)),
        n_new_entry = sum(tolower(entry_type) == "new_entry"),
        n_update = sum(tolower(entry_type) == "update")
    ) %>%
    filter(n_new_entry < n_type_sub_cat) %>%
    #filter(n_records > n_type_sub_cat) %>%
    #pull(policy_id) %>% paste(collapse = "\", \"") %>% cat("\"", ., "\"", sep = "")
    Viewxl()

## example:
dat_measures_coronanet_core_raw %>%
    filter(policy_id == "1011788") %>%
    arrange(country, type, type_sub_cat, date_announced) %>% 
    select(record_id, policy_id, country, date_announced, date_start, date_end, type, type_sub_cat, entry_type, update_type)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## record_id
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## check record_id:
dat_measures_coronanet_core_raw %>% 
    group_by(record_id) %>%
    summarize(n = n()) %$%
    table(n)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## find duplicate entries
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

dat_measures_coronanet_core_raw %>% 
    select(-record_id) %>%
    {filter(., duplicated(.))} %>%
    nrow()

dat_measures_coronanet_core_raw %>% 
    select(-record_id) %>%
    unique() %>% 
    nrow()

dat_measures_coronanet_core_raw %>% nrow()

# dat_measures_coronanet_core_raw %>% filter(record_id %in% c('2522563NA', '2522563Bi')) %>% Viewxl()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## check country and iso codes
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

dat_measures_coronanet_core_raw %>% 
    filter(ISO_A3 == "-") %>% 
    group_by(country) %>% tally()

## "northern cyprus" without country code ()

## check out northern cyprus:
dat_measures_coronanet_core_raw %>%
    filter(ISO_A3 == "-") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## date_start and date_end
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## check missings in start dates:
dat_measures_coronanet_core_raw %>%
    filter(is.na(date_start)) %>%
    nrow()

## check start- and end-dates
dat_measures_coronanet_core_raw %>%
    mutate(duration = date_end - date_start) %>% 
    select(record_id, policy_id, date_start, date_end, duration, country, type, type_sub_cat, compliance, entry_type, update_type, everything()) %>%
    Viewxl()
# NOTE: some measures don't have an end-date

## check missing end-dates by record and policy:
dat_measures_coronanet_core_raw %>%
    summarize(date_end_missing_n = sum(is.na(date_end)),
              date_end_missing_perc = sum(is.na(date_end)) / n())
# about 71% of entries don't have an end-date
dat_measures_coronanet_core_raw %>% 
    group_by(policy_id) %>%
    summarize(has_date_end = any(!is.na(date_end))) %>%
    ungroup() %>%
    summarize(
        n_policies = n(),
        date_end_missing_n = sum(!has_date_end),
        date_end_missing_perc = sum(!has_date_end) / n()
    )

dat_measures_coronanet_core_raw %>% 
    group_by(policy_id, type) %>%
    summarize(has_date_end = any(!is.na(date_end))) %>%
    ungroup() %>%
    group_by(type) %>%
    summarize(
        n_policies = n(),
        date_end_missing_n = sum(!has_date_end),
        date_end_missing_perc = sum(!has_date_end) / n()
    ) %>% print(n = 30)


## check start- and end-dates: all positive?
dat_measures_coronanet_core_raw %>%
    mutate(duration = date_end - date_start) %>% 
    filter(!is.na(duration), duration < 0) %>%
    select(record_id, policy_id, country, date_start, date_end, duration, entry_type, update_type, type, type_sub_cat, compliance, everything()) %>%
    #pull(record_id) %>% paste(collapse = ", ") %>% cat("\n", ., "\n")
    Viewxl()
# NOTE: some durations are negative
# REPORT (done)

## check how many "measures" (as used in the ms) have full coverage with end dates:
tab_meas_date_end <- dat_measures_coronanet_core_raw %>%
    mutate_compliance2() %>%
    mutate_init_country_level() %>%
    group_by(ISO_A3, country, type, init_country_level, compliance) %>%
    summarize(
        perc_has_date_end = mean(!is.na(date_end), na.rm = TRUE)
    ) 
tab_meas_date_end %>% Viewxl()
tab_meas_date_end %$% table(perc_has_date_end == 1)
tab_meas_date_end %$% table(perc_has_date_end == 1) %>% prop.table()
# FALSE  TRUE 
#  3210   583  ## for data of 2020-06-30
#  3210   583  ## for data of 2020-07-07

## check full coverage with date_end by splits:
tab_meas_date_end %>% 
    mutate(all_have_date_end = as.numeric(perc_has_date_end == 1)) %>%
    group_by(init_country_level, compliance, all_have_date_end) %>%
    summarize(n = n()) %>%
    group_by(init_country_level, compliance) %>%
    mutate(perc = n / sum(n)) %>%
    ggplot(aes(y = perc, x = all_have_date_end)) + geom_bar(stat = "identity") +
    facet_grid(rows = vars(init_country_level), cols = vars(compliance))


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## find end dates of measures
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## check how many policies have "End of Policy":
dat_measures_coronanet_core_raw %>%
    group_by(policy_id) %>%
    summarize(
        has_end_of_policy = as.numeric("End of Policy" %in% update_type)
    ) %>%
    group_by(has_end_of_policy) %>%
    summarize(n = n())
# NOTE: only very few have an `update_type == "End of Policy"`

## check out policies that have an `update_type == "End of Policy"`:
dat_measures_coronanet_core_raw %>%
    left_join(
        dat_measures_coronanet_core_raw %>%
            # group_by(policy_id) %>%
            group_by(policy_id, type, type_sub_cat) %>%
            summarize(
                has_end_of_policy = as.numeric("End of Policy" %in% update_type)
            ),
        # by = "policy_id") %>%
        by = c("policy_id", "type", "type_sub_cat")) %>%
    filter(has_end_of_policy == 1) %>%
    arrange(policy_id, type, type_sub_cat, date_announced) %>%
    select(one_of(varnames_check), has_end_of_policy, everything()) %>% 
    Viewxl()

## check out policies that have an `update_type == "End of Policy"` but no date_end:
dat_measures_coronanet_core_raw %>%
    filter(update_type == "End of Policy",
           is.na(date_end)) %>% 
    arrange(policy_id, type, type_sub_cat, date_announced) %>%
    select(one_of(varnames_check))
    select(one_of(varnames_check), everything()) %>%
    #pull(policy_id) %>% paste(collapse = "\", \"") %>% cat("\"", ., "\"", sep = "")
    Viewxl()

## check out policies that have no "End of Policy":
dat_measures_coronanet_core_raw %>%
    left_join(
        dat_measures_coronanet_core_raw %>%
            group_by(policy_id) %>%
            summarize(
                has_end_of_policy = as.numeric("End of Policy" %in% update_type)
            ),
        by = "policy_id") %>%
    filter(has_end_of_policy == 0) %>%
    select(one_of(varnames_check), has_end_of_policy, everything()) %>%
    Viewxl()

## Example: entries with entry_type=="update" with an update_type=="End of Policy" 
## that only feature a date_start, but no date_end. 
dat_measures_coronanet_core_raw %>%
    filter(record_id %in% c("1133827Ds", "1233452NA", "1620229NA", "1926832Az", "1410115NA")) %>%
    select(one_of(varnames_check), everything()) %>% 
    Viewxl()

## Example: entrys that don't have an update_type=="End of Policy" (e.g., record_id=="8270741Cs"). 
## This record only has a date_start (date_end is missing), but the event description 
## states that sport facilities are allowed to re-open on that date...
dat_measures_coronanet_core_raw %>%
    filter(record_id %in% c("8270741Cs", "1005610Dd", "1297273Dr")) %>%
    select(one_of(varnames_check), everything()) %>% 
    Viewxl()

## find end of measures in event description (as a very crude check)
dat_measures_coronanet_core_raw %>%
    filter(grepl("reopen|lift", event_description, ignore.case = TRUE)) %>%
    select(one_of(varnames_check), everything()) %>% 
    summarize(n = n(), n_end_of_policy = sum(update_type == "End of Policy", na.rm = TRUE))
#           n n_end_of_policy
#     1   629             147    ## on 29 Jun 2020

## Example: start date larger then end date: (with "Change of policy"):
dat_measures_coronanet_core_raw %>%
    filter(policy_id == "8782530") %>%
    arrange(type, type_sub_cat, date_announced) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## general question: is it safe to assume that when there is no date_end, just
## coalesce the date_start over to date_end and take the last date_end as the
## end of that measure?
## --> no, in general not!
## is that safe for `entry_type=="update"`?
dat_measures_coronanet_core_raw %>%
    filter(entry_type == "update") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()
## --> no, it's not.
## --> also not for `update_type=="Change of Policy"`, at least not in general
dat_measures_coronanet_core_raw %>%
    filter(entry_type == "update",
           update_type == "End of Policy",
           is.na(date_end)) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()
## --> it might be safe for `update_type=="End of Policy"`



## try to understand unit of measurement:

## create sequence number within policy_id (without arrange, just to check):
dat_measures_coronanet_core_raw %>%
    group_by(policy_id) %>%
    arrange(policy_id, date_announced) %>%
    mutate(seq_nr = 1:n(),
           seq_nr_last = ifelse(seq_nr == n(), "last", NA)) %>%
    ungroup() %>%
    arrange(country, type, type_sub_cat, date_announced) %>%
    select(record_id, policy_id, seq_nr, seq_nr_last, one_of(varnames_check), everything()) %>%
    Viewxl()


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## compliance
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## check grouping:
dat_measures_coronanet_core_raw %>%
    group_by(compliance) %>%
    tally() %>%
    Viewxl()

## check `compliance` subgroups: mandatory / voluntary measures:
dat_measures_coronanet_core_raw %>%
    mutate(
        compliance2 = plyr::revalue(
            compliance,
            c(
                "Mandatory (Unspecified/Implied)" = "mandatory",
                "Mandatory with Legal Penalties (Jail Time)" = "mandatory",
                "Mandatory with Fines" = "mandatory",
                "Mandatory with Exceptions (Please list exceptions in the text entry)" = "mandatory",
                "Mandatory (Other; please list other enforcement mechanisms in the text entry)" = "mandatory",
                "Voluntary/Recommended but No Penalties" = "voluntary"
            )),
        compliance2 = if_else(is.na(compliance2), "(missing)", compliance2),
        compliance2 = if_else(grepl(",", compliance2), "(inconsistent)", compliance2)
    ) %>%
    group_by(compliance, compliance2) %>%
    summarize(n = n()) %>%
    select(n, everything()) %>%
    arrange(desc(n)) %>%
    Viewxl()

## check measures with missing compliance:
dat_measures_coronanet_core_raw %>%
    filter(is.na(compliance)) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()


## Can Compliance change over time?
## and does this give use an end-date of a policy?
dat_measures_coronanet_core_raw %>% 
    group_by(policy_id, type, type_sub_cat) %>%
    mutate(
        compliance_orig = compliance,
        n_recs = n(),
        n_compliance_orig = length(unique(compliance_orig))
    ) %>%
    mutate_compliance2() %>%
    mutate(n_compliance2 = length(unique(compliance))) %>%
    arrange(policy_id, type, type_sub_cat, date_announced) %>% 
    filter(n_compliance_orig > 1) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

dat_measures_coronanet_core_raw %>% 
    mutate_compliance2() %>%
    group_by(policy_id, type, type_sub_cat) %>%
    arrange(policy_id, type, type_sub_cat, date_announced) %>%
    mutate(
        date_start_next = ifelse(compliance != lead(compliance, 1), 
                             lead(date_start, 1), NA)
    ) %>% 
    filter(!is.na(date_start_next), is.na(date_end)) #%>%
    # select(one_of(c(varnames_check, "date_start_next")), everything()) %>%
    # Viewxl()

length(unique(dat_measures_coronanet_core_raw$policy_id))
    
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## correct_type
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## what is `correct_type`?
dat_measures_coronanet_core_raw %>%
    group_by(correct_type) %>%
    summarize(n = n())

## check corrections:
dat_measures_coronanet_core_raw %>%
    filter(correct_type == "correction") %>%
    Viewxl()

## check corrections:
wch_pol <- dat_measures_coronanet_core_raw %>%
    filter(correct_type == "correction") %>%
    pull(policy_id)
dat_measures_coronanet_core_raw %>% 
    filter(policy_id %in% wch_pol) %>%
    Viewxl()
# NOTE: I don't know to what those corrections should apply...


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## init_country_level (targeted?)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## init_country_level:
dat_measures_coronanet_core_raw %>%
    group_by(init_country_level) %>%
    summarize(n = n()) %>%
    print(n = 50)
## NOTE: distinguish national vs. non-national policies / measures.

dat_measures_coronanet_core_raw %>%
    group_by(init_country_level) %>%
    summarize(n = n()) %>%
    mutate(perc = n / sum(n) * 100) %>%
    select(n, perc, everything()) %>%
    Viewxl()

## relative frequencies of init_country_level per type:
dat_measures_coronanet_core_raw %>%
    group_by(type, init_country_level) %>%
    summarize(n = n()) %>%
    mutate(perc = round(n / sum(n) * 100, 1)) %>%
    Viewxl()

## [[?]] Is the `init_country_level` variable valid?
## Is the distribution of the "yes/no" answers similar to the mun/nat answers, but reversed?
## # A tibble: 4 x 3
## # Groups:   type [1]
##   type                                     init_country_level                         n
##   <chr>                                    <chr>                                  <int>
## 1 Restriction and Regulation of Businesses Municipal                                106
## 2 Restriction and Regulation of Businesses National                                 853
## 3 Restriction and Regulation of Businesses No, it is at the national level           18
## 4 Restriction and Regulation of Businesses Yes, it is at the province/state level   866

## correlation with target_region, target_provice, target_city:
dat_measures_coronanet_core_raw %>% 
    group_by(init_country_level) %>%
    summarize_at(
        vars(one_of("target_region", "target_province", "target_city")),
        ~ round(mean(!is.na(.x)) * 100, 1)
    ) #%>% Viewxl()
    

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## check groupings: type_sub_cat, init_country_level, compliance, target_who_wat
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## check number of subtypes:
dat_measures_coronanet_core_raw %>%
    group_by(type) %>%
    summarize(n_type_sub_cat = length(unique(type_sub_cat))) %>%
    arrange(desc(n_type_sub_cat)) %>%
    print(n = 25)

## frequency tables of type and subtype:
dat_measures_coronanet_core_raw %>%
    group_by(type, type_sub_cat) %>%
    summarize(n = n()) %>%
    select(n, everything()) %>%
    #Viewxl("grp-typesub.xlsx")
    Viewxl()
#Viewxl(x = NULL, "grp-typesub.xlsx")


## frequency tables of type, subtype, national/subnational, mandatory/voluntary:
dat_measures_coronanet_core_raw %>%
    mutate_init_country_level %>%
    mutate_compliance %>%
    group_by(type, type_sub_cat, init_country_level, compliance) %>%
    #group_by(type, compliance) %>%
    summarize(n = n()) %>%
    select(n, everything()) %>%
    Viewxl()

## frequency tables of type, compliance:
dat_measures_coronanet_core_raw %>%
    mutate_init_country_level %>%
    mutate_compliance %>%
    group_by(type, compliance) %>%
    summarize(n = n()) %>%
    select(n, everything()) %>%
    #Viewxl("grp-compliance.xlsx")
    Viewxl()
#Viewxl(x = NULL, "grp-compliance.xlsx")


## target_who_what:
dat_measures_coronanet_core_raw %>%
    group_by(type, type_sub_cat, target_who_what) %>%
    summarize(n = n()) %>%
    select(n, everything()) %>%
    Viewxl()


## target_direction:
dat_measures_coronanet_core_raw %>%
    group_by(type, target_direction) %>%
    summarize(n = n()) %>%
    select(n, everything()) %>%
    Viewxl()

## travel mechanism: strange combinations
dat_measures_coronanet_core_raw %>%
    filter(grepl("kinds|pplicable", travel_mechanism)) %>%
    group_by(travel_mechanism) %>%
    summarize(n = n(), 
              record_ids = paste(head(record_id, 20), collapse = ", ")) %>%
    select(n, everything()) %>%
    Viewxl()



## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## check specific types of measures (policies)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##


## anti-disinformation measures:
wch_type <- "Anti-Disinformation Measures"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == wch_type) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()


## Closure and Regulation of Schools:
wch_type <- "Closure and Regulation of Schools"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == wch_type) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## Curfew:
wch_type <- "Curfew"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == wch_type) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## Declaration of Emergency
wch_type <- "Declaration of Emergency"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == wch_type) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()


## External Border Restrictions:
wch_type <- "External Border Restrictions"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
# default split might not work, see below...
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
# almost exclusively national! inbound/outbound might make more sense!
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
check_target_direction(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == wch_type) %$% 
    table(target_who_what, target_direction, useNA = "ifany")
dat_measures_coronanet_core_raw %>%
    filter(type == wch_type) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## check other than default split:
dat_measures_coronanet_core_raw %>%
    mutate_compliance2() %>%
    filter(type == wch_type) %$% 
    table(compliance, target_direction, useNA = "ifany")
# mandatory is mostly inbound, volunary is mostly outbound
dat_measures_coronanet_core_raw%>% 
    filter(type == wch_type) %>%
    mutate_compliance2() %>%
    mutate_init_country_level() %>%
    group_by(type, init_country_level, compliance, target_direction) %>%
    summarize_cust() %>%
    print(n = 30)


## "health monitoring": 
wch_type <- "Health Monitoring"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
check_travel_mechanism(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == wch_type) %$% 
    table(target_who_what, target_direction, useNA = "ifany")
dat_measures_coronanet_core_raw %>%
    filter(type == wch_type) %$% 
    table(target_who_what, target, useNA = "ifany")
dat_measures_coronanet_core_raw %>%
    filter(type == wch_type) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()


## "health resources": 
wch_type <- "Health Resources"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == "Health Resources") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()
## check health resources / masks:
dat_measures_coronanet_core_raw %>%
    filter(type == "Health Resources") %>%
    filter(type_sub_cat == "Masks") %>%
    Viewxl()
# mostly around ordering masks, but also distributing them (to re-distributers?)

## "health testing":
wch_type <- "Health Testing"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == "Health Testing") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## "Hygiene":
wch_type <- "Hygiene"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == "Hygiene") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## "Internal Border Restrictions":
wch_type <- "Internal Border Restrictions"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
check_target_direction(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == "Internal Border Restrictions") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## "Lockdown":
wch_type <- "Lockdown"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == "Lockdown") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## "New Task Force, Bureau or Administrative Configuration":
wch_type <- "New Task Force, Bureau or Administrative Configuration"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == "New Task Force, Bureau or Administrative Configuration") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## "Public Awareness Measures":
wch_type <- "Public Awareness Measures"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
# [[?]] use compliance at all?
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == "Public Awareness Measures") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## "Quarantine":
wch_type <- "Quarantine"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
check_target_direction(dat_measures_coronanet_core_raw, wch_type)
## check double grouping:
dat_measures_coronanet_core_raw %>%
    mutate_compliance() %>%
    mutate_init_country_level() %>%
    filter(type == wch_type) %>%
    group_by(init_country_level, compliance) %>%
    summarize_cust()
dat_measures_coronanet_core_raw %>%
    filter(type == "Quarantine") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## "Quarantine/Lockdown":
dat_measures_coronanet_core_raw %>%
    filter(type == "Quarantine/Lockdown") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## "Restriction and Regulation of Businesses":
wch_type <- "Restriction and Regulation of Businesses"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == "Restriction and Regulation of Businesses") %>%
    group_by(type, compliance) %>%
    summarize(n = n()) %>% print(n = 25)

## "Restriction and Regulation of Government Services":
wch_type <- "Restriction and Regulation of Government Services"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type) %>% print(n = 50)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)

## "Restriction of Non-Essential Businesses":
dat_measures_coronanet_core_raw %>%
    filter(type == "Restriction of Non-Essential Businesses") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()
# only two entries: record_id's 7053515NA, 8819062NA

## "Restrictions of Mass Gatherings":
wch_type <- "Restrictions of Mass Gatherings"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type) %>% print(n = 50)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>%
    filter(type == "Restrictions of Mass Gatherings") %>%
    group_by(type, init_country_level) %>%
    summarize(n = n()) %>% print(n = 25)

## check Curfew vs. Lockdown:
dat_measures_coronanet_core_raw %>%
    filter(country == "Turkey") %>%
    filter(type %in% c("Curfew", "Lockdown", "Quarantine", "Curfew/Lockdown")) %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## check "mask" anywhere:
dat_measures_coronanet_core_raw %>%
    filter(grepl("mask", type, ignore.case = TRUE) | 
               grepl("mask", type_sub_cat, ignore.case = TRUE) | 
               grepl("mask", event_description, ignore.case = TRUE)) %>%
    group_by(type, type_sub_cat) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    select(n, everything()) %>% 
    Viewxl()
dat_measures_coronanet_core_raw %>%
    filter(grepl("mask", type, ignore.case = TRUE) | 
               grepl("mask", type_sub_cat, ignore.case = TRUE) | 
               grepl("mask", event_description, ignore.case = TRUE)) %>%
    filter(type == "Social Distancing",
           type_sub_cat == "All public spaces / everywhere") %>%
    select(one_of(varnames_check), everything()) %>%
    Viewxl()

## check "travel" anywhere:
dat_measures_coronanet_core_raw %>%
    filter(grepl("travel", type, ignore.case = TRUE) | 
               grepl("travel", type_sub_cat, ignore.case = TRUE) | 
               grepl("travel", event_description, ignore.case = TRUE)) %>%
    group_by(type, type_sub_cat) %>%
    summarize(n = n()) %>%
    arrange(desc(n)) %>%
    select(n, everything()) %>% 
    Viewxl()

## "Social Distancing":
wch_type <- "Social Distancing"
check_default_split(dat_measures_coronanet_core_raw, wch_type)
check_type_sub_cat(dat_measures_coronanet_core_raw, wch_type) %>% print(n = 50)
check_init_country_level(dat_measures_coronanet_core_raw, wch_type)
check_compliance(dat_measures_coronanet_core_raw, wch_type)
check_target_who_what(dat_measures_coronanet_core_raw, wch_type)
dat_measures_coronanet_core_raw %>% 
    filter(type == "Social Distancing") %>%
    mutate(
        grep_mask = as.numeric(grepl("mask", event_description, ignore.case = TRUE)),
        grep_cover =  as.numeric(grepl("cover", event_description, ignore.case = TRUE))
    ) %>%
    select(one_of(varnames_check), grep_mask, grep_cover, everything()) %>%
    Viewxl()

## check "social distancing" with mask vs. non-mask:
dat_measures_coronanet_core_raw %>%
    filter(type == "Social Distancing") %>%
    group_by(type, type_sub_cat) %>%
    summarize(
        n = n(),
        n_mask = sum(grepl("mask", event_description, ignore.case = TRUE)),
        n_face = sum(grepl("face", event_description, ignore.case = TRUE)),
        n_mouth = sum(grepl("mouth", event_description, ignore.case = TRUE)),
        n_cover = sum(grepl("cover", event_description, ignore.case = TRUE)),
        n_wear = sum(grepl("wear", event_description, ignore.case = TRUE))
    ) %>% print(n = 25)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## check Austria
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## all policies/measures by date:
dat_measures_coronanet_core_raw %>%
    filter(ISO_A3 == "AUT") %>%
    arrange(date_start) %>%
    Viewxl()

## [[here]] 2020-06-19

## check policies that have more than one entry for the same subcategory:
dat_measures_coronanet_core_raw %>% 
    filter(ISO_A3 == "AUT") %>%
    left_join(
        dat_measures_coronanet_core_raw %>%
            group_by(policy_id, type, type_sub_cat) %>%
            summarize(n = n()) %>%
            ungroup(),
        by = c("policy_id", "type", "type_sub_cat")
    ) %>%
    filter(n > 1) %>%
    arrange(policy_id, type, type_sub_cat, date_start) %>%
    select(record_id, policy_id, n, type, type_sub_cat, date_start, date_end, entry_type, update_type, everything()) %>%
    Viewxl()


## check country vs. countrycode:
dat_measures_coronanet_core_raw %>% pull(country) %>% unique() %>% length()
dat_measures_coronanet_core_raw %>% pull(ISO_A3) %>% unique() %>% length()



## [[here]] -- until here; rest is old;
dat_measures_coronanet_core_raw %>% group_by(measure) %>% 
    summarize(n = n(),
              n_countries = length(unique(iso))) %>% 
    arrange(desc(n)) %>% 
    print(n = 40)
dat_measures_coronanet_core_raw %>% group_by(measure) %>% 
    summarize(n = n()) %>% 
    arrange(measure) %>% 
    print(n = 40)

dat_measures_coronanet_core_raw %>% group_by(measure, iso) %>% 
    summarize(n = n()) %>% 
    arrange(measure) %>% 
    print(n = 200)

## what i have:
## country, date, value(s)

## what I need: data.frame with:
## country, date, value(s), measure_1_days, measure_2_days

## how to get there:
## long: 
##   country, measure, earlierst effective date, measure_short_name (colname)
## to wide:
##   country, measure_effective_date

head(dat_measures_coronanet_core_raw)
dat_measures_coronanet_core_raw %>% select(country, iso, date_implemented, category, measure) %>% head()
names(dat_measures_coronanet_core_raw)

## intermediate step:
dat_measures_coronanet_core_raw <- dat_measures_coronanet_core_raw %>% mutate(
    measure_name = measure %>% tolower() %>%
        stringr::str_replace_all("[ /:-]", "_") %>%
        stringr::str_replace_all("health_screenings_in_airports_and_border_crossings",
                                 "health_screenings_airports_and_border") %>%
        stringr::str_replace_all("emergency_administrative_structures_activated_or_established",
                                 "emergency_administrative_structures") %>%
        stringr::str_replace_all("introduction_of_quarantine_policies",
                                 "quarantine_policies") %>%
        stringr::str_trim()
)

dat_measures_mindate <- dat_measures_coronanet_core_raw %>% 
    group_by(iso, country, measure_name) %>%
    summarize(date_min = min(date_implemented))

dat_measures_mindate %>% tidyr::pivot_wider(
    names_from = measure_name,
    values_from = date_min
)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Number of policies per country over time
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

dat_by_policy <- dat_measures_coronanet_core_raw %>%
    group_by(country, ISO_A3, type, policy_id) %>%
    summarize(
        date_start_min = min(date_start),
        date_start_max = max(date_start),
        date_end_max = max(date_end)
    ) %>% 
    ungroup() %>%
    group_by(country, ISO_A3, type) %>%
    arrange(country, ISO_A3, type, date_start_min) %>%
    mutate(policies_started = rank(date_start_min, ties.method = "max")) %>%
    ungroup()

dat_by_policy_labels <- dat_by_policy %>%
    group_by(country, type) %>%
    summarize(
        date_start_min = max(date_start_min, na.rm = TRUE),
        policies_started = max(policies_started, na.rm = TRUE)
    ) %>% 
    ungroup() %>%
    group_by(type) %>%
    filter(
        rank(policies_started) >= max(rank(policies_started)) - 3
    )

p <- dat_by_policy %>%
    filter(date_start_min < "2020-07-02") %>%
    ggplot(aes(x = date_start_min, y = policies_started, group = country)) +
    geom_line() +
    ggrepel::geom_label_repel(
        data = dat_by_policy_labels, 
        aes(label = country), 
        color = "darkgrey",
        xlim = as.Date(c("2020-01-01", "2020-03-01"))) +
    facet_wrap(vars(type))
p
ggsave(file.path(path_tmp, "number-of-policies.jpeg"), width = 15, height = 15)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## testing data in extended coronanet data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

dat_measures_coronanet_extended_raw %>% names() %>% grep("test", ., value = T)

dat_measures_coronanet_extended_raw %>% 
    select(contains("date"), country, contains("confirm"), contains("test")) %>%
    Viewxl()
