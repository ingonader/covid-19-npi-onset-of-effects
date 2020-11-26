## ######################################################################### ##
## Exploratory Data Analysis
## ######################################################################### ##

# rm(list = ls(), inherits = TRUE); rstudioapi::restartSession()
# rm(list = ls(), inherits = TRUE)

## ========================================================================= ##
## do basic setup
## ========================================================================= ##

source("./r-scripts/setup.R")
source("./r-scripts/function-library.R")

## ========================================================================= ##
## load and prep data
## ========================================================================= ##

wch_date <- "2020-04-12"
source("./r-scripts/03-data-prep.R")

## ========================================================================= ##
## load additional packages
## ========================================================================= ##

## ========================================================================= ##
## check missings in world bank data
## ========================================================================= ##

# dat_wb %>% filter(rowSums(is.na(dat_wb)) > 0) %>%
#     select(iso3c, country, contains("pop"))

dat_wb %>% 
    filter(rowSums(is.na(dat_wb)) > 0) %>%
    filter(iso3c %in% unique(dat_all$countrycode)) %>%
    select(iso3c, country, contains("pop"), contains("gdp")) %>%
    summarize_all(
        list(
            missing = ~ sum(is.na(.x)) / length(.x)
        )
    )


## ========================================================================= ##
## check measures for analysis
## ========================================================================= ##

varnames_measures_mincountries
varnames_measures_mincountries %>% grep("[^_]+__nat", ., value = TRUE)
varnames_measures_mincountries %>% grep("[^_]+__subnat", ., value = TRUE)

varnames_measures_mincountries %>% grep("[^_]+__[^_]+__man", ., value = TRUE)
varnames_measures_mincountries %>% grep("[^_]+__[^_]+__vol", ., value = TRUE)
varnames_measures_mincountries %>% grep("[^_]+__[^_]+__mis", ., value = TRUE)

## ========================================================================= ##
## table: measures
## ========================================================================= ##

## see 06a-materials-for-ms.R


## ========================================================================= ##
## data size
## ========================================================================= ##

## total data:
dat_all %>% nrow()
dat_all$countrycode %>% unique() %>% length()

## percentage of countries in train and test set:
length(in_sample_groups) / dat_all$country %>% unique() %>% length()

## percentage of data in train and test set:
dat_all %>% filter(countrycode %in% in_sample_groups) %>% nrow() / nrow(dat_all)

## ========================================================================= ##
## check base rate by number of cases
## ========================================================================= ##

dat_all_raw %>% Viewxl()
dat_all_raw %>%
    filter(value > 0) %>%
    group_by(countrycode, value) %>%
    summarize(
        value_rel = mean(value_rel, na.rm = TRUE),
        value_rel_max = max(value_rel, na.rm = TRUE)
    ) %>%
    ggplot(aes(x = value, y = value_rel, color = countrycode)) +
    geom_line() +
    coord_cartesian(xlim = c(0, 50)) +
    theme(legend.position = "none")

## ========================================================================= ##
## check base rate (unsmoothed daily increase rate)
## ========================================================================= ##


#' ## [[here]]
#' #' @param r vector of ratios, sorted by time
#' #' @param t_gen generation time
#' calc_r0_from_ratio <- function(ratio, t_gen = 4) {
#'     cumcases <- 1000 ## doesn't matter, cancels itself out
#'     n0 <- cumcases * ratio^t_gen - cumcases
#'     shiftval <- t_gen + 1
#'     n_shift <- cumcases * ratio^(t_gen + shiftval) - cumcases * ratio ^ shiftval
#'     return(n0 / n_shift)
#' 
#' }
#' calc_r0_from_ratio(1.1)
#' 
#' 
#' calc_newcases_from_ratio <- function(cumcases, ratio, t_gen = 4) {
#'     n0 <- cumcases * ratio^t_gen - cumcases
#'     return(n0)
#' }
#' calc_newcases_from_ratio(10, 1.2)

dim(dat_all_raw)
dim(dat_all)

## ------------------------------------------------------------------------- ##
## base rate
## ------------------------------------------------------------------------- ##

## base rate  after data prep for ML, but without windsorization:
## (needs to be moved to ML file, won't work before running it):
dat_all %>% pull(value_rel) %>% mean(na.rm = TRUE)

## base rate before after data prep for ML, with windsorization:
## (needs to be moved to ML file, won't work before running it):
bind_rows(dat_traineval, dat_test) %>% pull(value_rel) %>% mean()

## ------------------------------------------------------------------------- ##
## base rate within countries
## ------------------------------------------------------------------------- ##

## base rate  after data prep for ML, but without windsorization:
## (needs to be moved to ML file, won't work before running it):
dat_all %>% group_by(countrycode) %>% 
    summarize(value_rel = mean(value_rel, na.rm = TRUE)) %>%
    pull(value_rel) %>%
    summary()

## base rate before after data prep for ML, with windsorization:
## (needs to be moved to ML file, won't work before running it):
bind_rows(dat_traineval, dat_test) %>% group_by(countrycode) %>% 
    summarize(value_rel = mean(value_rel, na.rm = TRUE)) %>%
    pull(value_rel) %>%
    summary()

## ------------------------------------------------------------------------- ##
## base rate for days when no measures have been implemented:
## ------------------------------------------------------------------------- ##

## after data prep for ML, but without windsorization:
dat_j <- dat_all
wch_days <- (apply(dat_j[varnames_measures], 1, max, na.rm = TRUE) < 0)
dat_j[wch_days, ] %>% pull(value_rel) %>% mean(na.rm = TRUE)

## after data prep for ML, with windsorization:
## (needs to be moved to ML file, won't work before running it):
dat_j <- bind_rows(dat_traineval, dat_test)
wch_days <- (apply(dat_j[varnames_measures], 1, max, na.rm = TRUE) < 0)
dat_j[wch_days, ] %>% pull(value_rel) %>% mean(na.rm = TRUE)

## ========================================================================= ##
## check drop in growth rate after x cases has been reached
## ========================================================================= ##

dat_all %>% 
    mutate(grp = gtools::asc(substr(countrycode, 3, 3)) %% 12) %>%
    ggplot(aes(x = time_rel, y = value_rel_smooth, color = countrycode)) +
    geom_line(alpha = .2) +
    facet_wrap(vars(grp)) + 
    theme(legend.position = "none")

## ========================================================================= ##
## check mean smoothed daily increase ratio for each measure at implementation
## ========================================================================= ##

## at the time the measure was implemented:
dat_j <- bind_rows(dat_traineval, dat_test)

## early and low rank (maybe too early, ratio still low?):
get_mean_increase_ratio(dat_j, "health_screenings_airports_and_border_genpop", at = 0)
## large effect:
get_mean_increase_ratio(dat_j, "schools_closure_genpop", at = 0)
## high rank:
get_mean_increase_ratio(dat_j, "full_lockdown_genpop", at = 0)
## late implementation:
get_mean_increase_ratio(dat_j, "psychological_assistance_and_medical_social_work_targeted", at = 0)

## of general interest:
get_mean_increase_ratio(dat_j, "partial_lockdown_genpop", at = 0)
get_mean_increase_ratio(dat_j, "public_services_closure_genpop", at = 0)

# grep("psych", varnames_measures, value = TRUE)

## ========================================================================= ##
## check first data entries of each country (that's not zero)
## ========================================================================= ##

dat_confirmed %>% group_by(countrycode) %>% summarize_all(first) %>% filter(value > 0) %>% print(n = 30)

## ========================================================================= ##
## plots of daily changes (rates) -- value_rel
## ========================================================================= ##

# cases_min: defined in 03-data-prep.R
## but:
## [[todo]]
## can't do it this way! need to find first date where cases > cases_min!
countrycode_with_data_forexpl <- dat_confirmed %>%
    filter(value >= 100) %>%
    group_by(countrycode) %>%
    dplyr::summarize(n = n()) %>%
    filter(n >= 14) %>%
    pull(1)


dat_confirmed_filtered <- dat_confirmed %>% 
    filter(value >= 50) %>% 
    filter(countrycode %in% countrycode_with_data_forexpl)
countrycode_i <- dat_confirmed_filtered$countrycode[1]
for (countrycode_i in unique(dat_confirmed_filtered$countrycode)) {
    dat_i <- dat_confirmed_filtered %>% 
        filter(countrycode == countrycode_i)
    p <- dat_i %>%
        ggplot(aes(y = value_rel, 
                   x = date)) +
        #geom_abline(slope = 0, intercept = 1, linetype = "dashed", color = "darkgrey") +
        geom_line() + 
        geom_line(aes(y = value_rel_smooth), size = 2, col = "red") +
        coord_cartesian(ylim = c(1, max(dat_i[["value_rel"]], na.rm = TRUE))) +
        labs(
            title = paste0(dat_i$country[1], " (", countrycode_i, ")"),
            subtitle = sprintf("first = %i;  last = %i", 
                          dat_i$value[1], 
                          dat_i$value[nrow(dat_i)])
        )
    # plot(p)
    ggsave(filename = file.path(path_plots, 
                                paste0("value_rel-raw---", countrycode_i,".jpg")), 
           plot = p,
           width = 3, height = 3)
}
#dat_confirmed %>% filter(country_isocode == "CAN") %>% print(n = 300)

## problems: AUS, CAN, CHN

dat_confirmed_filtered %>% filter(country_isocode == "AUS") %>% print(n = 100)
dat_confirmed_filtered %>% filter(country_isocode == "AUT") %>% print(n = 100)

dat_confirmed_filtered %$% table(country_isocode, state) %>% rowSums()

## ========================================================================= ##
## plots of relative changes of 7d window (newcases_7d_rellog)
## ========================================================================= ##

## [[todo]]:
## can't do it like this! need to find first date where value > cases_min, 
## and plot everything from there!
dat_confirmed_filtered <- dat_confirmed %>% 
    filter(value >= 25) %>% 
    filter(countrycode %in% countrycode_with_data_forexpl)
countrycode_i <- dat_confirmed_filtered$countrycode[1]
for (countrycode_i in unique(dat_confirmed_filtered$countrycode)) {
    dat_i <- dat_confirmed_filtered %>% 
        filter(countrycode == countrycode_i)
    p <- dat_i %>%
        ggplot(aes(y = newcases_7d_rellog, 
                   x = date)) +
        geom_abline(slope = 0, intercept = 0, linetype = "dashed", color = "darkgrey") +
        geom_line() + 
        #geom_line(aes(y = value_rel_smooth), size = 2, col = "red") +
        #coord_cartesian(ylim = c(1, max(dat_i[["value_rel"]], na.rm = TRUE))) +
        labs(
            title = paste0(dat_i$country[1], " (", countrycode_i, ")"),
            subtitle = sprintf("first = %i;  last = %i", 
                               dat_i$value[1], 
                               dat_i$value[nrow(dat_i)])
        )
    #plot(p)
    ggsave(filename = file.path(path_plots, 
                                paste0("newcases_7d_rellog---", countrycode_i,".jpg")), 
           plot = p,
           width = 3, height = 3)
}

## why so many zeros? because in the beginning, with few cases, there is not much change
dat_confirmed_filtered %>% filter(countrycode == "AUS") %>%
    select(date, countrycode, contains("value"), contains("newcases")) %>% 
    print(n = 150)

## still needs filtering for cases n_min;



## ========================================================================= ##
## plots of relative changes of individual days (newcases_rellog)
## ========================================================================= ##

## [[todo]]:
## can't do it like this! need to find first date where value > cases_min, 
## and plot everything from there!
dat_confirmed_filtered <- dat_confirmed %>% 
    filter(value >= 25) %>% 
    filter(countrycode %in% countrycode_with_data_forexpl)
countrycode_i <- "AUT" #dat_confirmed_filtered$countrycode[1]
for (countrycode_i in unique(dat_confirmed_filtered$countrycode)) {
    dat_i <- dat_confirmed_filtered %>% 
        filter(countrycode == countrycode_i)
    p <- dat_i %>%
        ggplot(aes(y = newcases_rellog, 
                   x = date)) +
        geom_abline(slope = 0, intercept = 0, linetype = "dashed", color = "darkgrey") +
        geom_line() + 
        geom_line(aes(y = newcases_rellog_smooth), size = 2, col = "red") +
        #coord_cartesian(ylim = c(1, max(dat_i[["value_rel"]], na.rm = TRUE))) +
        labs(
            title = paste0(dat_i$country[1], " (", countrycode_i, ")"),
            subtitle = sprintf("first = %i;  last = %i", 
                               dat_i$value[1], 
                               dat_i$value[nrow(dat_i)])
        )
    #plot(p)
    ggsave(filename = file.path(path_plots, 
                                paste0("newcases_rellog---", countrycode_i,".jpg")), 
           plot = p,
           width = 3, height = 3)
}


## ========================================================================= ##
## check full_lockdown
## ========================================================================= ##

dat_measures_mindate_long %>% 
    filter(measure_name %in% c("full_lockdown", "partial_lockdown"))
dat_measures_mindate_wide %>% 
    select(iso, country, region, contains("_lockdown")) %>%
    filter(!is.na(partial_lockdown) & !is.na(full_lockdown))

## ========================================================================= ##
## check when measures have been put into place (implemented)
## ========================================================================= ##

countrycode_with_data_forexpl <- dat_confirmed %>%
    filter(value >= 25) %>%
    group_by(countrycode) %>%
    dplyr::summarize(n = n()) %>%
    filter(n >= 14) %>%
    pull(1)

## get date when confirmed (cum) cases have reached a certain threshold:
n_cases_threshold <- 25
dat_confirmed_threshold <- dat_confirmed %>% filter(value >= n_cases_threshold) %>% 
    group_by(countrycode) %>%
    arrange(date) %>%
    summarize_all(
        first
    ) %>% 
    ungroup()

## check:
dat_confirmed_threshold %>% select(-one_of(varnames_measures))

## join date when threshold was reached to measures data:
dat_measures_timing <- dat_measures_mindate_long %>% 
    left_join(
        dat_confirmed_threshold %>%
            select(countrycode, date, value) %>%
            rename("date_cases_reached_threshold" = "date"),
        by = c("iso" = "countrycode")) %>%
    mutate(
        days_implemented_after_threshold = as.numeric(
            (lubridate::date(date_min) - lubridate::date(date_cases_reached_threshold)), 
            unit = "days")
    ) %>%
    ## remove measures that have never implemented in a specific country:
    filter(!is.na(date_min)) %>%  
    ungroup()
    

## check most commonly used measures (to unclutter plot below):
varnames_measures_for_exploration <- dat_measures_timing %>% 
    group_by(measure_name) %>% 
    summarize(n = n()) %>%
    arrange(desc(n)) %>% 
    filter(n >= 20) %>%
    pull(measure_name)

ggplot(
    dat_measures_timing %>%
        ## only use countries that are deemed relevant for exploration:
        filter(iso %in% countrycode_with_data_forexpl) %>%
        ## only use most common measures:
        filter(iso %in% countrycode_with_data_forexpl),
        #filter(n_countries >= 5),
    aes(
        x = days_implemented_after_threshold, 
        y = forcats::fct_reorder(
            translate_measures(measure_name),
            days_implemented_after_threshold, median, 
            na.rm = TRUE,
            .desc = TRUE
            ),
        fill = grepl("targeted", measure_name)
        )) + 
    geom_boxplot(varwidth = FALSE) +  
    geom_vline(xintercept = 0, color = "darkgrey", linetype = "dashed") +
    labs(
        x = paste0("Days after ", n_cases_threshold, " cases have been reached"),
        y = "",
        fill = ""
    ) +
    scale_fill_discrete(
        labels = c(
            "FALSE" = "General population",
            "TRUE" = "Targeted"
        )
    ) +
    theme_cust()
ggsave(filename = file.path(path_plots, "measures_timing.jpg"), width = 10, height = 10)

# as.numeric(
# lubridate::date(current_datetime) - lubridate::date(date_implemented),
# unit = "days")

## ========================================================================= ##
## rank the measures by date and explore
## ========================================================================= ##

dat_rank <- dat_measures_mindate_long %>% group_by(iso, country) %>% 
    arrange(iso, date_min) %>% mutate(
        date_rank = rank(date_min)
    ) %>% ungroup() %>%
    group_by(measure_name) %>%
    mutate(
        n_countries = length(measure_name)
    ) %>% 
    ungroup()

ggplot(
    dat_rank %>% filter(n_countries >= 5),
    aes(
        x = date_rank, 
        y = forcats::fct_reorder(
            translate_measures(measure_name),
            date_rank, median, 
            na.rm = TRUE,
            .desc = TRUE),
        fill = n_countries
        )
    ) + 
    geom_boxplot(varwidth = FALSE) +  
    labs(
        x = paste0("Rank of measure"),
        y = "",
        fill = "Number of\nCountries"
    ) +
    scale_fill_distiller(
        type = "seq",
        palette = 1,
        direction = 1
    ) + 
    theme_cust()
ggsave(filename = file.path(path_plots, "measures_ranking_by_date.jpg"), width = 10, height = 10)

## ========================================================================= ##
## measure frequency (how many countries have implemented a measure)
## ========================================================================= ##

ggplot(
    dat_measures_timing %>%
        ## remove measures that have never implemented in a specific country:
        filter(!is.na(date_min)) %>%  
        ## only use countries that are deemed relevant for exploration:
        filter(iso %in% countrycode_with_data_forexpl) %>%
        ## only use most common measures:
        filter(measure_name %in% varnames_measures_for_exploration),
    aes(x = forcats::fct_rev(forcats::fct_infreq(measure_name)),
        fill = grepl("targeted", measure_name)
    )) + 
    geom_bar(stat = "count") + 
    coord_flip()
ggsave(filename = file.path(path_plots, "measures_counts.jpg"), width = 10, height = 10)



## ========================================================================= ##
## check correlations of predictors
## ========================================================================= ##

## check out size of correlations (summary via a histogram):
cor(dat_all_raw %>% 
        select(one_of(varnames_measures))) %>% 
    {.[upper.tri(.)]} %>%
    hist(nclass = 50, col = "grey")

# pdf(file.path(path_plots, "correlations-tmp.pdf"), width = 10, height = 10)
# corrplot::corrplot(cor(dat_all_raw[varnames_measures]))
# dev.off()

## calculate correlation matrix:
cormat <- cor(
    dat_all_raw[varnames_measures],
    use = "pairwise")
## replace lower triangle with NA:
upper_tri <- get_upper_tri(round(cormat, 3)) 

# source("./r-scripts/function-library.R")

# melt the correlation matrix for plotting with ggplot:
melted_cormat_plot <- reshape2::melt(upper_tri, na.rm = TRUE)
# create a ggheatmap:
p <- ggheatmap_cust(melted_cormat_plot, "value")
# plot the heatmap
print(p)
ggsave(plot = p, 
       file = file.path(path_plots, "correlations-predictors.jpg"), 
       width = 10, height = 10)

## display largest correlations:
## replace diagonal with NA:
diag(upper_tri) <- NA                       
# melt the correlation matrix
melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE) %>% mutate(
    value_abs = abs(value)
)

melted_cormat %>% arrange(desc(value_abs), Var1) %>%
    head(n = 10)


melted_cormat %>% filter(Var1 == "schools_closure_genpop") %>% 
    arrange(desc(value_abs)) %>%
    head(n = 10)

## ========================================================================= ##
## check correlations of measures _within_ countries
## ========================================================================= ##

# source("./r-scripts/function-library.R")

calc_melted_cormat <- function(dat) {
    cormat <- cor(
        dat,
        use = "pairwise")
    ## replace lower triangle with NA:
    upper_tri <- get_upper_tri(round(cormat, 3)) 
    # melt the correlation matrix:
    melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
    return(melted_cormat)
}

melted_cormat_percountry <- purrr::map_dfr(
    unique(dat_all_raw$countrycode),
    ~ calc_melted_cormat(dat_all_raw %>% 
                             filter(countrycode == .x) %>%
                             select(varnames_measures))
    )

melted_cormat_agg <- melted_cormat_percountry %>% 
    group_by(Var1, Var2) %>% 
    summarize(cor_mean = mean(value, na.rm = TRUE))

melted_cormat_agg %$% hist(cor_mean, nclass = 50, col = "darkgrey")

p <- ggheatmap_cust(melted_cormat_agg, "cor_mean")
# plot the heatmap
print(p)
ggsave(plot = p, 
       file = file.path(path_plots, "correlations-measures_within_countries.jpg"), 
       width = 10, height = 10)


## ========================================================================= ##
## various
## ========================================================================= ##

## check out amount of value == 0:
sapply(dat_all_raw[varnames_measures], function(i) mean(i == 0)) %>%
    hist(nclass = 50, col = "grey")
    
## check out amount of value != 0:
sapply(dat_all_raw[varnames_measures], function(i) mean(i != 0)) %>%
    hist(nclass = 50, col = "grey")
