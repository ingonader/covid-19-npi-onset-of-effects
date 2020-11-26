## ######################################################################### ##
## Materials for Manuscript
## ######################################################################### ##

# rm(list = ls(), inherits = TRUE); rstudioapi::restartSession()
# rm(list = ls(), inherits = TRUE)

## ========================================================================= ##
## load and prep data
## ========================================================================= ##

source("./r-scripts/setup.R")

# ## if only data prep part is needed, specify a date and do data prep:
# wch_date <- "2020-07-17"
# source("./r-scripts/03-data-prep.R")

## but...

## ...since this file is run after the analysis, a saved .Rdata file should be
## loaded to retrieve results from calculation run:
load(file = file.path(path_rscripts, "coronanet_withTime_withPop_training_pipeline_v008.Rdata"))

wch_date

## ========================================================================= ##
## do basic setup
## ========================================================================= ##

## after loading .Rdata file, to update function definitions!
source("./r-scripts/setup.R")
source("./r-scripts/function-library.R")


## ========================================================================= ##
## load additional packages
## ========================================================================= ##

library(ggpubr)
library(sjPlot)
library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(iml)
library(tictoc)
library(MLmetrics)


## ========================================================================= ##
## define additional global variables
## ========================================================================= ##

# ## define dataset that's used for the ML part:
# dat_ml <- bind_rows(dat_traineval, dat_test)[varnames_model]

## ========================================================================= ##
## materials for the manuscript
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## data sources
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## number of countries in the John Hopkins dataset:
dat_confirmed_raw %$% length(unique(countrycode))

## last date:
max(dat_confirmed_raw$date)
max(dat_all$date)

## number of total policies in the CoronaNet dataset:
nrow(dat_measures_coronanet_core_raw)

## number of distinct measures in the CoronaNet dataset (type):
dat_measures_coronanet_core_raw %$% length(unique(type))

## number of countries in the CoronaNet dataset:
dat_measures_coronanet_core_raw %$% length(unique(ISO_A3))

## number of countries in the analysis:
dat_all %$% length(unique(countrycode))


## measures (types) in the CoronaNet dataset:
dat_measures_coronanet_core_raw %>% group_by(type) %>% tally() %>% print(n = 30)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## basic sample description
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## number of features:
length(varnames_features)

## number of measures (NPIs):
varnames_measures_mincountries

## national vs. subnational measures:
varnames_measures_mincountries %>% grep("[^_]+__nat", ., value = TRUE)
varnames_measures_mincountries %>% grep("[^_]+__subnat", ., value = TRUE)

## mandatory vs. voluntary:
varnames_measures_mincountries %>% grep("[^_]+__[^_]+__man", ., value = TRUE)
varnames_measures_mincountries %>% grep("[^_]+__[^_]+__vol", ., value = TRUE)
varnames_measures_mincountries %>% grep("[^_]+__[^_]+__mis", ., value = TRUE)

## number of countries:
dat_all %$% unique(countrycode) %>% length()

## missing values in world bank data:
dat_wb %>% 
    filter(rowSums(is.na(dat_wb)) > 0) %>%
    filter(iso3c %in% unique(dat_all$countrycode)) %>%
    select(iso3c, country, contains("pop"), contains("gdp")) %>%
    summarize_all(
        list(
            missing = ~ sum(is.na(.x)) / length(.x)
        )
    ) %>% tidyr::pivot_longer(cols = everything())

## number of datapoints per country:
dat_all %>% group_by(countrycode) %>% summarize(n_days = n())

## number of days in total (sample size):
nrow(dat_all)

## last date:
max(dat_all$date)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## base rate in the whole data set
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## base rate  after data prep for ML (with windsorization):
dat_all %>% pull(value_rel_smooth) %>% DescTools::Gmean(conf.level = .95, na.rm = TRUE)

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## base rate in the whole dataste within countries
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## base rate  after data prep for ML (with windsorization):
## (needs to be moved to ML file, won't work before running it):
dat_all %>% group_by(countrycode) %>% 
    summarize(value_rel = DescTools::Gmean(value_rel_smooth, na.rm = TRUE)) %>%
    pull(value_rel) %>%
    summary()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## base rate for days when no measures have been implemented:
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## after data prep for ML:
dat_j <- dat_all
wch_days <- (apply(dat_j[varnames_measures_mincountries], 1, max, na.rm = TRUE) < 0)
dat_j[wch_days, ] %>% pull(value_rel_smooth) %>% DescTools::Gmean(conf.level = .95, na.rm = TRUE)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## table: model fit in train/eval/test sets
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## hyperparameter set that was chosen:
rr$data$learner[[1]]$tuning_result$learner_param_vals %>% unlist() %>% {paste0(names(.), "=", ., collapse = ", ")} %>% cat()  ## mlr3tuning 0.2.0

## results of original model on training sets:
res_train <- rr$learners[[1]]$predict(train_task)$score(measures)
res_train

## results of original model on test set:
res_test <- rr$learners[[1]]$predict(test_task)$score(measures)
res_test

## results of bootstrap models on training set:
perf_bootstrap_train <- lapply(all_models, function(model) {
    model$predict(train_task)$score(measures)
}) %>% bind_rows()
res_bootstrap_train <- perf_bootstrap_train %>% summarize_all(list(median = median, mean = mean, sd = sd))
res_bootstrap_train

## results of bootstrap models on test set:
perf_bootstrap_test <- lapply(all_models, function(model) {
    model$predict(test_task)$score(measures)
}) %>% bind_rows()
res_bootstrap_test <- perf_bootstrap_test %>% summarize_all(list(median = median, mean = mean, sd = sd))
res_bootstrap_test

## [[here]]
tab_fitresults <- bind_rows(
    c(res_train,res_bootstrap_train),
    c(res_test, res_bootstrap_test)
)
tab_fitresults <- bind_cols(
    "Dataset" = c("Training set", "Test set"),
    "n" = c(nrow(train_task$data()), 
            nrow(test_task$data())),
    "n_ctry" = c(length(in_sample_groups),
                 length(out_of_sample_groups)),
    tab_fitresults
)
tab_fitresults


tab_fitresults_ms <- tab_fitresults %>%
    mutate(
        bootstrap_rmse_meansd = paste0(
            sprintf("%.4f", regr.rmse_mean), " (", 
            sprintf("%.4f", regr.rmse_sd), ")"),
        bootstrap_rsq_meansd = paste0(
            sprintf("%.3f", regr.rsq_mean), " (", 
            sprintf("%.3f", regr.rsq_sd), ")")
    ) %>%
    select(Dataset, n, n_ctry, regr.rmse, bootstrap_rmse_meansd, regr.rsq, bootstrap_rsq_meansd)
tab_fitresults_ms

# tableHTML::tableHTML(tab_fitresults_ms, 
#                      rownames = FALSE,
#                      header = c("", "n", "RMSE", "R²")) %>%
#     add_css_column(columns = 1:4, 
#                    css = list(c('border'),
#                               c('1px solid black'))) %>%
#     write_tableHTML(file.path(path_ms, "tmp.html"))

## create word file:
sjPlot::tab_df(
    tab_fitresults_ms,
    digits = 3,
    col.header = c(
        " ", "n", "Countries", "RMSE", "Bootstrap RMSE", "R²", "Bootstrap R²"
    ), file = file.path(path_ms, "tmp-fit-stats.html")
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## table: countries in the analysis
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## create full table:
tbl_countries_ms_full <- dat_all %>% 
    group_by(country, countrycode) %>%
    summarize(
        n = n(),
        split = paste(
            unique(ifelse(countrycode %in% in_sample_groups, "training", "test")), collapse = "+"),
        scores = list(get_measures_per_group(
            first(countrycode), dat_all, "countrycode", learner, measures, add_cor = TRUE)
        ),
        .groups = "drop_last"
    ) %>%
    ungroup() %>%
    mutate(
        countrywithcode = paste0(country, " (", countrycode, ")"),
        rmse = purrr::map_dbl(scores, ~.x[[1]]),
        rsq = purrr::map_dbl(scores, ~.x[[2]])
    )

## select stuff for presentation in ms:
tbl_countries_ms <- tbl_countries_ms_full %>%
    select(countrywithcode, n, split, rmse, rsq) %>%
    mutate(
        rmse = sprintf("%.4f", rmse),
        rsq = sprintf("%.3f", rsq)
    )
tbl_countries_ms

## export to word file:
sjPlot::tab_df(
    tbl_countries_ms,
    #digits = 3,
    col.header = c(
        "Country", "n", "Dataset", "RMSE", "R²"
    ), file = file.path(path_ms, "tmp-tab-countries.html")
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## table: frequency of measures and more
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

translate_measures_cn <- function(x) {
    plyr::mapvalues(x, from = trans_measures_dat[["sanitized"]], 
                    to = trans_measures_dat[["original"]],
                    warn_missing = FALSE)
}

## table of measures:
tab_measures_base <- dat_measures_mindate_long %>% 
    ## filter: only consider countries that we have confirmed data for after filtering!
    filter(ISO_A3 %in% unique(dat_all$countrycode)) %>%
    group_by(type, init_country_level, compliance, split3) %>% 
    tally() %>%
    filter(n > n_countries_min) %>% # n_countries_min defined in 03-data-prep
    mutate(measure_name = paste0(type, 
                                 "__", init_country_level, 
                                 "__", compliance),
           measure_name = ifelse(!is.na(split3), 
                                 paste0(measure_name, "__", split3), 
                                 measure_name)
    ) %>%
    mutate(
        type = translate_measures_cn(type),
        init_country_level = plyr::mapvalues(
            init_country_level,
            from = c("nat", "subnat"),
            to = c("National", "Sub-national"),
            warn_missing = FALSE
        ),
        compliance = plyr::mapvalues(
            compliance,
            from = c("man", "vol", "miss"),
            to = c("Mandatory", "Voluntary", "(Missing)"),
            warn_missing = FALSE
        ),
        split3 = plyr::mapvalues(
            split3, 
            from = c("in", "out", "inout", "miss", NA),
            to = c("Inbound", "Outbound", "Both", "(Missing)", ""),
            warn_missing = FALSE
        )
    ) %>% 
    arrange(type, init_country_level, compliance, desc(split3)) %>%
    ungroup()
tab_measures_base

tab_measures_clean <- tab_measures_base %>%
    mutate(
        compliance = ifelse(is.na(lag(type)) | (lag(type) != type) | (lag(init_country_level) != init_country_level) | lag(compliance) != compliance, compliance, ""),
        init_country_level = ifelse(is.na(lag(type)) | (lag(type) != type) | (lag(init_country_level) != init_country_level), init_country_level, ""),
        type = ifelse(is.na(lag(type)) | (lag(type) != type), type, "")
    )
tab_measures_clean

# sjPlot::tab_df(
#     tab_measures_clean,
#     col.header = c("Measure", "Level", "Enforcement", "Direction", "Countries"),
#     file = file.path(path_ms, "tmp.html"))


## table base: measure names with number of countries implemented
#tabfreq_base <- tibble(measure_name = varnames_features)
tabfreq_base <- dat_all %>% group_by(countrycode) %>% 
    summarize_at(varnames_measures_mincountries, max, na.rm = TRUE) %>% 
    tidyr::pivot_longer(varnames_measures_mincountries, names_to = "measure_name", values_to = "maxdays") %>%
    ungroup() %>%
    group_by(measure_name) %>%
    summarize(
        n_countries_impl = sum(maxdays > -15)
    )
# tabfreq_base %>% print(n = 100)
# tabfreq_base %>% left_join(tab_measure_freq) %>% print(n = 30)

## avg. mean smoothed daily increase ratio at time of implementation:
tabfreq_ratioatimpl <- tabfreq_base %>%
    mutate(
        mean_ratio_at_impl = get_mean_increase_ratio(
            dat_all, measure_name, at = 0, retval = "mean"
        ),
        n_mean = get_mean_increase_ratio(
            dat_all, measure_name, at = 0, retval = "n"
        ) %>% as.integer(),
        ci_lower = get_mean_increase_ratio(
            dat_all, measure_name, at = 0, retval = "lwr.ci"
        ),
        ci_upper = get_mean_increase_ratio(
            dat_all, measure_name, at = 0, retval = "upr.ci"
        )
    ) %>%
    arrange(desc(mean_ratio_at_impl))

# tabfreq_ratioatimpl
# tabfreq_ratioatimpl %>% arrange(desc(mean_ratio_at_impl)) %>% print(n = 55)


# ## drop in ALE plot in first 28 days:
# tabfreq_eff <- dat_effs_measureeffects %>% 
#     filter(dataset == "test",
#            .type == "ale") %>%
#     select(measure_name, measure_effect_disp)

## join frequency and mean ratio tables to table with "beautified" measure names:
tabfreq_ratioatimpl <- tab_measures_clean %>% 
    left_join(tabfreq_ratioatimpl,
              by = c("measure_name")
    )


## create modification for ms:
tabfreq_ratioatimpl_ms <- tabfreq_ratioatimpl %>%
    mutate_if(is.double, format_float, comma_char = ".") %>%
    mutate(
        ci = paste0(
            "(", ci_lower, "–", ci_upper, ")"
            )
    ) %>%
    select(type, init_country_level, compliance, split3, n_countries_impl, mean_ratio_at_impl, ci, n_mean) 
tabfreq_ratioatimpl_ms

## create word file:
sjPlot::tab_df(
    tabfreq_ratioatimpl_ms,
    digits = 3,
    col.header = c(
        "Measure", "Level", "Enforcement", "Direction",
        "Countries implemented",
        "Mean growth rate",
        "CI",
        "n" 
    ), file = file.path(path_ms, "tmp-npis.html")
)

## CHECK:
## why are there sometimes so few values for n_mean?

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Figure: selected predictions for specific countries
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

dim(pred_all)
names(pred_all)
head(pred_all)
table(pred_all$itr_n)

## create table to join country names to country codes:
tab_ctrynames <- dat_all_raw %>% group_by(country, countrycode) %>% summarize(.groups = "drop_last")

# ## npg colors:
# plot(1:10, pch = 15, col = ggsci::pal_npg()(10))

plot_ctry_panel <- function(wch_ctry_pred) {
    pred_all_sel <- pred_all %>% 
        filter(countrycode %in% wch_ctry_pred) %>%
        left_join(tab_ctrynames, by = "countrycode") %>%
        group_by(countrycode) %>%
        mutate(
            facet_title = paste0(
                country, " (", countrycode, ", ", 
                ifelse(first(countrycode) %in% in_sample_groups, "training set", "test set"), ")")
        ) %>%
        ungroup()
    
    ctry_ylims <- c(1, max(c(pred_all_sel$value_rel_smooth, pred_all_sel$prediction), na.rm = TRUE))
    ctry_xlims <- range(pred_all_sel$date, na.rm = TRUE)
    
    pred_all_sel %>%
        ggplot() +
        geom_line(aes(date, prediction, group=itr_n, color=sample_type), alpha = 0.2) +
        geom_line(
            data = pred_all_sel %>% 
                filter(itr_n == 1),
            aes(date, value_rel_smooth, color = 'Truth')
        ) +
        facet_wrap(
            vars(facet_title),
            strip.position = "top",
            scales = "free"
        ) + 
        labs(
            y = "Growth rate",
            x = "Time"
        ) +
        scale_color_manual(
            values = c("InSample" =    ggsci::pal_npg()(10)[5], #"#d95f02",    ## colorbrewer 6-class dark2: dark orange
                       "OutOfSample" = ggsci::pal_npg()(10)[6], #"#7570b3", ## colorbrewer 6-class dark2: dark purple
                       "Truth" = "black"),
            labels = c("InSample" = "Prediction, bootstrap sample\ncontaining country",
                       "OutOfSample" = "Prediction, bootstrap sample\nnot containing country",
                       "Truth" = "Growth rate (truth)")
            #guide = guide_legend(ncol = 1)
        ) +
        # scale_color_brewer(palette = "Dark2") +
        scale_x_date_npg(limits = ctry_xlims) +
        scale_y_continuous_npg(limits = ctry_ylims) +
        theme_npg() +
        theme(legend.title = element_blank(), 
              legend.position = "bottom",
              #legend.position = c(0.95, 0.99), # c(0,0) left bottom, c(1,1) right top 
              #legend.justification = c(1, 1),
              strip.placement = "outside",
              strip.background = element_rect(fill = "white"),
              #strip.text.y = element_text(angle = 0, hjust = 0, vjust = 0),
              strip.text.x = element_text(angle = 0, hjust = 0, vjust = 0),
              legend.background = element_rect(fill = "white", colour = NA)
        )
}

wch_ctry_pred <- c("CYP", "CUB", "IRQ", "HUN", "ALB", "DZA", "MKD", "ISL", "CIV")
plot_ctry_panel(wch_ctry_pred)
ggsave(filename = file.path(path_ms, "fig_ctrypred_good_v003.jpg"), width = 10, height = 7, dpi = 300)

# wch_ctry_pred <- c("AGO", "AZE", "BGR", #"LVA", "AND", 
#                    "LBN", "SRB", "VNM", "UZB", "DJI", "CAN", "KWT", "USA", "KHM", "DEU", "BRN", "NPL",
#                    "SGP", "STP", "RUS", "NAM", "LKA", "ETH", "BWA")
wch_ctry_pred <- c("AZE", "BWA", "BRN", 
                   "KHM", "KWT", "NPL", 
                   "RUS", "SGP", "USA")
plot_ctry_panel(wch_ctry_pred)
ggsave(filename = file.path(path_ms, "fig_ctrypred_bad_v003.jpg"), width = 10, height = 7, dpi = 300)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## sorting measures by drop in growth rate over measure implementation
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

eff_all_feat_df <- data.table::rbindlist(eff_all_feat)

eff_all_feat_df

## calculate difference between ALE plots at different time points (for median):
dat_effs_measureeffects <- eff_all_feat_df %>% 
    filter(.type == "ale") %>%
    filter(!grepl("^ctry_|^time_", measure_name)) %>%
    group_by(measure_name) %>% 
    summarize(
        value_median_tm1 = mean(
            ## which(...): index of closest value of .x to -7 (one or two values):
            value_median[which(abs(.x - (-7)) == min(abs(.x - (-7))))]
        ),
        value_median_t0 = mean(
            ## which(...): index of closest value of .x to 0 (one or two values):
            value_median[which(abs(.x - 0) == min(abs(.x - 0)))]
        ),
        value_median_t1 = mean(
            ## which(...): index of closest value of .x to 60 (one or two values):
            value_median[which(abs(.x - 60) == min(abs(.x - 60)))]
        ),
        measure_effect = value_median_t1 - value_median_t0,
        ## for display reasons: large reduction -> large effect:
        measure_effect_disp = -measure_effect
    ) %>% 
    ungroup()

## check:
dat_effs_measureeffects %>% arrange(measure_effect)

## get most important measures (sorted by median reduction):
varnames_measures_sorted <- dat_effs_measureeffects %>% arrange(measure_effect) %>% pull(measure_name)
varnames_measures_sorted[1:8]

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## plots of measures (and other features)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## setup
## ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ##

## NOTE: need to execute block above, to get sorted NPI/measures names:

## define variable names of plots:
varnames_ctry <- varnames_features[grep('ctry', varnames_features)]
varnames_time <- varnames_features[grep('time', varnames_features)]
# varnames_features[grep('restriction_mass_gatherings', varnames_features)]
varnames_measures_wch <- varnames_measures_sorted[1:4]  #[1:8] ## [[DEVEL]]: Shorten number of varnames to reduce # of plots

## get ylim values:
yvalues <- eff_all_feat_df[measure_name %in% varnames_measures_mincountries, .value]  ## varnames_measures_wch or varnames_measures_mincountries?
ylims <- range(yvalues, na.rm = TRUE)
ctry_yvalues <- eff_all_feat_df[measure_name %in% varnames_ctry, .value]
ctry_ylims <- range(ctry_yvalues, na.rm = TRUE)
time_yvalues <- eff_all_feat_df[measure_name %in% varnames_time, .value]
time_ylims <- range(time_yvalues, na.rm = TRUE)


## generate basic plots of all types
## ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ##

## number of plots per page (for supplement):
nplt <- 15

## generate basic plots:
plotlist_npi_ms <-         varnames_measures_wch %>%                       produce_ale_list()
plotlist_npi_suppl_pt01 <- tab_measures_base$measure_name[(1:nplt)+nplt*0] %>% produce_ale_list()
plotlist_npi_suppl_pt02 <- tab_measures_base$measure_name[(1:nplt)+nplt*1] %>% produce_ale_list()
plotlist_npi_suppl_pt03 <- tab_measures_base$measure_name[(1:nplt)+nplt*2] %>% produce_ale_list()
#plotlist_npi_suppl_pt04 <- tab_measures_base$measure_name[(1:nplt)+nplt*3] %>% produce_ale_list()
plotlist_npi_suppl_pt04 <- tab_measures_base$measure_name[(1+nplt*3):nrow(tab_measures_base)] %>% produce_ale_list()
plotlist_timeeff <-        varnames_time %>%                               produce_ale_list()
plotlist_ctryeff <-        varnames_ctry %>%                               produce_ale_list()


## modify basic plots of all types
## ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ##

## modify basic plots -- NPIs / measure effects:
modify_npi_plots <- function(i) {
    i %<>% lapply(function(p)
        p + 
            xlim(-14, 100) + ## actually exclude -15
            coord_cartesian(xlim = c(-14, 60), ylim = ylims) + ## plot only to 60
            scale_x_continuous_npg(breaks = seq(-10, 60, 10)) +
            scale_y_continuous_npg(breaks = seq(-0.01, 0.015, 0.005))
            # rremove("ylab")
            # rremove("xlab") +
    )
    return(i)
} 
plotlist_npi_ms[["plots"]] %<>%         modify_npi_plots()
plotlist_npi_suppl_pt01[["plots"]] %<>% modify_npi_plots()
plotlist_npi_suppl_pt02[["plots"]] %<>% modify_npi_plots()
plotlist_npi_suppl_pt03[["plots"]] %<>% modify_npi_plots()
plotlist_npi_suppl_pt04[["plots"]] %<>% modify_npi_plots()
# plotlist_npi_suppl_pt05[["plots"]] %<>% modify_npi_plots()
#plot(plotlist_npi_ms[["plots"]][[1]])
#plot(plotlist_npi_suppl_pt01[["plots"]][[1]])

## modify basic plots -- time effects:
plotlist_timeeff[["plots"]] %<>% lapply(function(p)
    p +
        coord_cartesian(ylim = time_ylims) +
        scale_x_continuous_npg(breaks = seq(-10, 120, 10)) +
        scale_y_continuous_npg(breaks = seq(-0.01, 0.06, 0.01))
)

## modify basic plots -- country-specific covariates:
plotlist_ctryeff[["plots"]] %<>% lapply(function(p)
    p +
        coord_cartesian(ylim = ctry_ylims) +
        scale_y_continuous_npg()
)
plotlist_ctryeff[["plots"]][["ctry_pop65perc"]] %<>%        {. + scale_x_continuous_npg(breaks = seq(0, 25, 5))}
plotlist_ctryeff[["plots"]][["ctry_popurbanperc"]] %<>%     {. + scale_x_continuous_npg(breaks = seq(10, 100, 10))}
plotlist_ctryeff[["plots"]][["ctry_poppollutionperc"]] %<>% {. + scale_x_continuous_npg(breaks = seq(0, 100, 10))}
plotlist_ctryeff[["plots"]][["ctry_gdppcppp_trans"]] %<>%   {. + scale_x_continuous_npg(breaks = seq(6, 12, 0.5))}


## save panel figures
## ''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''''' ##

## arrange plots and save:
postfix_version <- "_v003"
plotlist_npi_ms %>%         arrange_and_save_ale_list(filename = file.path(path_ms, paste0("fig_effects_npi_ms", postfix_version, ".jpg")), ncol = 2)
plotlist_npi_suppl_pt01 %>% arrange_and_save_ale_list(filename = file.path(path_ms, paste0("fig_effects_npi_suppl_pt01", postfix_version, ".jpg")), ncol = 3)
plotlist_npi_suppl_pt02 %>% arrange_and_save_ale_list(filename = file.path(path_ms, paste0("fig_effects_npi_suppl_pt02", postfix_version, ".jpg")), ncol = 3)
plotlist_npi_suppl_pt03 %>% arrange_and_save_ale_list(filename = file.path(path_ms, paste0("fig_effects_npi_suppl_pt03", postfix_version, ".jpg")), ncol = 3)
plotlist_npi_suppl_pt04 %>% arrange_and_save_ale_list(filename = file.path(path_ms, paste0("fig_effects_npi_suppl_pt04", postfix_version, ".jpg")), ncol = 3)
# plotlist_npi_suppl_pt05 %>% arrange_and_save_ale_list(filename = file.path(path_ms, paste0("fig_effects_npi_suppl_pt05", postfix_version, ".jpg")), ncol = 3)
plotlist_timeeff %>%        arrange_and_save_ale_list(filename = file.path(path_ms, paste0("fig_effects_time", postfix_version, ".jpg")), ncol = 2)
plotlist_ctryeff %>%        arrange_and_save_ale_list(filename = file.path(path_ms, paste0("fig_effects_ctry", postfix_version, ".jpg")), ncol = 2)


# ## develop:
# plotlist_npi_ms <-         varnames_measures_wch %>%                       produce_ale_list()
# plotlist_npi_ms[["plots"]] %<>%         modify_npi_plots()
# plot(plotlist_npi_ms[["plots"]][[1]])
# #ggsave(plot = plotlist_npi_ms[["plots"]][[1]], filename = file.path(path_ms, "tmp.jpg"), width = 5, height = 3, unit = "in", dpi = 600, scale = 2)
# ggsave(plot = plotlist_npi_ms[["plots"]][[1]], filename = file.path(path_ms, "tmp.jpg"), width = 5, height = 3, unit = "in", dpi = 600)
# plotlist_npi_ms %>%         arrange_and_save_ale_list(filename = file.path(path_ms, "fig_effects_npi_ms.jpg"), ncol = 2)
# 
# 
# # p <- plotlist_npi_ms[["plots"]][[1]]
# # p <- p + theme(
# #     axis.text = element_text(size = 8),  ## axes tick marks
# #     plot.title = element_text(size = 18), 
# #     axis.title = element_text(size = 12),
# #     legend.text = element_text(size = 12)
# # )
# # p
# 
# ## NOTE: 
# ## Font sizes (Title, ylab, xlab, tick mark text, legend) are overly large when plot is produced
# ## on DS Lab Server (instead of locally)

# ## specific measures:
# eff_all_feat_df[measure_name %in% varnames_measures_mincountries, ]
# eff_all_feat_df

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## timing of measures
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## get date when confirmed (cum) cases have reached a certain threshold:
n_cases_threshold <- 25
dat_confirmed_threshold <- dat_confirmed %>% filter(value >= n_cases_threshold) %>% 
    group_by(countrycode) %>%
    arrange(date) %>%
    summarize_all(
        first
    ) %>% 
    ungroup()


## how many contries have implemented a measure? (similar to tabfreq_base above)
tabfreq_nctry <- dat_all %>% group_by(countrycode) %>% 
    summarize_at(varnames_measures_mincountries, max, na.rm = TRUE) %>% 
    tidyr::pivot_longer(varnames_measures_mincountries, names_to = "measure_name", values_to = "maxdays") %>%
    ungroup() %>%
    group_by(measure_name) %>%
    summarize(
        n_countries_impl = sum(maxdays > -15)
    )
## join date when threshold was reached to measures data:
dat_measures_timing <- dat_measures_mindate_long %>% 
    filter(measure_name %in% varnames_features) %>%
    left_join(
        dat_confirmed_threshold %>%
            select(countrycode, date, value) %>%
            rename("date_cases_reached_threshold" = "date"),
        by = c("ISO_A3" = "countrycode")) %>%
    mutate(
        days_implemented_after_threshold = as.numeric(
            (lubridate::date(date_min) - lubridate::date(date_cases_reached_threshold)), 
            unit = "days")
    ) %>% 
    filter(!is.na(days_implemented_after_threshold)) %>%
    left_join(
        tabfreq_nctry, by = "measure_name"
    )

dat_measures_timing
dat_measures_timing %>% group_by(measure_name) %>%
    summarize(
        day_median = median(days_implemented_after_threshold)
    ) %>%
    arrange(day_median) %>%
    print(n = 55)

## also show ratio at implementation (defined above):
dat_measures_timing %>% group_by(measure_name) %>%
    summarize(
        day_median = median(days_implemented_after_threshold)
    ) %>%
    #arrange(day_median) %>%
    left_join(tabfreq_ratioatimpl) %>%
    select(measure_name, day_median, mean_ratio_at_impl, n, n_mean) %>%
    print(n = 55)

## modify for plotting two axes (npg):
dat_measures_timing <- dat_measures_timing %>%
    mutate(
        y = forcats::fct_reorder(
            measure_name,
            days_implemented_after_threshold, median, 
            na.rm = TRUE,
            .desc = TRUE
        ),
        y_int = as.integer(y)
    )
y_labs <- clean_txt(levels(dat_measures_timing$y))

# ## npg colors:
# plot(1:10, pch = 15, col = ggsci::pal_npg()(10))

## make plot:
ggplot(
    dat_measures_timing,
    aes(
        x = days_implemented_after_threshold, 
        y = y_int,
        group = y_int,  ## needed if y is numeric 
        fill = n_countries_impl
    )) + 
    geom_boxplot(varwidth = FALSE) +  
    geom_vline(xintercept = 0, color = "darkgrey", linetype = "dashed") +
    labs(
        x = paste0("Days after ", n_cases_threshold, " cases have been reached"),
        y = "",
        fill = "Number of\nCountries"
    ) +
    # scale_fill_distiller(
    #     type = "seq",
    #     palette = 5,
    #     direction = 1
    # ) + 
    # scale_fill_gradient(
    #     # high = ggsci::pal_npg()(10)[8], low = ggsci::pal_npg()(10)[5]
    #     # high = ggsci::pal_npg()(10)[3], low = ggsci::pal_npg()(10)[7]
    #     high = ggsci::pal_npg()(10)[9], low = ggsci::pal_npg()(10)[10]  ## browns look kind of okay
    # ) +
    scale_fill_gradient2(
        high = ggsci::pal_npg()(10)[9], mid = "white", low = "black", midpoint = 0
    ) +
    scale_x_continuous_npg(
        breaks = seq(-150, 150, 25)
    ) + 
    scale_y_continuous_npg(
        breaks = 1:length(y_labs),
        labels = y_labs
    ) +
    coord_cartesian(
        ylim = c(1 + 1.5, length(y_labs) - 1.5)  ## needed if y is an integer (continuous) instead of discrete
    ) +
    theme(
        axis.text = element_text(size = 6),  ## axes tick marks
        plot.title = element_text(size = 10), 
        axis.title = element_text(size = 8),
        legend.text = element_text(size = 8)
    ) +
    theme_npg()
    

# ggsave(filename = file.path(path_plots, "fig_measures_timing_v001.pdf"), width = 10, height = 10)
# ggsave(filename = file.path(path_plots, "fig_measures_timing_v001.jpg"), width = 10, height = 10)

ggsave(filename = file.path(path_ms, "fig_measures_timing_v003.pdf"), 
       width = 107, height = 107, units = "mm", dpi = 400, scale = 2)
ggsave(filename = file.path(path_ms, "fig_measures_timing_v003.jpg"),
       width = 107, height = 107, units = "mm", dpi = 400, scale = 2)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## correlation of measures and correlation of predictors
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## ------------------------------------------------------------------------- ##
## correlations of measures _within_ countries
## ------------------------------------------------------------------------- ##

calc_melted_cormat <- function(dat, removediag = FALSE) {
    cormat <- cor(
        dat,
        use = "pairwise")
    ## replace diagonal with NA, if needed:
    if (removediag) diag(cormat) <- NA
    ## replace lower triangle with NA:
    upper_tri <- get_upper_tri(round(cormat, 3))
    # melt the correlation matrix:
    melted_cormat <- reshape2::melt(upper_tri, na.rm = TRUE)
    return(melted_cormat)
}

melted_cormat_percountry <- purrr::map_dfr(
    unique(dat_all$countrycode),
    ~ calc_melted_cormat(dat_all %>% 
                             filter(countrycode == .x) %>%
                             select(varnames_features),
                         removediag = TRUE)
)
## warnings mean that two measures were implemented at the same within a country,
## or not at all in that country. So warnings are to be expected.

melted_cormat_agg <- melted_cormat_percountry %>% 
    group_by(Var1, Var2) %>% 
    summarize(cor_mean = mean(value, na.rm = TRUE))

melted_cormat_agg %$% summary(cor_mean)

#melted_cormat_agg %$% hist(cor_mean, nclass = 50, col = "darkgrey")
#melted_cormat_percountry %$% hist(value, nclass = 50, col = "darkgrey")

melted_cormat_agg %>%
    ggplot(aes(cor_mean)) + 
    geom_histogram(bins = 80) + 
    labs(
        x = "Correlation of measures within countries, averaged over countries",
        y = "Count"
    ) + 
    theme_cust()
ggsave(file = file.path(path_plots, "fig_correlations_hist_measures_within_countries_v001.jpg"),
       width = 107, height = 50, units = "mm", dpi = 600, scale = 2)

# p <- ggheatmap_cust(melted_cormat_agg, "cor_mean")
# # plot the heatmap
# print(p)
# ggsave(plot = p, 
#        file = file.path(path_plots, "correlations-measures_within_countries.jpg"), 
#        width = 10, height = 10)


## ------------------------------------------------------------------------- ##
## correlations of measures _across_ countries (features in the model)
## ------------------------------------------------------------------------- ##

melted_cormat_features <- calc_melted_cormat(dat_all %>% select(varnames_features),
                                             removediag = TRUE)
melted_cormat_features %$% summary(value)

melted_cormat_features %>%
    ggplot(aes(value)) + 
    geom_histogram(bins = 80) + 
    labs(
        x = "Correlation of measures across countries",
        y = "Count"
    ) + 
    theme_cust()
ggsave(file = file.path(path_plots, "fig_correlations_hist_predictors_v001.jpg"),
       width = 107, height = 50, units = "mm", dpi = 600, scale = 2)

    # p <- ggheatmap_cust(melted_cormat_features, "value")
# # plot the heatmap
# print(p)

# * diagonal removed

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## ranking the measures by date, within countries
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## order of measures that are in the ML model:
dat_rank <- dat_measures_mindate_long %>% 
    filter(measure_name %in% varnames_features) %>%
    group_by(iso, country) %>% 
    arrange(iso, date_min) %>% mutate(
        date_rank = rank(date_min)
    ) %>% ungroup() %>%
    group_by(measure_name) %>%
    mutate(
        n_countries = length(measure_name)
    ) %>% 
    ungroup()

ggplot(
    dat_rank,
    aes(
        x = date_rank, 
        y = forcats::fct_reorder(
            translate_measures(measure_name),
            date_rank, median, 
            na.rm = TRUE,
            .desc = TRUE),
        fill = n_countries)
    ) + 
    geom_boxplot(varwidth = FALSE) +  
    labs(
        x = paste0("Rank of measure within countries"),
        y = "",
        fill = "Number of\nCountries"
    ) +
    scale_fill_distiller(
        type = "seq",
        palette = 1,
        direction = 1
    ) + 
    theme_cust()
ggsave(filename = file.path(path_plots, "fig_measures_ranking_by_date_v001.jpg"), 
       width = 107, height = 107 * 0.8, units = "mm", dpi = 600, scale = 2)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## number of measures per country
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## ------------------------------------------------------------------------- ##
## create dataset
## ------------------------------------------------------------------------- ##

dat_measures_per_ctry <- dat_all %>% 
    group_by(countrycode, country) %>% 
    summarize_at(varnames_features, 
                 ~ as.integer(max(.x) > 0)) %>%
    ungroup() %>%
    {
        mutate(., 
               n_measures = rowSums(.[varnames_features]))
        
    }

## summary:
dat_measures_per_ctry[["n_measures"]] %>% summary()
dat_measures_per_ctry %>% 
    ggplot(aes(n_measures)) + 
    geom_histogram(bins = 20)

## check a few special cases:
dat_measures_per_ctry %>% filter(n_measures == 1)

## ------------------------------------------------------------------------- ##
## measures in place with schools closure
## ------------------------------------------------------------------------- ##

## not necessarily at the same time, but in the same country:
dat_measures_per_ctry %>% filter(schools_closure_genpop == 1) %$% summary(n_measures)

## at the same time:
dat_measures_with_schools_closure <- dat_all %>% 
    filter(schools_closure_genpop >= 0) %>%
    group_by(countrycode, country) %>%
    summarize_at(varnames_features, 
                 ~ as.integer(max(.x) > 0)) %>%
    ungroup() %>%
    {
        mutate(., 
               n_other_measures = rowSums(.[varnames_features]) - 1)
        
    }

## summary:
dat_measures_with_schools_closure[["n_other_measures"]] %>% summary()

## most common measures that were in place with schools closure:
dat_measures_with_schools_closure %>% 
    summarize_at(varnames_features, sum) %>%
    tidyr::pivot_longer(varnames_features, names_to = "measure_name", values_to = "count") %>%
    arrange(desc(count))


## ------------------------------------------------------------------------- ##
## measures in place with x
## ------------------------------------------------------------------------- ##

measures_in_place_with <- function(m) {
    quo_m <- enquo(m)
    dat_measures_with_x <- dat_all %>% 
        filter(!! quo_m >= 0) %>%
        group_by(countrycode, country) %>%
        summarize_at(varnames_features, 
                     ~ as.integer(max(.x) > 0)) %>%
        ungroup() %>%
        {
            mutate(., 
                   n_other_measures = rowSums(.[varnames_features]) - 1)
            
        }
    ret <- list(
        "per_country" = dat_measures_with_x,
        "other_measures" = dat_measures_with_x %>% 
            summarize_at(varnames_features, sum) %>%
            tidyr::pivot_longer(varnames_features, names_to = "measure_name", values_to = "count") %>%
            arrange(desc(count))
        
    )
    return(ret)
}
measures_in_place_with(schools_closure_genpop)$other_measures
measures_in_place_with(limit_public_gatherings_genpop)$other_measures
measures_in_place_with(public_services_closure_genpop)$other_measures

## summary:
dat_measures_with_limit_public_gatherings[["n_other_measures"]] %>% summary()

## most common measures that were in place with schools closure:





