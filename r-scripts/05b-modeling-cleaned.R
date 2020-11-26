## ######################################################################### ##
## Modeling
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
if (!exists("wch_date")) wch_date <- "2020-07-09"
source("./r-scripts/03-data-prep.R")

## ========================================================================= ##
## load additional packages
## ========================================================================= ##

library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(tictoc)
library(scales)
library(iml)


## ========================================================================= ##
## define additional global variables
## ========================================================================= ##

## parallel computing:
n_cores <- parallel::detectCores()
future::plan("multiprocess", workers = min(n_cores, 100)) 
# future::plan("sequential")

# ## logging (doesn't seem to work, can't get json output):
# requireNamespace("lgr")
# ## get the logger as R6 object:
# logger <- lgr::get_logger("mlr3")
# tf <- tempfile("mlr3log_", fileext = ".json", tmpdir = path_logs) 
# ## add json appender:
# logger$add_appender(lgr::AppenderJson$new(file = tf), name = "json")
# #logger$appenders$json$set_threshold(NA) # "info" or NA
# 
# # ## remove logger:
# # logger$remove_appender("json"); unlink(tf)
# 
# #logger$appenders$json$show()

## ========================================================================= ##
## additional function definitions
## ========================================================================= ##

## function to modify feature effect plots:
eff_plot_mod <- function(p) {
    p <- p + 
        xlim(c(-14, 40)) +
        geom_vline(xintercept = 0, color = "darkgrey", linetype = "dashed") +
        labs(
            x = paste0(
                translate_measures(varnames_imp[i]), 
                ": Days implemented"
            )
        ) +
        theme_cust()
    return(p)
}

#' function to add feature effect plot data to data.frame that collects 
#' that data for a combined plot:
#' @param dat_collected data frame that effs data should be appended to 
#' @param effs object returned by iml::FeatureEffect$new()
add_effs_data <- function(dat_collected, effs) {
    ## extract results data:
    dat_effs_i <- as_tibble(effs$results)
    ## find variable name: not starting with "." (first for pdp, last for ale):
    wch_varname <- which(!grepl("^\\.", names(dat_effs_i)))
    varname <- names(dat_effs_i)[wch_varname] 
    names(dat_effs_i)[wch_varname] <- ".x"
    dat_effs_i[["measure_name"]] <- varname
    dat_collected <- bind_rows(dat_collected, dat_effs_i)
}

## extract the rug data from a feature effect plot
## (contains all features of the dataset at hand)
get_rug_data <- function(effs) {
    rug.dat <- effs$.__enclos_env__$private$sampler$get.x()
    rug.dat$.id <- NA
    rug.dat$.type <- effs$results$.type[1]
    rug.dat$.value <- effs$results$.value[1]
    rugdata_wide <- rug.dat %>% as_tibble()
    rugdata_long <- rugdata_wide %>% tidyr::pivot_longer(
        cols = varnames_features, 
        names_to = "measure_name", 
        values_to = ".x")
    return(rugdata_long)
}


## ========================================================================= ##
## ML 
## ========================================================================= ##

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## define features and target
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## select target variable:
#varnames_target <- "value_rel"
varnames_target <- "value_rel_smooth"

## select features:
# varnames_features <- setdiff(
#     names(dat_all),
#     varnames_target)
varnames_features <- c(
    varnames_measures_mincountries, 
    "time_abs", "time_rel",
    "pop65perc", "popdensity")

varnames_features
length(varnames_features)

## combine:
varnames_model <- union(varnames_target, varnames_features)

## select subset of data:
dat_model <- dat_traineval[varnames_model]

# ## for testing and calculating stats on ml sample lateron:
dat_ml <- bind_rows(dat_traineval, dat_test)[varnames_model]
 
# purrr::map_int(dat_ml[varnames_features], ~ sum(.x == 0)) %>% {.[. < 15]}
# # purrr::map_int(dat_ml[varnames_features], ~ sum(.x == 10))
# 
# bind_cols(
#     name = varnames_features,
#     nm2 = purrr::map_int(dat_ml[varnames_features], ~ sum(.x == -13)),
#     nm1 = purrr::map_int(dat_ml[varnames_features], ~ sum(.x == -10)),
#     n0 = purrr::map_int(dat_ml[varnames_features], ~ sum(.x == 0)),
#     n1 = purrr::map_int(dat_ml[varnames_features], ~ sum(.x == 10)),
#     n1 = purrr::map_int(dat_ml[varnames_features], ~ sum(.x == 20)),
#     n1 = purrr::map_int(dat_ml[varnames_features], ~ sum(.x == 30)),
#     n1 = purrr::map_int(dat_ml[varnames_features], ~ sum(.x == 40)),
#     n2 = purrr::map_int(dat_ml[varnames_features], ~ sum(.x == 50))
# ) %>% 
#     {.[.$n0 < 15, ]} %>% 
#     print(n = 50)
# 
# dat_ml$changes_in_prison_related_policies_targeted %>% table()
# dat_measures_mindate_long %>% filter(
#     measure_name == "changes_in_prison_related_policies_targeted") %>%
#     print(n = 20)
# dat_all %>% group_by(countrycode) %>% summarize(n = n()) %>% arrange(n) %>% print(n = 20)
# # dat_measures_mindate_long %>%
# 
# dat_all$additional_health_documents_requirements_upon_arrival_targeted %>% table() %>% {.[3:40]} %>% {any(. >= 15)}
# dat_all$border_checks_genpop %>% table() %>% {.[3:40]}# %>% {any(. >= 30)}

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## define tasks
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## create a task: (= data + meta-information)

task_valuerel <- TaskRegr$new(id = "value_rel_smooth", #"value_rel",
                                backend = dat_model,
                                target = varnames_target
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## define estimatation: inner and outer resampling
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## number of evaluation runs in inner resampling (CV):
n_evals <- 5
## number of runs for random parameter search:
batch_size <- 100

# mlr_learners
# lrn("regr.ranger")$param_set

learner <- lrn("regr.ranger")
#learner <- lrn("regr.xgboost")
resampling_inner <- rsmp("cv")
measures <- list(
    msr("regr.rmse"),
    msr("regr.rsq")
)
param_set <- paradox::ParamSet$new(
    params = list(
        paradox::ParamInt$new("mtry", lower = 2, upper = 10), #length(varnames_features)),
        paradox::ParamInt$new("min.node.size", lower = 50, upper = 250),
        paradox::ParamInt$new("num.trees", lower = 100, upper = 800)
    )
)
# param_set <- paradox::ParamSet$new(
#     params = list(
#         paradox::ParamInt$new("max_depth", lower = 2, upper = 10),
#         paradox::ParamDbl$new("eta", lower = 0.01, upper = 0.4),
#         paradox::ParamInt$new("nrounds", lower = 5, upper = 400)  ##[[?]]
#     )
# )
terminator <- term("evals", n_evals = n_evals) ## number of iterations
tuner <- tnr("random_search", batch_size = batch_size)
at <- AutoTuner$new(learner, resampling_inner, measures = measures,
                    param_set, terminator, tuner = tuner)
#at$store_tuning_instance = TRUE

resampling_outer <- rsmp("holdout", ratio = ratio_train_eval)
#resampling_outer$help()

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## estimate model with defined resampling
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

# progressr::with_progress({
rr <- resample(task = task_valuerel, 
               learner = at, 
               resampling = resampling_outer,
               store_models = TRUE)
#}, handlers = progressr::handlers("progress"))

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## check model results
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## get datasets retrospectively from mlr3 outer resampling:
dat_train <- dat_traineval[rr$resampling$instance$train, ]
dat_eval <-  dat_traineval[rr$resampling$instance$test, ]

## results on training sets:
rr$learners[[1]]$learner$predict(
    task = task_valuerel,
    row_ids = rr$resampling$instance$train
    )$score(measures)
#at$predict(task = task_valuerel) # Error: Cannot predict, Learner 'regr.ranger.tuned' has not been trained yet

## [[todo]]
## this doesn't actually give the performance measures on the training sets of
## the internal resampling (CV), which would be great to have to assess
## overfitting


## results on eval set:
rr$aggregate(measures)
# ## or:
# rr$learners[[1]]$learner$predict(
#     task = task_valuerel,
#     row_ids = rr$resampling$instance$test  ## eval set defined above is the test set for the resampling
# )$score(measures)


## results on test set:
dat_this <- dat_test
c(
    "rmse" = mlr3measures::rmse(
        truth = dat_this[[varnames_target]],
        response = predict(rr$learners[[1]]$learner, newdata = dat_this)),
    "rsq" = mlr3measures::rsq(
        truth = dat_this[[varnames_target]],
        response = predict(rr$learners[[1]]$learner, newdata = dat_this))
)


## various stuff:
# rr$aggregate(msr("regr.rsq"))
# rr$aggregate(msr("regr.rmse"))
# rr$score()

## results on full dataset:
rr$learners[[1]]$learner$predict(task = task_valuerel)$score(measures)

## ========================================================================= ##
## inspect best model using iml package
## ========================================================================= ##

## set repetitions for variable importance:
n_reps_varimp <- 2 #100

## select data for model inspection; use training data here:
## (and possibly take sample for quicker model exploration):
# set.seed(442)
# dat_iml <- dat_model  %>% sample_n(50)
# dat_iml <- dat_eval[varnames_model]
# dat_iml <- dat_test[varnames_model]

set.seed(442)
dat_list <- list(
    "train" = dat_model,
    "eval" = dat_eval[varnames_model],
    "test" = dat_test[varnames_model]
)

i_datlist <- 3
## reset lists that collect data:
dat_effs_list <- vector(mode = "list")
dat_rug_list <- vector(mode = "list")
varimp_list <- vector(mode = "list")
for (i_datlist in c(3)) {  #seq_along(dat_list)) {  ## only use test set
    dat_iml <- dat_list[[i_datlist]]
    predictor_ranger <- Predictor$new(
        ## use learner from outer resampling:
        model = rr$learners[[1]]$learner,
        data = dat_iml %>% select(-varnames_target),
        y = dat_iml[varnames_target]
    )
    
    ## "choose" a standard predictor to be used below:
    predictor <- predictor_ranger
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    ## feature importance: main effects
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    

    # ## most important features:
    # set.seed(44126)
    # tic("time: feature importance")
    # varimp <- FeatureImp$new(predictor, loss = "rmse", n.repetition = n_reps_varimp)
    # toc()
    ## timing for no distinction of targeted and non-targeted measures, n_reps = 20:
    ## time: feature importance: 1561.029 sec elapsed (importance-train.jpg)
    ## time: feature importance:  505.555 sec elapsed (importance-eval.jpg)
    ## time: feature importance:  510.043 sec elapsed (importance-test.jpg)
    ## timing for separate targeted and non-targeted measures (targeted / genpop), 54 predictors, n_reps = 20):
    ## time: feature importance: 4377.322 sec elapsed (importance-train.jpg)
    ## time: feature importance: 1482.829 sec elapsed (importance-eval.jpg)
    ## time: feature importance: 1497.255 sec elapsed (importance-test.jpg)
    ## timing with sep. targeted/genpop measures, 45/10/45, split correctly via mlr; n_reps_varimp = 20
    ## time: feature importance: 3504.157 sec elapsed (train)
    ## time: feature importance: 1777.745 sec elapsed (eval)
    ## time: feature importance: 3522.764 sec elapsed (test)
    ## timing with sep. targeted/genpop measures, 45/10/45, split correctly via mlr; n_reps_varimp = 25
    ## time: feature importance: 6445.545 sec elapsed (train)
    ## time: feature importance: 2153.749 sec elapsed (eval)
    ## time: feature importance: 4306.984 sec elapsed (test)
    ## timing with sep. targeted/genpop measures, 45/10/45, split correctly via mlr; n_reps_varimp = 25, 46 featurs (no economic measures)
    ## time: feature importance: 4216.044 sec elapsed
    ## time: feature importance: 1411.897 sec elapsed
    ## time: feature importance: 2835.919 sec elapsed
    ## timing, same as before (46 featurs, no economic measures), on IBM Power8 100 cores
    ## time: feature importance: 1080.072 sec elapsed
    ## timing, same as before (49 featurs, no economic measures), on IBM Power8 100 cores, n_reps_varimp = 100
    ## time: feature importance: 5079.134 sec elapsed
    ## time: feature importance: 1692.498 sec elapsed
    ## time: feature importance: 5102.867 sec elapsed
    
    ## save varimp results in list:
    #varimp_list[[i_datlist]] <- varimp
    
    # ## get varimp from varimp_list for re-drawing plots from saved .Rdata files:
    # varimp <- varimp_list[[i_datlist]]
    
    # ## get most important features (for feature effect plots):
    # varnames_imp <- varimp$results %>% 
    #     arrange(desc(importance)) %>% 
    #     head(50) %>%
    #     pull(feature)
    ## extract plots for all features:
    varnames_imp <- varnames_features
    
    # ## make and customize variable importance plot:
    # p <- plot(varimp)
    # p$data$feature <- forcats::fct_reorder(translate_measures(p$data$feature), p$data$importance)
    # #p <- p + geom_point(aes(color = grepl("targeted", feature)), size = 3)
    # p <- p + theme_cust()
    # plot(p)
    # 
    # ## save plot:
    # filename_current <- paste0(
    #     "importance-", names(dat_list)[i_datlist], ".jpg"
    # )
    # cat("\n", filename_current, "\n")
    # ggsave(file.path(path_plots, filename_current), width = 8, height = 9)
    
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    ## feature effects (with iml)
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
    
    # # ## accumulated local effects (ALE) for specific feature:
    # # # (similar to partial dependence plots):
    # effs <- FeatureEffect$new(predictor, feature = "Age")
    # plot(effs)
    
    ## ------------------------------------------------------------------------- ##
    ## ale and partial dependence plots with ice plots for glmnet_fact
    ## ------------------------------------------------------------------------- ##
    
    
    i <- 1
    i <- i + 1
    ## create empty data.frame for storing feature effect plot data:
    dat_effs <- tibble()
    for (i in seq_along(varnames_imp)) {
        cat(".") ## echo something to show some progress
        
        ## calculate grid size:
        grid_size <- diff(range(dat_iml[[varnames_imp[i]]])) + 1
        
        ## calculate feature importance (ALE):
        effs <- FeatureEffect$new(
            predictor, 
            feature = varnames_imp[i], 
            grid.size = grid_size
        ) #, method = "pdp+ice")
        # plot(effs)

        ## add data to data.frame:
        dat_effs <- add_effs_data(dat_collected = dat_effs, effs = effs)
        
        ## if this is the first run, get the rug data (contains all features):
        if (i == 1) {
            dat_rug <- get_rug_data(effs)
        }

        # ## get plot and modify it:
        # p <- plot(effs)
        # p <- eff_plot_mod(p)
        # plot(p)
        # 
        # ## save plot:
        # filename_current <- paste0(
        #     "effect-", names(dat_list)[i_datlist], "-", 
        #     sprintf("%02i", i), "-", varnames_imp[i], "-ALE.jpg")
        # cat(filename_current, "\n")
        # ggsave(file.path(path_plots, filename_current), width = 5, height = 3)
        
        # ## calculate feature importance (PDP):
        # effs <- FeatureEffect$new(
        #     predictor, 
        #     feature = varnames_imp[i], 
        #     grid.size = as.integer(grid_size / 5), ## use way larger grid (for speed)
        #     method = "pdp")
        # 
        # ## add data to data.frame:
        # dat_effs <- add_effs_data(dat_collected = dat_effs, effs = effs)
        # 
        # ## if this is the first run, add the rug data to an existing data frame:
        # if (i == 1) {
        #     dat_rug <- bind_rows(dat_rug, get_rug_data(effs))
        # }
        
        # ## get plot and modify it:
        # p <- plot(effs)
        # p <- eff_plot_mod(p)
        # plot(p)
        # 
        # ## save plot:
        # filename_current <- paste0(
        #     "effect-", names(dat_list)[i_datlist], "-", 
        #     sprintf("%02i", i), "-", varnames_imp[i], "-PDP.jpg")
        # cat(filename_current, "\n")
        # ggsave(file.path(path_plots, filename_current), width = 5, height = 3)
    }
    ## store feature effects and rug data data.frame in list for train/eval/dataset:
    dat_effs_list[[i_datlist]] <- dat_effs
    dat_rug_list[[i_datlist]] <- dat_rug
}

## convert dat_effs list to data.frame:
names(dat_effs_list) <- names(dat_list)
names(dat_rug_list) <- names(dat_list)
#names(varimp_list) <- names(dat_list)
dat_effs_all_orig <- bind_rows(dat_effs_list, .id = "dataset")
dat_rug_all_orig <- bind_rows(dat_rug_list, .id = "dataset")
dat_effs_all <- dat_effs_all_orig
dat_rug_all <- dat_rug_all_orig


## ========================================================================= ##
## additional plots after model estimation
## ========================================================================= ##

## inspect effs data:
dat_effs_all %>% group_by(dataset, .type, measure_name) %>% summarize(n = n())
dat_effs_all %>% group_by(dataset, .type) %>% summarize(n = n())

## inspect rug data
dat_rug_all %>% group_by(dataset, .type, measure_name) %>% summarize(n = n())
dat_rug_all %>% group_by(dataset, .type) %>% summarize(n = n())


## ----------------------------------------------------------------- ##
## define my own variable importance plot
## ----------------------------------------------------------------- ##

# ## check equivalence of ALE and PDP plots:
# dat_effs_all %>% 
#     group_by(dataset, measure_name, .type) %>% 
#     summarize(diff = diff(range(.value, na.rm = TRUE))) %>%
#     tidyr::pivot_wider(names_from = ".type", values_from = "diff") %>%
#     arrange(desc(ale))

## calculate difference between ALE plots at different time points:
dat_effs_measureeffects <- dat_effs_all %>% 
    group_by(dataset, measure_name, .type) %>% 
    summarize(
        .value_tm1 = mean(
            ## index of closest value of .x to -7 (one or two values):
            .value[which(abs(.x - (-7)) == min(abs(.x - (-7))))]
        ),
        .value_t0 = mean(
            ## index of closest value of .x to 0 (one or two values):
            .value[which(abs(.x - 0) == min(abs(.x - 0)))]
        ),
        .value_t1 = mean(
            ## index of closest value of .x to 28 (one or two values):
            .value[which(abs(.x - 28) == min(abs(.x - 28)))]
        ),
        measure_effect = .value_t1 - .value_t0,
        ## for display reasons: large reduction -> large effect:
        measure_effect_disp = -measure_effect
    ) %>% 
    ungroup()

## check:
dat_effs_measureeffects %>% filter(.type == "ale") %>% arrange(measure_effect)

## barplot of measure effects based on ALE plots:
ggplot(
    dat_effs_measureeffects %>% 
        filter(dataset == "test",
               .type == "ale") %>%
        mutate(measure_name = translate_measures(measure_name)), 
    aes(x = forcats::fct_reorder(
        measure_name,
        measure_effect_disp), 
        y = measure_effect_disp)) +
    geom_bar(stat = "identity") +
    coord_flip() +
    labs(
        y = "Reduction in daily infection rate\nin first 28 days after implementation",
        x = ""
    ) +
    theme_cust()

## save plot:
filename_current <- "measures_reduction_infection_rate.jpg"
cat(filename_current, "\n")
ggsave(file.path(path_plots, filename_current), width = 8, height = 9)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## combined feature effect plots
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## [[todo]]
## * center plots at zero at day = 0;

## ? also plot rest of plots (least important?)
## todo: also plot ale

## ------------------------------------------------------------------------- ##
## measures
## ------------------------------------------------------------------------- ##

## define measure names and their order in the plot:
measure_names_sorted <- dat_effs_measureeffects %>%
    filter(dataset == "test", .type == "ale") %>%
    filter(!measure_name %in% c("time_rel", "time_abs")) %>%
    filter(!measure_name %in% c("pop65perc", "popdensity")) %>%
    arrange(desc(measure_effect_disp)) %>%
    pull(measure_name)
list_wch_measures <- list(
    1:9,
    1:length(measure_names_sorted)
)
# ncols <- 3
ncols_few <- 3
ncols_many <- 4
border_fewmany <- 12 # at border, they are still "few"
plot_types <- "ale" # c("pdp", "ale")  ## only do ale plots

## make and save plots:
plot_type <- "ale"
wch_measures <- list_wch_measures[[1]]
wch_measure_names <- measure_names_sorted[wch_measures]

## choose dataset:
wch_dataset <- "test"
for (wch_measures in list_wch_measures) {
    for (plot_type in plot_types) {
        ## get common ylim measures (all measures, not only this loop):
        ylims <- dat_effs_all %>% filter(dataset == wch_dataset) %>%
            filter(.type == plot_type) %$%
            range(.value, na.rm = TRUE)
        wch_measure_names <- measure_names_sorted[wch_measures]
        rugdata_this <- dat_rug_all %>% 
            filter(measure_name %in% wch_measure_names) %>%
            filter(dataset == wch_dataset) %>%
            mutate(
                measure_name = translate_measures(measure_name),
                measure_name = forcats::fct_relevel(measure_name, levels = translate_measures(wch_measure_names))
            )
        dat_this <- dat_effs_all %>% filter(dataset == wch_dataset) %>%
            filter(.type == plot_type) %>%
            filter(measure_name %in% wch_measure_names) %>%
            mutate(
                measure_name = translate_measures(measure_name),
                measure_name = forcats::fct_relevel(measure_name, levels = translate_measures(wch_measure_names))
            )
        ncols <- ifelse(length(wch_measures) <= border_fewmany, ncols_few, ncols_many)
        ggplot(dat_this, aes(x = .x, y = .value)) + 
            geom_line() + 
            xlim(c(-14, 40)) +
            geom_vline(xintercept = 0, color = "darkgrey", linetype = "dashed") +
            geom_rug(
                data = rugdata_this,
                alpha = .2,
                sides = "b", ## bottom
                position = position_jitter(width = .8),
                color = "darkgrey"
            ) +
            coord_cartesian(
                ylim = ylims
                #ylim = range(dat_this[[".value"]]),  ## needed because rug rescales y axis...
            ) + 
            labs(y = "Difference in growth rate (centred)",
                 x = "Days implemented") +
            facet_wrap(vars(measure_name), ncol = ncols) +
            theme_cust()
        
        ## save plot as jpg:
        filename_current <- paste0("effects-", 
                                   sprintf("%02i-%02i", min(wch_measures), max(wch_measures)),
                                   "-", plot_type, ".jpg")
        cat(filename_current, "\n")
        ggsave(file.path(path_plots, filename_current), 
               width = (107 / 2) * ncols, height = (47 / 2) * length(wch_measures) / ncols, 
               units = "mm", dpi = 600, scale = 2)
#               width = 4 * ncols, height = 1.75 * length(wch_measures) / ncols, dpi = 600)
        
        ## save plot as pdf for ms:
        filename_current <- paste0("fig_effects-", 
                                   sprintf("%02i-%02i", min(wch_measures), max(wch_measures)),
                                   "-", plot_type, "_v001.pdf")
        cat(filename_current, "\n")
        ggsave(file.path(path_plots, filename_current), 
               width = (107 / 2) * ncols, height = (47 / 2) * length(wch_measures) / ncols, 
               units = "mm", dpi = 600, scale = 2)

    }
}

## ------------------------------------------------------------------------- ##
## additional plots not in the same raster: time variables
## ------------------------------------------------------------------------- ##

wch_measure_names <- c("time_abs", "time_rel")
plot_type <- "ale"
ylims <- dat_effs_all %>% filter(dataset == wch_dataset) %>%
    filter(.type == plot_type) %$%
    range(.value, na.rm = TRUE)
rugdata_this <- dat_rug_all %>% 
    filter(measure_name %in% wch_measure_names) %>%
    filter(dataset == wch_dataset)
dat_this <- dat_effs_all %>% filter(dataset == wch_dataset) %>%
    filter(.type == plot_type) %>%
    filter(measure_name %in% wch_measure_names) 
ggplot(dat_this, aes(x = .x, y = .value)) + 
    geom_line() + 
    geom_vline(xintercept = 0, color = "darkgrey", linetype = "dashed") +
    geom_rug(
        data = rugdata_this,
        alpha = .2,
        sides = "b", ## bottom
        position = position_jitter(width = .8),
        color = "darkgrey"
    ) +
    coord_cartesian(
        ylim = ylims
        #ylim = range(dat_this[[".value"]]),  ## needed because rug rescales y axis...
    ) + 
    labs(y = "Difference in growth rate (centred)",
         x = "Days") +
    facet_wrap(vars(measure_name), ncol = 1) +
    theme_cust()
## save plot as jpg:
filename_current <- paste0("effects-time-", plot_type, ".jpg")
cat(filename_current, "\n")
ggsave(file.path(path_plots, filename_current), 
       width = (107 / 2), height = (47 / 2), 
       units = "mm", dpi = 600, scale = 2)
#               width = 4 * ncols, height = 1.75 * length(wch_measures) / ncols, dpi = 600)




## quick and dirty plot of population variables 
## (w/o using the data generated in loop above)
effs <- FeatureEffect$new(
    predictor, 
    feature = "popdensity"
)
plot(effs)
ggsave(file.path(path_plots, "effects-popdensity.jpg"), 
       width = 8, height = 4)
effs <- FeatureEffect$new(
    predictor, 
    feature = "pop65perc"
)
plot(effs)
ggsave(file.path(path_plots, "effects-pop65perc.jpg"), 
       width = 8, height = 4)

## output most important measure names:
# paste0('"', measure_names_sorted, '", ') %>% paste(collapse = "\n") %>% cat()



## combined plot for the three datasets for comparison:
wch_measures <- list_wch_measures[[1]]
wch_measure_names <- varimp_list[["test"]]$results$feature[wch_measures]
# wch_measure_names <- "schools_closure_genpop"
# wch_measure_names <- "introduction_of_isolation_and_quarantine_policies_genpop"
dat_effs_all %>% #filter(dataset == "test") %>%
    filter(.type == plot_type) %>%
    filter(measure_name %in% wch_measure_names) %>%
    mutate(
        measure_name = translate_measures(measure_name),
        measure_name = forcats::fct_relevel(measure_name, levels = translate_measures(wch_measure_names))) %>%
    ggplot(aes(x = .x, y = .value, color = dataset, group = dataset)) + 
    geom_line() + 
    facet_wrap(vars(measure_name), ncol = 3) +
    xlim(c(-14, 40)) +
    geom_vline(xintercept = 0, color = "darkgrey", linetype = "dashed") +
    labs(y = "Daily increase (rate)",
         x = "Days implemented") +
    theme_cust()



# source("./r-scripts/setup.R")
# save.image(file = file.path(path_rscripts, "05b-modeling-cleaned_v032.Rdata"), compress = TRUE)
# load(file =       file.path(path_rscripts, "05b-modeling-cleaned_v032.Rdata"))



