# ################################################################################## #
#
# # WIP: Analysis Pipeline ####
# 
# * (1) Split data to test and train sets with a blocking variable (60% train set)
# * (2) Perform a nested resampling for hyperparameter search on a random forest 
# with 10-fold CV as an inner resampling and holdout split as an outer resampling
# * (3) Score and plot predictions per country, while keeping track of in/out-of sample
# country codes
# * (4) With the tuned model get the feature effects using the full dataset
# * (5) Bootsrtap the training set and for each resampling train the chosen model again
# and compute the feature effect with the full data set
# ################################################################################## #

# rm(list = ls(), inherits = TRUE); rstudioapi::restartSession()
# rm(list = ls(), inherits = TRUE)

#sink("sink-examp.txt")
# open:
# * what does the terminator, more specifically n_eval (in the autotuner) do?
library(dplyr)
library(ggplot2)

library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(ranger)    ## explicitly load ranger for renv to detect package (not detected when loaded via mlr3 learner)
library(iml)
library(paradox)
library(tictoc)
library(MLmetrics)

import::from(data.table, setDT, ":=")


source("./r-scripts/function-library.R")


# ---------------------------------------------------------------------------------------------- #
# option to load the image of the saved run ####
# 
# un-comment bellow to have the option to recreate the plots from a saved image
# ---------------------------------------------------------------------------------------------- #
# path_raw <- "."
# path_rscripts <- file.path(path_raw, "r-scripts")
# measures_dataset <- 'coronanet_withTime_withPop'
# load(file.path(path_rscripts, paste0(measures_dataset, "_training_pipeline.Rdata")))


##
# ================================================================================================= #
# for the rest of the code the data prepared in 05b-modelling-cleaned.R was used 
# to test the functionality of the functions
# code was sourced till line titled ML
# ================================================================================================= #
##

#wch_date <- "2020-07-17"
wch_date <- "2020-08-18"

script_txt <- readLines('./r-scripts/05b-modeling-cleaned.R')
end_line <- grep('## ML', script_txt)
source(textConnection(script_txt[1:end_line]))

## NOTE: This should theoretically not be necessary any longer, except for the 
## initialization of parallel computing (future::plan("multiprocess", ...)), as 
## all data prep has been moved to 03-data-prep.R (also the train-test-split 
## that was initially in the modeling script). So 03-datap-prep.R should be sourced instead.


# ## remove unnecessary stuff from memory:
# rm(dat_confirmed, dat_measures_coronanet_core_all, dat_measures_coronanet_core_raw, dat_traineval)
# gc(full = TRUE)

# ----------------------------------- #
# variables ####
# ----------------------------------- #
cv_folds <- 10
train_ratio <- 0.6
bootstrap_ratio <- 1
bootstrap_repeats <- 100 # 1000
# number of iterations the tuner has? terminator n_eval
tuner_itr <- 5
# random search batch_size, for more info: ?mlr_tuners_random_search
rs_batch_size <- 100 
#  eff grid size
eff_grid_size <- NA #100 # grid size will be determined from data
eff_grid_size_density <- 1  ## one ALE plot estimate every 1 unit(s) in a feature

# defined in 05b-modelling-cleaned.R:
#    dat_all and varnames_measures_mincountries 
varnames_target <- "value_rel_smooth"
varnames_block <- "countrycode"
varnames_features <- c(
    varnames_measures_mincountries, 
    "time_abs", "time_rel",
    "ctry_pop65perc", 
    #"ctry_popdensity_trans", ## [[?]] dont' use, use urban percentage (below) instead
    "ctry_popurbanperc",
    # "ctry_popbelowpovertylineperc", ## can't use: 45% missing, systematically between regions
    "ctry_poppollutionperc",
    "ctry_gdppcppp_trans"
)

measures_dataset <- 'coronanet_withTime_withPop'

path_ignore <- file.path(path_raw, "ignore")
path_plots <- file.path(path_ignore, 'plots')
# ----------------------------------- #
# define a task with all the data ####
# ----------------------------------- #
task_all <- blocked_TaskReg(dat_all, 
                            varnames_features, 
                            varnames_target, 
                            varnames_block)
# ----------------------------------- #
# define resampling objects ####
# 
# make sure to instatiate with the right 
# data before every use
# ----------------------------------- #

#
# these are the resampling strategies
# we use in this script.
#   * CV + Holdout are used during training 
# the autotuner
#   * bootstrap is used for creating the 
# effect plots
set.seed(842)
resampling_cv <- rsmp("cv", 
                      folds = cv_folds)
resampling_holdout <- rsmp("holdout", 
                           ratio = train_ratio)
resampling_bootstrap <- rsmp("bootstrap", 
                             repeats = bootstrap_repeats, 
                             ratio = bootstrap_ratio)
# ----------------------------------- #
# (1) split train - test tasks ####
# ----------------------------------- #
# instantiate the holdout resampling with the full dataset
resampling_holdout$instantiate(task_all)
# groups in each set
in_sample_groups <- resampling_holdout$instance$train
out_of_sample_groups <- resampling_holdout$instance$test

# split the task to train and test task
resampling_splits <- split_task_with_rsmp(
    task_obj = task_all, 
    rsmp_obj = resampling_holdout)

train_task <- blocked_TaskReg(resampling_splits$`1`$train, 
                              varnames_features, 
                              varnames_target, 
                              varnames_block)

test_task <- blocked_TaskReg(resampling_splits$`1`$test, 
                             varnames_features, 
                             varnames_target, 
                             varnames_block)


# ----------------------------------- #
# (2) Parameter search ####
# ----------------------------------- #
# the learner / model
learner <- lrn("regr.ranger", predict_sets=c("train", "test"))

#terminator <- term("evals", n_evals = tuner_itr)
terminator <- trm("evals", n_evals = tuner_itr)

# the tuner for the hyperparamter search
tuner <- tnr("random_search", batch_size = rs_batch_size)
# the evaluation measures (first one is used for optimization / paramter search):
measures <- list(
    msr("regr.rmse"),
    msr("regr.rsq")
)
#measures_msrs <- msrs(c("regr.rmse", "regr.rsq"))  ## mlr3tuning 0.2

# the set of parameters for tunning
param_set <- ParamSet$new(
    params = list(
        ParamInt$new("mtry", lower = 2, upper = 20),
        ParamInt$new("min.node.size", lower = 1000, upper = 5000),
        ParamInt$new("max.depth", lower = 1, upper = 5),
        ParamInt$new("num.trees", lower = 2, upper = 500)
    )
)
# Autotuner 
set.seed(7941)
at <- AutoTuner$new(learner = learner, 
                    resampling = resampling_cv,
                    #measures = measures, ## for mlr3tuning 0.1.2
                    measure = measures[[1]],   ## for mlr3tuning 0.2
                    #tune_ps = param_set,      ## for mlr3tuning 0.1.2
                    search_space = param_set,  ## for mlr3tuning 0.2
                    terminator = terminator, 
                    tuner = tuner)
# save intermediate results
at$store_tuning_instance = TRUE

# run Autotuner with outer resampling
# first: instantiate the holdout resampling with the train set
resampling_holdout$instantiate(train_task)
tic('Training Autotuner with outer resampling')
rr <- resample(task = train_task, 
               learner = at, 
               resampling = resampling_holdout, 
               store_models = TRUE)
toc()
# [10 folds and 50 batch_size] Training Autotuner with outer resampling: 69.23 sec elapsed

# # train the autotuner without outer resampling
# tic("Training Autotuner")
# at$train(train_task)
# toc()
# # [10 folds and 50 batch_size] Training Autotuner: 383.456 sec elapsed

# =========================== #
## Evaluation ####
# =========================== #

## hyperparameter set that was chosen:
#rr$data$learner[[1]]$tuning_result$tune_x %>% unlist() %>% {paste0(names(.), "=", ., collapse = ", ")} %>% cat()
rr$data$learner[[1]]$tuning_result$learner_param_vals %>% unlist() %>% {paste0(names(.), "=", ., collapse = ", ")} %>% cat()  ## mlr3tuning 0.2.0
# mtry=10, min.node.size=244, num.trees=356 INFO  [20:25:07.912] Tuned y: regr.rmse=0.03985, regr.rsq=0.5611  with 2020-07-17, with restricted hyperparameter space (for less overfitting) 
# mtry=12, min.node.size=254, num.trees=228 INFO  [20:43:47.109] Tuned y: regr.rmse=0.03994, regr.rsq=0.5138  with 2020-07-17, with restricted++ hyperparameter space (for even less overfitting)  
# mtry=12, min.node.size=250, num.trees=61  INFO  [18:08:23.812] Tuned y: regr.rmse=0.0393, regr.rsq=0.5305 with 2020-07-17 on 2020-07-21, restricted++
# mtry=16, min.node.size=303, num.trees=392    with 2020-07-17 on 2020-07-22, restricted++
# mtry=16, min.node.size=1018, max.depth=4, num.trees=290    with 2020-07-17 on 2020-07-22, restricted++++ via max.depth
# mtry=16, min.node.size=1018, max.depth=4, num.trees=290    with 2020-07-17 on 2020-07-23 new wb vars, with popdensity and popurbanperc both in the model
# mtry=16, min.node.size=1018, max.depth=4, num.trees=290    with 2020-07-17 on 2020-07-27, removed old data (3m after 25 cases), removed popdensity
# mtry=16, min.node.size=1018, max.depth=4, num.trees=290    with 2020-07-17 on 2020-07-28, with winsorization at 99% (omitted before)
# mtry=16, min.node.size=1018, max.depth=4, num.trees=290    with 2020-07-17 on 2020-07-28, with winsorization at 1 and at 99%
# mtry=16, min.node.size=1018, max.depth=4, num.trees=290    with 2020-07-17 on 2020-07-28, with winsorization and gdppcppp
# mtry=17, min.node.size=1059, max.depth=4, num.trees=343    on 2020-08-18
# mtry=17, min.node.size=1059, max.depth=4, num.trees=343    on 2020-08-18 (DS-Lab server)

rr$aggregate(measures)
# ---------------------------- #
## coronanet_withTime_withPop
# ---------------------------- #
#  regr.rmse   regr.rsq 
# 0.04514485 0.56062704   on 2020-07-10
# 0.04593708 0.50296957   on 2020-07-17
# 0.0458312  0.5052580    on 2020-07-17 with additional split in ext_border_restrictions
# 0.04904944 0.44469684   on 2020-07-17 ext_border_restr-split + set.seed
# 0.04831110 0.46128880   with 2020-07-17, with restricted hyperparameter space (for less overfitting) 
# 0.04816162 0.46461745   with 2020-07-17, with restricted++ hyperparameter space (for even less overfitting)
# 0.04781171 0.47236846   with 2020-07-17 on 2020-07-21, restricted++
# 0.04771124 0.47458381   with 2020-07-17 on 2020-07-22, restricted++
# 0.0489570  0.4467878    with 2020-07-17 on 2020-07-22, restricted++++ via max.depth
# 0.04904779 0.44473399   with 2020-07-17 on 2020-07-23 new wb vars, with popdensity and popurbanperc both in the model
# 0.0548758  0.4222010    with 2020-07-17 on 2020-07-27, removed old data (3m after 25 cases), removed popdensity
# 0.04999126 0.44771261   with 2020-07-17 on 2020-07-28, with winsorization at 99% (omitted before)
# 0.04956652 0.45070532   with 2020-07-17 on 2020-07-28, with winsorization at 1 and at 99%
# 0.04970582 0.44761364   with 2020-07-17 on 2020-07-28, with winsorization and gdppcppp
# 0.04600361 0.51691193   on 2020-08-18
# 0.04616766 0.51346032   on 2020-08-18 (DS-Lab server)

# ---------------------------- #
## coronanet_withTime
# ---------------------------- #
# regr.rmse   regr.rsq 
# 0.04572894 0.54918405   on 2020-07-10
# 
# ---------------------------- #
## coronanet
# ---------------------------- #
# regr.rmse   regr.rsq 
# 0.05243538 0.31442303   on 2020-07-10 (probably)
#

rr$learners[[1]]$predict(train_task)$score(measures)
# ---------------------------- #
## coronanet_withTime_withPop
# ---------------------------- #
#  regr.rmse   regr.rsq 
# 0.03322009 0.72982258   on 2020-07-10
# 0.03610406 0.67442707   on 2020-07-17
# 0.03494422 0.69500913   on 2020-07-17 with additional split in ext_border_restrictions
# 0.03351185 0.72169065   on 2020-07-17 ext_border_restr-split + set.seed
# 0.03709658 0.65896507   with 2020-07-17, with restricted hyperparameter space (for less overfitting) 
# 0.03696895 0.66130783   with 2020-07-17, with restricted++ hyperparameter space (for even less overfitting)
# 0.03676295 0.66507179   with 2020-07-17 on 2020-07-21, restricted++
# 0.03721090 0.65685990   with 2020-07-17 on 2020-07-22, restricted++
# 0.04321939 0.53709876   with 2020-07-17 on 2020-07-22, restricted++++ via max.depth
# 0.04325055 0.53643116   with 2020-07-17 on 2020-07-23 new wb vars, with popdensity and popurbanperc both in the model
# 0.04827975 0.52394750   with 2020-07-17 on 2020-07-27, removed old data (3m after 25 cases), removed popdensity
# 0.04426792 0.55184571   with 2020-07-17 on 2020-07-28, with winsorization at 99% (omitted before)
# 0.04407762 0.55356237   with 2020-07-17 on 2020-07-28, with winsorization at 1 and at 99%
# 0.04424499 0.55016538   with 2020-07-17 on 2020-07-28, with winsorization and gdppcppp
# 0.04363668 0.55862063   on 2020-08-18
# 0.04367071 0.55793184   on 2020-08-18 (DS-Lab server)

# ---------------------------- #
## coronanet_withTime
# ---------------------------- #
#  regr.rmse   regr.rsq 
# 0.02868529 0.79855055   on 2020-07-10
# 
# ---------------------------- #
## coronanet
# ---------------------------- #
#  regr.rmse   regr.rsq 
# 0.04064009 0.64518916   on 2020-07-10 (probably)
# 

rr$learners[[1]]$predict(test_task)$score(measures)
# ---------------------------- #
## coronanet_withTime_withPop
# ---------------------------- #
#  regr.rmse   regr.rsq 
# 0.04722351 0.51795893   on 2020-07-10
# 0.04388711 0.54877516   on 2020-07-17
# 0.04391319 0.54823880   on 2020-07-17 with additional split in ext_border_restrictions
# 0.04373126 0.54528864   on 2020-07-17 ext_border_restr-split + set.seed
# 0.04448784 0.52941901   with 2020-07-17, with restricted hyperparameter space (for less overfitting) 
# 0.04414436 0.53665743   with 2020-07-17, with restricted++ hyperparameter space (for even less overfitting)
# 0.04398038 0.54009329   with 2020-07-17 on 2020-07-21, restricted++
# 0.04397184 0.54027189   with 2020-07-17 on 2020-07-22, restricted++
# 0.04714763 0.47146768   with 2020-07-17 on 2020-07-22, restricted++++ via max.depth
# 0.04721098 0.47004643   with 2020-07-17 on 2020-07-23 new wb vars, with popdensity and popurbanperc both in the model
# 0.05230334 0.45741880   with 2020-07-17 on 2020-07-27, removed old data (3m after 25 cases), removed popdensity
# 0.04831109 0.47577823   with 2020-07-17 on 2020-07-28, with winsorization at 99% (omitted before)
# 0.04828492 0.47595134   with 2020-07-17 on 2020-07-28, with winsorization at 1 and at 99%
# 0.04843699 0.47264516   with 2020-07-17 on 2020-07-28, with winsorization and gdppcppp
# 0.04872358 0.44317798   on 2020-08-18
# 0.04879624 0.44151603   on 2020-08-18 (DS-Lab server)

# ---------------------------- #
## coronanet_withTime
# ---------------------------- #
#  regr.rmse   regr.rsq 
# 0.04828865 0.49596851   on 2020-07-10
#
# ---------------------------- #
## coronanet
# ---------------------------- #
# regr.rmse  regr.rsq 
# 0.0479107 0.4125967   on 2020-07-10 (probably)
#

## difference training-test:
rr$learners[[1]]$predict(train_task)$score(measures) - rr$learners[[1]]$predict(test_task)$score(measures)

# ##
# tic("Extracting scores from resample results")
# rr_intermediate_results <- apply(
#     expand.grid(cv_itr = 1:cv_folds, batch_n = 1:rs_batch_size),
#     MARGIN = 1, exctract_score_on_itr, rr$learners[[1]], measures) %>%
#     do.call(rbind, .)
# toc()
# # [10 folds and 50 batch_size] Extracting scores from resample results: 287.899 sec elapsed
# 
# rr_intermediate_results %>%
#     ggplot() +
#     geom_boxplot(aes(set, regr.rsq))
# ggsave(file.path(path_plots,
#                  paste0(measures_dataset, '_resample_scores_cv_rsq.jpg')),
#        width = 8, height = 9)
# 
# rr_intermediate_results %>%
#     ggplot() +
#     geom_boxplot(aes(set, regr.rmse))
# ggsave(file.path(path_plots,
#                  paste0(measures_dataset, '_resample_scores_cv_rmse.jpg')),
#        width = 8, height = 9)

# # results of the returned model from autotuner
# # parameter tuning results
# at$tuning_result
# # the trained learner (the best model)
# at$learner
# # the outputted lerner seems to be overfitting the data
# at$learner$predict(train_task)$score(measures)
# at$learner$predict(test_task)$score(measures)
# 
# # results on the cv folds per batch of parameters 
# # the overfit is clear on th cv level as well
# tic("Extracting scores from autotuner")
# at_intermediate_results <- apply(
#     expand.grid(cv_itr = 1:cv_folds, batch_n = 1:rs_batch_size, measures),
#     MARGIN = 1, exctract_score_on_itr, at) %>% 
#     do.call(rbind, .)
# toc()
# # [10 folds and 50 batch_size] Extracting scores from autotuner: 283.324 sec elapsed
# at_intermediate_results %>% 
#     ggplot() +
#     geom_boxplot(aes(set, regr.rsq))
# ggsave(file.path(path_plots, 'autotuner_scores_cv_rsq.jpg'), width = 8, height = 9)
# 
# at_intermediate_results %>% 
#     ggplot() +
#     geom_boxplot(aes(set, regr.rmse))
# ggsave(file.path(path_plots, 'autotuner_scores_cv_rmse.jpg'), width = 8, height = 9)

## ------------------------------------------ #
## (3) prediction per group (countrycode) ####
## ------------------------------------------ #
learner <- rr$learners[[1]]

for(group in in_sample_groups){
    filename_current <- paste0(measures_dataset, '_pred_', group, '_', 'inSample', '.jpg')
    p <- plot_pred_per_group(group, 
                             plot_title_postfix = '(in-sample)', 
                             dat_all, 'countrycode', learner, measures)
    ggsave(plot = p, filename = file.path(path_plots, filename_current), width = 15, height = 9)
}

for(group in out_of_sample_groups){
    filename_current <- paste0(measures_dataset, '_pred_', group, '_', 'outOfSample', '.jpg')
    p <- plot_pred_per_group(group, 
                             plot_title_postfix = '(out-of-sample)', 
                             dat_all, 'countrycode', learner, measures)
    ggsave(plot = p, filename = file.path(path_plots, filename_current), width = 15, height = 9)
}

# -------------------------------------------------- #
## (3b) table of countries ####
# -------------------------------------------------- #

tbl_countries <- dat_all %>% 
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
        rmse = purrr::map_dbl(scores, ~.x[[1]]),
        rsq = purrr::map_dbl(scores, ~.x[[2]]),
        r = purrr::map_dbl(scores, ~.x[[3]]),
        rnk_rmse = rank(-rmse), ## low rmse -- high rank
        rnk_rsq = rank(rsq),    ## high rsq -- high rank
        geomean = sqrt(rnk_rmse * rnk_rsq)
    ) %>%
    select(-scores)

#Viewxl(tbl_countries)
## [[todo]] calculate r_squared myself, to see if the values hold 
## (or not; and if not, if they correlate better with RMSE)

# -------------------------------------------------- #
# effect plots ####
# -------------------------------------------------- #
# model returned by the autotuner in rr (to avoid cv training when creating the plots)
full_model <- rr$learners[[1]]$learner$clone(deep = TRUE)
# sample feature name
# feature <- varnames_features[1]
# feature <- 'pop65perc' # 'time_rel' 
eff_method <- 'ALE'



## [[HERE]] ----------------------------
## do garbage collection:
gc(full = TRUE)

# one bootstrap sample for each set of effect plots (all features on the same split):
# ------ #
# (5) Bootstrap, train, and compute FeatEff ####
# ------ #
# instansiate the bootstrap resampling with the training set
resampling_bootstrap$instantiate(train_task)
# browser()
# bootstrap_split <- split_task_with_rsmp(task_obj = train_task, 
#                                         rsmp_obj = resampling_bootstrap)

# number of resampling iterations
iterations <- resampling_bootstrap$iters

# retrain the model with each bootsrtap sample and save all
tic('Training the model with the bootstrapped samples')
all_models <- lapply(1:iterations, #bootstrap_split[as.character(1:iterations)], 
                     function(iter) retrain_model(model=full_model, 
                                                              mod_dataset=get_bootstrap_split(train_task, resampling_bootstrap, iter)$train, 
                                                              mod_features=varnames_features,
                                                              mod_target=varnames_target
                     ))
toc()

# --- #
# compute the feature effects
# eff_all_feat <- lapply(varnames_features, 
                       f <- function(feature){
                           # for each iteration generate the feature effect data
                           tic(paste0(feature, ": Computing effects for each model"))
                           cat("(", which(varnames_features == feature), "/", length(varnames_features), ") ",
                               feature, ": computing effects...", sep = "")
                           flush.console()
                           effect_res_lst <- lapply(1:iterations, 
                                                    wrapper_generate_FeatEff,  
                                                    models_lst = all_models, 
                                                    mod_features = varnames_features,
                                                    mod_target = varnames_target,
                                                    eff_dataset = dat_all,
                                                    eff_feature = feature, 
                                                    eff_grid_size = eff_grid_size, 
                                                    eff_method = eff_method,
                                                    add_cols = c(varnames_block, 'date'),
                                                    eff_grid_size_density = eff_grid_size_density
                           )
                           
                           ## convert to data.frame:
                           cat("...converting to data.frame..."); flush.console()
                           # effect_res1 <- effect_res_lst %>% 
                           #     do.call(rbind, .) 
                           effect_res <- data.table::rbindlist(effect_res_lst)
                           
                           ## add median:
                           cat("...adding median..."); flush.console()
                           # effect_res1 <- effect_res1 %>% 
                           #     group_by(.x) %>% 
                           #     mutate(value_median = median(.value, na.rm = TRUE))
                           setDT(effect_res)[, value_median := median(.value, na.rm = TRUE), by = .x]
                           cat("...done.\n"); flush.console()
                           
                           toc()
                           return(effect_res)
                       }
                       #)

# # initialize empty list(s):
# list_split_limit <- floor(length(varnames_features) / 2)
# list_lens <- rep(list_split_limit, length(varnames_features) %/% list_split_limit)
# list_lens_cum <- cumsum(list_lens)
# if ((length(varnames_features) %% list_split_limit) > 0)
#     list_lens <- c(list_lens, length(varnames_features) - sum(list_lens))
# eff_all_feat_pt1 <- vector(mode = "list", length = list_lens[[1]])
# eff_all_feat_pt2 <- vector(mode = "list", length = list_lens[[2]])
eff_all_feat <- vector(mode = "list", length = length(varnames_features))
# fill in for loop:
for (i in seq_along(varnames_features)) {
    eff_all_feat[[i]] <- f(varnames_features[i])
}
# for (i in seq_along(varnames_features)) {
#     if (i <= list_lens_cum[1]) {
#         eff_all_feat_pt1[[i]] <- f(varnames_features[i])
#     } else if (i <= list_lens_cum[2]) {
#         eff_all_feat_pt2[[i]] <- f(varnames_features[i])
#     }
# }
# tmp <- f(varnames_features[i])

# ---------------------------------------------------------------- #
## prediction with all the models and sample plots ####
# ---------------------------------------------------------------- #

# use all models to see prediction per country
pred_all <-
    lapply(1:iterations, 
           function(iteration){
               # browser()
               model <- all_models[[iteration]]
               dat_bootstrap_split <- get_bootstrap_split(train_task, resampling_bootstrap, iteration)
               inSample_codes <- unique(dat_bootstrap_split$train[[varnames_block]])
               outOfSample_codes <- unique(dat_bootstrap_split$test[[varnames_block]])
               
               # sanity check
               stopifnot(length(intersect(inSample_codes, outOfSample_codes))==0)
               
               pred <- model$predict(task_all)
               dat_all_pred <- dat_all %>%
                   mutate(prediction = pred$data$tab$response,
                          itr_n = iteration,
                          sample_type = ifelse(get(varnames_block) %in% inSample_codes, 'InSample', 'OutOfSample'))
               return(dat_all_pred[c(varnames_block, 'date', varnames_target, 'itr_n', 'prediction', 'sample_type')])
           }
    ) %>% data.table::rbindlist()

# sample plots
all_groups <- unique(dat_all[[varnames_block]])
for(c_code in all_groups){
    filename_current <- paste0(measures_dataset, '_pred_', c_code)
    plot_this <- pred_all %>% 
        filter(countrycode == c_code)
    
    p <- plot_this %>% 
        ggplot() +
        geom_line(aes(date, prediction, group=itr_n, color=sample_type), alpha = 0.3) +
        geom_line(data = plot_this %>% 
                      filter(itr_n == 1),
                  aes(date, value_rel_smooth, color = 'Truth')) +
        labs(title = c_code, x='Date', y=varnames_target, color='') +
        theme_minimal()
    ggsave(plot = p, filename = file.path(path_plots, paste0(filename_current, '_all.jpg')), 
           width = 15, height = 9)
    
    # p <- plot_this %>% 
    #     group_by(itr_n, sample_type) %>% 
    #     summarise(r_sqr = R2_Score(prediction, get(varnames_target))) %>% 
    #     ggplot() + 
    #     geom_boxplot(aes(sample_type, r_sqr)) +
    #     #    coord_cartesian(ylim = c(0.5, 1)) +
    #     labs(title = c_code, x='Sample type', y='R Squared') +
    #     theme_minimal()
    # ggsave(plot = p, filename = file.path(path_plots, paste0(filename_current, '_rsqr.jpg')),
    #        width = 15, height = 9)
    
    # p <- plot_this %>% 
    #     group_by(itr_n, sample_type) %>% 
    #     summarise(rmse = RMSE(prediction, get(varnames_target))) %>% 
    #     ggplot() + 
    #     geom_boxplot(aes(sample_type, rmse)) +
    #     #    coord_cartesian(ylim = c(0, 0.5)) +
    #     labs(title = c_code, x='Sample type', y='RMSE') +
    #     theme_minimal()
    # ggsave(plot = p, filename = file.path(path_plots, paste0(filename_current, '_rmse.jpg')),
    #        width = 15, height = 9)
}

## until [[HERE]] ----------------------------


# save.image(file = file.path(path_rscripts, paste0(measures_dataset, "_training_pipeline_v008.Rdata")), compress = TRUE)
# source("./r-scripts/setup.R"); load(file = file.path(path_rscripts, "coronanet_withTime_withPop_training_pipeline_v008.Rdata"))
# ###
length(eff_all_feat)


## NOTE:
## Analysis (figures, tables) continued in 06a-materials-for-ms.R
## Below are prototypes for the tables and figures created there.



eff_all_feat_df <- data.table::rbindlist(eff_all_feat)
# too slow!!
# eff_all_feat_df <- do.call(rbind, eff_all_feat)

## define variable names of plots:
varnames_ctry <- varnames_features[grep('ctry', varnames_features)]
varnames_time <- varnames_features[grep('time', varnames_features)]
# varnames_features[grep('restriction_mass_gatherings', varnames_features)]
varnames_measures_wch <- varnames_measures_sorted[1:4]  #[1:8] ## [[DEVEL]]: Shorten number of varnames to reduce # of plots


## get ylim values:
yvalues <- eff_all_feat_df[measure_name %in% varnames_measures_mincountries, .value]
ylims <- range(yvalues, na.rm = TRUE)

#tmp_varnames_test <- c("social_distancing__nat__man", "schools_regulation__nat__man", "ctry_gdp_trans")
#lapply(tmp_varnames_test, 
lapply(varnames_features, 
       function(feature){
           tic(paste0('Time to create plot for ', feature))
           # ------ #
           # (4) get the effect without the additional training ####
           # ------ #
           eff_noTraining <- generate_FeatEff(full_model, varnames_features, varnames_target, 
                                              dat_all, feature, eff_grid_size, eff_method)
           # effect_res <- eff_all_feat_df[measure_name == feature]
           # create plot 
           p <- plot_feature_effect(feature, eff_all_feat_df, 
                                    eff_method, varnames_target, 
                                    'value_median', eff_noTraining,
                                    eff_dataset = dat_all %>%
                                        select(one_of("countrycode", feature, "value_rel_smooth")) %>%
                                        setNames(c("countrycode", ".x", ".value")) %>%
                                        filter(.x > -15) %>%
                                        group_by(countrycode, .x) %>%
                                        summarize(
                                            .value = first(.value), ## could be any, but dataset needs to have .value
                                            .groups = "drop_last"
                                        ),
                                    addvertline = !grepl("^ctry_", feature))
           # add coord limits if needed
           if(feature %in% varnames_measures_mincountries){
               p <- p + 
                   xlim(-14, 100) + ## actually exclude -15
                   coord_cartesian(xlim = c(-14, 60), ylim = ylims) ## plot only to 60
           } else {
               ## (re-)set ylimits because of rug, which messes this up:
               ylims_data <- range(eff_all_feat_df[measure_name == feature, .value], na.rm = TRUE)
               p <- p + 
                   coord_cartesian(ylim = ylims_data)
           }
           plot_name <- paste0(measures_dataset, '_', eff_method, '_', feature, '_bootstrap_', bootstrap_repeats, '.jpg')
           # save plot
           ggsave(plot = p, filename = file.path(path_plots, plot_name), 
                  width = 10, height = 8)
           toc()
       })

## -------------------------------------------------------------------------------- #
## ggpubr: Publication Ready Plots ####
## -------------------------------------------------------------------------------- #
library(ggpubr)

varnames_ctry <- varnames_features[grep('ctry', varnames_features)]
varnames_time <- varnames_features[grep('time', varnames_features)]
# varnames_features[grep('restriction_mass_gatherings', varnames_features)]
varnames_measures_wch <- c("schools_regulation__subnat__man", "schools_regulation__nat__man" , 
                           "social_distancing__nat__man" ,  "quarantine__nat__man", 
                           "ext_border_restrictions__nat__man__inout",
                           "restriction_businesses__nat__man" , "restriction_mass_gatherings__nat__man" )


ctry_yvalues <- eff_all_feat_df[measure_name %in% varnames_ctry, .value]
ctry_ylims <- range(ctry_yvalues, na.rm = TRUE)
time_yvalues <- eff_all_feat_df[measure_name %in% varnames_time, .value]
time_ylims <- range(time_yvalues, na.rm = TRUE)

lapply(list(varnames_ctry, varnames_time, varnames_measures_wch), 
       # for each group of varnames
       function(varnames_vec){
           addvertline <- !grepl("^ctry_|^time_", varnames_vec[1])
           if(grepl("^ctry_", varnames_vec[1])){
               filename_current <- paste0(eff_method, '_ctry_features.jpg')
               xlab=''
           }else if(grepl("^time_", varnames_vec[1])){
               filename_current <- paste0(eff_method, '_time_features.jpg')
               xlab='Days'
           }else{
               filename_current <- paste0(eff_method, '_wch_measures_features.jpg')
               xlab='Days since first implemented'
           }
           # with ggplot create plots to publish 
           plots <- lapply(varnames_vec, 
                           function(feature){
                               tic(paste0('Time to create plot for ', feature))
                               
                               # get the effect without the additional training
                               eff_noTraining <- generate_FeatEff(full_model, varnames_features, varnames_target, 
                                                                  dat_all, feature, eff_grid_size, eff_method)
                               
                               # create plot 
                               p <- plot_feature_effect(feature, eff_all_feat_df, 
                                                        eff_method, varnames_target, 
                                                        'value_median', eff_noTraining,
                                                        eff_dataset = dat_all %>%
                                                            select(one_of("countrycode", feature, "value_rel_smooth")) %>%
                                                            setNames(c("countrycode", ".x", ".value")) %>%
                                                            filter(.x > -15) %>%
                                                            group_by(countrycode, .x) %>%
                                                            summarize(
                                                                .value = first(.value), ## could be any, but dataset needs to have .value
                                                                .groups = "drop_last"
                                                            ),
                                                        addvertline = addvertline)
                               # add coord limits if needed
                               if(feature %in% varnames_measures_mincountries){
                                   p <- p + 
                                       xlim(-14, 100) + ## actually exclude -15
                                       coord_cartesian(xlim = c(-14, 60), ylim = ylims) + ## plot only to 60
                                       # rremove("xlab") +
                                       rremove("ylab")
                               } else if(feature %in% varnames_ctry){
                                   p <- p + 
                                       coord_cartesian(ylim = ctry_ylims) +
                                       # rremove("xlab") +
                                       rremove("ylab")
                               } else if(feature %in% varnames_time){
                                   p <- p + 
                                       coord_cartesian(ylim = time_ylims) +
                                       # rremove("xlab") +
                                       rremove("ylab") 
                               }
                               toc()
                               return(p)
                           })
           
           ## add to the list of plots other parameters to pass to ggarrange
           plots_len <- length(plots)
           plots['nrow'] <- ceiling(sqrt(plots_len))
           plots['ncol'] <- floor(sqrt(plots_len))
           if(plots[['nrow']] * plots[['ncol']] < plots_len){
               plots[['nrow']] = plots[['nrow']]  + 1  
           }
           plots['common.legend'] <- TRUE
           plots['legend'] <- "top"
           ## call ggarrange with the list of parameters
           fig <- do.call(ggarrange, plots)
           ## add title and axis labels
           fig <- annotate_figure(fig, 
                                  left = text_grob(varnames_target, rot = 90, size = 30),
                                  top = text_grob(paste(eff_method, 'Plots'), size = 35))
           
           ## export figure
           ## A4 with 300 DPI --> 2480 x 3508 px
           ## A4 with 600 DPI --> 4960 x 7016 px
           ggexport(
               fig,
               filename = file.path(path_plots, filename_current),
               width = 2480/3 * plots[['ncol']],
               height = (3508/2)/3 * plots[['nrow']]
           )
           
       })


#sink()

# ## check rug data:
# dat_all %>%
#     select(one_of("countrycode", feature, "value_rel_smooth")) %>%
#     setNames(c("countrycode", ".x", ".value")) %>%
#     filter(.x > -15) %>%
#     group_by(countrycode, .x) %>%
#     summarize(.value = first(.value), .groups = "drop_last") %$% table(.x)

# # test bootstrap resampling and splitting
# resampling_bootstrap$instantiate(train_task)
# bootstrap_split <- split_task_with_rsmp(task_obj = train_task, 
#                                         rsmp_obj = resampling_bootstrap)
# # sanity checks 
# bootstrap_split %>% names()
# bootstrap_split$`1`$train$..row_id %>% length()
# bootstrap_split$`1`$train$..row_id %>% unique() %>% length()