## ######################################################################### ##
## Set up stuff
## ######################################################################### ##

# rm(list = ls(), inherits = TRUE)
options(bitmapType = "cairo")

## ========================================================================= ##
## load basic packages
## ========================================================================= ##

# ## to run the analysis with the exact package versions used for estimation,
# ## restore environment using renv:
# renv::restore()

# library(plyr)
# library(tidyverse)
library(readr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(readxl)
library(tibble)
library(stringr)

options(tibble.width = Inf)

## ========================================================================= ##
## global variables
## ========================================================================= ##

path_raw <- "."
path_rscripts <- file.path(path_raw, "r-scripts")
path_dat <- file.path(path_raw, "data")
path_plots <- file.path(path_raw, "plots")
path_logs <- file.path(path_raw, "logs")
path_ms <- file.path(path_raw, "manuscript")
path_fonts <- file.path(path_raw, "fonts")
path_tmp <- file.path(path_raw, "tmp")

## ========================================================================= ##
## colors
## ========================================================================= ##

## colours of IT-PS theme:
blue_itps_logo <- "#4DB1E2"  ## taken from pdf Visitenkarte
#blue_itps_logo <- "#16AADD"  ## taken from powerpoint template and home page

## ========================================================================= ##
## fonts using showtext package ####
## ========================================================================= ##

# # devtools::install_github("yixuan/showtext")
# # ## see https://github.com/yixuan/showtext
# 
# ## add font (showtext):
# sysfonts::font_add(
#     family = "Barlow", 
#     regular = file.path(path_fonts, "Barlow-Regular.ttf"),
#     bold = file.path(path_fonts, "Barlow-SemiBold.ttf"),
#     bolditalic = file.path(path_fonts, "Barlow-SemiBoldItalic.ttf"))
# showtext::showtext_auto()
# 
# ## Tell showtext the resolution of the device,
# ## only needed for bitmap graphics. Default is 96
# showtext::showtext_opts(dpi = 300)
# 
# ## select font for plots below:
# font_base <- "Barlow"


