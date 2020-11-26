## ######################################################################### ##
## Download data from online sources
## ######################################################################### ##

# rm(list = ls(), inherits = TRUE)

## ========================================================================= ##
## load packages
## ========================================================================= ##

# library(tidyverse)
library(dplyr)
library(magrittr)
library(readr)
library(wbstats)


## ========================================================================= ##
## global variables
## ========================================================================= ##

path_raw <- "."
path_dat <- file.path(path_raw, "data")

filename_dat_confirmed <- paste0(
    "dat_confirmed_", Sys.Date(), ".csv"
)
filename_dat_measures_coronanet_core <- paste0(
    "dat_measures_coronanet_core_", Sys.Date(), ".csv"
)
filename_dat_measures_coronanet_extended <- paste0(
    "dat_measures_coronanet_extended_", Sys.Date(), ".csv"
)
filename_dat_tests <- paste0(
    "dat_tests_", Sys.Date(), ".csv"
)
filename_dat_wb <- paste0(
    "dat_wb_", Sys.Date(), ".rds"
)


## ========================================================================= ##
## function definitions
## ========================================================================= ##

#' get data for the most recent year (if there is data in the last 10 years)
get_latest_wb <- function(indicator) {
    wb(indicator = indicator, mrv = 10) %>%
        arrange(iso3c, date) %>%
        group_by(iso3c) %>%
        summarize_all(last) %>%
        ungroup()
}
#get_latest_wb(indicator = "SP.POP.65UP.TO.ZS")

## ========================================================================= ##
## download stuff
## ========================================================================= ##


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Novel Coronavirus (COVID-19) Cases Data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## download Novel Coronavirus (COVID-19) Cases Data via HDX
## ()

current_time <- paste(as.character(Sys.time()), Sys.timezone())
url_source <- "https://data.humdata.org/hxlproxy/data/download/time_series_covid19_confirmed_global_narrow.csv?dest=data_edit&filter01=merge&merge-url01=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D1326629740%26single%3Dtrue%26output%3Dcsv&merge-keys01=%23country%2Bname&merge-tags01=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&filter02=merge&merge-url02=https%3A%2F%2Fdocs.google.com%2Fspreadsheets%2Fd%2Fe%2F2PACX-1vTglKQRXpkKSErDiWG6ycqEth32MY0reMuVGhaslImLjfuLU0EUgyyu2e-3vKDArjqGX7dXEBV8FJ4f%2Fpub%3Fgid%3D398158223%26single%3Dtrue%26output%3Dcsv&merge-keys02=%23adm1%2Bname&merge-tags02=%23country%2Bcode%2C%23region%2Bmain%2Bcode%2C%23region%2Bsub%2Bcode%2C%23region%2Bintermediate%2Bcode&merge-replace02=on&merge-overwrite02=on&filter03=explode&explode-header-att03=date&explode-value-att03=value&filter04=rename&rename-oldtag04=%23affected%2Bdate&rename-newtag04=%23date&rename-header04=Date&filter05=rename&rename-oldtag05=%23affected%2Bvalue&rename-newtag05=%23affected%2Binfected%2Bvalue%2Bnum&rename-header05=Value&filter06=clean&clean-date-tags06=%23date&filter07=sort&sort-tags07=%23date&sort-reverse07=on&filter08=sort&sort-tags08=%23country%2Bname%2C%23adm1%2Bname&tagger-match-all=on&tagger-default-tag=%23affected%2Blabel&tagger-01-header=province%2Fstate&tagger-01-tag=%23adm1%2Bname&tagger-02-header=country%2Fregion&tagger-02-tag=%23country%2Bname&tagger-03-header=lat&tagger-03-tag=%23geo%2Blat&tagger-04-header=long&tagger-04-tag=%23geo%2Blon&header-row=1&url=https%3A%2F%2Fraw.githubusercontent.com%2FCSSEGISandData%2FCOVID-19%2Fmaster%2Fcsse_covid_19_data%2Fcsse_covid_19_time_series%2Ftime_series_covid19_confirmed_global.csv"
download.file(
    url = url_source, 
    destfile = file.path(path_dat, filename_dat_confirmed)
)

## write download date to disk:
write_file(
    current_time, 
    path = file.path(path_dat, 
                     paste0(filename_dat_confirmed, "_download-time.txt"))
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## CoronaNet data (core and extended)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

## core version:

current_time <- paste(as.character(Sys.time()), Sys.timezone())
#url_source <- "http://coronanet-project.org/data/coronanet_release.csv"
url_source <- "https://raw.githubusercontent.com/saudiwin/corona_tscs/master/data/CoronaNet/coronanet_release.csv"
download.file(
    url = url_source, 
    destfile = file.path(path_dat, filename_dat_measures_coronanet_core)
)

## write download date to disk:
write_file(
    current_time, 
    path = file.path(path_dat, 
                     paste0(filename_dat_measures_coronanet_core, "_download-time.txt"))
)


## extended version:
current_time <- paste(as.character(Sys.time()), Sys.timezone())
#url_source <- "http://coronanet-project.org/data/coronanet_release_allvars.csv"
url_source <- "https://raw.githubusercontent.com/saudiwin/corona_tscs/master/data/CoronaNet/coronanet_release_allvars.csv"
download.file(
    url = url_source, 
    destfile = file.path(path_dat, filename_dat_measures_coronanet_extended)
)

## write download date to disk:
write_file(
    current_time, 
    path = file.path(path_dat, 
                     paste0(filename_dat_measures_coronanet_extended, "_download-time.txt"))
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## covid-testing (our world in data database)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

current_time <- paste(as.character(Sys.time()), Sys.timezone())
#url_source <- "https://drive.google.com/drive/folders/1HPzXon49teN-kMQlY1ry9rh2eq-TyGQI?usp=sharing"
#url_source <- "https://docs.google.com/uc?export=download&id=1HPzXon49teN-kMQlY1ry9rh2eq-TyGQI"
#url_source <- "https://drive.google.com/drive/folders/1HPzXon49teN-kMQlY1ry9rh2eq-TyGQI"
url_source <- "https://github.com/owid/covid-19-data/raw/master/public/data/testing/covid-testing-all-observations.csv"
download.file(
    url = url_source,
    destfile = file.path(path_dat, filename_dat_tests)
)

## write download date to disk:
write_file(
    current_time,
    path = file.path(path_dat,
                     paste0(filename_dat_tests, "_download-time.txt"))
)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## world bank data
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

current_time <- paste(as.character(Sys.time()), Sys.timezone())

## Population ages 65 and above (% of total)
dat_wb_pop65perc <- get_latest_wb(indicator = "SP.POP.65UP.TO.ZS")
# dat_wb_pop65perc %>% head()
# dat_wb_pop65perc %>% dim()

## Population density (people per sq. km of land area)
dat_wb_popdens <- get_latest_wb(indicator = "EN.POP.DNST")
# dat_wb_popdens %>% head()
# dat_wb_popdens %>% dim()

# tmp <- get_latest_wb(indicator = "SP.URB.TOTL.ZS") percentage of population in urban areas: no data!
dat_wb_poptot <- get_latest_wb(indicator = "SP.POP.TOTL")
dat_wb_popurban <- get_latest_wb(indicator = "SP.URB.TOTL")
dat_wb_popurbanperc <- dat_wb_poptot %>%
    select(iso3c, date, poptot = value) %>%
    left_join(
        dat_wb_popurban %>%
            select(iso3c, date, popurban = value),
        by = c("iso3c" = "iso3c", "date" = "date")
    ) %>%
    mutate(popurbanperc = popurban / poptot * 100)

dat_wb_pov <- get_latest_wb(indicator = "SI.POV.DDAY") ## Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population) -- international poverty line
dat_wb_pollution <- get_latest_wb(indicator = "EN.ATM.PM25.MC.ZS") ## PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)

dat_wb_gpdtot <- get_latest_wb(indicator = "NY.GDP.MKTP.CD") ## GDP (current USD)
dat_wb_gpdpcppp <- get_latest_wb(indicator = "NY.GDP.PCAP.PP.CD") ## GDP per capita, PPP (current international $)

## combine to single dataset:
dat_wb <- wb_cachelist$countries %>%
    left_join(dat_wb_pop65perc %>% 
                  select(iso3c, 
                         ctry_pop65perc = value),
              by = "iso3c") %>%
    left_join(dat_wb_popdens %>% 
                  select(iso3c, 
                         ctry_popdensity = value),
              by = "iso3c") %>%
    left_join(dat_wb_popurbanperc %>%
                  select(iso3c, 
                         ctry_popurbanperc = popurbanperc),
              by = "iso3c") %>%
    left_join(dat_wb_pov %>% 
                  select(iso3c, 
                         ctry_popbelowpovertylineperc = value),
              by = "iso3c") %>%
    left_join(dat_wb_pollution %>%
                  select(iso3c, 
                         ctry_poppollutionperc = value),
              by = "iso3c") %>%
    left_join(dat_wb_gpdtot %>%
                  select(iso3c, 
                         ctry_gdptot = value),
              by = "iso3c") %>%
    left_join(dat_wb_gpdpcppp %>%
                  select(iso3c, 
                         ctry_gdppcppp = value),
              by = "iso3c") 




#dat_wb %>% filter(iso3c %in% c("AND", "DMA", "ERI", "KNA", "LIE", "MCO", "SMR", "TWN", "VAT"))
#hist(dat_wb$popdensity, nclass = 50)
#dat_wb %>% arrange(popdensity) %>% na.omit() %>% tail()

# ## check:
# dat_wb %>% select(c(1, 19:20)) %>% head(n = 100)

## save to disk:
saveRDS(dat_wb, file = file.path(path_dat, filename_dat_wb))

## write download date to disk:
write_file(
    current_time, 
    path = file.path(path_dat, 
                     paste0(filename_dat_wb, "_download-time.txt"))
)

# ## find indicators:
# wbs <- wbsearch("gdp")
# #Viewxl(wbs)
# wbs %>%
#     filter(
#         # grepl("urban", indicator, ignore.case = TRUE),
#         grepl("capita", indicator, ignore.case = TRUE),
#         grepl("ppp", indicator, ignore.case = TRUE)
#     ) %>% Viewxl()
# 
# 
# # additional indicators:
# tmp <- get_latest_wb(indicator = "")
# tmp <- get_latest_wb(indicator = "EN.POP.SLUM.UR.ZS") ## percentage of urban pop living in slums: only 112 countries report data (2014 newest)
# tmp <- get_latest_wb(indicator = "SI.POV.URHC") ## Urban poverty headcount ratio at national poverty lines (% of urban population): not found
# tmp <- get_latest_wb(indicator = "IN.POV.HCR.EST.TOTL") ## Poverty HCR Estimates (%) - Total: only 1 country with data
# tmp <- get_latest_wb(indicator = "SI.POV.NAHC") ## 113 countries with relatively recent data: Poverty headcount ratio at national poverty lines (% of population)
# tmp <- get_latest_wb(indicator = "SI.POV.DDAY") ## 157 countries with relatively recent data: Poverty headcount ratio at $1.90 a day (2011 PPP) (% of population) -- international poverty line
# tmp <- get_latest_wb(indicator = "EN.ATM.PM25.MC.ZS") ## PM2.5 air pollution, population exposed to levels exceeding WHO guideline value (% of total)
# tmp <- get_latest_wb(indicator = "NY.GDP.MKTP.CD") ## GDP (current USD)
# tmp <- get_latest_wb(indicator = "SI.POV.GINI") ## GINI index (World Bank estimate)
# tmp <- get_latest_wb(indicator = "NY.GDP.PCAP.PP.CD") ## GDP per capita, PPP (current international $)
# dim(tmp)
# tmp
# table(tmp$date)



