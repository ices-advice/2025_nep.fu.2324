## Before: data and functions in bootstrap/initial
## After: data and functions copied to bootstrap

library("icesTAF")
library("tidyverse")
library("ggpubr")

## Initialising script

draft.data(originator = "WGNEPH", year = "2025", source = "file", file = "bootstrap/DATA.bib")
draft.software(package = "bootstrap/initial/software/*",
               source = "file", file = "bootstrap/SOFTWARE.bib")

## Read files in bootstrap/initial/data and save a copy in bootstrap/data
taf.bootstrap()

## Create data file in folder root
mkdir("data")

#mkdir("data/data_01_assessment")




##------------------------------------------------------------------------------
## Parameter
stock_code <- "nep-2324"

# year of the advice
curr.year <- 2026
dat.year <- curr.year -1
fu.n <- "FU2324"

save.plots <- T # set it up as T or F whether you want to save the plots when running all the chunks or kniting the document
save.tables <- T # set it up as T or F whether you want to save the tables "propmature"" and the "L50" into .txt files
#when running all the chunks or knitting the document

# current year
curr.year.adg <- 2025
dat.year.adg <- curr.year.adg -1

wgneph.yr <- curr.year.adg


##------------------------------------------------------------------------------
## Load data

## Load Fishery activity and UWTV survey indices time series
# Time serie of Summary Table
fu2324.exp <- read.csv("bootstrap/data/Table_8_Summary_LAN_DIS_UWTV_Time_Serie.csv"
                       ,header=T,sep=";",dec=".",na.strings = "NA",encoding = "UTF-8")
Summary_SAG <- fu2324.exp

# F and SSB thresholds
ref <- read.csv("bootstrap/data/MSY_nep_stocks.csv"
                ,header=T,sep=",",dec=".",na.strings = "NA",encoding = "UTF-8")

# LANGOLF-TV indices
langolftv_index <- read.csv("bootstrap/data/UWTV_Survey_Report_Adjusted_Abundance_CV.csv"
                       ,header=T,sep=";",dec=".",na.strings = "NA",encoding = "UTF-8")

langolftv_index <- langolftv_index[langolftv_index$Year %in% c(2016:curr.year.adg),]

##------------------------------------------------------------------------------
## Formating Data
# ------------------------------------------------------------------------ #
# ------------------------------------------------------------------------ #
print(fu2324.exp)

fu2324.exp <- fu2324.exp[, c("Year", "UWTV_abundance_millions"
                             ,"Landings_Nb_millions"
                             ,"Discards_Nb_millions"
                             ,"Removals_Nb_millions"
                             ,"Harvest_Rate"
                             ,"Landings_t"
                             ,"Discards_t"
                             ,"Discard_Rate"
                             ,"Dead_Discard_Rate"
                             ,"Landings_Mean_Weight_g"
                             ,"Discards_Mean_Weight_g")]

#fu2324.exp$Discards <- as.numeric(fu2324.exp$Discards)

# Assign new names to the fu11.assess dataframe
new_column_names <- c("year", "abund", "int.lan.num", "int.dis.num", "removals.n", 
                      "hr", "int.lan.wgt", "int.dis.wgt", "dis.rn", "dead.disc.r", "mw.lan", "mw.dis")
colnames(fu2324.exp) <- new_column_names


fu2324.assess <- fu2324.exp
