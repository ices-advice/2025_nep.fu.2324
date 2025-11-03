## Run analysis, write model results

## Before: "Table_8_Summary_LAN_DIS_UWTV_Time_Serie.csv", "MSY_nep_stocks.csv"
## After: Assessment results - Catch scenarios

## Create model file in folder root
mkdir("model")




# Check if the renaming worked
head(fu2324.assess)

# Calculation of proportion of removals
fu2324.assess$prop.removals.ret <- fu2324.assess$int.lan.num/fu2324.assess$removals.n


# Period definition for average calculation
land.wt.yrs <<- seq(wgneph.yr-3,wgneph.yr-1,1)
disc.wt.yrs <<- seq(wgneph.yr-3,wgneph.yr-1,1)

discard.rate.yrs <- seq(wgneph.yr-3,wgneph.yr-1,1)
dead.discard.rate.yrs <- seq(wgneph.yr-3,wgneph.yr-1,1)
prop.removal.ret.yrs <- seq(wgneph.yr-3,wgneph.yr-1,1)




##------------------------------------------------------------------------------
## Processing Data
# ------------------------------------------------------------------------ #
# ------------------------------------------------------------------------ #

# Calculation of the average parameters inputs
stock.abundance <- fu2324.assess$abund[length(fu2324.assess$abund)]


land.mean.wt <- mean(fu2324.assess$mw.lan[fu2324.assess$year %in% land.wt.yrs],na.rm=T)
disc.mean.wt <- mean(fu2324.assess$mw.dis[fu2324.assess$year %in% disc.wt.yrs],na.rm=T)

discard.rate <- mean(fu2324.assess$dis.rn[fu2324.assess$year%in% discard.rate.yrs],na.rm=T)

dead.discard.rate <- mean(fu2324.assess$dead.disc.r[fu2324.assess$year %in% dead.discard.rate.yrs],na.rm=T)
prop.removal.ret.n <- mean(fu2324.assess$prop.removals.ret[fu2324.assess$year %in% prop.removal.ret.yrs], na.rm=T)
disc.survival <- 50


fu2334.catch.inputs <- data.frame(wgneph.yr, stock.abundance, land.mean.wt, disc.mean.wt,discard.rate,dead.discard.rate,disc.survival)

knitr::kable(fu2334.catch.inputs, digits=5)
print(fu2334.catch.inputs)




##------------------------------------------------------------------------------
## MSY ranges inputs for the forecast tables
# ------------------------------------------------------------------------ #
# ------------------------------------------------------------------------ #
# filter of variables of interest
ref2324 <- ref[ref$Stock.code==stock_code, c("F_MSY", "MSY_F_lower", "MSY_F_upper", "MSY_Btrigger")]

MSY_Btrigger <- as.numeric(ref2324$MSY_Btrigger)

HR2324 <- cbind(ref2324[,1:3],
                # "MSY approach"= ref22$F_MSY*stock.abundance/MSY_Btrigger,
                # "Flower_Trig"= round(ref22$MSY_F_lower*stock.abundance/MSY_Btrigger, 3),
                "F_recent"= mean (fu2324.assess[fu2324.assess$year %in% c(wgneph.yr-3):(wgneph.yr-1),] $ hr /100),
                "F_current"= fu2324.assess[fu2324.assess$year==(wgneph.yr-1),] $ hr)


row.names(HR2324) <- NULL

HR2324 <- HR2324[, c("F_MSY", "MSY_F_lower", "MSY_F_upper",  "F_current")]

# reorder for advice sheet
HR2324 <- HR2324[,c("F_MSY","MSY_F_lower", "MSY_F_upper", "F_MSY", "F_current")]
colnames(HR2324)[4] <- "MSY approach"
colnames(HR2324)[5] <- paste("F_",dat.year.adg,sep="")

knitr::kable(HR2324)
print(HR2324)



##------------------------------------------------------------------------------
## Catch scenarios
# ------------------------------------------------------------------------ #
# ------------------------------------------------------------------------ #
# Table 3: Catch scenarios assuming zero discards
# Annual catch scenarios. All weights are in tonnes.

forecast.year <- (curr.year + 1)
wanted.catch <-  ((100-discard.rate)*(land.mean.wt/100)*stock.abundance*(HR2324))
unwanted.catch <- ((discard.rate)*(disc.mean.wt/100)*stock.abundance*(HR2324))
total.catch <- wanted.catch + unwanted.catch



LO <- rbind(total.catch, wanted.catch, unwanted.catch, HR2324*100, forecast.year)
LO <- as.data.frame(t(LO))
names(LO) <- c("total.catch", "projected landings", "projected discards", "% harvest rate",  "forecast.year")



for (i in 1:(ncol(LO))){
  LO[,i] <- as.character(LO[,i])
  LO[,i] <- as.numeric(LO[,i])
  if (names(LO)[i] == "% harvest rate"){
    LO[,i] <- round(LO[,i], 3)
  } else {
    LO[,i] <- round(LO[,i])
  }
}

knitr::kable(LO)
print("Catch scenarios assuming zero discards")
print(LO)

# ------------------------------------------------------------------------ #
# Table 3: Catch scenarios assuming recent discards
# Annual catch scenarios. All weights are in tonnes.
# Check stock abundance below Btrigger as this requires extra catch options.

landings <-  (land.mean.wt/100)*stock.abundance*(prop.removal.ret.n*100)*(HR2324)
dead.discards <- (disc.mean.wt/100)*stock.abundance*(100-(prop.removal.ret.n*100))*(HR2324)
surviving.discards <- dead.discards
dead.removals <- landings + dead.discards
total.catch <- landings+dead.discards+surviving.discards


DA <- rbind(total.catch, dead.removals, landings, dead.discards, 
            surviving.discards, HR2324*100, forecast.year)
DA <- as.data.frame(t(DA))

names(DA) <- c("total.catch", "dead.removals", "projected landings", "projected dead discards", 
               " projected surviving discards", "% harvest rate","forecast.year") 

for (i in 1:(ncol(DA))){
  DA[,i] <- as.character(DA[,i])
  DA[,i] <- as.numeric(DA[,i])
  if (names(DA)[i] == "% harvest rate"){
    DA[,i] <- round(DA[,i], 3)
  } else {
    DA[,i] <- round(DA[,i])
  }
}


knitr::kable(DA)
print("Catch scenarios assuming recent discards")
print(DA)







