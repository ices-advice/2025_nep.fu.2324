## Output tables and figures

## Before:
## After:


## Create output file in folder root
mkdir("output")

##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
### export data to Standard Assessment Graph website using icesSAG package

## Before: Run icesTAF::source.all()
## After: create info and fishdata to be used for SAG upload using personnal token

library("icesSAG")

# ------------------------------------------------------------------------------------------------------------------------------- #


info <- icesSAG::stockInfo(StockCode = "nep.fu.2324",
                           AssessmentYear = assessment_year,
                           ContactPerson = "damien.delaunay@ifremer.fr",
                           StockCategory = "1",
                           ModelType = "Other",
                           ModelName = "Other",
                           Purpose = "Advice",
                           RecruitmentAge = 0,
                           CatchesLandingsUnits = "t",
                           #RecruitmentUnits = "NE3",
                           StockSizeUnits = "NE6",
                           #Fage = "3-7",
                           #MSYBtrigger = ref_points[["MSYBtrigger"]],
                           FMSY = ref_points$F_MSY[ref_points$Stock.code %in% stock_code],
                           #Blim = ref_points[["Blim"]],
                           #Bpa = ref_points[["Bpa"]],
                           #Flim = ref_points[["Flim"]],
                           #Fpa = ref_points[["Fpa"]],
                           ConfidenceIntervalDefinition = "95%")

Year <- c(min(Summary_SAG$Year):assessment_year)
Year_rec <- c(min(Summary_SAG$Year):(assessment_year-1))



###-----------------------------------------------------------------------------
### Fish data for SAG

Year <- Summary_SAG[,c("Year")]
  
df_ssb <- Summary_SAG[,c("Year","UWTV_abundance_millions","Coef_Inter_millions")] 
df_ssb$Low <- df_ssb$UWTV_abundance_millions - df_ssb$Coef_Inter_millions
df_ssb$high <- df_ssb$UWTV_abundance_millions + df_ssb$Coef_Inter_millions
df_ssb <- df_ssb[,c("Year","UWTV_abundance_millions","Low","high")]
colnames(df_ssb) <- c("Year","Estimate","Low","High")

df_fbar <- Summary_SAG[,c("Year","Harvest_Rate")]
colnames(df_fbar) <- c("Year","Estimate")

df_exploitation <- Summary_SAG[,c("Year","Landings_t","Discards_t")]
df_exploitation$catches <- df_exploitation$Landings_t + df_exploitation$Discards_t
colnames(df_exploitation) <- c("Year","landing","discard","catches")


fishdata <- icesSAG::stockFishdata(Year,
                                   #Low_Recruitment = c(df_rec$Low, NA),
                                   #Recruitment = c(df_rec$Estimate, df_sam_stf_rec$median),
                                   #High_Recruitment = c(df_rec$High, NA),
                                   Low_StockSize = c(df_ssb$Low),
                                   StockSize = c(df_ssb$Estimate),
                                   High_StockSize = c(df_ssb$High),
                                   #Low_TBiomass = c(df_tsb$Low, df_sam_stf_tsb$low),
                                   #TBiomass = c(df_tsb$Estimate, df_sam_stf_tsb$median),
                                   #High_TBiomass = c(df_tsb$High, df_sam_stf_tsb$high),
                                   #Low_FishingPressure = c(df_fbar$Low, NA),
                                   FishingPressure = c(df_fbar$Estimate),
                                   #High_FishingPressure = c(df_fbar$High, NA),
                                   Catches = round(c(df_exploitation$catches), digits = 0),
                                   Landings = round(c(df_exploitation$landing), digits = 0),
                                   Discards = round(c(df_exploitation$discard), digits = 0))

###-----------------------------------------------------------------------------
# create an XML file to upload yourself
xml <- createSAGxml(info, fishdata)

#
writeSAGxml(info, fishdata, file = "upload_for_sage.xml")

icesSAG::uploadStock(file = "upload_for_sage.xml",
                     upload = FALSE,
                     verbose = TRUE)

saveRDS(list(info = info,
             fishdata = fishdata),
        "report/sag_objects.rds"  )



##------------------------------------------------------------------------------
##------------------------------------------------------------------------------
### Input Data Graphs 

# LANGOLF-TV Time Serie
plot_LangolfTV <- ggplot(fu2324.assess, aes(x = year)) +
  geom_bar(aes(y = abund), 
           stat = "identity", linewidth = 2, show.legend = FALSE, fill = "#00AFBB", color = "cyan4") +
  labs(title = "LANGOLF-TV abundance indice",
       x = "Year",
       y = "Abundance indice (millions)") +
  geom_text(aes(y = abund, label = round(abund, 1)), 
            vjust = -1.0, size = 4, color = "darkslategray") +
  scale_x_continuous(limits = c(2015, curr.year.adg + 1), breaks = 2015:curr.year.adg + 1) +
  scale_y_continuous(limits = c(0, max(fu2324.assess$abund, na.rm = T) * 1.2)) +
  theme_minimal()


plot_LangolfTV  %>%  ggexport(filename = paste("output/LANGOLF_TV_Abundance_TS.png",sep=""), width = 800, height = 400)

#-------------------------------------------------------------------------------
# Landings, Discards and Removals Time Serie in number

ymax <- max(fu2324.assess$int.lan.num,fu2324.assess$int.dis.num,fu2324.assess$removals.n, na.rm = TRUE)

plot_nb <- ggplot(fu2324.assess, aes(x = year)) +
  geom_line(aes(y = int.lan.num, color = "Landings"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = int.lan.num, color = "Landings", shape = "Landings"), size = 4, na.rm = TRUE) +
  
  geom_line(aes(y = int.dis.num, color = "Discards"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = int.dis.num, color = "Discards", shape = "Discards"), size = 4, na.rm = TRUE) +
  
  geom_line(aes(y = removals.n, color = "Removals"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = removals.n, color = "Removals", shape = "Removals"), size = 4, na.rm = TRUE) +
  
  geom_text(aes(y = int.lan.num, label = round(int.lan.num, 0)), 
            vjust = 2.0, size = 3, color = "darkslategray") +
  geom_text(aes(y = int.dis.num, label = round(int.dis.num, 0)), 
            vjust = -1.0, size = 3, color = "darkred") +
  geom_text(aes(y = removals.n, label = round(removals.n, 0)), 
            vjust = -1.0, size = 3, color = "darkorange") +
  
  scale_color_manual(
    name = "Category",
    values = c("Landings" = "#00AFBB", "Discards" = "darkred", "Removals" = "darkorange")
  ) +
  
  scale_shape_manual(
    name = "Category",
    values = c("Landings" = 16, "Discards" = 17, "Removals" = 15)  # cercle, triangle, carre
  ) +
  
  labs(
    title = "Landings, Discards and Removals Time Serie in number (millions)",
    x = "Year",
    y = "number (millions)"
  ) +
  
  scale_y_continuous(limits = c(0, ymax * 1.2)) +
  theme_minimal() +
  theme(legend.position = "right")

plot_nb  %>%  ggexport(filename = paste("output/LAN_DIS_REM_Nb_TS.png",sep=""), width = 800, height = 400)

#-------------------------------------------------------------------------------
# Landings and Discards Time Serie in weight

ymax <- max(fu2324.assess$int.lan.wgt,fu2324.assess$int.dis.wgt, na.rm = TRUE)

plot_wght <- ggplot(fu2324.assess, aes(x = year)) +
  geom_line(aes(y = int.lan.wgt, color = "Landings"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = int.lan.wgt, color = "Landings", shape = "Landings"), size = 4, na.rm = TRUE) +
  
  geom_line(aes(y = int.dis.wgt, color = "Discards"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = int.dis.wgt, color = "Discards", shape = "Discards"), size = 4, na.rm = TRUE) +
  
  geom_text(aes(y = int.lan.wgt, label = round(int.lan.wgt, 0)), 
            vjust = -1.0, size = 3, color = "darkslategray") +
  geom_text(aes(y = int.dis.wgt, label = round(int.dis.wgt, 0)), 
            vjust = 2.0, size = 3, color = "darkred") +
  
  scale_color_manual(
    name = "Category",
    values = c("Landings" = "#00AFBB", "Discards" = "darkred")
  ) +
  scale_shape_manual(
    name = "Category",
    values = c("Landings" = 16, "Discards" = 17, "Removals" = 15)  # cercle, triangle, carre
  ) +
  
  labs(
    title = "Landings and Discards Time Serie in weight (tonnes)",
    x = "Year",
    y = "weigth (tonnes)"
  ) +
  
  scale_y_continuous(limits = c(0, ymax * 1.2)) +
  theme_minimal() +
  theme(legend.position = "right")

plot_wght  %>%  ggexport(filename = paste("output/LAN_DIS_Wght_TS.png",sep=""), width = 800, height = 400)


#-------------------------------------------------------------------------------
# Individual Mean weight of Landings and Discards Time Serie

ymax <- max(fu2324.assess$mw.lan,fu2324.assess$mw.dis, na.rm = TRUE)

plot_ind_mean_wght <- ggplot(fu2324.assess, aes(x = year)) +
  geom_line(aes(y = mw.lan, color = "Landings"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = mw.lan, color = "Landings", shape = "Landings"), size = 4, na.rm = TRUE) +
  
  geom_line(aes(y = mw.dis, color = "Discards"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = mw.dis, color = "Discards", shape = "Discards"), size = 4, na.rm = TRUE) +
  
  geom_text(aes(y = mw.lan, label = round(mw.lan, 1)), 
            vjust = -1.0, size = 3, color = "darkslategray") +
  geom_text(aes(y = mw.dis, label = round(mw.dis, 1)), 
            vjust = 2.0, size = 3, color = "darkred") +
  
  scale_color_manual(
    name = "Category",
    values = c("Landings" = "#00AFBB", "Discards" = "darkred")
  ) +
  scale_shape_manual(
    name = "Category",
    values = c("Landings" = 16, "Discards" = 17)  # cercle, triangle
  ) +
  
  labs(
    title = "Individual mean weights of Landings and Discards Time Serie (g)",
    x = "Year",
    y = "weigth (g)"
  ) +
  
  scale_y_continuous(limits = c(0, ymax * 1.2)) +
  theme_minimal() +
  theme(legend.position = "right")

plot_ind_mean_wght  %>%  ggexport(filename = paste("output/LAN_DIS_Individual_mean_Wght_TS.png",sep=""), width = 800, height = 400)


#-------------------------------------------------------------------------------
# Discards rate Time Serie

plot_dis_rate <- ggplot(fu2324.assess, aes(x = year)) +
  geom_line(aes(y = dis.rn, color = "Discard rate"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = dis.rn, color = "Discard rate", shape = "Discard rate"), size = 4, na.rm = TRUE) +
  
  geom_line(aes(y = dead.disc.r, color = "Dead discard rate"), linewidth = 1.2, na.rm = TRUE) +
  geom_point(aes(y = dead.disc.r, color = "Dead discard rate", shape = "Dead discard rate"), size = 4, na.rm = TRUE) +
  
  geom_text(aes(y = dis.rn, label = round(dis.rn, 2)), 
            vjust = -1.0, size = 3, color = "darkgoldenrod4") +
  geom_text(aes(y = dead.disc.r, label = round(dead.disc.r, 2)), 
            vjust = 2.0, size = 3, color = "darkorchid4") +
  
  scale_color_manual(
    name = "Category",
    values = c("Discard rate" = "darkgoldenrod4", "Dead discard rate" = "darkorchid4")
  ) +
  scale_shape_manual(
    name = "Category",
    values = c("Discard rate" = 16, "Dead discard rate" = 17)  # cercle, triangle
  ) +
  
  labs(
    title = "Discard rate and Dead discard rate Time Serie",
    x = "Year",
    y = "Rate (%)"
  ) +
  
  scale_y_continuous(limits = c(0, 1)) +
  theme_minimal() +
  theme(legend.position = "right")

plot_dis_rate  %>%  ggexport(filename = paste("output/Discard_Rate_TS.png",sep=""), width = 800, height = 400)





