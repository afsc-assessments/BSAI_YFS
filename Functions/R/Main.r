setwd(".../Functions")  ## need to set the working directory to the Functions folder  

source('R/utils.r')
source('R/LENGTH_BY_CATCH_BS_short.r')
source('R/GET_BS_BIOM.r')



libs <- c("tidyverse", "dplyr","RODBC","mgcv","FSA","nlstools","data.table","ggplot2","sizeMat","devtools","r4ss","lubridate","rgdal","fishmethods","reshape2","swo","vcdExtra","misty")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
  lapply(libs, library, character.only = TRUE)


afsc_user  = "****"   ## enter afsc username
afsc_pwd   = "****"    ## enter afsc password
akfin_user = "****"  ## enter AKFIN username
akfin_pwd  = "****"   ## enter AKFIN password


  afsc = DBI::dbConnect(odbc::odbc(), "afsc",
                      UID = afsc_user, PWD = afsc_pwd)
  akfin = DBI::dbConnect(odbc::odbc(), "akfin",
                      UID = akfin_user, PWD = akfin_pwd)


ysolF_LCOMP=LENGTH_BY_CATCH_short(species= 140 ,species_catch= 'YSOL', for_species_catch='YELLOWFIN SOLE',sp_area='BS' ,ly=2022, SEX=TRUE, PORT=FALSE)
ggplot(ysolF_LCOMP[[1]][LENGTH<60],aes(x=LENGTH,y=FREQ,color=SEX))+geom_line()+facet_wrap(~YEAR)+theme_bw(base_size=16)+labs(title="BS Yellowfin sole Fishery Lcomps")


aipcod=LENGTH_BY_CATCH_short(species= 202 ,species_catch= 'PCOD', for_species_catch='PACIFIC COD',sp_area='AI' ,ly=2022, SEX=FALSE, PORT=FALSE)
gfplot(aipcod[1]],aes(x=LENGTH,y=FREQ))+geom_line()+facet_wrap(~YEAR)+theme_bw(base_size=16)+labs(title="AI Pcod Fishery Lcomps")


ysol[[1]] ## length comp with single fleet
aipcod[[1]] ## length comp with single fleet
aipcod[[2]] ## length comp with three fleets