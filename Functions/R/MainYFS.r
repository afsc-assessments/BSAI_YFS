#This set of code includes instructions for putting together the input file for the fm model (yfs_2021.dat, etc.)

#For any RACEBASE queries, YFS is 10210.
#In Obsint (observer database), YFS is 140.
#AI catches of YFS are considered  negligible so the AI data is not used in the assessment.

setwd("C:/Users/ingrid.spies/Downloads/FunctionsYFS/Functions")  ## need to set the working directory to the Functions folder  

source('R/utils.r')
source('R/LENGTH_BY_CATCH_BS_short.r')
source('R/GET_BS_BIOM.r')
source('R/GET_BS_ACOMP1.r')
source('R/GET_RAW_AGE_LEN_WT.R')

devtools::install_github("BenWilliams-NOAA/swo")

libs <- c("tidyverse", "dplyr","RODBC","mgcv","FSA","nlstools","data.table","ggplot2","sizeMat","devtools","r4ss","lubridate","rgdal","fishmethods","reshape2","vcdExtra","misty","swo")

if(length(libs[which(libs %in% rownames(installed.packages()) == FALSE )]) > 0) {
  install.packages(libs[which(libs %in% rownames(installed.packages()) == FALSE)])}
  lapply(libs, library, character.only = TRUE)

afsc_user  = "spiesi"   ## enter afsc username
afsc_pwd   = "ISis_aug_31324"    ## enter afsc password
akfin_user = "ispies"  ## enter AKFIN username
akfin_pwd  = "sed3r3ct"   ## enter AKFIN password

afsc = DBI::dbConnect(odbc::odbc(), "afsc", UID = afsc_user, PWD = afsc_pwd)
akfin = DBI::dbConnect(odbc::odbc(), "akfin",UID = akfin_user, PWD = akfin_pwd)
AFSC=odbcConnect("AFSC",afsc_user,afsc_pass,believeNRows=FALSE)
CHINA=odbcConnect("AKFIN",akfin_user,akfin_pass,believeNRows=FALSE)

#survey biomass
GET_BS_BIOM(srv_sp_str="10210")

#oac_srv go into this function to create survey age comps for EBS and EBS+NBS
GET_BS_ACOMP1()  #will give you the two files below.

#Survey weight at age
#Get survey age frequencies. These were normalized to the observed length frequencies in the population and These are just normalized so that males and females add to 1.
srv_age=read.csv("/Users/ingridspies/Documents/WorkDellStuff/Assessments/YFS/2022/YFS_SurveyAgecompEBS.csv",header=TRUE)
srv_age=read.csv("/Users/ingridspies/Documents/WorkDellStuff/Assessments/YFS/2022/YFS_SurveyAgecompEBSNBS.csv",header=TRUE)
colnames(srv_age)[6]='AGEPOP'

yrs=names(table(srv_age$YEAR))
agematF=matrix(0,length(yrs),20)
colnames(agematF)=seq(1,20,1);rownames(agematF)=yrs
agematM=agematF

for(i in 1:length(yrs)){
 for(j in 1:19){
  agematF[i,j]=sum(srv_age$AGEPOP[which(srv_age$YEAR==yrs[i]&srv_age$AGE==j&srv_age$SEX==2)],na.rm=TRUE)
  agematM[i,j]=sum(srv_age$AGEPOP[which(srv_age$YEAR==yrs[i]&srv_age$AGE==j&srv_age$SEX==1)],na.rm=TRUE)
  agematF[i,20]=sum(srv_age$AGEPOP[which(srv_age$YEAR==yrs[i]&srv_age$AGE>19&srv_age$SEX==2)],na.rm=TRUE)
  agematM[i,20]=sum(srv_age$AGEPOP[which(srv_age$YEAR==yrs[i]&srv_age$AGE>19&srv_age$SEX==1)],na.rm=TRUE)
 }
}
agematF_n=agematF/rowSums(agematF)
agematM_n=agematM/rowSums(agematM)
agematF_nn=agematF_n*(rowSums(agematF)/(rowSums(agematF)+rowSums(agematM)))
agematM_nn=agematM_n*(rowSums(agematM)/(rowSums(agematF)+rowSums(agematM)))

oac_srv=cbind(agematF_nn,agematM_nn)
write.csv(oac_srv,"/Users/ingridspies/Downloads/oac_srv.csv")

#oac_fsh
ysolF_LCOMP=LENGTH_BY_CATCH_short(species= 140 ,species_catch= 'YSOL', for_species_catch='YELLOWFIN SOLE',sp_area='BS' ,ly=2022, SEX=TRUE, PORT=FALSE)
ggplot(ysolF_LCOMP[[1]][LENGTH<60],aes(x=LENGTH,y=FREQ,color=SEX))+geom_line()+facet_wrap(~YEAR)+theme_bw(base_size=16)+labs(title="Bering Sea Yellowfin Sole fishery length compositions by year")+xlab("Length (cm)")+ylab("Frequency")

YFS_lens=data.frame(ysolF_LCOMP[[1]])

YFSAGE=GET_DOM_AGE(fsh_sp_str="140",sp_area="'BS'",max_age=45)

Year=seq(1990,2021,1)
oac_matrix=matrix(0,length(Year),40)#Create this matrix to hold the fishery age comps weighted by fishery length observations
colnames(oac_matrix)

for(k in 1:length(Year)){
#Get a vector of length frequencies of fishery lengths from observers
LF_F=(YFS_lens$FREQ[which(YFS_lens$YEAR==Year[k]&YFS_lens$SEX==1)])[1:80]
LF_M=(YFS_lens$FREQ[which(YFS_lens$YEAR==Year[k]&YFS_lens$SEX==2)])[1:80]

#Make a matrix of length at age from age data.
oac_LenAge_F=matrix(0,80,40);oac_LenAge_M=matrix(0,80,40)
rownames(oac_LenAge_F)=seq(1,80,1);rownames(oac_LenAge_M)=seq(1,80,1)
colnames(oac_LenAge_F)=seq(1,40,1);colnames(oac_LenAge_M)=seq(1,40,1)
for(j in 1:40){
  for (i in 1:80){
    oac_LenAge_F[i,j]=length(YFSAGE$AGE1[which(YFSAGE$YEAR==Year[k]&YFSAGE$LENGTH==i&YFSAGE$AGE1==j&YFSAGE$SEX=="F")])
    oac_LenAge_M[i,j]=length(YFSAGE$AGE1[which(YFSAGE$YEAR==Year[k]&YFSAGE$LENGTH==i&YFSAGE$AGE1==j&YFSAGE$SEX=="M")])
      }
}
FL=YFS_lens$NSAMP[which(YFS_lens$YEAR==Year[k]&YFS_lens$SEX==1)][1]
FM=YFS_lens$NSAMP[which(YFS_lens$YEAR==Year[k]&YFS_lens$SEX==2)][1]
PropF=FL/(FM+FL)

colSums(oac_LenAge_F)#This is the frequency of each age that were female.
T1_f=oac_LenAge_F/rowSums(oac_LenAge_F)#normalize so ages at each length sum to 1.
T2_f=LF_F*T1_f  #Apply fishery length frequencies to the proportion of age at length matrix
T3_f=colSums(T2_f,na.rm=TRUE)/sum(T2,na.rm=TRUE)#sum over columns and normalize to 1
T4_f=c(T3_f[1:19],sum(T3_f[20:40]))#Create a 20plus group. 
T5_f=T4_f*PropF  #Multiply by the proportion of females lengthed in the fishery that year so age at length sums to 1 for males and females

T1_m=oac_LenAge_M/rowSums(oac_LenAge_M)#normalize so ages at each length sum to 1.
T2_m=LF_M*T1_m  #Apply fishery length frequencies to the proportion of age at length matrix
T3_m=colSums(T2_m,na.rm=TRUE)/sum(T2,na.rm=TRUE)#sum over columns and normalize to 1
T4_m=c(T3_m[1:19],sum(T3_m[20:40]))#Create a 20plus group. 
T5_m=T4_m*(1-PropF)  #Multiply by the proportion of females lengthed in the fishery that year so age at length sums to 1 for males and females

oac_matrix[k,]=c(T5_f,T5_m)
}

write.csv(oac_matrix,"C:/Users/ingrid.spies/Downloads/oac_matrix.csv")


#Get survey weight at age
srv_wtage=GET_RAW_AGE_LEN_WT()

table(srv_wtage$REGION)#some GOA and BS in here.
#yrs=as.numeric(names(table(srv_wtage$Year[which(srv_wtage$REGION=='BS')])))#do 1982 and later

yrs=seq(1982,2022,1)[-39]
mat_wtage_srv_F=matrix(0,length(yrs),20)
colnames(mat_wtage_srv_F)=seq(1,20,1)
rownames(mat_wtage_srv_F)=yrs
mat_wtage_srv_M=mat_wtage_srv_F
age=seq(1,20,1)
for (i in 1:length(yrs)){
 for(j in 1:19){
  mat_wtage_srv_F[i,j]=mean(srv_wtage$WEIGHT[which(srv_wtage$Year==yrs[i]&srv_wtage$AGE==age[j]&srv_wtage$SEX=='2')],na.rm=TRUE)
  mat_wtage_srv_F[i,20]=mean(srv_wtage$WEIGHT[which(srv_wtage$Year==yrs[i]&srv_wtage$AGE>19&srv_wtage$SEX=='2')],na.rm=TRUE)
  mat_wtage_srv_M[i,j]=mean(srv_wtage$WEIGHT[which(srv_wtage$Year==yrs[i]&srv_wtage$AGE==age[j]&srv_wtage$SEX=='1')],na.rm=TRUE)
  mat_wtage_srv_M[i,20]=mean(srv_wtage$WEIGHT[which(srv_wtage$Year==yrs[i]&srv_wtage$AGE>19&srv_wtage$SEX=='1')],na.rm=TRUE)
 }
}
write.csv(round(mat_wtage_srv_F),"/Users/ingridspies/Downloads/mat_wtage_srv_F.csv")
write.csv(round(mat_wtage_srv_M),"/Users/ingridspies/Downloads/mat_wtage_srv_M.csv")
#males age 1 just add 4g for weight and females add 6g for weight.


#get bottom temperature
bottomtemp=coldpool::cold_pool_index$MEAN_BT_LT100M
tempanom=bottomtemp-mean(bottomtemp)

#get date anomaly

my_date <- as.Date("2022-05-31")
my_julian2 <- format(my_date, "%j")
