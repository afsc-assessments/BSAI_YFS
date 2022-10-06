GET_BS_ACOMP1<-function(srv_sp_str="10210",max_age=45,Seas=1,FLT=4,Gender=1,Part=0,Ageerr=0,Lgin_lo=1,Lgin_hi=120,Nsamp=100,VAST=FALSE,vast_file="Proportions.csv"){
 ## create sql query
 species = srv_sp_str
 Count = readLines('sql/count_EBS.sql')
 Count = sql_filter(sql_precode = "IN", x =srv_sp_str , sql_code = Count, flag = '-- insert species')
 Count = sql_run(afsc, Count) %>% data.table() %>%
  dplyr::rename_all(toupper)
 
 
 # NBS_Count<-paste0("
 #		SELECT
 #    			afsc.race_length_nbs.species_code,
 #    			COUNT(DISTINCT afsc.race_length_nbs.hauljoin) AS nbs_hauls,
 #    			afsc.race_length_nbs.cruise
 #		FROM
 #    			afsc.race_length_nbs
 #		WHERE
 #   			 afsc.race_length_nbs.species_code = ",srv_sp_str,
 #		"GROUP BYs
 #   			 afsc.race_length_nbs.species_code,
 #    			afsc.race_length_nbs.cruise
 #		ORDER BY
 #   			 afsc.race_length_nbs.cruise")
 
 
 #   Count = data.table(sqlQuery(AFSC,EBS_Count))
 #  Count2 = data.table(sqlQuery(AFSC,NBS_Count))
 
 
 
 #  Count1$YEAR<-trunc(Count1$CRUISE/100)
 #  Count2$YEAR<-trunc(Count2$CRUISE/100)
 #  Count1<- Count1[,list(EBS_HAULS=sum(HAULS)),by="YEAR"]
 #  Count2<- Count2[,list(NBS_HAULS=sum(NBS_HAULS)),by="YEAR"]
 #  Count <- merge(Count1,Count2,all=T)
 #  Count[is.na(NBS_HAULS)]$NBS_HAULS<-0
 #  Count$HAULS<-Count$EBS_HAULS+Count$NBS_HAULS
 
 
 if(!VAST) {
  
  test1<-paste0("
		SELECT
    			haehnr.agecomp_ebs_plusnw_stratum.year,
    			haehnr.agecomp_ebs_plusnw_stratum.stratum,
    			haehnr.agecomp_ebs_plusnw_stratum.age,
    			SUM(haehnr.agecomp_ebs_plusnw_stratum.agepop) AS sum_agepop,
    			haehnr.agecomp_ebs_plusnw_stratum.species_code
		FROM
    			haehnr.agecomp_ebs_plusnw_stratum
		WHERE
    			haehnr.agecomp_ebs_plusnw_stratum.stratum > 9999
    			AND haehnr.agecomp_ebs_plusnw_stratum.age > 0
    			AND haehnr.agecomp_ebs_plusnw_stratum.sex < 9
    			AND haehnr.agecomp_ebs_plusnw_stratum.species_code = ",srv_sp_str,
                "GROUP BY
    			haehnr.agecomp_ebs_plusnw_stratum.year,
    			haehnr.agecomp_ebs_plusnw_stratum.stratum,
    			haehnr.agecomp_ebs_plusnw_stratum.age,
    			haehnr.agecomp_ebs_plusnw_stratum.species_code
			ORDER BY
    			haehnr.agecomp_ebs_plusnw_stratum.year,
    			haehnr.agecomp_ebs_plusnw_stratum.age")
  
  test2<-paste0("
		SELECT
    			haehnr.agecomp_nbs_stratum.year,
    			haehnr.agecomp_nbs_stratum.stratum,
    			haehnr.agecomp_nbs_stratum.age,
    			SUM(haehnr.agecomp_nbs_stratum.agepop) AS sum_agepop_NBS,
    			haehnr.agecomp_nbs_stratum.species_code
		FROM
    			haehnr.agecomp_nbs_stratum
		WHERE
    			haehnr.agecomp_nbs_stratum.stratum > 9999
    			AND haehnr.agecomp_nbs_stratum.age > 0
    			AND haehnr.agecomp_nbs_stratum.species_code = ", srv_sp_str,
                "AND haehnr.agecomp_nbs_stratum.sex < 9
		GROUP BY
    			haehnr.agecomp_nbs_stratum.year,
    			haehnr.agecomp_nbs_stratum.stratum,
    			haehnr.agecomp_nbs_stratum.age,
    			haehnr.agecomp_nbs_stratum.species_code
		ORDER BY
    			haehnr.agecomp_nbs_stratum.year,
    			haehnr.agecomp_nbs_stratum.age")
  
  ## run database query
  Acomp1 = sqlQuery(AFSC,test1)
  Acomp2 = sqlQuery(AFSC,test2)
  Acomp3 = data.table(merge(Acomp1,Acomp2,all=T))
  Acomp3[is.na(SUM_AGEPOP_NBS)]$SUM_AGEPOP_NBS<-0
  Acomp3$AGEPOP<-Acomp3$SUM_AGEPOP+Acomp3$SUM_AGEPOP_NBS
  
  Acomp<-data.frame(YEAR=Acomp3$YEAR,AGE=Acomp3$AGE,AGEPOP=Acomp3$AGEPOP)
  Acomp1_plus<-data.frame(YEAR=Acomp1$YEAR,AGE=Acomp1$AGE,AGEPOP=Acomp1$SUM_AGEPOP)
  max_age=20
  #set up an EBS and NBS dataset
  
  YR<-unique(sort(Acomp$YEAR))
  grid=expand.grid(AGE=c(0:max_age),YEAR=YR)
  Acomp30<-subset(Acomp,Acomp$AGE>=max_age)
  
  if(nrow(Acomp30)>0){
   A30<-aggregate(list(AGEPOP=Acomp30$AGEPOP),by=list(YEAR=Acomp30$YEAR),FUN=sum)
   A30$AGE=max_age
   Acomp<-subset(Acomp,Acomp$AGE<max_age)
   Acomp<-merge(Acomp,A30,all=T)
  }
  
  Acomp<-merge(grid,Acomp,all=T)
  Acomp$AGEPOP[is.na(Acomp$AGEPOP)==T]<-0
 }
 
 
 #set up an EBS only dataset
 
 YR<-unique(sort(Acomp1_plus$YEAR))
 grid=expand.grid(AGE=c(0:max_age),YEAR=YR)
 Acomp20<-subset(Acomp1_plus,Acomp1_plus$AGE>=max_age)
 
 if(nrow(Acomp20)>0){
   A20<-aggregate(list(AGEPOP=Acomp20$AGEPOP),by=list(YEAR=Acomp20$YEAR),FUN=sum)
   A20$AGE=max_age
   Acomp1_plus<-subset(Acomp1_plus,Acomp1_plus$AGE<max_age)
   Acomp1_plus<-merge(Acomp1_plus,A20,all=T)
 }
 
 Acomp1_plus<-merge(grid,Acomp1_plus,all=T)
 Acomp1_plus$AGEPOP[is.na(Acomp1_plus$AGEPOP)==T]<-0
 
 
 write.csv(Acomp1,"YFS_SurveyAgecompEBSNBS.csv")
 write.csv(Acomp1_plus,"YFS_SurveyAgecompEBS.csv")
 

 
 }
 
 if(VAST){
  setwd(paste0(working_dir,"\\Functions\\ALT_DATA\\VAST"))
  
  if (!file.access(vast_file,mode=4))
  {
   Proportions<-data.table(read.csv(vast_file,header=T))
  }
  Proportions<-Proportions[Region=='Both']
  YR<-unique(sort(Proportions$Year))
  n=ncol(Proportions)-2
  AGECOMP<-Proportions[,2:n]
  
  if(max_age > ncol(AGECOMP)-1){ max_age <- ncol(AGECOMP)-1}
  
  MAGE=max_age+1
  if(ncol(AGECOMP) > MAGE){
   AGECOMP1<-AGECOMP[,1:MAGE]
   AGECOMP1_plus<-AGECOMP[,(MAGE+1):ncol(AGECOMP1)]
   AGEP<-rowSums(AGECOMP1_plus)
   AGECOMP1[,MAGE:MAGE]<-AGECOMP1[,MAGE:MAGE]+AGEP
   AGECOMP<-AGECOMP1
  }
  AGECOMP$YEAR<-Proportions$Year
  names(AGECOMP)<-c(0:max_age,"YEAR")
  Acomp<-melt(AGECOMP,"YEAR")
  names(Acomp)<-c("YEAR","AGE","AGEPOP")
 }
 setwd(working_dir)
 
 FIN<-MAGE+9 
 Nsamp<-Count[YEAR%in%YR]$HAULS
 y<-matrix(ncol=FIN,nrow=length(YR))
 x<-data.frame(y)
 names(x)<-c("YEAR","Seas","FltSvy","Gender","Part","Ageerr","Lgin_lo","Lgin_hi","Nsamp",paste0("F",c(0:max_age)))
 x$YEAR=YR
 x$Seas=Seas
 x$FltSvy=FLT
 x$Gender=Gender
 x$Part=Part
 x$Ageerr=Ageerr
 x$Lgin_lo=Lgin_lo
 x$Lgin_hi=Lgin_hi
 x$Nsamp=Nsamp
 
 for (i in 1:length(YR)){
  x[i,10:FIN]<-Acomp$AGEPOP[Acomp$YEAR==YR[i]]
 }
 
 x
}