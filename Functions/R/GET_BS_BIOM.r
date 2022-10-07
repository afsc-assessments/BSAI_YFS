# adapted/generalized from Steve Barbeaux' files for
# generating SS files for EBS/AI Greenland Turbot
# ZTA, 2013-05-08, R version 2.15.1, 32-bit

GET_BS_BIOM <- function(srv_sp_str="10210")
{
 test<-paste("SELECT afsc.race_biomass_ebsshelf_plusnw.YEAR as YEAR,\n ",
             "afsc.race_biomass_ebsshelf_plusnw.BIOMASS as BIOM,\n ",
             "afsc.race_biomass_ebsshelf_plusnw.POPULATION as POP,\n ",
             "afsc.race_biomass_ebsshelf_plusnw.VARBIO as BIOMVAR,\n ",
             "afsc.race_biomass_ebsshelf_plusnw.VARPOP as POPVAR,\n ",
             "afsc.race_biomass_ebsshelf_plusnw.HAULCOUNT as NUMHAULS,\n ",
             "afsc.race_biomass_ebsshelf_plusnw.CATCOUNT as NUMCAUGHT\n ",
             "FROM afsc.race_biomass_ebsshelf_plusnw\n ",
             "WHERE afsc.race_biomass_ebsshelf_plusnw.stratum = 999 \n",
             "AND afsc.race_biomass_ebsshelf_plusnw.SPECIES_CODE in (",srv_sp_str,") \n ",
             "ORDER BY afsc.race_biomass_ebsshelf_plusnw.YEAR",sep="")
 
 biom <- sql_run(akfin,test)
 
 # this calculation assumes that YEAR is the first column for biom
 sum.biom <- aggregate(biom[,-1],by=list(YEAR=biom$YEAR),FUN=sum)
 sum.biom
 
}


haehnr.agecomp_nbs_stratum.year,
haehnr.agecomp_nbs_stratum.stratum,
haehnr.agecomp_nbs_stratum.age,
haehnr.agecomp_nbs_stratum.sex,
haehnr.agecomp_nbs_stratum.species_code

NBS<-paste("SELECT haehnr.BIOMASS_NBS_AKFIN.YEAR as YEAR,\n ",
                 "haehnr.BIOMASS_NBS_AKFIN.STRATUM_BIOMASS as BIOM,\n ",
                 "haehnr.BIOMASS_NBS_AKFIN.STRATUM_POP as POP,\n ",
                 "haehnr.BIOMASS_NBS_AKFIN.BIOMASS_VAR as BIOMVAR,\n ",
                 "haehnr.BIOMASS_NBS_AKFIN.POP_VAR as POPVAR,\n ",
                 "haehnr.BIOMASS_NBS_AKFIN.HAUL_COUNT as NUMHAULS,\n ",
                 "haehnr.BIOMASS_NBS_AKFIN.CATCH_COUNT as NUMCAUGHT\n ",
                 "FROM haehnr.BIOMASS_NBS_AKFIN\n ",
                 "WHERE haehnr.BIOMASS_NBS_AKFIN.STRATUM = 999 \n",
                 "AND haehnr.BIOMASS_NBS_AKFIN.SPECIES_CODE in (",srv_sp_str,") \n ",
                 "ORDER BY haehnr.BIOMASS_NBS_AKFIN.YEAR",sep="")

biomNBS <- sqlQuery(AFSC,NBS)
biomNBS <- sql_run(afsc,NBS)

biomNBS <- sqlQuery(afsc,NBS)
biomNBS <- sql_run(AFSC,NBS)


