test<-paste("SELECT afsc.haehnr.BIOMASS_NBS_AKFIN.YEAR as YEAR,\n ",
            "afsc.haehnr.BIOMASS_NBS_AKFIN.BIOMASS as BIOM,\n ",
            "afsc.haehnr.BIOMASS_NBS_AKFIN.POPULATION as POP,\n ",
            "afsc.haehnr.BIOMASS_NBS_AKFIN.VARBIO as BIOMVAR,\n ",
            "afsc.haehnr.BIOMASS_NBS_AKFIN.VARPOP as POPVAR,\n ",
            "afsc.haehnr.BIOMASS_NBS_AKFIN.HAULCOUNT as NUMHAULS,\n ",
            "afsc.haehnr.BIOMASS_NBS_AKFIN.CATCOUNT as NUMCAUGHT\n ",
            "FROM afsc.haehnr.BIOMASS_NBS_AKFIN\n ",
            "WHERE afsc.haehnr.BIOMASS_NBS_AKFIN.stratum = 999 \n",
            "AND afsc.haehnr.BIOMASS_NBS_AKFIN.SPECIES_CODE in (",srv_sp_str,") \n ",
            "ORDER BY afsc.haehnr.BIOMASS_NBS_AKFIN.YEAR",sep="")


test=paste("select h.vessel||','||
   h.haul||',' ||
   h.cruise||',' ||
   h.nmfs_area                   ||','||
   to_char(h.haul_date,'mm')     ||','||
   to_char(h.haul_date,'dd')     ||','|| 
   to_char(h.haul_date,'yyyy')   ||','||
   to_char((h.latdd_end+h.latdd_start)/2,'0999.999') ||','||
   to_char((h.londd_end+h.londd_start)/2,'09999.999') ||','||
   h.vessel_type                 ||','||
   h.official_total_catch        ||','||
   x.extrapolated_weight         ||','||
   x.extrapolated_number         ||','||
   h.nmfs_area      			||','|| 
   h.gear_type
   
 from 
   current_haul  h, 
   current_spcomp  x
where
  /*join between domestic_age and domestic_hauls trunc(h.latitude/100)+ h.latitude-(trunc(h.latitude/100) ||','|| trunc(h.longitude/100)||','|| */      
  h.haul_join=x.haul_join and
  /*x.species =202 and*/
  x.species = 140 and
  h.year between 1920 and 2022 and  
  h.nmfs_area between 500 and 539 ;")

fishingYFS <- sqlQuery(AFSC,test)
