SELECT RACEBASE.SPECIMEN.REGION,
                RACE_DATA.V_CRUISES.YEAR AS YEAR,
                RACEBASE.SPECIMEN.CRUISE,
                RACEBASE.HAUL.VESSEL,
                RACEBASE.SPECIMEN.HAULJOIN,
                RACEBASE.SPECIMEN.HAUL,
                RACEBASE.SPECIMEN.SPECIES_CODE,
                RACEBASE.SPECIMEN.LENGTH,
                RACEBASE.SPECIMEN.SEX,
                RACEBASE.SPECIMEN.WEIGHT,
                RACEBASE.SPECIMEN.MATURITY,
                RACEBASE.SPECIMEN.AGE,
                RACEBASE.HAUL.END_LONGITUDE,
                RACEBASE.HAUL.HAUL_TYPE,
                RACEBASE.HAUL.GEAR,
                RACEBASE.HAUL.PERFORMANCE,
                RACEBASE.SPECIMEN.SPECIMEN_SAMPLE_TYPE,
                RACEBASE.SPECIMEN.SPECIMENID,
                RACEBASE.SPECIMEN.BIOSTRATUM
FROM RACE_DATA.V_CRUISES 
                INNER JOIN RACEBASE.HAUL 
                ON RACEBASE.HAUL.CRUISEJOIN = RACE_DATA.V_CRUISES.CRUISEJOIN 
                INNER JOIN RACEBASE.SPECIMEN 
                ON RACEBASE.SPECIMEN.CRUISEJOIN = RACEBASE.HAUL.CRUISEJOIN 
                AND RACEBASE.SPECIMEN.HAULJOIN  = RACEBASE.HAUL.HAULJOIN 
WHERE RACE_DATA.V_CRUISES.SURVEY_DEFINITION_ID 
                -- insert survey 
                AND RACEBASE.SPECIMEN.SPECIES_CODE 
                -- insert species
                AND RACEBASE.HAUL.HAUL_TYPE = 3 
                AND RACEBASE.HAUL.PERFORMANCE >= 0  
                AND RACEBASE.HAUL.STATIONID IS NOT NULL 
                AND RACE_DATA.V_CRUISES.YEAR 
                -- insert year
ORDER BY RACE_DATA.V_CRUISES.YEAR