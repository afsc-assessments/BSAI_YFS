#Get raw age comps with lenght and weight for Bering Sea for YFS

test=paste0("SELECT
a.cruise,
a.region, 
a.vessel,
a.stratum,
a.haul,
a.start_latitude,
a.start_longitude,
a.end_latitude,
a.end_longitude,
a.gear_depth,
b.specimenid,
b.age,
b.age_determination_method,
b.sex,
b.species_code,
b.length,
b.weight
from racebase.haul a,
racebase.specimen b
where a.hauljoin = b.hauljoin
and b.species_code = 10210
and a.region = 'BS'")

ALWraw = sqlQuery(AFSC,test)
ALWraw
