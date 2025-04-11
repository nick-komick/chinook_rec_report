select rc.run_year as run_year, fe.fishery_era_id as fishery_id, fe.Name as fishery_name,sum(rc.adjusted_estimated_number) as cwt_estimate
from (((camp_cwt_recovery rc 
inner join camp_cwt_release rl on rc.tag_code = rl.tag_code)
inner join camp_fishery_fine ff on rc.fishery_fine_id = ff.fishery_fine_id)
inner join camp_fishery_era fe on ff.fishery_era_id = fe.fishery_era_id)
where run_year > 2004 and run_year < 2024
and rc.fishery = '40'
and rc.reporting_agency = 'CDFO'
and rl.Included = 1
and rl.Auxiliary = 0
group by rc.run_year, fe.fishery_era_id, fe.Name
order by run_year, fe.fishery_era_id