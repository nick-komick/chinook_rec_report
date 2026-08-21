select * from (select distinct reg.reg_description, sts_stra_id, sts_year, 
case when STS_PERIOD_POOL_START  = 1 and STS_PERIOD_POOL_END = 12 then 'Annual'
 when STS_PERIOD_POOL_START  = 1 then 'Spring'
  when STS_PERIOD_POOL_START  = 5 then 'Summer'
  when STS_PERIOD_POOL_START  = 10 then 'Fall'
  end as season,  
  case when STS_PERIOD_POOL_START  = 1 and STS_PERIOD_POOL_END = 12 then 1
 when STS_PERIOD_POOL_START  = 1 then 1
  when STS_PERIOD_POOL_START  = 5 then 2
  when STS_PERIOD_POOL_START  = 10 then 3
  end as season_id ,sts_submit_rate, est_method_name
from mrp_ops.mrp_strata_summary_tbl sts
inner join mrp_ops.mrp_region_tbl reg on sts.sts_reg_id = reg.reg_id
inner join mrp_ops.mrp_est_method_tbl est_mthd on sts.sts_est_method_id = est_method_id 
where sts_year > 2004 and sts_species_id = 124 and sts_samp_pgrm_code = 'S')
order by sts_stra_id, sts_year, season_id


