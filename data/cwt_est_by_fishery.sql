select rc.runyear as run_year, fe.FisheryERAID as fishery_id, fe.Name as fishery_name,sum(AdjustedEstimatedNumber) as cwt_estimate
from ((((CWDBRecovery rc 
inner join WireTagCode rl on rc.TagCode = rl.TagCode)
inner join CAMPFisheryFine ff on rc.Fishery = ff.FisheryFineID)
inner join CAMPFisheryCoarse fc on ff.FisheryCoarseID = fc.FisheryCoarseID)
inner join CAMPFisheryERA fe on fc.FisheryERAID = fe.FisheryERAID)
where RunYear > 2004 and RunYear < 2024
and rc.CWDBFishery = '40'
and rc.Agency = 'CDFO'
and rl.Included = 1
and rl.Auxiliary = 0
group by rc.runyear, fe.FisheryERAID, fe.Name
order by runyear, fe.FisheryERAID