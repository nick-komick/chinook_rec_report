library(tidyverse)
library(formattable)
library(openxlsx)

rm(list = ls()) 

# load functions
source("scripts\\GarciaFunLibrary.R")

mdt_data_dir <- r"(.\data\MDT)"

# ------------------------------------------------------------------------------

# define constants (tg: don't change the name of this - MDT2Excel needs it globally)
thisyr <- 2024

# results folder (tg: don't change the name of this MDT2Excel needs it globally)
# dir.create(paste0(getwd(),'/Results/',thisyr, 'ERAReport'))
# thisyr <- paste0('Results/', thisyr, "ERAReport")
outDir <- 'ream_output/'

# fishery lookups
fishery_lookup <- read.delim(file.path(mdt_data_dir, paste0(thisyr,"ERA_FisheryLookup.txt")), header=TRUE)

# stock lookups
slookup_cyer <- read.delim(file.path(mdt_data_dir, paste0(thisyr, "CYER_StockLookup_mod.txt")), header = TRUE)
slookup_mdt <- read.delim(file.path(mdt_data_dir, paste0(thisyr, "ERA_MDTStockLookup.txt")))

# escapement data
escapement <- read.csv(file.path(mdt_data_dir, paste0(thisyr, "EscData_GarciaPlots.csv")), header=TRUE)

# load hrts
hrt_ak <-          read.delim(file.path(mdt_data_dir, "alaska hatchery hrts.txt"))
hrt_hoh <-         read.delim(file.path(mdt_data_dir, "hoh.txt"))
hrt_quillayute <-  read.delim(file.path(mdt_data_dir, "quillayute.txt"))
hrt_graysharbor <- read.delim(file.path(mdt_data_dir, "graysharbor.txt"))
hrt_nehalem <-     read.delim(file.path(mdt_data_dir, "nehalem_mr.txt"))
hrt_siletz <-      read.delim(file.path(mdt_data_dir, "siletz_mr.txt"))
hrt_siuslaw <-     read.delim(file.path(mdt_data_dir, "siuslaw_mr.txt"))
hrt_southumpqua <- read.delim(file.path(mdt_data_dir, "southumpqua.txt"))
hrt_coquille <-    read.delim(file.path(mdt_data_dir, "coquille.txt"))
hrt_nwvi <-        read.delim(file.path(mdt_data_dir, "nwvi.txt"))
hrt_swvi <-        read.delim(file.path(mdt_data_dir, "swvi.txt"))
hrt_evin <-        read.delim(file.path(mdt_data_dir, "evin.txt"))
hrt_nooksack <-    read.delim(file.path(mdt_data_dir, "nooksack-v7.1.txt"))


################################################################################
# Previous ERA
################################################################################

# load HRJ data
z_cy4 <- addPTableHRJ(x=convertHRJ_RtoAccess(
  convertHRJ_BYtoCY(
    readHRJdir(userDir=file.path("ream_output", "Server HRJ", "unmarked"),
               nFisheries=nrow(fishery_lookup),
               straysinescap=TRUE,
               Age6="include"))),
  hrjclass = "Access")

# apply external HR adjustment
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_nwvi,        hrjstk="RBT", type="tm", newstkname="NWVI")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_swvi,        hrjstk="RBT", type="tm", newstkname="SWVI")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_evin,        hrjstk="QUI", type="tm", newstkname="EVIN")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_hoh,         hrjstk="QUE", type="tm", newstkname="Hoh")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_quillayute,  hrjstk="QUE", type="tm", newstkname="Quillayute")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_graysharbor, hrjstk="QUE", type="tm", newstkname="Grays Harbor")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_nehalem,     hrjstk="SRH", type="tm", newstkname="Nehalem")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_siletz,      hrjstk="SRH", type="tm", newstkname="Siletz")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_siuslaw,     hrjstk="SRH", type="tm", newstkname="Siuslaw")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_southumpqua, hrjstk="ELK", type="tm", newstkname="South Umpqua")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_coquille,    hrjstk="ELK", type="tm", newstkname="Coquille")
#z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_nooksack,    hrjstk="NSF", type="tm", newstkname="NSF adj")

# write outputs
fishery_lookup$CMB <- factor(fishery_lookup$MDT, levels = c("AABM SEAK Troll","AABM SEAK Net","AABM SEAK Sport",
                                                            "AABM NBC Troll","AABM NBC Sport",
                                                            "AABM WCVI Troll","AABM WCVI Sport",
                                                            "ISBM NBC&CBC Troll","ISBM NBC&CBC Net","ISBM NBC&CBC Sport",
                                                            "ISBM Southern BC Troll","ISBM Southern BC Net","ISBM Southern BC Sport",
                                                            "ISBM North of Falcon Troll","ISBM North of Falcon Sport","ISBM South of Falcon Troll","ISBM South of Falcon Sport",
                                                            "ISBM WA Coast Net","ISBM Puget Sound Net","ISBM Puget Sound Sport",
                                                            "Terminal SEAK Troll","Terminal SEAK Net","Terminal SEAK Sport",
                                                            "Terminal Canada Net","Terminal Canada Sport",
                                                            "Terminal Southern US Troll","Terminal Southern US Net","Terminal Southern US Sport",
                                                            "ESC"))

# cmz file
cmz <- MDT2CMZ(hrj=z_cy4,
               fmap=fishery_lookup,
               filename = paste0(outDir, "/", "MARKED_FEB_ERA_catchDistribution_CMZ_4ages.csv"))

# cyer limits

fishery_lookup$CMB <- factor(fishery_lookup$CYER, levels = c("AABM US","AABM Canada","ISBM US","ISBM Canada","Terminal","ESC"))

res_m_spring <- NULL  

slookup_cyer <- 
  slookup_cyer |>
  filter(ERIS %in% z_cy4$stknames)

for(i in 1:nrow(slookup_cyer)) {
  
  temp <- cyer(eis=as.character(slookup_cyer$EIS_StockName[i]),
               eris=as.character(slookup_cyer$ERIS[i]),
               fmap=fishery_lookup,
               smap=slookup_cyer,
               esc=escapement,
               hrjobj=z_cy4,
               period1=2009:2023,
               period2=2019:(thisyr-2))
  
  temp_criteria <- tibble(year=as.character(list(temp$MDT)[[1]]$Years), criteria=list(temp$MDT)[[1]]$Criteria)
  
  res_m_spring <- 
    data.frame(temp$CYER_BasePeriodAnnual, 
               stock_name=paste0(slookup_cyer$EIS_CleanStockName[i], " (", slookup_cyer$CleanERIS[i], ")"), 
               include = if_else(slookup_cyer$EIS_CleanStockName[i] == "-", FALSE, TRUE),
               region = slookup_cyer$EIS_Region[i],
               clip="Unmarked (2024 MSF)") |>
    rownames_to_column("year") |>
    pivot_longer(cols = starts_with("ISBM")) |>
    inner_join(temp_criteria, by = join_by(year)) |>
    mutate(value = ifelse(criteria != "ok", NA, value)) |>
    bind_rows(res_m_spring)
}

################################################################################
# MARKED SUMMER
################################################################################

# load HRJ data
z_cy4 <- addPTableHRJ(x=convertHRJ_RtoAccess(
  convertHRJ_BYtoCY(
    readHRJdir(userDir=file.path("ream_output", "Local HRJ", "unmarked"),
               nFisheries=nrow(fishery_lookup),
               straysinescap=TRUE,
               Age6="include"))),
  hrjclass = "Access")

# apply external HR adjustment
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_nwvi,        hrjstk="RBT", type="tm", newstkname="NWVI")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_swvi,        hrjstk="RBT", type="tm", newstkname="SWVI")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_evin,        hrjstk="QUI", type="tm", newstkname="EVIN")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_hoh,         hrjstk="QUE", type="tm", newstkname="Hoh")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_quillayute,  hrjstk="QUE", type="tm", newstkname="Quillayute")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_graysharbor, hrjstk="QUE", type="tm", newstkname="Grays Harbor")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_nehalem,     hrjstk="SRH", type="tm", newstkname="Nehalem")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_siletz,      hrjstk="SRH", type="tm", newstkname="Siletz")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_siuslaw,     hrjstk="SRH", type="tm", newstkname="Siuslaw")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_southumpqua, hrjstk="ELK", type="tm", newstkname="South Umpqua")
z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_coquille,    hrjstk="ELK", type="tm", newstkname="Coquille")
#z_cy4 <- externalHRadjustment(x = z_cy4, hrt=hrt_nooksack,    hrjstk="NSF", type="tm", newstkname="NSF adj")

# write outputs
fishery_lookup$CMB <- factor(fishery_lookup$MDT, levels = c("AABM SEAK Troll","AABM SEAK Net","AABM SEAK Sport",
                                                            "AABM NBC Troll","AABM NBC Sport",
                                                            "AABM WCVI Troll","AABM WCVI Sport",
                                                            "ISBM NBC&CBC Troll","ISBM NBC&CBC Net","ISBM NBC&CBC Sport",
                                                            "ISBM Southern BC Troll","ISBM Southern BC Net","ISBM Southern BC Sport",
                                                            "ISBM North of Falcon Troll","ISBM North of Falcon Sport","ISBM South of Falcon Troll","ISBM South of Falcon Sport",
                                                            "ISBM WA Coast Net","ISBM Puget Sound Net","ISBM Puget Sound Sport",
                                                            "Terminal SEAK Troll","Terminal SEAK Net","Terminal SEAK Sport",
                                                            "Terminal Canada Net","Terminal Canada Sport",
                                                            "Terminal Southern US Troll","Terminal Southern US Net","Terminal Southern US Sport",
                                                            "ESC"))

# cmz file
cmz <- MDT2CMZ(hrj=z_cy4,
               fmap=fishery_lookup,
               filename = paste0(outDir, "/", "MARKED_ERA_catchDistribution_CMZ_4ages.csv"))

# cyer limits

fishery_lookup$CMB <- factor(fishery_lookup$CYER, levels = c("AABM US","AABM Canada","ISBM US","ISBM Canada","Terminal","ESC"))

slookup_cyer <- 
  slookup_cyer |>
  filter(ERIS %in% z_cy4$stknames)
res_m_summer <- NULL  
for(i in 1:nrow(slookup_cyer)) {
  
  temp <- cyer(eis=as.character(slookup_cyer$EIS_StockName[i]),
               eris=as.character(slookup_cyer$ERIS[i]),
               fmap=fishery_lookup,
               smap=slookup_cyer,
               esc=escapement,
               hrjobj=z_cy4,
               period1=2009:2023,
               period2=2019:(thisyr-2))
  
  temp_criteria <- tibble(year=as.character(list(temp$MDT)[[1]]$Years), criteria=list(temp$MDT)[[1]]$Criteria)
  
  res_m_summer <- 
    data.frame(temp$CYER_BasePeriodAnnual, 
               stock_name=paste0(slookup_cyer$EIS_CleanStockName[i], " (", slookup_cyer$CleanERIS[i], ")"), 
               include = if_else(slookup_cyer$EIS_CleanStockName[i] == "-", FALSE, TRUE),
               region = slookup_cyer$EIS_Region[i],
               clip="Unmarked (Updated Rec)") |>
    rownames_to_column("year") |>
    pivot_longer(cols = starts_with("ISBM")) |>
    inner_join(temp_criteria, by = join_by(year)) |>
    mutate(value = ifelse(criteria != "ok", NA, value)) |>
    bind_rows(res_m_summer)
  
}

########
# PLOT #
########

res <- 
  bind_rows(res_m_spring, res_m_summer) |>
  tibble() |>
  mutate(year = as.integer(year),
         clip = factor(clip, levels=c('Unmarked (2024 MSF)', 'Unmarked (Updated Rec)')),
         full_name = paste0(stock_name, " - ", name)) |>
  filter(include == TRUE)
#
#windows(9.48, 4.61)
#
#today <- as.Date(Sys.Date(),format='%m/%d/%Y')
#date_stamp <- paste0(month(today, label=TRUE), " ", day(today), " ", year(today))
#
#stocks <- unique(res$stock)
#for(i in 1:length(stocks)) {
#  png(filename = file.path("./data/cyer_graph", paste0(gsub("/", "_", stocks[i]), "_cyer.png")),
#      width = 800, 
#      height = 400)
#  temp <- subset(res, res$stock == stocks[i])
#  g <- 
#    ggplot(data=temp, aes(x=year, y=value, colour=clip)) +
#    geom_point(shape=21, size=2.5) + geom_line() +
#    scale_colour_manual(values=c('gray50', 'black', '#7CAE00')) +
#    facet_wrap(~name) +
#    labs(x="Calendar Year", y="CYER", title=temp$stock[1]) +
#    theme(legend.title= element_blank()) + theme_bw() + 
#    xlim(c(2009, NA))
#  print(g)
#  dev.off()
#}
#
#res_out <- res
#res_out$stock <- paste0("' ", res_out$stock)
#
#write.csv(res_out, paste0("all cyer results ", date_stamp, ".csv"))

png(filename = "./data/cyer_graph_bc.png", width = 800, height = 1000)

g <-
  res |>
  filter(region == "BC") |>
  ggplot(aes(x=year, y=value, color=clip)) + 
  geom_point(shape=21, size=2.5) + 
  geom_line() +
  scale_colour_manual(values=c('gray50', 'black', '#7CAE00')) +
  facet_wrap(~full_name, scales = "free", ncol=2) +
  theme(legend.position="bottom", legend.title= element_blank())+
  theme_bw() + 
  labs(x="Calendar Year", y="CYER")

print(g)
dev.off()

png(filename = "./data/cyer_graph_wa_or.png", width = 800, height = 1500)

g <-
  res |>
  filter(region == "WA/OR") |>
  ggplot(aes(x=year, y=value, color=clip)) + 
  geom_point(shape=21, size=2.5) + 
  geom_line() +
  scale_colour_manual(values=c('gray50', 'black', '#7CAE00')) +
  facet_wrap(~full_name, scales = "free", ncol=2) +
  theme(legend.position="bottom", legend.title= element_blank())+
  theme_bw() + 
  labs(x="Calendar Year", y="CYER")

print(g)
dev.off()
