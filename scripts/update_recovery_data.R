library(mrpcore)
library(cetl)

############################################################
##
## The following script retrieves the CWT recovery data
## from the test database and imports the data into the 
## local (MS Access) CAMP database
##
############################################################


cwt_report_dir <- "cwt-report"

mrp_config_filename <- r"(mrpConfig.yaml)"
user_settings_filename <- normalizePath("camp_2025_local.yaml")

cmd_params <- c("get", "psc", "CDFO")

Sys.setenv(MRP_YAML_ENVIRONMENT = "production")


pw <- askpass::askpass("CWT Database password")

if (file.exists(cwt_report_dir) == FALSE){
  dir.create(cwt_report_dir)
}
mrpAdminMain(c(cmd_params, "2005:2009"), mrp_config_filename, db_password = pw)
mrpAdminMain(c(cmd_params, "2010:2019"), mrp_config_filename, db_password = pw)
mrpAdminMain(c(cmd_params, "2020:2024"), mrp_config_filename, db_password = pw)


catch_years <- 2005:2024
report_filenames <- file.path(cwt_report_dir,paste0("RC042_CDFO_", catch_years, ".csv"))

cetl::campImportRecFile(camp_conn_filename = user_settings_filename,
                        rec_filenames = report_filenames,
                        test_mode = FALSE,
                        update_sync_tbl = FALSE)
