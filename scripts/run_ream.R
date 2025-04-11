library(ream)
library(furrr)
library(purrr)
library(sitfour)

plan(multisession, workers = 6)

data_dir <- "./data"

stock_codes <-
  read.csv(file.path(data_dir,"ream_stocks.csv"))$stock_code |>
  unique()


run_ream_stock <- function(stock_code, user_settings_filename, rds_dir) {
  command_data <- get_def_command_setting(stock_code, 2024, user_settings_filename)
  prod_result <- PrimarySub(command_data, user_settings_filename)
  saveRDS(prod_result, file=file.path(rds_dir, paste0(stock_code, ".RDS")))
  return(TRUE)
}

local_hrj_dir <- "./ream_output/Local HRJ"

#Remove the previous copies of the HRJ files
list.files(local_hrj_dir,  full.names = TRUE) |>
  file.remove()

future_map(stock_codes, 
           run_ream_stock, 
           user_settings_filename = "camp_local.yaml",
           rds_dir = "./ream_output/local/")

list.files("./ream_output/local/", 
           pattern=".HRJ$", 
           full.names = TRUE, 
           ignore.case = TRUE, 
           recursive = TRUE) |>
  file.copy(local_hrj_dir)


stock_filenames <-
  list.files("./ream_output/local/", 
             pattern=".RDS$", 
             full.names = TRUE, 
             ignore.case = TRUE, 
             recursive = TRUE) |>
  normalizePath()

future_map(stock_filenames,
           run_sit_4,
           method = "CY", 
           output_dir = normalizePath(file.path(local_hrj_dir, "unmarked")))

future_map(stock_filenames,
           run_sit_4,
           method = "BY", 
           output_dir = normalizePath(file.path(local_hrj_dir, "unmarked")))

###############################################################
#
#  Refresh the ERA from the Cloud server
#
#
################################################################

server_hrj_dir <- "./ream_output/Server HRJ"
# 
# #Remove the previous copies of the HRJ files
# list.files(server_hrj_dir,  full.names = TRUE) |>
#   file.remove()
# 
# 
# future_map(stock_codes, 
#            run_ream_stock, 
#            user_settings_filename = "camp_server.yaml",
#            rds_dir = "./ream_output/server/")
# 
# 
# 
# list.files("./ream_output/server/", 
#            pattern=".HRJ$", 
#            full.names = TRUE, 
#            ignore.case = TRUE, 
#            recursive = TRUE) |>
#   file.copy(file.path(server_hrj_dir, "marked"))
#
#
stock_filenames <-
  list.files("./ream_output/server/", 
             pattern=".RDS$", 
             full.names = TRUE, 
             ignore.case = TRUE, 
             recursive = TRUE) |>
  normalizePath()

future_map(stock_filenames, 
           run_sit_4, 
           method = "CY", 
           output_dir = normalizePath(file.path(server_hrj_dir, "unmarked")))

future_map(stock_filenames, 
           run_sit_4, 
           method = "BY", 
           output_dir = normalizePath(file.path(server_hrj_dir, "unmarked")))


run_sit_4(r"(C:\Users\komickn\Documents\src\SA_Projects\chinook_rec_report\ream_output\server\ATN.RDS)",
          method = "CY", 
          output_dir = r"(C:\Users\komickn\Documents\temp)")
