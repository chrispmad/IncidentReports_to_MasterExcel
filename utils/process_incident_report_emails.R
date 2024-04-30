# Title: Process Incident Report Emails by Type
#
# Date: 2024-04-25
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
# 
# Description: This script attempts to pull out key pieces of info from aquatic
# invasive species email reports

# =======================
#  Libraries      
# =======================
library(tidyverse)
# remotes::install_github("hrbrmstr/msgxtractr")
library(msgxtractr)
library(sf)
library(openxlsx)

# Folder location for reports
proj_folder = here::here()

email_path = "J:/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Aquatic Reports/Interim"

setwd(proj_folder)

lapply(
  list.files(path = 'utils', pattern = 'process_[a-zA-Z]*\\.R', full.names = T),
  \(x) source(x)
)

source('utils/make_species_lookup_table.R')
source('utils/move_reports_to_interim_folder.R')

setwd(email_path)

# =======================
#  Set-up and Options      
# =======================

# Make look-up table for species names.
lookup_tbl_excel_loc = "J:/2 SCIENCE - Invasives/SPECIES/AIS_priority_species.xlsx"

lookup_tbl = make_species_lookup_table(lookup_tbl_excel_loc)

# Create excel file to pop these new records into #
my_wb = openxlsx::createWorkbook()

openxlsx::addWorksheet(wb = my_wb, sheetName = 'data')

data_for_excel = structure(list(Incidence_Report_Name = character(0), Date = character(0), 
                                Sender = character(0), Email_Address = character(0), Phone_Number = character(0), 
                                Photo_Available = logical(0), Photo_Location = structure(character(0), class = "formula"), 
                                Submitted_Common_Name = character(0), Submitted_Scientific_Name = character(0), 
                                Confirmed_Common_Name = logical(0), Confirmed_Scientific_Name = logical(0), 
                                Taxonomic_Group = character(0), Privacy = character(0), ID_Confirmation = character(0), 
                                Location = character(0), Natural_Resource_Region = character(0), 
                                Latitude = numeric(0), Longitude = numeric(0), Lat_Long_Verification = character(0), 
                                Outcome_and_or_Action_Old = logical(0), Outcome_and_or_Action = character(0), 
                                Notes = logical(0), Records_for_Submission = character(0)), row.names = integer(0), class = c("tbl_df", 
                                                                                                                              "tbl", "data.frame"))
# Sort emails in the 'Unsorted' folder.
move_reports_to_interim_folder()

# REPORT TYPES #

# 1. Android
android_dat = suppressWarnings(process_android(lookup_tbl))

# 2. IOS
ios_dat = suppressWarnings(process_ios(lookup_tbl))

# 3. App
# Note: currently no email reports in the 'App' folder - this function has NOT been developed yet.
# app_dat = process_app(lookup_tbl)

# 4. COS
# Note: currently no email reports in the 'App' folder - this function has NOT been developed yet.
# cos_dat = process_cos(lookup_tbl)

# 5. iNaturalist
# inat_dat = process_iNaturalist()

# 6. Webform
webform_dat = suppressWarnings(process_webform(lookup_tbl))

data_for_excel = dplyr::bind_rows(
  android_dat,
  ios_dat,
  # app_dat,
  # cos_dat,
  # inat_dat,
  webform_dat |> 
    dplyr::mutate(Latitude = as.numeric(Latitude),
                  Longitude = as.numeric(Longitude))
)

# Add data compiled so far to the worksheet.
writeData(my_wb, "data", x = data_for_excel)

# Save the workbook
saveWorkbook(my_wb, "Automatically_read_reports.xlsx", overwrite = T)
