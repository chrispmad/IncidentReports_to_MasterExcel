# Title: Move Reports to Interim Folder
#
# Date: 2024-04-25
#
# Author(s): Chris Madsen (chris.madsen@gov.bc.ca)
# 
# Description: This script looks for citizen-submitted aquatic invasive species
# incident report emails in the folder "Interim" on the shared LAN drive.
# It attempts to identify the type of incident report and move emails into
# their respective folders.

# =======================
#  Libraries      
# =======================
library(tidyverse)

# =======================
#  Set-up and Options      
# =======================

# Folder location for reports
email_path = "J:/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Aquatic Reports/Interim"

setwd(email_path)

# =======================
#  Sort Emails      
# =======================

move_reports_to_interim_folder = function(){
  # Find all files in the 'Interim' folder. Check subfolders, if they exist.
  ems = list.files(path = 'Unsorted/',
                   recursive = TRUE)
  
  cat(paste0("\nNumber of emails found in folder 'Unsorted': ",length(ems)))
  
  ems_d = tibble(
    em_name = ems,
    em_type = NA
  )
  
  # Parse out report type from email name, if possible.
  ems_d = ems_d |> 
    dplyr::mutate(
      em_type = case_when(
        str_detect(em_name, 'RI-IOS') ~ 'IOS',
        str_detect(em_name, 'RI-And') ~ 'Android',
        str_detect(em_name, 'RAW Smartphone App Submission') ~ 'App',
        str_detect(em_name, 'Notification of ERS') ~ 'COS',
        str_detect(em_name, 'Form Submission_ Invasive Species Report') ~ 'Webform',
        str_detect(em_name, 'iNaturalist Canada_') ~ 'iNaturalist',
        T ~ "CustomEmail"
      )
    ) |> 
    dplyr::mutate(current_loc = file.path('Unsorted',em_name),
                  dest_loc = file.path(em_type,em_name))
  
  # Check if there are any emails already living in those folders. We probably want to
  # clean them up first! The code below will not run if there are any emails
  # in any of the folders inside 'Interim' (besides the folder 'Unsorted')
  
  em_test = list.files(getwd(),recursive = TRUE)
  
  if(length(em_test[str_detect(em_test,'^[^Unsorted]')]) == 0){
    # Move emails to their respective folders.
    1:nrow(ems_d) |> 
      iwalk( ~ {
        print(.x)
        file.copy(from = ems_d[.x,]$current_loc,
                  to = ems_d[.x,]$dest_loc)
        if(file.exists(ems_d[.x,]$dest_loc)){
          file.remove(ems_d[.x,]$current_loc)
        }
      })
  }
}
