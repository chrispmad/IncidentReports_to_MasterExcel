process_android = function(lookup_tbl){

  files = list.files(path = 'Android/',
                     full.names = T)
  
  print(paste0(length(files), " emails of webform type. Processing email: "))
  
  for(i in 1:length(files)){
    
    print(i)
    
    email_contents = msgxtractr::read_msg(files[i])
    
    text_dat = str_replace_all(email_contents$body$text,"(\n|\r|\t)"," ") |> 
      str_extract("ID.*$") |> 
      str_squish()
    
    att = msgxtractr::save_attachments(email_contents)
    
    text_df = tibble(
      Date = str_extract(text_dat, '(?<=Date )[^ ]*'),
      Species = str_extract(text_dat, '(?<=Species ).*(?= Area)'),
      Location = str_extract(text_dat, '(?<=Location ).*(?= Phone)'),
      Phone = str_extract(text_dat, '(?<=Phone ).*(?= Comments)'),
      Name = str_to_title(str_extract(text_dat, '(?<=Name ).*(?= Email)')),
      Email = str_extract(text_dat, '(?<=Email ).*(?= Phone)'),
      Comments = str_extract(text_dat, '(?<=Comments ).*(?= Photo)'),
      Photo = str_extract(text_dat, '(?<=Photo ).*')
    ) |> 
      dplyr::mutate(Location = str_remove(Location, " <.*")) |> 
      dplyr::mutate(lat = as.numeric(str_extract(Location,'[0-9\\.]*(?=,)')),
                    lon = as.numeric(str_extract(Location,'(?<=, )[\\-0-9\\.]*$'))) |> 
      dplyr::mutate(Species = case_when(
        str_detect(Species, '(Gold [F,f]ish|Koi)') ~ 'Goldfish',
        T ~ Species
      ))
    
    # # Extract link to photo, if any.
    # photo_link = str_remove_all(str_extract(text_df$Photo,'<.*'),'(<|>)')
    # 
    # text_df = text_df |> 
    #   dplyr::mutate(Photo = str_remove(Photo, '<.*'))
    # 
    # # Download image from online and save to our photo folder
    # photo_file_destination = paste0('../Aquatic Reports Photos/',str_replace_all(text_df$Species,' ','-'),'_',text_df$Date,'_',str_extract(text_df$Name,'[a-zA-Z]*$'),'.jpeg')
    # 
    # # If the photo hasn't been downloaded yet, download it and save it to LAN folder!
    # if(!file.exists(photo_file_destination) & !is.na(photo_link)){
    #   magick::image_read(photo_link) |> 
    #     magick::image_write(path = photo_file_destination)
    # }
    
    # Find Natural Resource Region based on coordinates.
    nr_reg = tryCatch(
      expr = text_df |> st_as_sf(coords = c("lon","lat"), crs = 4326) |> 
        sf::st_join(bcmaps::nr_regions() |> 
                      sf::st_transform(crs = 4326) |> 
                      dplyr::select(name = ORG_UNIT_NAME)) |> 
        dplyr::pull(name) |> 
        str_remove(' Natural.*'),
      error = function(e) return("Unknown")
    )
    
    # Try to get the common name or species name from our lookup table (whichever of 
    # those two is currently missing!)
    
    if(text_df$Species %in% paste0(lookup_tbl$genus,' ',lookup_tbl$species)){
      # Looks like we gotta bring over the common name.
      common_name_for_excel = lookup_tbl[stringr::str_detect(text_df$Species,lookup_tbl$genus) & stringr::str_detect(text_df$Species,lookup_tbl$species),]$common_name
    } else {
      # Already had the scientific name
      common_name_for_excel = text_df$Species
    }
    
    if(text_df$Species %in% lookup_tbl$common_name){
      # Looks like we gotta bring over the scientific name.
      sci_name_for_excel = paste0(lookup_tbl[lookup_tbl$common_name == text_df$Species,]$genus,' ',lookup_tbl[lookup_tbl$common_name == text_df$Species,]$species)
    } else {
      # Already had the scientific name
      sci_name_for_excel = text_df$Species
    }
    
    # Extract link to photo, if any.
    photo_link = str_remove_all(str_extract(text_df$Photo,'<.*'),'(<|>)')
    
    text_df = text_df |> 
      dplyr::mutate(Photo = str_remove(Photo, '<.*'))
    
    # Download image from online and save to our photo folder
    photo_file_destination = paste0('../Aquatic Reports Photos/',str_replace_all(common_name_for_excel,' ','-'),'_',text_df$Date,'_',str_extract(text_df$Name,'[a-zA-Z]*$'),'.jpeg')
    
    # If the photo hasn't been downloaded yet, download it and save it to LAN folder!
    if(!file.exists(photo_file_destination) & !is.na(photo_link)){
      tryCatch(
        expr = magick::image_read(photo_link) |> 
          magick::image_write(path = photo_file_destination),
        error = function(e) print(paste0("Could not successfully download photo for file ",i))
      )
    }
    
    photo_file_destination_complete = str_replace(photo_file_destination,"\\.\\.","J:/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Aquatic Reports")
    
    common_name_for_report_name = stringr::str_replace_all(stringr::str_to_sentence(common_name_for_excel), ' ', '-')
    
    # Compose row for excel document.
    excel_row = tibble(
      Incidence_Report_Name = paste0(text_df$Date,"_",common_name_for_report_name,"_",str_extract(text_df$Name,'[a-zA-Z]*$')),
      Date = text_df$Date,
      Sender = text_df$Name,
      Email_Address = text_df$Email,
      Phone_Number = text_df$Phone,
      Photo_Available = ifelse(file.exists(photo_file_destination),'y','n'),
      Photo_Location = ifelse(file.exists(photo_file_destination),"LAN folder",NA),
      Submitted_Common_Name = common_name_for_excel,
      Submitted_Scientific_Name = sci_name_for_excel,
      Confirmed_Common_Name = NA,
      Confirmed_Scientific_Name = NA,
      Taxonomic_Group = 'FILL IN',
      Privacy = 'Normal',
      ID_Confirmation = 'Unconfirmed',
      Location = text_df$Location,
      Natural_Resource_Region = nr_reg,
      Latitude = text_df$lat,
      Longitude = text_df$lon,
      Lat_Long_Verification = 'Unknown',
      Outcome_and_or_Action_Old = NA,
      Outcome_and_or_Action = 'In Progress',
      Notes = NA,
      Records_for_Submission = 'Yes'
    )
    
    
    if(file.exists(photo_file_destination)){
      excel_row = excel_row |> 
        dplyr::mutate(
          Photo_Location = paste0(
            "HYPERLINK(\"",
            photo_file_destination_complete,
            "\", \"",
            "LAN folder",
            "\")"
          )
        )
    }
    
    class(excel_row$Photo_Location) <- "formula"
    
    # Make sure the new row is able to be added to the excel sheet as it stands so far.
    more_data_for_excel = tryCatch(
      expr = rbind(data_for_excel,excel_row),
      error = function(e) break("Not able to read this email!")
    )
    
    # If it is addable, do that now.
    if(is.data.frame(more_data_for_excel)){
      
      data_for_excel = rbind(data_for_excel,excel_row)
      
      # new_email_name = paste0(excel_row$Date,'_',str_replace_all(text_df$Species,' ','-'),'_',str_extract(text_df$Name,'[a-zA-Z]*$'))
      
      new_email_name = excel_row$Incidence_Report_Name
      # Also make a copy of the email and place that in the 'Successfully_digested folder.
      # Note: is there already another report with the same name? That may indicate that
      # there is a second (or third...?!) email that is a response to that first one.
      if(file.exists(paste0(file.path('./Successfully_digested',excel_row$Incidence_Report_Name),'.msg'))){
        excel_row$Incidence_Report_Name = paste0(excel_row$Incidence_Report_Name,"-2")
        new_email_name = excel_row$Incidence_Report_Name
      }
      
      file.copy(from = file.path(getwd(),files[i]),
                to = paste0(file.path(getwd(),'Successfully_digested',new_email_name),'.msg')
      )
    }
    
  }
  return(data_for_excel)
}
