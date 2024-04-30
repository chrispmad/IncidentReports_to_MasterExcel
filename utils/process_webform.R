process_webform = function(lookup_tbl){
  
  files = list.files(path = 'Webform/',
                     full.names = T)
  
  print(paste0(length(files), " emails of webform type. Processing email: "))
  
  for(i in 1:length(files)){
    
    print(i)
  
    email_contents = msgxtractr::read_msg(files[i])
    
    if(length(email_contents$attachments) > 0){
      # Pull out names of photo attachment files
      photo_attachment_names = sapply(1:length(email_contents$attachments), \(x) email_contents$attachments[[x]]$"long_filename")
      
      # Save photo attachments
      email_attachments = msgxtractr::save_attachments(email_contents, path = file.path(proj_folder,'tempfiles'),
                                                       use_short = F)
      
      # Keep only the largest photo attachment.
      photo_file_sizes = file.size(email_attachments)
      
      # Find which of the attachments is the biggest (if there is more than one)
      biggest_attachment = tibble(attachment_name = photo_attachment_names) |> 
        cbind(photo_file_sizes) |> 
        cbind(file.path(proj_folder,'tempfiles',photo_attachment_names)) |> 
        dplyr::arrange(dplyr::desc(photo_file_sizes)) |> 
        slice(1)
      
      # Delete the other attachments...
      photo_attachment_names[!photo_attachment_names %in% biggest_attachment$attachment_name] |> 
        purrr::iwalk( ~ {
          file.remove(file.path(proj_folder,'tempfiles',.x))
        })
    }
    
    
    main_email_contents = email_contents$body$text[stringr::str_length(email_contents$body$text) > 10]
    
    text_dat = str_replace_all(main_email_contents,"(\n|\r|\t)"," ") |> 
      str_extract("Sent:.*$") |> 
      str_squish()
    
    text_df = tibble(
      Date = str_extract(text_dat, '(?<=Date observed: ).*(?= Suspected)'),
      Species = str_extract(text_dat, '(?<=Suspected Species: ).*(?= City)'),
      Location = str_extract(text_dat, '(?<=or UTMs\\): ).*'),
      Phone = str_extract(text_dat, '(?<=Telephone: ).*(?= Date observed)'),
      Name = str_to_title(str_extract(text_dat, '(?<=Name: ).*(?= Email)')),
      Email = str_extract(text_dat, '(?<=Email: ).*(?= <mailto)'),
      Comments = str_extract(text_dat, '(?<=comments: ).*'),
      Photo = paste0(str_extract_all(text_dat, 'https://forms.gov.bc.ca/industry/index.php?.*(?=> (\\*|City))'), collapse = ", ")
    ) |> 
      dplyr::mutate(Date = stringr::str_replace_all(Date, "\\/", "-"),
                    Species = stringr::str_replace_all(Species, "\\/", "")) |> 
      dplyr::mutate(Date = lubridate::mdy(Date)) |> 
      dplyr::mutate(Date = as.character(Date)) |> 
      dplyr::mutate(Species = case_when(
        str_detect(Species, '(Gold [F,f]ish|Koi)') ~ 'Goldfish',
        T ~ Species
      ))
    
    # If species name column also picked up 'Upload Photographs', remove that bit.
    if(stringr::str_detect(text_df$Species, 'Upload Photographs')){
      text_df$Species = stringr::str_remove(text_df$Species, ' Upload Photographs.*')
    }
    
    # If the Location field has erroneously picked up excess info starting with 'Area of infestation', delete that
    if(stringr::str_detect(text_df$Location,' Area of infestation')){
      text_df$Location = stringr::str_remove(text_df$Location,' Area of infestation.*')
    }
    
    # If the location is just an address rather than coordinates, use the BC Geocoder.
    if(!stringr::str_detect(text_df$Location, '[0-9]{2}\\.[0-9]+')){

      # Pull out place name.
      my.name = text_df$Location
      #Clean up names. Remove anything in brackets.
      my.name = stringr::str_remove_all(my.name, " \\(.*\\)")
      #Add spaces to names.
      my.name = stringr::str_replace_all(my.name, " ", "%20")
      
      url = paste0('https://geocoder.api.gov.bc.ca/addresses.json?addressString=',
                   my.name,'&maxResults=1&outputSRS=4326')
      
      my.coords =  tryCatch(
        expr = jsonlite::fromJSON(url)$features$geometry |>
        dplyr::summarise(lon = as.numeric(stringr::str_extract(coordinates, "(?<=c\\().*(?=\\,)")),
                         lat = as.numeric(stringr::str_extract(coordinates, "(?<=\\,).*(?=\\))"))),
        error = function(e) return(data.frame(lon = NA, lat = NA))
      )
      
      text_df$lon = as.numeric(my.coords$lon)
      text_df$lat = as.numeric(my.coords$lat)
    } else {
      text_df$lat = stringr::str_extract(text_df$Location, '(?<!-)[0-9]\\.[0-9]+')
      text_df$lon = stringr::str_extract(text_df$Location, '(?<=-)[0-9]\\.[0-9]+')
    }      
    
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

    # Photo location and name; we compose this even if there is a photo, so that we can
    # check that location for a photo.
    photo_file_destination = paste0('../Aquatic Reports Photos/',str_replace_all(common_name_for_excel,' ','-'),'_',text_df$Date,'_',str_extract(text_df$Name,'[a-zA-Z]*$'),'.jpeg')
    
    # If we have a photo, save it to LAN folder
    if(length(email_contents$attachments) > 0){
      
      file.copy(
        from = file.path(proj_folder,'tempfiles',biggest_attachment$attachment_name),
        to = photo_file_destination
      )
      
      # Delete local version of the photo
      file.remove(file.path(proj_folder,'tempfiles',biggest_attachment$attachment_name))
    }
    
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
  
    if(excel_row$Photo_Available == 'y'){
      photo_file_destination_complete = str_replace(photo_file_destination,"\\.\\.","J:/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Aquatic Reports")
    }
    
    if(excel_row$Photo_Available == 'y'){
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
      
      new_email_name = paste0(excel_row$Date,'_',str_replace_all(text_df$Species,' ','-'),'_',str_extract(text_df$Name,'[a-zA-Z]*$'))
      
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
