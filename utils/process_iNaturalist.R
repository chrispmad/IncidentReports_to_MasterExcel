process_iNaturalist = function(){
  
  files = list.files(path = 'iNaturalist/',
                     full.names = T)
  
  for(i in 1:length(files)){
    
    email_contents = msgxtractr::read_msg(files[i])
    
    # Extract the specific names and links of reports
    iNat_report_links = email_contents$body$text |> 
      stringr::str_replace_all("(\\\r|\\\n|\\\n)", " ") |> 
      stringr::str_remove_all('View your dashboard.*') |> 
      stringr::str_remove_all(".* British Columbia\\, CA") |>
      stringr::str_remove_all(".* Canada") |> 
      stringr::str_remove(" \\<[^ ]*\\>") |> 
      stringr::str_remove(" \\<[^ ]*\\>") |> 
      str_squish() |> 
      str_extract_all(pattern = '[a-zA-Z ]* <[^ ]*>( <[^ ]*> )?') |> 
      unlist()
      
    iNat_reports = tibble(
      id = 1:length(iNat_report_links)
    ) |> 
      dplyr::mutate(species_name = NA,
                    species_link = NA)
    
    for(y in 1:length(iNat_report_links)){
      y = 1
      species_name = str_remove(iNat_report_links[y],' <.*> ')
      species_link = str_remove_all(str_squish(str_remove(iNat_report_links[y], '.*(?=\\<)')),'(<|>)')
      iNat_reports[y,]$species_name = species_name
      iNat_reports[y,]$species_link = species_link
      
      page_session = rvest::session(url = iNat_reports[y,]$species_link)
      
      page_contents = rvest::read_html(page_session$url)
    
      photo_link = page_contents |> 
        html_node('head') |> 
        html_nodes('meta') |> 
        html_attr('content') |> 
        str_extract('.*\\.jpeg') |> 
        unique()
      
      photo_link = photo_link[!is.na(photo_link)]
      
      key_info = page_contents |> 
        html_text() |> 
        str_replace_all("(\\\n|\\\t|\\\r)"," ") |> 
        str_remove("\\/\\/<\\!\\[CDATA\\[ .*") |> 
        str_squish()
      
      text_df = tibble(
        Species = str_extract(key_info, '.*(?= from)'),
        Location = str_extract(key_info, '(?<= from )[a-zA-Z ]*(?=, BC)'),
        Date = str_extract(key_info, '(?<= on ).* [0-9]{4}(?= at)'),
        Name = str_extract(key_info, '(?<= by )[a-zA-Z ]*(?=( ·|\\.))'),
        Photo = ifelse(!is.null(photo_link),'LAN folder',NA)
      ) |> 
        dplyr::mutate(Date = lubridate::mdy(Date)) |> 
        dplyr::mutate(Incidence_Report_Name = paste0(Date,"_",Species,"_",str_extract(Name,'[a-zA-Z]*$')))
      
      photo_file_destination = paste0('../Aquatic Reports Photos/',str_replace_all(text_df$Species,' ','-'),'_',text_df$Date,'_',str_extract(text_df$Name,'[a-zA-Z]*$'),'.jpeg')
      
      file.copy(from = photo_link,
                to = photo_file_destination)
    }
    
    iNaturalist_report = stringr::str_extract_all(iNaturalist_reports,"<.*>") |> 
      stringr::str_remove_all("(<|>)")
    
    report_content = rvest::read_html(x = iNaturalist_report)
    
    report_content |> 
      html_elements(".image_gallery_image")
    
    report_content |> 
      rvest::html_text() |> 
      str_detect("https://inaturalist-open-data.s3.amazonaws.com/photos/352306971/large.jpeg")
    
    text_dat = str_replace_all(email_contents$body$text,"(\n|\r|\t)"," ") |> 
      str_extract("ID.*$") |> 
      str_squish()
    
    text_df = tibble(
      Date = str_extract(text_dat, '(?<=Date )[^ ]*'),
      Species = str_extract(text_dat, '(?<=Species ).*(?= Area)'),
      Location = str_extract(text_dat, '(?<=Location ).*(?= Phone)'),
      Phone = str_extract(text_dat, '(?<=Phone ).*(?= Comments)'),
      Name = str_extract(text_dat, '(?<=Name ).*(?= Email)'),
      Email = str_extract(text_dat, '(?<=Email ).*(?= Phone)'),
      Comments = str_extract(text_dat, '(?<=Comments ).*(?= Photo)'),
      Photo = str_extract(text_dat, '(?<=Photo ).*')
    ) |> 
      dplyr::mutate(Location = str_remove(Location, " <.*")) |> 
      dplyr::mutate(lat = as.numeric(str_extract(Location,'[0-9\\.]*(?=,)')),
                    lon = as.numeric(str_extract(Location,'(?<=, )[\\-0-9\\.]*$')))
    
    # Extract link to photo, if any.
    photo_link = str_remove_all(str_extract(text_df$Photo,'<.*'),'(<|>)')
    
    text_df = text_df |> 
      dplyr::mutate(Photo = str_remove(Photo, '<.*'))
    
    # Download image from online and save to our photo folder
    photo_file_destination = paste0('../Aquatic Reports Photos/',str_replace_all(text_df$Species,' ','-'),'_',text_df$Date,'_',str_extract(text_df$Name,'[a-zA-Z]*$'),'.jpeg')
    
    # If the photo hasn't been downloaded yet, download it and save it to LAN folder!
    if(!file.exists(photo_file_destination) & !is.na(photo_link)){
      tryCatch(
        expr = magick::image_read(photo_link) |> 
          magick::image_write(path = photo_file_destination),
        error = function(e) print(paste0("Could not successfully download photo for file ",i))
      )
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
    
    # Compose row for excel document.
    excel_row = tibble(
      Incidence_Report_Name = paste0(text_df$Date,"_",text_df$Species,"_",str_extract(text_df$Name,'[a-zA-Z]*$')),
      Date = text_df$Date,
      Sender = text_df$Name,
      Email_Address = text_df$Email,
      Phone_Number = text_df$Phone,
      Photo_Available = file.exists(photo_file_destination),
      Photo_Location = ifelse(file.exists(photo_file_destination),"LAN folder",NA),
      Submitted_Common_Name = text_df$Species,
      Submitted_Scientific_Name = text_df$Species,
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
    
    photo_file_destination_complete = str_replace(photo_file_destination,"\\.\\.","J:/2 SCIENCE - Invasives/SPECIES/5_Incidental Observations/Aquatic Reports")
    
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
    
    data_for_excel = rbind(data_for_excel,excel_row)
    
  }
  return(data_for_excel)
}
