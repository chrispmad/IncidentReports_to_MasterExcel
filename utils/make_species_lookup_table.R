
make_species_lookup_table = function(excel_sheet_location){
  
  inv_sp_tbl = openxlsx::read.xlsx(excel_sheet_location,
                                             "combined",
                                             startRow = 21) |> 
    tidyr::as_tibble() |> 
    purrr::set_names(c('type','status','common_name','genus','species')) |> 
    # Get rid of extra blank spaces in all columns...
    dplyr::mutate(dplyr::across(dplyr::everything(), stringr::str_squish))
  
  lookup_table = inv_sp_tbl |> 
    dplyr::filter(type %in% c("Amphibian","Fish","Other invertebrates","Reptile"))
  
  return(lookup_table)
}
