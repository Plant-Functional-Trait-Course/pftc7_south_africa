
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(remotes)
library(dataDownloader)

# get current trait data

  # drive_ls("PFTC7 - Student folder") %>%
  #   dplyr::filter(name == "trait_data" ) %>%
  #   dplyr::pull(id) %>%
  #   as_id() %>%
  #   drive_ls()%>%
  #   dplyr::filter(name=="PFTC7_SA_raw_traits_2023")%>%
  #   dplyr::pull(id) %>%
  #   googlesheets4::read_sheet(sheet = "Gradient") -> grad_traits


# Example

  # get_species_scans(species_text = "Helichry",
  #                   google_traits_sheet = NULL,
  #                   scan_directory = "D:/PFTC7_leaf_scans",
  #                   temp_directory = "raw_data/temp/")

#' @param species Text to match.
#' @param google_traits_sheet current version of google traits sheet. Defaults to the gradient sheet if left blank
#' @param scan_directory Directory containing scans
get_species_scans <- function(species_text,
                              google_traits_sheet = NULL,
                              scan_directory = "D:/PFTC7_leaf_scans",
                              temp_directory = "raw_data/temp/"){

  # Create output dir if needed

    if(!dir.exists(temp_directory)){dir.create(temp_directory,recursive = TRUE)}

  # if google_traits_sheet is null, try to get it

      if(is.null(google_traits_sheet)){

        drive_ls("PFTC7 - Student folder") %>%
          dplyr::filter(name == "trait_data" ) %>%
          dplyr::pull(id) %>%
          as_id() %>%
          drive_ls()%>%
          dplyr::filter(name=="PFTC7_SA_raw_traits_2023")%>%
          dplyr::pull(id) %>%
          googlesheets4::read_sheet(sheet = "Gradient") -> google_traits_sheet
      }

  # get list of scans in the dir

    scans <- data.frame(file_location  = list.files(path = scan_directory,
                                                    full.names = TRUE,
                                                    recursive = TRUE,
                                                    pattern = ".jpeg"),
                        file_name = list.files(path = scan_directory,
                                                full.names = FALSE,
                                                recursive = TRUE,
                                                pattern = ".jpeg") %>%
                                                basename()

                        ) %>%
      mutate(leaf_id = gsub(pattern = ".jpeg",
                            replacement = "",
                            x = file_name))

  # Get IDS that match "species_text"

    google_traits_sheet %>%
      rowwise() %>%
      filter(grepl(pattern = species_text, x = species)) %>%
      pull(ID) -> focal_ids

  # filter to relevant leaves

    scans %>%
      filter(leaf_id %in% focal_ids) -> focal_scans

  # copy scans to new folder

    copied  <- file.copy(from = focal_scans$file_location,
                to = temp_directory,
                overwrite = TRUE)



}


##get heli nudifolium and pallidum
drive_ls("PFTC7 - Student folder") %>%
     dplyr::filter(name == "trait_data" ) %>%
     dplyr::pull(id) %>%
     as_id() %>%
     drive_ls()%>%
     dplyr::filter(name=="PFTC7_SA_raw_traits_2023")%>%
     dplyr::pull(id) %>%
     googlesheets4::read_sheet(sheet = "Gradient") -> grad_traits


get_file(node = "hk2cy",
         file = "PFTC7_SA_cleanish_traits_2023.csv",
         path = "clean_data",
         remote_path = "trait_data")

grad_traits <- read_csv("clean_data/PFTC7_SA_cleanish_traits_2023.csv") |>
  rename(ID = id)

#get all nudifolium scans
get_species_scans(species_text = "palli",
                  google_traits_sheet = grad_traits,
                  scan_directory = "D:/PFTC7_leaf_scans",
                  temp_directory = "raw_data/temp/")
