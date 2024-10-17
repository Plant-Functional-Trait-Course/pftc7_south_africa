
library(googledrive)
library(googlesheets4)
library(tidyverse)
library(remotes)
library(osfr)
library(dataDownloader)

#get most recent trait data
FT <- read.csv("clean_data\\PFTC7_SA_clean_traits_19Apr2024.csv", row.names = 1)

#species names that we need to check scans for
spnames <- read.csv("clean_data\\PFTC7_SA_clean_community_19Apr2024.csv", col.names = c("name", "problems")) |>
  filter(problems == "flag") |>
  select(name)

spnames <- spnames$name[10:17]

#get the filenames in each subfolder of the raw_scans folder
#put the filenames in each subfolder in a list
#subfolders in the raw scans folder
subs <- c("6", "7", "8", "9", "10", "11", "13", "14", "15")
remote_file_list <- vector(mode = "list", length = length(subs))
names(remote_file_list) <- subs

for(s in 1:length(subs)) {
  #create the remote path
  remote_path = paste0("raw_data\\raw_scans\\", subs[s])

  #list the files in remote_path
  all_scans_in_folder <- osf_ls_files(x = osf_retrieve_node("hk2cy"), path = remote_path, type = "file", n_max = Inf)

  remote_file_list[[s]] <- all_scans_in_folder
} #this takes a while to run


for(i in 1:length(spnames)) {
#get id's corresponding to this species name
idlist <- FT |>
  filter(species %in% spnames[i] ) |>
  select(id) |>
  distinct()

#create the filenames we are looking for
scan_names <- paste0(idlist$id, ".jpeg")

#subfolders in the raw scans folder
subs <- c("6", "7", "8", "9", "10", "11", "13", "14", "15")

for(n in 1:length(scan_names)) {
  one_scan <- scan_names[n]

  for(s in 1:length(subs)) {

    remote_files_sub <- remote_file_list[[s]]

    if(one_scan %in% remote_file_list[[s]]$name) { #check if the file is in this subfolder

      scan_to_get <- remote_files_sub |> #if it is there, download the scan
        filter(name == one_scan)

      if(dir.exists(paste0("scans\\", spnames[i], "\\")) == FALSE) {
      dir.create(paste0("scans\\", spnames[i], "\\"))}

      osf_download(x = scan_to_get, path = paste0("scans\\", spnames[i], "\\"), conflicts = "skip")

      # Break out of the inner loop if the scan is downloaded
      break
      }
    }}}

##Old code do not run ####


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
