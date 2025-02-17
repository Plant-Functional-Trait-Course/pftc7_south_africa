###Cleaning veg cover, fertility and functional trait data###


clean_community <- function(raw_cover, raw_extra, raw_fertility, name_trail, heli_naming_system){

  raw_cover2 <- raw_cover |>
    t() |> #transpose the matrix
    row_to_names(row_number = 1) |> #make row 1 row names
    clean_names() #make all the row and column names lower case and add underscores

  #clean up the dates of raw cover
  date <- rownames(raw_cover2) |> #get the rownames
    as_tibble() |> #make it a tibble
    rename(date = value) |> #rename the date variable to "date"
    mutate(date = str_remove(date, "x"), #remove the x in front of the dates
           date = gsub("_[^_]*$", "", date), # remove the extra number at the end
           date = dmy(date)) #standardise the date format

  #now add date to cover and get it ready to bind to extra
  cover <- bind_cols(date, raw_cover2) |>
    pivot_longer(cols = -c(date:treatment_only_for_range_x), names_to = "species", values_to = "cover") |> #make it long format
    filter(!is.na(cover)) |> #remove rows where the sp was not present in a plot
    mutate(cover = as.numeric(cover), #make cover numeric
           elevation = as.numeric(elevation))   #make elevation numeric

  ##Import and clean the extra data with the species added during FT sampling
  extra <- raw_extra |>
    clean_names() |> #add underscore and decapitalize
    select(-date) |> #remove the dat column
    filter(!is.na(site)) |> #remove all the empty rows
    rename(fertility = fertile) |> #rename ferile
    mutate(site = as.character(site), #make site a character variable
           plot = as.character(plot),  #make plot a character variable
           date = ymd(case_when(site == "2" ~ "2023-12-6", #add dates that this data was collected
                                site == "3" ~ "2023-12-9",
                                site == "4" ~ "2023-12-8")),
           fertility = case_when(fertility == "N" ~ "n", #change fertility to lower case
                                 fertility == "Y" ~ "y"),
           species = tolower(species),
           species = str_replace(species, " ", "_")) |>
    rename(site_id = site, aspect = transect, plot_id = plot) #rename variables

  ## add extra to community
  complete_cover <- bind_rows(cover, extra)


  ##import raw fertility data
  raw_fert <- raw_fertility |>
    t() |>
    row_to_names(row_number = 1) |>
    clean_names()
  #delete the last two empty rows
  raw_fert <- raw_fert[which(!is.na(raw_fert[ , 2])) , ]

  ##clean up the dates of raw fert
  fert_date <- rownames(raw_fert) |> #get the rownames
    as_tibble() |> #make it a tibble
    rename(date = value) |> #rename the date variable to "date"
    mutate(date = str_remove(date, "x"), #remove the x in front of the dates
           date = gsub("_[^_]*$", "", date), # remove the extra number at the end
           date = dmy(date)) #standardise the date format

  #add the corrected date column to raw_fert
  fert <- bind_cols(fert_date, raw_fert) |>
    pivot_longer(cols = -c(date:treatment_only_for_range_x), names_to = "species", values_to = "fertility") |>
    filter(!is.na(fertility)) |> #remove rows where the sp was not present in a plot
    mutate(elevation = as.numeric(elevation)) #make elevation numeric


  ### name trail###
  #standard names are in the species column
  name_trail$synonyms <- paste(name_trail$synonym1, name_trail$synonym2, name_trail$synonym3, sep = "; ") # merge synonym columns to make "synonyms"

  name_trail <- name_trail %>%
    select(species, synonyms) # only keep "species and "synonyms"

  ###Fix names in the complete cover
  cover_change_tracker <- data.frame( # create a dataframe that stores information about samples for which species names are modified by the code below
    old_spec = character(),
    new_spec = character(),
    stringsAsFactors=FALSE)
  cover_data_harmony <-  complete_cover# create a copy of the raw data that will store the changes

  for (i in 1:nrow(complete_cover)) {
    old_sp <- cover_data_harmony[i, which(colnames(cover_data_harmony) == "species")]
    new_sp <- NA

    found <- FALSE
    for (j in 1:nrow(name_trail)) { # looks whether species name is a synonym and replaces it with the true_name if it is found to be a synonym
      found <- grepl(old_sp, name_trail[j, 2])

      if (found){ # only runs if the species is a synonym
        new_sp <- name_trail[j, 1] # finds the true name of the species and saves it
        break
      }
    }

    if (found) { # replaces the species in the trait database with the saved true name if "found" is "TRUE"
      cover_data_harmony[i, which(colnames(cover_data_harmony) == "species")] <- new_sp

      # add a new row with information about change to the change trackers dataset
      cover_change_tracker[i, 1] <- old_sp
      cover_change_tracker[i, 2] <- new_sp

      cover_change_tracker <- na.omit(cover_change_tracker)
    }
  }; rm(old_sp, new_sp, found, i, j)  # remove unnecessary variables

  #head(cover_change_tracker, 10)


  ###fix the names in the fertility data
  fert_change_tracker <- data.frame( # create a dataframe that stores information about samples for which species names are modified by the code below
    old_spec = character(),
    new_spec = character(),
    stringsAsFactors=FALSE)
  fert_data_harmony <-  fert# create a copy of the raw data that will store the changes

  for (i in 1:nrow(fert)) {
    old_sp <- fert_data_harmony[i, which(colnames(fert_data_harmony) == "species")]
    new_sp <- NA

    found <- FALSE
    for (j in 1:nrow(name_trail)) { # looks whether species name is a synonym and replaces it with the true_name if it is found to be a synonym
      found <- grepl(old_sp, name_trail[j, 2])

      if (found){ # only runs if the species is a synonym
        new_sp <- name_trail[j, 1] # finds the true name of the species and saves it
        break
      }
    }

    if (found) { # replaces the species in the trait database with the saved true name if "found" is "TRUE"
      fert_data_harmony[i, which(colnames(fert_data_harmony) == "species")] <- new_sp

      # add a new row with information about change to the change trackers dataset
      fert_change_tracker[i, 1] <- old_sp
      fert_change_tracker[i, 2] <- new_sp

      fert_change_tracker <- na.omit(fert_change_tracker)
    }
  }; rm(old_sp, new_sp, found, i, j)  # remove unnecessary variables

  #head(fert_change_tracker, 10)

  ##now join fert_data_harmony and cover_data_harmony so that the fertility and cover of each sp is in one table.
  community_join <- cover_data_harmony |>
    rename(fertility_extra = fertility) |>  #there are some values in the fertility column of cover_data_harmony. These are from the extra table. We have to rename this column otherwise the join won't work
    left_join(fert_data_harmony)

  ##now we just have to put all the fertility values in one column
  community_join$fertility_all <- NA
  for (i in 1:nrow(community_join)) {
    if(is.na(community_join[i , which(colnames(community_join) == "fertility")])) {
      #if the fertility column is NA, assign it the fertility value in fertility_extra
      community_join[i , which(colnames(community_join) == "fertility_all")] <-  community_join[i , which(colnames(community_join) == "fertility_extra")]
    }else{ #else eassign it the value in fertility
      community_join[i , which(colnames(community_join) == "fertility_all")] <-  community_join[i , which(colnames(community_join) == "fertility")]
    }
  }
  #remove the fertility and fertility extra columns
  community_join <- community_join |>
    select(!(fertility:fertility_extra))

  # fix names
  community <- new_heli_naming_system(data = community_join,
                                                 naming_system = heli_naming_system,
                                                 data_species_column = "species")

  # streamline with other data
  community  |>
    mutate(aspect = if_else(aspect == "W", "west", "east"),
           treatment_only_for_range_x = if_else(treatment_only_for_range_x == "Control", "ambient", "warm"))

}



