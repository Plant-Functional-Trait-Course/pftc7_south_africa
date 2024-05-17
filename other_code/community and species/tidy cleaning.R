###Cleaning veg cover, fertility and functional trait data###
###ICS###
library(tidyverse)
library(readxl)
library(janitor)
library(tidylog)
library(TNRS)
library(remotes)
#remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

####Import and clean up the vegetation cover, fertility and functional trait data####
##Import cover data
get_file(node = "hk2cy",
         file = "Data entry 13Dec.xlsx",
         path = "raw_data",
         remote_path = "raw_data/raw_community_data")



raw_cover <- read_excel("raw_data/Data entry 13Dec.xlsx", sheet = "Sp cover") |> #import raw cover data
  t() |> #transpose the matrix
  row_to_names(row_number = 1) |> #make row 1 row names
  clean_names() #make all the row and column names lower case and add underscores


#clean up the dates of raw cover
date <- rownames(raw_cover) |> #get the rownames
  as_tibble() |> #make it a tibble
  rename(date = value) |> #rename the date variable to "date"
  mutate(date = str_remove(date, "x"), #remove the x in front of the dates
         date = gsub("_[^_]*$", "", date), # remove the extra number at the end
          date = dmy(date)) #standardise the date format

#now add date to cover and get it ready to bind to extra
cover <- bind_cols(date, raw_cover) |>
  pivot_longer(cols = -c(date:treatment_only_for_range_x), names_to = "species", values_to = "cover") |> #make it long format
  filter(!is.na(cover)) |> #remove rows where the sp was not present in a plot
  mutate(cover = as.numeric(cover), #make cover numeric
         elevation = as.numeric(elevation))   #make elevation numeric


##Import and clean the extra data with the species added during FT sampling
get_file(node = "hk2cy",
         file = "Species_added_during_FT_sampling.xlsx",
         path = "raw_data",
         remote_path = "raw_data/raw_community_data")

extra <- read_excel("raw_data/Species_added_during_FT_sampling.xlsx", skip = 4) |> #do not read the first 4 rows, they are empty
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

##add extra to community
complete_cover <-   bind_rows(cover, extra)

complete_cover |>
  filter(is.na(treatment_only_for_range_x)) |> #only work with site 1-5, not rangex site
  mutate(plotref = paste(site_id, aspect, plot_id, sep = "_")) |>
  select(plotref, species, cover) |>
  group_by(plotref, species) |>
  summarise(n = n(), .groups = "drop") |>
  filter(n > 1L)


##import raw fertility data
raw_fert <- read_excel("raw_data/Data entry 13Dec.xlsx", sheet = "Sp fertility") |>
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



###Standardise the species names####
###The completed name trail is on OSF, do not run this if you want to download it###


###Creating the name trail
#import the raw trait data and get distinct names
ft_names <- read_excel("raw_data/PFTC7_SA_raw_traits_2023.xlsx", sheet = "Gradient") |>
  distinct(species) |>
  mutate(species = tolower(species),
         species = str_replace(species, " ", "_"))

#get distinct names in the fertility data
fert_names <- fert |>
  distinct(species)

#get distinct names from the veg cover data
cov_names <- complete_cover |>
  distinct(species)

#Check species names against tnrs
names_tocheck <- cov_names |>
  mutate(species = str_replace(species, "_", " "))
tnrs_names <- TNRS(taxonomic_names = names_tocheck$species)
#these names will become our standard to change other names to
#however some tnrs matches aren't great. In some cases we will keep our original names.
#export the tnrs names file. In excel we will delete wrong matches
write_csv(tnrs_names, "raw_data/tnrs_names.csv")

#import edited tnrs data and make names small letters and underscores again
std_names <- read_csv("raw_data/tnrs_names_editing.csv") |>
  distinct(name_standard) |>
  mutate(name_standard = tolower(name_standard),
         name_standard = str_replace(name_standard, " ", "_")) |>
  rename(species = name_standard)

#then compare all datasets to this standard.
#names in fert but not in std names
fert_excl <- fert_names |>
  anti_join(std_names)
#names in ft data but not in std names
ft_excl <- ft_names |>
  anti_join(std_names)
#names in cov but not in std names
cov_excl <- cov_names |>
  anti_join(std_names)

write_csv(std_names, "raw_data/std_names.csv")
write_csv(cov_excl, "raw_data/cover_mismatch_names.csv")
write_csv(fert_excl, "raw_data/fertility_mismatch_names.csv")
write_csv(ft_excl, "raw_data/ft_mismatch_names.csv")
#in excel I combined the above files into a name trail with the standard name and its synonyms.


###run from here###
###Import name trail###
get_file(node = "hk2cy",
         file = "std_names_editing.csv",
         path = "raw_data",
         remote_path = "raw_data/raw_community_data")

name_trail <- read_csv("raw_data/std_names_editing.csv")
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
head(cover_change_tracker, 10)



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
head(fert_change_tracker, 10)

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

#final step is to make sure a species name does not appear more than once in the same plot.
doubles <- community_join |>
              group_by(site_id, aspect, plot_id, species) |>
              summarise(n = n(), .groups = "drop") |>
              filter(n > 1L)
#these doubles need to be removed, let the entry with the highest cover remain
community_join_final <- community_join |>
  distinct(site_id, aspect, plot_id, treatment_only_for_range_x, species, cover, fertility_all, .keep_all = TRUE)

for(d in 1:nrow(doubles)) {
  record <- doubles[d, ]

  community_problems <- community_join |>
    filter(site_id == record$site_id,
           aspect == record$aspect,
           plot_id == record$plot_id,
           species == record$species)

  if(community_problems$cover[1] != community_problems$cover[2]) {
  community_join_final <- community_join_final |>
      filter(!(site_id == record$site_id &
               aspect == record$aspect &
                   plot_id == record$plot_id &
                       species == record$species &
                           cover == min(community_problems$cover)))
  }
} #doubles had 5 species, one with 3 occurrences in a plot and the rest with two.
#Thus, community join final must have 6 records fewer than community join.

#write the file to a .csv
write_csv(community_join_final, "clean_data/community_data_names_cleaned.csv")


###fix the names in the trait data
#download the trait data
get_file(node = "hk2cy",
         file = "PFTC7_SA_raw_traits_2023.xlsx",
         path = "raw_data",
         remote_path = "raw_data/raw_trait_data")# raw pftc trait data with species names to be harmonized
ft_raw <- read_excel("raw_data/PFTC7_SA_raw_traits_2023.xlsx") %>%
  mutate(species = tolower(species),
         species = str_replace(species, " ", "_")) %>% # change species variable formatting
  filter(!is.na(ID)) # filter out rows where ID is NA in case there are any



ft_change_tracker <- data.frame(id = character(), # create a dataframe that stores information about samples for which species names are modified by the code below
                             old_spec = character(),
                             new_spec = character())

ft_data_harmony <- ft_raw # create a copy of the raw data that will store the changes
ft_data_harmony <- ft_data_harmony %>% # filter out rows where species is NA
  filter(!is.na(species))
rows_dropped <- ft_raw %>% # save rows where species is NA and append them later
  filter(is.na(species))

#loop to standardise all names to the name trail
for (i in 1:nrow(ft_raw)) {
  old_sp <- ft_data_harmony[i, which(colnames(ft_data_harmony) == "species")]
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
    ft_data_harmony[i, 6] <- new_sp

    # add a new row with information about change to the change trackers dataset
    ft_change_tracker[i, 1] <- ft_raw[i, 1]
    ft_change_tracker[i, 2] <- old_sp
    ft_change_tracker[i, 3] <- new_sp

    ft_change_tracker <- na.omit(ft_change_tracker)
  }
}; rm(old_sp, new_sp, found, i, j)  # remove unnecessary variables

ft_data_harmony <- rbind(ft_data_harmony, rows_dropped)

head(change_tracker, 10)

#export the corrected ft data
write_csv(ft_data_harmony, "clean_data/ft_data_names_cleaned.csv")


####DONE ICS###




