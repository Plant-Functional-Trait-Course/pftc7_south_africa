#This is the script to change the species names of trait records that had wrongly identified leaves.
#After concerns were raised about some helichrysum species being wrongly identified and scanned that way,
#Imke looked at all the scans of helichrysum species and identified records which names needed to be changed.
#These records are in the Heli_name_changes.xlsx file.
#These names need to be changed in all datasets that are coupled to or derived from leaf scans!

#Also, three Helichrysum species were wrongly identified during the course.
#These names need to be changed in all datasets that contain Helichrysum species.
#This is called the new Helichrysum naming system, and applying it should be the last step in data cleaning.

#remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)
library(tidyverse)
library(tidylog)
library(readxl)

###Step 1 - changing spnames of incorrectly identified leaves in the trait data####
#Download trait data
#Here I work with PFTC7_SA_cleanish_traits_2023. IS this correct?
get_file(node = "hk2cy",
         file = "PFTC7_SA_cleanish_traits_2023.csv",
         path = "raw_data",
         remote_path = "trait_data")
#import trait data
FT <- read.csv("raw_data//PFTC7_SA_cleanish_traits_2023.csv")

###import table containing the leafID's of the records which need name changes
#Download the following file from OSF:
get_file(node = "hk2cy",
         file = "Heli_name_changes.xlsx",
         path = "raw_data",
         remote_path = "raw_data/raw_trait_data")
tochange <- read_excel("raw_data//Heli_name_changes.xlsx")

#create a copy of the FT data to change the names in
FT_step_1 <- FT

for(i in 1:nrow(FT_step_1)) {

  #if the id in the trait data matches any any of the id's in tochange:
  if(FT_step_1[i, which(colnames(FT_step_1) == "id")] %in% c(tochange$ID)) {

    #get the old name
    old_name <- FT_step_1[i, which(colnames(FT_step_1) == "species")]

    #get the row in tochange which contains the correct name for this id
    tochange_row <- match(FT_step_1[i, which(colnames(FT_step_1) == "id")], c(tochange$ID))

    #get the correct name in this row
    correct_name <- tochange[tochange_row, which(colnames(tochange) == "correct_name")]

    #overwrite the species column in FT with this correct name
    FT_step_1[i, which(colnames(FT_step_1) == "species")] <- as.character(correct_name)

    #print a message to track changes made
    print(paste0(i, "_", FT_step_1[i, which(colnames(FT_step_1) == "id")], " ",
                 "old = ", old_name, " ",
                "new = ", as.character(correct_name)))
    }
}

rm(correct_name, tochange, old_name, tochange_row, i)

#### Step 2 - change the naming system for Helichrysum ####

#download the new naming system from OSF
get_file(node = "hk2cy",
         file = "New Helichrysum naming system.xlsx",
         path = "raw_data",
         remote_path = "raw_data/raw_community_data")
#import new naming system
naming_system <- read_excel("raw_data/New Helichrysum naming system.xlsx")

#import other data which also needs names changed to the new naming system
get_file(node = "hk2cy",
         file = "PFTC_SA_clean_community_2023.csv",
         path = "clean_data",
         remote_path = "community_data")
#import clean community data
comm_step2 <- read.csv("clean_data/PFTC_SA_clean_community_2023.csv")

FT_step_2 <- FT_step_1 #create another copy for step 2 (changing Helichrysum names according to the new naming system)

#create a list of dataframes to apply the new naming system to
datalist <- list(FT_step2, comm_step2)

for(d in 1:length(datalist)) {

  data <- datalist[[d]]

for (i in 1:nrow(data)) {
  old_name <- data[i, 6]
  new_name <- NA

  found <- FALSE
  for (j in 1:nrow(naming_system)) { # looks whether species name should be corrected and replaces it with the new_heli_name_system in case
    found <- grepl(old_name, naming_system[j, 1])

    if (is.na(found)){ # only runs if the species is missing
      found <- FALSE
    }

    if (found){ # only runs if the species is a synonym
      new_name <- naming_system[j, 2] # finds the true name of the species and saves it
      break
    }
  }

  if (found) { # replaces the species in the trait database with the saved true name if "found" is "TRUE"
    FT_step_2[i, 6] <- new_name
    FT_step_2[i, 17] <- paste0("name changed from ", old_name, " to ", new_name)
  }
}
}#endloop through dataframes

rm(old_name, new_name, found, i, j) # remove unnecessary variables

write.csv(FT_step_2, "clean_data/PFTC7_SA_clean_traits_2023.csv") # write a new file with the finalized data

# since this should be the final step in the data cleaning process data will be save in clean_data
