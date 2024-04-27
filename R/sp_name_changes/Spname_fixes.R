#This is the script to change the species names of trait records that had wrongly identified leaves.
#After concerns were raised about some helichrysum species being wrongly identified and scanned that way,
#Imke looked at all the scans of helichrysum species and identified records which names needed to be changed.
#These records are in the Heli_name_changes.xlsx file.
#These names need to be changed in all datasets that are coupled to or derived from leaf scans!

#Also, three Helichrysum species were wrongly identified during the course.
#These names need to be changed in all datasets that contain Helichrysum species.
#This is called the new Helichrysum naming system, and applying it should be the last step in data cleaning.

#In this script, functions are created to change the names of incorrectly identified leaf ID's, and to apply the new heli naming system.
#The functions are then applied to the trait data and community data

#remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)
library(tidyverse)
library(tidylog)
library(readxl)

####Create functions to (1) change names of incorrectly identified leaf scans, and (2) apply the new heli naming system####

###function to change speciesnames of wrongly identified leaves:
correct_leaf_ID <- function(data, # dataframe to which name changes need to be applied
                            changes, #dataframe containg leafID's, their incorrect names as well as their correct names
                            data_ID_column,  #the name of the column in data that contains the leafID's, must be a string
                            data_species_column) #the name of the column in data that contains the species names, must be a string
  #species names must be in the format genus_species.
  #I.e. no capitals, with an underscore separating genus and specific epithet
{
  for(i in 1:nrow(data)) {

    #if the id in the trait data matches any any of the id's in tochange:
    if(data[i, which(colnames(data) == data_ID_column)] %in% c(changes$ID)) {

      #get the old name
      old_name <- data[i, which(colnames(data) == data_species_column)]

      #get the row in tochange which contains the correct name for this id
      tochange_row <- match(data[i, which(colnames(data) == data_ID_column)], c(changes$ID))

      #get the correct name in this row
      correct_name <- changes[tochange_row, which(colnames(changes) == "correct_name")]

      #overwrite the species column in FT with this correct name
      data[i, which(colnames(data) == data_species_column)] <- as.character(correct_name)

      #print a message to track changes made
      print(paste0("row", "=", i, " ", ";", " ",
                   "ID", "=",  data[i, which(colnames(data) == data_ID_column)], " ", ";", " ",
                   "old = ", old_name, " ", ";", " ",
                   "new = ", as.character(correct_name)))
    }#close if loop
  }#close loop through rows of data
  return(data)
}#end function

###function to apply new heli naming system
new_heli_naming_system <- function(data, #dataframe to which the new helichrysum naming system should be applied to
                                   data_species_column, #column in data contain the speciesnames, must be a string
                                   #species names must be in the format genus_species.
                                   #I.e. no capitals, with an underscore separating genus and specific epithet
                                   naming_system) #dataframe containg the old and new helichrysum Id's
{

  for (i in 1:nrow(data)) {
    old_name <- data[i, which(colnames(data) == data_species_column)]
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
      data[i, which(colnames(data) == data_species_column)] <- new_name

      #print message to keep track of which names changed
      print(paste0("row", "=", i, " ", "name changed from ", old_name, " to ", new_name))
    }
  }#end loop through rows
  return(data)
}#end function


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

#apply function to functional trait data
FT_step_1 <- correct_leaf_ID(data = FT, changes = tochange, data_ID_column = "id", data_species_column = "species")



#### Step 2 - change the naming system for Helichrysum ####
#download the new naming system from OSF
get_file(node = "hk2cy",
         file = "New Helichrysum naming system.xlsx",
         path = "raw_data",
         remote_path = "raw_data/raw_community_data")
#import new naming system
heli_naming_system <- read_excel("raw_data/New Helichrysum naming system.xlsx")

<<<<<<< HEAD
=======
#download the new naming system from OSF
get_file(node = "hk2cy",
         file = "New Helichrysum naming system.xlsx",
         path = "raw_data",
         remote_path = "raw_data/raw_community_data")
#import new naming system
naming_system <- read_excel("raw_data/New Helichrysum naming system.xlsx")

>>>>>>> dddc539cfe95f9e149a67dd1533d966490faf24e
#import other data which also needs names changed to the new naming system
get_file(node = "hk2cy",
         file = "PFTC_SA_clean_community_2023.csv",
         path = "clean_data",
         remote_path = "community_data")
#import clean community data
<<<<<<< HEAD
comm <- read.csv("clean_data/PFTC_SA_clean_community_2023.csv")

##apply function to trait data and community data
FT_new_name_system <- new_heli_naming_system(data = FT_step_1, naming_system = heli_naming_system, data_species_column = "species")
comm_new_name_system <- new_heli_naming_system(data = comm, naming_system = heli_naming_system, data_species_column = "species")

write.csv(FT_new_name_system, "clean_data/PFTC7_SA_clean_traits_19Apr2024.csv") # write a new file with the finalized data
write.csv(comm_new_name_system, "clean_data/PFTC7_SA_clean_community_19Apr2024.csv")

# since this should be the final step in the data cleaning process data will be saved in clean_data
=======
comm_step_2 <- read.csv("clean_data/PFTC_SA_clean_community_2023.csv")

FT_step_2 <- FT_step_1 #create another copy for step 2 (changing Helichrysum names according to the new naming system)

#create a list of dataframes to apply the new naming system to
datalist <- c("FT_step_2", "comm_step_2")
#a vector of names to assign to the data once the new naming system has been applied
finished_list <- c("FT_new_name_system", "comm_new_name_system")

for(d in 1:length(datalist)) {

  data <- get(datalist[d])
  #create a column to store the name changes in
  data$name_changes <- NA

  for (i in 1:nrow(data)) {
    old_name <- data[i, which(colnames(data) == "species")]
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
      data[i, which(colnames(data) == "species")] <- new_name
      data[i, which(colnames(data) == "name_changes")] <- paste0("name changed from ", old_name, " to ", new_name)

      #assign a new name to data
      assign(finished_list[d], data)

    }
  }#end loop through rows
}#end loop through dataframes

rm(old_name, new_name, found, i, j) # remove unnecessary variables

write.csv(FT_new_name_system, "clean_data/PFTC7_SA_clean_traits_2023.csv") # write a new file with the finalized data
write.csv(comm_new_name_system, "clean_data/PFTS_SA_clean_community.csv")

# since this should be the final step in the data cleaning process data will be save in clean_data
>>>>>>> dddc539cfe95f9e149a67dd1533d966490faf24e
