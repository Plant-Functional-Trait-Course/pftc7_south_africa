#### Functions to fix species names

#This is the script to change the species names of trait records that had wrongly identified leaves.
#After concerns were raised about some helichrysum species being wrongly identified and scanned that way,
#Imke looked at all the scans of helichrysum species and identified records which names needed to be changed.
#These records are in the Heli_name_changes.xlsx file.
#These names need to be changed in all datasets that are coupled to or derived from leaf scans!

#Also, three Helichrysum species were wrongly identified during the course.
#These names need to be changed in all datasets that contain Helichrysum species.
#This is called the new Helichrysum naming system, and applying it should be the last step in data cleaning.

#In this script, functions are created to change the names of incorrectly identified leaf ID's, and to apply the new heli naming system.

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

