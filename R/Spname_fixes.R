#This is the script to change the species names of trait records that had wrongly identified leaves.
#After concerns were raised about some helichrysum species being wrongly identified and scanned that way,
#Imke looked at all the scans of helichrysum species and identified records which names needed to be changed.
#These records are in the Heli_name_changes.xlsx file.
#These names need to be changed in all datasets that are coupled to or derived from leaf scans!

#Also, three Helichrysum species were wrongly identified during the course.
#These names need to be changed in all datasets that contain Helichrysum species.
#This is called the new Helichrysum naming system, and applying it should be the last sstep in data cleaning.

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
#Download the following file from google drive:
#PFTC7 - Student folder > trait_data > Data > Heli_name_changes.xlsx
tochange <- read_excel("raw_data//Heli_name_changes.xlsx")

#create a copy of the FT data to change the names in
FT_names_fixed <- FT

for(i in 1:nrow(FT_names_fixed)) {

  #if the id in the trait data matches any any of the id's in tochange:
  if(FT_names_fixed[i, which(colnames(FT_names_fixed) == "id")] %in% c(tochange$ID)) {

    #get the old name
    old_name <- FT_names_fixed[i, which(colnames(FT_names_fixed) == "species")]

    #get the row in tochange which contains the correct name for this id
    tochange_row <- match(FT_names_fixed[i, which(colnames(FT_names_fixed) == "id")], c(tochange$ID))

    #get the correct name in this row
    correct_name <- tochange[tochange_row, which(colnames(tochange) == "correct_name")]

    #overwrite the species column in FT with this correct name
    FT_names_fixed[i, which(colnames(FT_names_fixed) == "species")] <- as.character(correct_name)

    #print a message to track changes made
    print(paste0(i, "_", FT_names_fixed[i, which(colnames(FT_names_fixed) == "id")], " ",
                 "old = ", old_name, " ",
                "new = ", as.character(correct_name)))
    }
}
