#This is the script to change the plant ID's of wronlgly identified leaves on scans.
#as well as change some helichrysum names according to the new helichrysum naming system.
#remotes::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)
library(tidyverse)
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

for(i in 1:nrow(FT)) {

  if(FT$id %in% c(tochange$ID)) {

  }
}
