###Standardise the species names####
###The completed name trail is on OSF, do not run this is you want to download it###


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
