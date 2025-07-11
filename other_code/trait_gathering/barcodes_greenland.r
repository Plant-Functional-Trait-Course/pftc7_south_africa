# Script to make barcodes

# load libraries
remotes::install_github("Plant-Functional-Trait-Course/PFTCFunctions")
library(PFTCFunctions)
library(baRcodeR)

# create unique IDs
all_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 2025)
# save IDs as csv file (if needed)
write_csv(all_codes, file = "Unique_ID_Greenland.csv")

# Function to make and print labels on PDF
# The magic combination for these lables: Avery 4778
#https://www.lyreco.com/webshop/NONO/etiketter-avery-45-7-x-21-2-mm-hvit-eske-c3a0-960-stk-product-000000000002760191.html

# 45.7mm  and 21.2mm
# 4.57 cm and 2.12 cm
# cm / 2.54 = inch
# 1.799213 and 0.8346457 inch
baRcodeR::custom_create_PDF(Labels = all_codes$hashcode,
                  name = "Greenland_uniqueID",
                  type = "linear",
                  Fsz = 12, Across = TRUE,
                  trunc = TRUE, numrow = 12, numcol = 4,
                  page_width = 8.3, page_height = 11.7,
                  width_margin = 0.3937008, height_margin = 1,
                  label_width = 1.7, label_height = 0.5)

# explanation of the arguments in the function
custom_create_PDF(
  Labels = all_codes$hashcode,          # Your unique IDs
  name = "Greenland_uniqueID",          # Output PDF file name
  type = "linear",                      # Use Code128 linear barcodes
  Fsz = 12,                             # Font size for text
  Across = TRUE,                        # Fill labels left-to-right
  trunc = TRUE,                         # Truncate long labels if needed
  numrow = 12, numcol = 4,              # 12 rows × 4 columns = 48 per page
  page_width = 8.3, page_height = 11.7, # A4 size in inches
  width_margin = 0.3937008,             # Side margin (10 mm)
  height_margin = 1,                    # Top margin (1 inch)
  label_width = 1.7, label_height = 0.5 # Label size in inches (roughly 43 × 12.7 mm)
)