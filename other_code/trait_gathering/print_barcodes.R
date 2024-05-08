# make IDs

library(PFTCFunctions)
library(baRcodeR)
library(tidyverse)

all_codes <- PFTCFunctions::get_PFTC_envelope_codes(seed = 202312)

# Function to make and print labels on PDF
# The magic combination for these lables: Avery 4778
#https://www.lyreco.com/webshop/NONO/etiketter-avery-45-7-x-21-2-mm-hvit-eske-c3a0-960-stk-product-000000000002760191.html

# 45.7mm  and 21.2mm
# 4.57 cm and 2.12 cm
# cm / 2.54 = inch
# 1.799213 and 0.8346457 inch
custom_create_PDF(Labels = all_codes$hashcode,
                  name = "PFTC7_SA_uniqueID",
                  type = "linear",
                  Fsz = 12, Across = TRUE,
                  trunc = TRUE, numrow = 12, numcol = 4,
                  page_width = 8.3, page_height = 11.7,
                  width_margin = 0.3937008, height_margin = 1,
                  label_width = 1.7, label_height = 0.5)

