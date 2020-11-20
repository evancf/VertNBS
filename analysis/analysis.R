devtools::load_all() # This will load functions in the R folder

ipak(c("tidyverse"))

# Pull data from GROA.
# Further info and metadata here https://github.com/forc-db/GROA
GROAsites <- read.csv("https://raw.githubusercontent.com/forc-db/GROA/master/data/sites.csv")
GROAbiomass <- read.csv("https://raw.githubusercontent.com/forc-db/GROA/master/data/biomass_litter_CWD.csv")

