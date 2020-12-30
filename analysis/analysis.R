devtools::load_all() # This will load functions in the R folder

ipak(c("tidyverse", "ggmap", "leaflet"))

# Pull data from GROA.
# Further info and metadata here https://github.com/forc-db/GROA
g.sites <- read.csv("https://raw.githubusercontent.com/forc-db/GROA/master/data/sites.csv")[,-1]
g.biomass <- read.csv("https://raw.githubusercontent.com/forc-db/GROA/master/data/biomass_litter_CWD.csv")


# Get a sense for the distribution of sites

leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>%
  addMarkers(lng = g.sites$long_dec,
             lat = g.sites$lat_dec)


# # Want to get bird and mammal species lists at each site.
# # Will do this outside of this code, as it requires IUCN bird and mammal range
# # shapefiles that have access controlled by IUCN.
#
#
# write.csv(select(g.sites, c("long_dec", "lat_dec", "site.id")) %>%
#             filter(!duplicated(.$site.id)),
#           file = "~/Downloads/groa.coords.csv")
#
#
# # This is the code that I'll execute on a cluster
# ipak("raster", "rgdal", "sf")
#
#
# # Load mammal range data
# mamm.ranges <- shapefile("/nfs/efricke-data/species ranges/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp")
#
#
# # Load bird range data
# setwd("/nfs/efricke-data/species ranges/")
# bird.ranges <- sf::st_read(dsn = "BOTW.gdb", layer="All_Species")
#
#
# # Bring in study coordinates
# groa.coords <- read.csv("~/Downloads/groa.coords.csv", row.names = 1)
#
# projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#
# groa.coords <- st_as_sf(groa.coords,
#                         coords = c("long_dec", "lat_dec"),
#                         crs = projcrs)
#
# # Want a 5km buffer (using appropriate projection for the study site)
# utm_prj4 <- function(x) {
#
#   coords <- cbind(x, st_coordinates(x))
#
#   long <- coords$X
#   lat <- coords$Y
#
#   zone <- if(lat >= 56 && lat < 64 && long >= 3 && long < 12){x <- 32} else if(
#     lat >= 72 && lat < 84 && long >= 0 && long < 9) {x <- 31} else if(
#       lat >= 72 && lat < 84 && long >= 9 && long < 21) {x <- 33} else if(
#         lat >= 72 && lat < 84 && long >= 21 && long < 33) {x <- 35} else if(
#           lat >= 72 && lat < 84 && long >= 33 && long < 42) {x <- 37} else{
#             x <- (floor((long + 180)/6) %% 60) + 1
#           }
#   prj <- purrr::map2_chr(zone, lat, function(y, z){
#     if (z >= 0){
#       paste0("+proj=utm +zone=", y, " +datum=WGS84 +units=m +no_defs")
#     } else{
#       paste0("+proj=utm +zone=", y, " +south", " +datum=WGS84 +units=m +no_defs")
#     }})
#   prj
# }
#
# groa.coords <- map2(1:nrow(groa.coords), utm_prj4(groa.coords), function(x, y){
#   st_transform(groa.coords[x,], y)
# })
# groa.coords <- map(groa.coords, ~ st_buffer(., 5000))
# groa.coords <- map(groa.coords, ~ st_transform(., projcrs)) # Go back to orig projection
# groa.coords <- do.call(rbind, groa.coords) # Go from list back to df
#
#
# # Spatial join and spread to make a big matrix of species presence
# mamm.ranges.sf <- st_as_sf(mamm.ranges)
#
# mamm.presence.by.groacoords <- groa.coords %>% st_join(filter(mamm.ranges.sf,
#                                                               presence == 1))
#
# mamm.presence.by.groacoords.wide <- mamm.presence.by.groacoords[,c("groa.id", "binomial")]
# st_geometry(mamm.presence.by.groacoords.wide) <- NULL
# mamm.presence.by.groacoords.wide <- mamm.presence.by.groacoords.wide[!duplicated(mamm.presence.by.groacoords.wide),]
# mamm.presence.by.groacoords.wide$value <- T
# mamm.presence.by.groacoords.wide <- spread(data = mamm.presence.by.groacoords.wide, key = groa.id, value = value, fill = F)
#
# setwd("/nfs/efricke-data/species ranges/presence.by.groacoords")
# save(mamm.presence.by.groacoords.wide, file = "mamm.presence.by.groacoords.wide.RData")
#
#
# # Spatial join and spread to make a big matrix of species presence
# bird.ranges.sf <- st_as_sf(bird.ranges,
#                            crs = projcrs)
#
# bird.ranges.sf <- filter(bird.ranges.sf,
#                          PRESENCE == 1) %>% st_zm()
#
# bird.ranges.sf[which(st_geometry_type(bird.ranges.sf) != "MULTIPOLYGON"),] <- bird.ranges.sf[which(st_geometry_type(bird.ranges.sf) != "MULTIPOLYGON"),] %>% lwgeom::st_make_valid()#st_cast(to = "MULTIPOLYGON")
#
#
# bird.presence.by.groacoords <- groa.coords %>% st_join(bird.ranges.sf)
# bird.presence.by.groacoords.wide <- bird.presence.by.groacoords[,c("groa.id", "SCINAME")]
# st_geometry(bird.presence.by.groacoords.wide) <- NULL
# bird.presence.by.groacoords.wide <- bird.presence.by.groacoords.wide[!duplicated(bird.presence.by.groacoords.wide),]
# bird.presence.by.groacoords.wide$value <- T
# bird.presence.by.groacoords.wide <- spread(data = bird.presence.by.groacoords.wide, key = groa.id, value = value, fill = F)
#
# setwd("/nfs/efricke-data/species ranges/presence.by.groacoords")
# save(bird.presence.by.groacoords.wide, file = "bird.presence.by.groacoords.wide.RData")



#

groa.dat <- filter(g.biomass,
                   variables.name == "aboveground_biomass" &
                     stand.age > 0 &
                     mean_ha > 0)

groa.dat <- groa.dat[complete.cases(select(groa.dat,
                                           c("mean_ha", "stand.age"))),]

plot(mean_ha ~ stand.age, data = groa.dat, log = "y")

plot(log(mean_ha) ~ stand.age, data = groa.dat)


mod <- glm(mean_ha ~ 1, data = groa.dat, family = "Gamma")
summary(mod)

mod.shape <- summary(mod)$dispersion
mod.scale <- 1 / coef(mod) / summary(mod)$dispersion

par(mfrow=c(2,1))
groa.dat$mean_ha %>% log() %>% hist() # [-which(groa.dat$mean_ha > 1200)]
rgamma(1000000, shape = mod.shape, scale = mod.scale) %>% log() %>% hist()


# Testing a Michaelis-Menten style model

dataList <- list(
  biomass = groa.dat$mean_ha,
  age = groa.dat$stand.age,
  n = length(groa.dat$mean_ha),

  pred.ages = 1:100
)



###
### Define model
###

sink("./analysis/biomass_jags.txt")
cat("

model {

  # Likelihood
  for (i in 1:n) {
    #mean [i] <- a*age[i] * exp(-b*age[i])

    mean [i] <- a*age[i] / (b + age[i])

    # convert this mean into the rate paramter
    rate [i] <- shape/mean[i]

    # assuming gamma distributed biomass data
    biomass[i] ~ dgamma(shape, rate[i])
  }

  # Priors
  a ~ dgamma (0.1, 0.1)
  b ~ dgamma (0.1, 0.1)
  shape ~ dgamma (0.01, 0.01)

  # Derived quantities

  for(i in 1:100){
    #pred.biomass[i] <- a*pred.ages[i]*exp(-b*pred.ages[i])
    pred.biomass[i] <- a*pred.ages[i] / (b + pred.ages[i])
  }


} # End of model

    ",fill=TRUE)
sink()

ipak("rjags")

inits <- list(list(a = 4, b = 0.2, shape = 90),
              list(a = 1, b = 0.1, shape = 50),
              list(a = 8, b = 0.1, shape = 150))

biomass.jags <- jags.model("biomass_jags.txt", data = dataList,
                           n.chains = 3, inits = inits)

biomass.samples <- coda.samples(biomass.jags,
                           variable.names = c("a",
                                              "b",
                                              "pred.biomass",
                                              "shape"),
                           thin = 10,
                           n.iter = 1000)


biomass.samples.df <- do.call(rbind, biomass.samples)

biomass.samples.df %>% as.data.frame() %>% select(starts_with("pred")) %>% colMeans() %>% plot()

#plot(biomass.samples)

plot(mean_ha ~ stand.age, data = groa.dat, log = "y")

curve(7.5 * x * exp(-0.012 * x),
      add = T,
      col = "blue",
      lwd = 3)

biomass.samples.df %>% as.data.frame() %>% select(-starts_with("pred")) %>% colMeans()

j.mean <- 150

j.shape <- 1.25
j.scale <- j.mean / j.shape

par(mfrow=c(3,1))
groa.dat$mean_ha %>% log() %>% hist() # [-which(groa.dat$mean_ha > 1200)]
rgamma(1000, shape = mod.shape, scale = mod.scale) %>% log() %>% hist()
rgamma(1000, shape = j.shape, scale = j.scale) %>% log() %>% hist()

