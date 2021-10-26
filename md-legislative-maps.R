# Maryland maps


# Political maps ----------------------------------------------------------

# MD geodata page: https://geodata.md.gov/imap/rest/services/Boundaries/MD_ElectionBoundaries/MapServer


# State Legislative maps -------------------------------------------------------

# Download "Maryland Legislative Districts 2012" as zip, unzip, and place in 'data/'.

library(tidyverse)
library(rgdal)
library(sf)

df = rgdal::readOGR("data/BNDY_LegislativeDistricts2012_MDP/BNDY_LegislativeDistricts2012_MDP.shp")

df_sf = df %>% st_as_sf()

district = df_sf %>% dplyr::filter(DISTRICT %in% c("1A")) %>% as_Spatial()

gg = ggplot() +
  geom_polygon(data=df,aes(x=long,y=lat,group=group),
               fill="grey", color="white", size=0.15) +
  geom_polygon(data=district,aes(x=long,y=lat,group=group),
               fill="#ff0000", color="white", size=0.15) +
  theme_void() +
  coord_quickmap()

ggsave(file="out/MD-District-1A.svg", plot=gg, width=12, height=8,bg="transparent")

