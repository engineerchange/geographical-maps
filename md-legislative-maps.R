# Maryland maps


# Political maps ----------------------------------------------------------

# 2012 MD legislative redistricting: https://planning.maryland.gov/Redistricting/Pages/2010/legiDist.aspx
# MD geodata page: https://geodata.md.gov/imap/rest/services/Boundaries/MD_ElectionBoundaries/MapServer

# 2022 MD legislative redistricting: https://planning.maryland.gov/Redistricting/Pages/2020/legiDist.aspx

# State Legislative maps -------------------------------------------------------

# Download "Maryland Legislative Districts 2012" as zip, unzip, and place in 'data/'.

# SO: https://stackoverflow.com/questions/69781013/method-in-r-to-crop-whitespace-on-svg-file/69783110#69783110

library(tidyverse)
library(rgdal)
library(sf)
library(magick) # rsvg required
library(xml2)

df = rgdal::readOGR("data/BNDY_LegislativeDistricts2012_MDP/BNDY_LegislativeDistricts2012_MDP.shp")

district_list = c("29C")
district_name = "29C"

df_sf = df %>% st_as_sf()

district = df_sf %>% dplyr::filter(DISTRICT %in% district_list) %>% as_Spatial()

gg = ggplot() +
  # geom_polygon(data=df,aes(x=long,y=lat,group=group),
  #              fill="white", color="black", size=0.4) +
  # geom_polygon(data=district,aes(x=long,y=lat,group=group),
  #              fill="#ff0000", color="black", size=0.4) +
  geom_polygon(data=df,aes(x=long,y=lat,group=group),
               fill="grey", color="white", size=0.1) +
  geom_polygon(data=district,aes(x=long,y=lat,group=group),
               fill="#ff0000", color="white", size=0.1) +
  theme_void() +
  coord_quickmap()

gg

ggsave(file=paste0("out/MD-Legislative-District-",district_name,".svg"), plot=gg,bg="transparent",device = "svg",dpi = 600, width=12, height=8)


# this seems to be broken -------------------------------------------------

img = image_read_svg(paste0("out/MD-Legislative-District-",district_name,".svg"))

data   <- image_data(img) 
opaque <- which(data[4,,] != 0, arr.ind = TRUE)
limits <- paste(paste(apply(opaque, 2, min), collapse = ", "), 
                paste(apply(opaque, 2, max), collapse = ", "), sep = ", ")

img_xml <- read_xml(paste0("out/MD-Legislative-District-",district_name,".svg"))
xml_set_attr(img_xml, attr = "viewBox", limits)
#xml_set_attr(img_xml, attr = "width", max(opaque[,1])+min(opaque[,1]))
#xml_set_attr(img_xml, attr = "height", max(opaque[,2])+min(opaque[,2]))
write_xml(img_xml, paste0("out/MD-Legislative-District-",district_name,".svg"))


