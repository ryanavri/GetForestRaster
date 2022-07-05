rm(list = ls()) # clear workspace

#Preparation####
# load packages
library(raster)
library(sp)
library(rgdal)
library(tidyverse)
library(ggplot2)
library(sf)
library(gfcanalysis)
library(maptools)

#set folder to save gfc data
output_folder <- "D:/Ryan/GetForestRaster/output"

#load area of interest
aoi <- st_read("Layer/KSNP.shp")

##donwload dataset from GFC####

#calculate how many tiles needed
tiles <- calc_gfc_tiles(aoi)
print(length(tiles))

#see how the aoi met GFC tiles
plot(tiles)
plot(aoi, add=TRUE, lty=2, col="#00ff0050")

#download tiles
download_tiles(tiles, output_folder, images = c("treecover2000", "lossyear", "gain", "datamask"),
               dataset = "GFC-2021-v1.9")

##clean data to AOI####
#set canopy cover to differentiate between forest (>=75) and non forest (<75)
threshold <- 75

#extract and write raster
gfc_extract <- extract_gfc(aoi, output_folder, 
                           filename="KSNP.tif", 
                           overwrite = TRUE, dataset = "GFC-2021-v1.9")

gfc_thresholded <- threshold_gfc(gfc_extract, forest_threshold = threshold, 
                                 filename="KSNP_thresholded.tif",
                                 overwrite = TRUE)

#animate forest loss per year
gfc_annual_stack <- annual_stack(gfc_thresholded)
aoi$label <- "KSNP" # Label the polygon on the plot
animate_annual(aoi, gfc_annual_stack, out_dir='.', site_name='KSNP', type = 'gif')

#Calculate forest loss####
#extract one more time to match with aoi
fr <- rasterize(aoi, gfc_thresholded)   
Forest <- mask(x=gfc_thresholded, mask=fr)

#Forest (1 - forest )
forest_cover <- Forest$forest2000
plot(forest_cover, main = "Primary forest cover")

# and yearly forest loss
# 1-21 for year of forest loss
loss_year <-Forest$lossyear
plot(loss_year, main = "Loss year")

# Produce remaining forest layer
forest_22 <- forest_cover
forest_22[loss_year >= 1 & loss_year <= 21 & forest_cover == 1] <- 0
#plot layer
plot(forest_22, main = "Forest in 2022")


# write layer out (simplified)
writeRaster(forest_22, "forest22int.tif",
            overwrite=TRUE,
            datatype='INT1U') #as integer with 1 decimal

# produce yearly deforestation overview
loss_year_data <- as.data.frame(loss_year, xy = T) %>% 
  rename(loss_year = lossyear) %>% 
  filter(loss_year != 0) %>% # excludes all NAs and data from 2000
  group_by(loss_year) %>% # summarise loss by year
  summarise(loss_km2 = (n()*769.26)/1000000) %>% #convert from c. 27x27 m pixel to kmsq
  mutate(loss_year = loss_year + 2000) # format year

ggplot()+
  geom_bar(data = loss_year_data  ,
           aes(x = loss_year, y = loss_km2),stat = "identity", fill = "yellow")+
  theme_minimal() +
  ggtitle("forest loss over time")+
  xlab("year") +
  ylab("forest loss (km2)") 

forest_area_km2 <- (sum(as.data.frame(forest_22$forest2000, na.rm=T))*769.26)/1000000
forest_area_init <- (sum(as.data.frame(Forest$forest2000, na.rm=T))*769.26)/1000000
total_loss <- forest_area_init - forest_area_km2
