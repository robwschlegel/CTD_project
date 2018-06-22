#############################################################################
###"pipeline/0_bathy_masks.R"
## This script does:
# 1. Create bathy masks and save
## DEPENDS ON:
library(tidyverse)
library(data.table)
# library(doMC); doMC::registerDoMC(4)
# source("func/shoreNormalTransectFunc.R")
## USED BY:
# Nothing
## CREATES:
# Create bathymetry masks at 10 m intervals for better interpolating
#############################################################################


# 2. Create interpolated product ------------------------------------------

## Load bathy mask
# Then convert it to a 0.1 degree resolution
load("metadata/bathy_mask.Rdata")
bathy_mask$lon <- round(bathy_mask$lon,1)
bathy_mask$lat <- round(bathy_mask$lat,1)
bathy_mask <- data.table(bathy_mask)
bathy_mask <- bathy_mask[,.(depth = mean(depth, na.rm = T)), by = .(lon, lat)]
bathy_mask <- filter(bathy_mask, depth < 0)
# bathy_mask <- filter(bathy_mask, depth > -1000 & depth < 0)

## Load southern africa coast to account for Namibia data
# Then reduce it to 0.1 degree resolution
load("metadata/southern_africa_coast.Rdata")
southern_africa_coast <- southern_africa_coast[southern_africa_coast$group == 1.1,]
southern_africa_coast <- southern_africa_coast[,1:2]
colnames(southern_africa_coast)[1] <- "lon"
southern_africa_coast$lat <- round_any(southern_africa_coast$lat, 0.1)
southern_africa_coast$lon <- round_any(southern_africa_coast$lon, 0.1)
southern_africa_coast <- unique(southern_africa_coast)
southern_africa_coast$site <- "SA"
southern_africa_coast$index <- seq(1:length(southern_africa_coast$lon))

# Function for calculating bounding boxes
bounding.box.double.depth <- function(dat, isobath1 = 0, isobath2 = -1000){
  heading <- shore.normal.transect(dat, width = 10)
  pixels <- transect.pixel.isobath(heading, isobath = isobath2-500)
  if(pixels$depth[1] < isobath1){
    bathy1 <- data.frame(lon = dat$lon, lat = dat$lat, depth = isobath1)
  } else {
    bathy1 <- pixels[pixels$depth < isobath1, c(2,3,5)][1,]
  }
  pixels2 <- pixels[pixels$depth<isobath2+200,]
  if(nrow(pixels2)==0){
    bathy2 <- data.frame(lon = NA, lat = NA, depth = NA)
  } else {
    bathy2 <- pixels2[knnx.index(as.matrix(pixels2$depth), isobath2, k = 1), c(2,3,5)]
  }
  bathy1$type <- 1
  bathy2$type <- 2
  res <- rbind(bathy1, bathy2)
  return(res)
}

# Function that sorts out the results
unshuffle.bathy <- function(df){
  df <- df[complete.cases(df),]
  df1 <- df[df$type == 1,]
  df2 <- df[df$type == 2,]
  df2 <- df2[rev(rownames(df2)),]
  res <- rbind(df1, df2, df1[1,])
  return(res)
}

bounding.box.double.depth.out <- function(df){
  res1 <- ddply(southern_africa_coast, .(index), bounding.box.double.depth, isobath1 = df$depth_index, .parallel = T)
  res2 <- unshuffle.bathy(res1)
  return(res2)
}

depth_index <- data.frame(depth_index = rev(seq(-990, 0, 10)),
                          x = 1:length(seq(-990, 0, 10)))

system.time(bathy_masks <- ddply(depth_index, .(depth_index), bounding.box.double.depth.out, .progress = "text"))
# Rather use a for loop for more control
bathy_masks <- data.frame()
for(i in 1:1){#length(depth_index$depth_index)){
  mask1 <- bounding.box.double.depth.out(depth_index[i,])
  mask1$depth_index <- depth_index$depth_index[i]
  bathy_masks <- rbind(mask1, bathy_masks)
}
save(bathy_masks, file = "metadata/bathy_masks.Rdata")

ggplot(data = bathy_masks, aes(x = lon, y = lat)) +
  # geom_polygon() +
  geom_contour(data = bathy_mask, aes(x = lon, y = lat, z = depth), bins = 20) +
  geom_path(data = southern_africa_coast, aes(x = lon,y = lat)) +
  geom_path(colour = "red") +
  # geom_path(aes(colour = depth)) +
  facet_wrap(~depth_index)
ggsave(file = "~/Desktop/all_masks.pdf", width = 24, height = 24)
