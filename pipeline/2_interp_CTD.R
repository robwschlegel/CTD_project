#############################################################################
###"proc/interp.CTD.R"
## This script does:
# 1. Load and prep CTD data
# 2. 
## DEPENDS ON:
library(readr)
library(lubridate)
library(plyr)
library(dplyr)
library(data.table)
library(ggplot2)
library(viridis)
library(akima)
library(raster)
library(mgcv)
library(animation)
library(marmap)
library(doMC); doMC::registerDoMC(4)
source("func/shoreNormalTransectFunc.R")
## USED BY:
# Nothing
## CREATES:
# A lekker cleaned up CTD data base
#############################################################################


# 1. Interpolate CTD data -------------------------------------------------

## Load the 3D data
load("data/DAFF_pelagic.Rdata")
load("data/DEA_SADCO.Rdata")

## Combine
DAFF_pelagic_sub <- DAFF_pelagic[,c(1:5,7,6,8,9,11,13)]
DEA_CTD_sub <- DEA_CTD[,c(1:8,10:12)]
ALL_CTD <- rbind(DAFF_pelagic_sub, DEA_CTD_sub)

## Add stuff and things
ALL_CTD <- data.table(ALL_CTD[ALL_CTD$depth >= 0 & ALL_CTD$depth <= 1000,]) # Currently removing data deeper than 1000m for speed purposes
ALL_CTD <- ALL_CTD[complete.cases(ALL_CTD$date),]
# ALL_CTD <- ALL_CTD[complete.cases(ALL_CTD$depth),]
ALL_CTD$depth <- round_any(ALL_CTD$depth, 10)
ALL_CTD <- ALL_CTD[,.(temp = mean(temp, na.rm = T)), by = .(date, lon, lat, depth)]
ALL_CTD$month <- lubridate::month(ALL_CTD$date, label = TRUE)
ALL_CTD$year <- lubridate::year(ALL_CTD$date)

## Create grid
# x.range <- as.numeric(c(10.0, 40.0))  # min/max longitude of the interpolation area
# y.range <- as.numeric(c(-40.0, -20.0))  # min/max latitude of the interpolation area
# grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1), 
# y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid
# coordinates(grd) <- ~x + y
# gridded(grd) <- TRUE


## Function for creating interpolated slices per monthly clim
# Must manually tell the function which column to use for "z
# df <- filter(ALL_CTD, month == "Jan", depth == 10)
# mask <- bathy_mask
interpp.CTD <- function(df){
  df1 <- data.table(df[complete.cases(df),])
  if(length(df1$temp) < 5){
    res <- data.frame(lon = NA, lat = NA, temp = NA,
                      depth = df$depth[1], month = df$month[1], year = df$year[1])
    message("Fewer than 5 values. Interpolation not calculated.")
    return(res)
  }
  grid_interp_t <- interpp(x = df1$lon, y = df1$lat, z = df1$temp,
                           xo = bathy_mask$V1, yo = bathy_mask$V2, linear = TRUE, 
                           extrap = FALSE, dupl = "mean")
  res <- data.frame(grid_interp_t)
  # grid_interp_s <- interpp(x = df1$lon, y = df1$lat, z = df1$salinity,
  #                        xo = mask$V1, yo = mask$V2, linear = TRUE, 
  #                        extrap = FALSE, dupl = "mean")
  # res$salinity <- grid_interp_s$z
  colnames(res)[1:3] <- c("lon", "lat", "temp")
  res$depth <- df$depth[1]
  res$month <- df$month[1]
  res$year <- df$year[1]
  res <- res[complete.cases(res),]
  return(res)
}

# Isolate interpolation issues to feed solutions back into the above function
# test <- data.frame()
# for(i in 1:5){#length(levels(as.factor(ALL_CTD$depth)))){
#   for(j in 1:length(levels(as.factor(ALL_CTD$month)))){
#     df1 <- filter(ALL_CTD, depth == levels(as.factor(ALL_CTD$depth))[i] & month == levels(as.factor(ALL_CTD$month))[i])
#     df2 <- interpp.CTD(df1, mask = bathy_mask)
#     test <- rbind(test, df2)
#   }
# }

## Run it
# Monthly values
system.time(CTD_interp_monthly <- ddply(ALL_CTD, .(depth, month), interpp.CTD, .progress = "text")) ## 7 seconds
CTD_interp_monthly$year <- NULL
save(CTD_interp_monthly, file = "data/CTD_interp_monthly.Rdata")
# Annual values
system.time(CTD_interp_annual <- ddply(ALL_CTD, .(depth, year), interpp.CTD, .progress = "text")) ## 9 seconds
CTD_interp_annual$month <- NULL
save(CTD_interp_annual, file = "data/CTD_interp_annual.Rdata")
# Monthly and annual values
system.time(CTD_interp_monthly_annual <- ddply(ALL_CTD, .(depth, month, year), interpp.CTD, .progress = "text")) ## 20 seconds
save(CTD_interp_monthly_annual, file = "data/CTD_interp_monthly_annual.Rdata")

# Look at one slice
slice <- filter(CTD_interp_monthly, month == "Nov", depth == "50")

load("metadata/shore.Rdata")
ggplot(data = slice, aes(x = lon, y = lat)) +
  geom_raster(aes(fill = temp)) +
  geom_polygon(data = shore, aes(x = X, y = Y, group = PID)) +
  scale_fill_viridis()





######## Soap-film smoothing
# Create bathy mask and temperature data frames
foutline <- sa0
fdepth <- filter(ALL_CTD, month == "Jan", depth == 0)
foutline_sub <- foutline[foutline$lat < (max(fdepth$lat)+0.5) & foutline$lon < (max(fdepth$lon)+0.5),]
foutline_sub <- rbind(foutline_sub, foutline_sub[1,])
# Visualise
ggplot(foutline, aes(x = lon, y = lat)) +
  geom_path() +
  geom_path(data = foutline_sub, colour = "red", size = 0.5) +
  geom_point(data = fdepth, aes(x = lon, y = lat, colour = temp)) +
  coord_fixed() + ylab("Northing") + xlab("Easting") +
  scale_color_viridis()

## Run an additive model
library(mgcv)
crds <- foutline_sub[,2:3] # These coords were meant to be pulled from a shape file...
tprs <- gam(temp ~ s(lon, lat, k = 60), data = fdepth, method = "REML")
summary(tprs)
# Adjsut values so it can be plotted
grid.x <- with(tprs$var.summary,
               seq(min(c(lon, crds[,1])), max(c(lon, crds[,1])), by = 0.1))
grid.y <- with(tprs$var.summary,
               seq(min(c(lat, crds[,2])), max(c(lat, crds[,2])), by = 0.1))
pdata <- with(tprs$var.summary, expand.grid(lon = grid.x, lat = grid.y))
names(pdata) <- c("lon","lat")
##predictions
pdata <- transform(pdata, temp = predict(tprs, pdata, type = "response"))
tmp <- pdata                         # temporary version...
# take <- with(tmp, Depth > 0)        # getting rid of > 0 depth points
# tmp$Depth[take] <- NA
# Visualise the results
ggplot(foutline, aes(x = lon, y = lat)) + # lol. This looks terrible...
  geom_raster(data = tmp, aes(x = lon, y = lat, fill = temp)) +
  geom_path() +
  geom_point(data = fdepth, aes(x = lon, y = lat), size = 0.5) +
  coord_fixed() + ylab("Northing") + xlab("Easting") +
  scale_fill_viridis(na.value = NA)

# Now we get to the soap-filter
bound <- list(list(lon = crds$lon, lat = crds$lat))
# N <- 10 # not using
gx <- seq(min(crds[,1]), max(crds[,1]), len = 300)
gy <- seq(min(crds[,2]), max(crds[,2]), len = 400)
gp <- expand.grid(gx, gy)
names(gp) <- c("lon","lat")
knots <- gp[with(gp, inSide(bound, lon, lat)), ]
names(knots) <- c("lon", "lat")

# Making a plan
knots$lon <- round(knots$lon,1)
knots$lat <- round(knots$lat,1)
knots <- unique(knots)
ki <- knnx.index(as.matrix(knots), as.matrix(crds), k = 1)
knots <- knots[-ki,]

# Freestyle
# fs.test(foutline[,2:3], fdepth[,2:3])
bound <- list(list(lon = foutline$lon, lat = foutline$lat))
knots <- fdepth[with(fdepth, inSide(bound, lon, lat)),]
knots <- fdepth[with(fdepth, inSide(foutline[,2:3], lon, lat)),]
knots <- knots[with(knots, inSide(bound, lon, lat)),]

knots <- fdepth[with(fdepth, inSide(bound, lon, lat)), ]
inlake <- with(pdata, inSide(bound, lon, lat))
pdata <- pdata[inlake, ]

ggplot(foutline, aes(x = lon, y = lat)) +
  geom_raster(data = pdata, aes(x = lon, y = lat, fill = temp)) +
  geom_path() +
  # geom_point(data = knots, aes(x = lon, y = lat), size = 0.5) +
  geom_point(data = fdepth, aes(x = lon, y = lat), size = 0.1) +
  coord_fixed() + ylab("Northing") + xlab("Easting") +
  scale_fill_viridis(na.value = NA)



cheeky <- data.frame(lon = 20, lat = -35)

# The gam for interpolating
m2 <- gam(temp ~ s(lon, lat, bs = "so", xt = list(bnd = bound)),
          data = knots, method = "REML", knots = cheeky)


## Comparison
inlake <- with(pdata, inSide(bound, lon, lat))
pdata <- pdata[inlake, ]

pdata2 <- transform(rbind(pdata, pdata2),
                    Model = rep(c("TPRS", "Soap-film"),
                                times = c(nrow(pdata), nrow(pdata2))))

## let's drop the NAs from the Soap-film too...
take <- with(pdata2, !is.na(Depth))
pdata2 <- pdata2[take, ]

poutline <- transform(rbind(foutline, foutline),
                      Model = rep(c("TPRS", "Soap-film"), each = nrow(foutline)))
names(poutline)[1:2] <- c("os_x", "os_y")

ggplot(poutline, aes(x = os_x, y = os_y)) +
  geom_raster(data = pdata2, aes(x = os_x, y = os_y, fill = Depth)) +
  geom_path() +
  geom_point(data = fdepth, aes(x = os_x, y = os_y), size = 0.5) +
  coord_fixed() +
  ylab("Northing") + xlab("Easting") +
  scale_fill_viridis(na.value = NA) +
  facet_wrap( ~ Model) +
  theme(legend.position = "top", legend.key.width = unit(2.5, "cm"))

