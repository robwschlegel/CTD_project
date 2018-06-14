#############################################################################
###"proc/interp.CTD.R"
## This script does:
# 1. Load and prep CTD data
# 2. Prepare interpolation grid
## DEPENDS ON:
library(tidyverse)
library(lubridate)
library(data.table)
# library(viridis)
library(akima)
# library(raster)
# library(mgcv)
# library(animation)
# library(marmap)
# library(doMC); doMC::registerDoMC(4)
# source("func/shoreNormalTransectFunc.R")
## USED BY:
# pipeline/pipeline.R
## CREATES:
# A lekker cleaned up CTD data base
#############################################################################


# 1. Load and prep CTD data -----------------------------------------------

# Load the CTD data
load("data/DAFF_pelagic.Rdata")
load("data/DEA_SADCO.Rdata")

# Combine
ALL_CTD <- rbind(DAFF_pelagic, DEA_SADCO)
rm(DAFF_pelagic, DEA_SADCO)

# Currently removing data deeper than 1000m for speed purposes
ALL_CTD <- data.table(ALL_CTD[ALL_CTD$depth >= 0 & ALL_CTD$depth <= 1000,])

# Remove all rows with missing dates and depths
ALL_CTD <- ALL_CTD[complete.cases(ALL_CTD$date),]
ALL_CTD <- ALL_CTD[complete.cases(ALL_CTD$depth),]

# Round down depth, lon, and lat values
ALL_CTD <- ALL_CTD %>% 
  mutate(depth = plyr::round_any(depth, 10),
         lon = plyr::round_any(lon, 0.1),
         lat = plyr::round_any(lat, 0.1))

# Create reduced gridded mean values
  ## NB: This presently removes the cruise, station, and type categories
  ## This will be added back in as the app takes shape
ALL_CTD <- data.table(ALL_CTD)[,.(temp = mean(temp, na.rm = T),
                                  salinity = mean(salinity, na.rm = T),
                                  oxygen = mean(oxygen, na.rm = T)),
                               by = .(date, lon, lat, depth)]

# Add month and year columns
ALL_CTD <- ALL_CTD %>% 
  mutate(month = lubridate::month(date, label = TRUE),
         year = lubridate::year(date))


# 2. Prepare interpolation grid -------------------------------------------

## Create grid
# x.range <- as.numeric(c(10.0, 40.0))  # min/max longitude of the interpolation area
# y.range <- as.numeric(c(-40.0, -20.0))  # min/max latitude of the interpolation area
# grd <- expand.grid(x = seq(from = x.range[1], to = x.range[2], by = 0.1), 
# y = seq(from = y.range[1], to = y.range[2], by = 0.1))  # expand points to grid
# coordinates(grd) <- ~x + y
# gridded(grd) <- TRUE

# Load bathymetry masks
load("data/bathy_mask.Rdata")
load("data/bathy_masks.Rdata")

## Function for interpolating a column in the CTD data.frame
# df <- filter(ALL_CTD, month == "Jan", depth == 10)
# column <- "temp"
interpp_col <- function(df, column){
  df1 <- df
  df1$z <- df1[,colnames(df1) == column]
  df1 <- df1[complete.cases(df1$z),]
  # column1 <- column[complete.cases(column)]
  if(nrow(df1) < 5){
    res <- data.frame(lon = NA, lat = NA, z = NA,
                      depth = df$depth[1], month = df$month[1], year = df$year[1])
    colnames(res)[3] <- column
  } else {
    res <- data.frame(interpp(x = df1$lon, y = df1$lat, z = df1$z,
                              xo = bathy_masks$lon, yo = bathy_masks$lat, linear = TRUE, 
                              extrap = FALSE, dupl = "mean"))
  colnames(res) <- c("lon", "lat", column)
  res <- res %>% 
    mutate(depth = df$depth[1], 
           month = df$month[1], 
           year = df$year[1]) %>% 
    unique()
  }
  return(res)
}

## Function for creating interpolated slices
# df <- filter(ALL_CTD, month == "Jan", depth == 10)
# column <- df$temp
# mask <- bathy_mask
interpp_CTD <- function(df){
  res_temp <- interpp_col(df, "temp") %>% 
    na.omit()
  res_sal <- interpp_col(df, "salinity") %>% 
    select(lon, lat, salinity) %>% 
    na.omit()
  res_ox <- interpp_col(df, "oxygen") %>% 
    select(lon, lat, oxygen) %>% 
    na.omit()
  res <- res_temp %>% 
    left_join(res_sal, by = c("lon", "lat")) %>%
    left_join(res_ox, by = c("lon", "lat"))
  res <- res %>%
    select(year, month, lon, lat, depth, temp, salinity, oxygen)
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
CTD_interp_monthly <- ALL_CTD %>% 
              group_by(depth, month) %>% 
              nest() %>% 
              mutate(res = map(data, interpp_CTD))

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

