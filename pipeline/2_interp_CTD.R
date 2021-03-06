#############################################################################
###"proc/interp.CTD.R"
## This script does:
# 1. Load and prep CTD data
# 2. Linear interpolation
# 3. Soap-film smoothing
## DEPENDS ON:
library(tidyverse)
library(lubridate)
library(data.table)
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

# Load the normal bathymetry mask for linear interpolation
load("data/bathy_mask.Rdata")
# ggplot(data = bathy_mask, aes(x = lon, y = lat)) +
# geom_point(aes(colour = depth))

bathy_mask_filter <- bathy_mask %>% 
  filter(depth <= 0, depth >= -1000) %>%
  # filter(depth <= 0, abs(depth) <= max(CTD_pre_interp$depth)) %>% 
  mutate(lon = round(lon, 1),
         lat = round(lat, 1)) %>% 
  group_by(lon, lat) %>% 
  summarise(depth = mean(depth, na.rm = T)) %>% 
  mutate(depth = abs(round(depth, -1)))
rm(bathy_mask)

# Load the CTD data
load("data/DAFF_pelagic.Rdata")
load("data/DEA_SADCO.Rdata")

# Combine
ALL_CTD <- rbind(DAFF_pelagic, DEA_SADCO)
rm(DAFF_pelagic, DEA_SADCO)

# Currently removing data deeper than 1000m for speed purposes
ALL_CTD <- data.table(ALL_CTD[ALL_CTD$depth >= 0 & ALL_CTD$depth <= 1000,])
# Leaving depths greter than 1000 metres in for now
# ALL_CTD <- data.table(ALL_CTD[ALL_CTD$depth >= 0,])

# Remove all rows with missing dates, depths, lons, and lats
ALL_CTD <- ALL_CTD[complete.cases(ALL_CTD$date),]
ALL_CTD <- ALL_CTD[complete.cases(ALL_CTD$depth),]
ALL_CTD <- ALL_CTD[complete.cases(ALL_CTD$lon),]
ALL_CTD <- ALL_CTD[complete.cases(ALL_CTD$lat),]

# Replace bottom depth less than 20 with NA and remove those lines
# Currently rather using the GEBCO bathymetry mask for proper bottom depths
# ALL_CTD$bot_depth[ALL_CTD$bot_depth <= 20] <- NA
# ALL_CTD$bot_depth[ALL_CTD$bot_depth == 9999] <- NA  
# ALL_CTD$bot_depth[ALL_CTD$bot_depth == 9999.99] <- NA
# ALL_CTD <- ALL_CTD[complete.cases(ALL_CTD$bot_depth),]

# Round down depth, lon, and lat values
ALL_CTD <- ALL_CTD %>% 
  mutate(depth = round(depth, -1),
         lon = round(lon, 1),
         lat = round(lat, 1))

# Filter out anomolous oxygen and salinity values
  # Temperatures were already screened in "1_load_CTD.R"
ALL_CTD$salinity[ALL_CTD$salinity > 40 | ALL_CTD$salinity < 30] <- NA
ALL_CTD$oxygen[ALL_CTD$oxygen > 11 | ALL_CTD$oxygen < 0] <- NA
# ALL_CTD <- ALL_CTD %>% 
#   filter(salinity <= 40, salinity >= 30,
#          oxygen <= 11, oxygen >= 0)

# Determine the average bottom depth per pixel and round to the nearest 10 metres
# Presenty using GEBCO bathymetry for bottom depths
# ALL_CTD <- ALL_CTD %>% 
#   group_by(lon, lat) %>% 
#   mutate(bot_depth = mean(bot_depth, na.rm = T),
#          bot_depth = round(bot_depth, -1)) %>% 
#   mutate(bot_depth = if_else(max(depth) > bot_depth, max(depth), bot_depth))
# Check that no depths are deeper than the bttom depths
# There are several that were off by 10 metres due to rounding and so are corrected
# filter(ALL_CTD, depth > bot_depth)

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
         year = lubridate::year(date)) %>% 
  mutate(month = factor(month, levels = c("Jan", "Feb", "Mar", "Apr",
                                          "May", "Jun", "Jul", "Aug",
                                          "Sep", "Oct", "Nov", "Dec"))) %>%
  arrange(date)

# Clean up NA values
ALL_CTD$temp[ALL_CTD$temp == "NaN"] <- NA  
ALL_CTD$salinity[ALL_CTD$salinity == "NaN"] <- NA  
ALL_CTD$oxygen[ALL_CTD$oxygen == "NaN"] <- NA  

# Reduce precision of measurements to reduce file size
CTD_pre_interp <- ALL_CTD %>% 
  mutate(temp = round(temp, 2),
         salinity = round(salinity, 2),
         oxygen = round(oxygen, 2)) %>% 
  ungroup() %>% 
  select(-date) %>% 
  select(year, month, everything()) %>%
  left_join(bathy_mask_filter, by = c("lon", "lat")) %>% 
  rename(depth = depth.x, depth_bot = depth.y) %>% 
  mutate(depth_to = abs(depth - depth_bot)) %>% 
  select(year, month, lon, lat, depth, depth_to, depth_bot, everything()) %>% 
  arrange(depth)

# Save for further use
save(CTD_pre_interp, file = "data/CTD_pre_interp.Rdata")
save(CTD_pre_interp, file = "./shiny/CTD_project/CTD_pre_interp.Rdata")
rm(ALL_CTD)

# 2. Linear interpolation -------------------------------------------------

# Load result from previous
load("data/CTD_pre_interp.Rdata")

## Function for interpolating a column in the CTD data.frame
# df <- filter(CTD_pre_interp, month == "Jan", depth == 10) %>%
  # mutate(depth2 = depth)
# column <- "temp"
interpp_col <- function(df, column){
  df1 <- as.data.frame(df)
  df1$z <- as.vector(df1[,colnames(df1) == column])
  df1 <- df1[complete.cases(df1$z),]
  bathy <- bathy_mask_filter %>% 
    filter(depth >= df$depth2[1])
  # bot_depth <- bathy[bathy$lon %in% df$lon & bathy$lat %in% df$lat,]
  if(nrow(df1) <= 10){
    res <- data.frame(lon = df1$lat, lat = df1$lon, z = df1$z)
    colnames(res)[3] <- column
  } else {
    suppressWarnings(res <- data.frame(interpp(x = df1$lon, y = df1$lat, z = df1$z,
                              xo = bathy$lon, yo = bathy$lat, linear = TRUE, 
                              extrap = FALSE, duplicate = "mean",
                              jitter.random = TRUE, jitter.iter = 10, jitter = 0.1)))
  res <- res %>%
    mutate(z = round(z, 2)) %>% 
    unique()
  colnames(res) <- c("lon", "lat", column)
  }
  return(res)
}

## Function for creating interpolated slices
# df <- filter(CTD_pre_interp, month == "Nov", depth == 650) %>%
  # mutate(depth2 = depth) %>%
  # select(-date) %>%
  # mutate(depth2 = depth) %>%
  # select(-depth, -month)
interpp_CTD <- function(df){
  res_temp <- interpp_col(df, "temp") %>% 
    na.omit()
  res_sal <- interpp_col(df, "salinity") %>% 
    na.omit()
  res_ox <- interpp_col(df, "oxygen") %>% 
    na.omit()
  res <- res_temp %>% 
    left_join(res_sal, by = c("lon", "lat")) %>%
    left_join(res_ox, by = c("lon", "lat"))
  return(res)
}
# rm(df, res, res_ox, res_sal, res_temp, test)
# test <- interpp_CTD(df)
# test <- unnest(CTD_interp_monthly[1,3])
# test <- purrr::map(test, interpp_CTD)

## Run it
# Monthly values
system.time(CTD_interp_clim <- CTD_pre_interp %>%
              select(-year, -depth_to, -depth_bot) %>% 
              mutate(depth2 = depth) %>% 
              group_by(month, depth) %>% 
              nest() %>%
              mutate(res = purrr::map(data, interpp_CTD)) %>% 
              select(-data) %>% 
              unnest() %>% 
              left_join(bathy_mask_filter, by = c("lon", "lat")) %>% 
              rename(depth = depth.x, depth_bot = depth.y) %>% 
              mutate(depth_to = abs(depth - depth_bot)) %>% 
              select(month, lon, lat, depth, depth_to, depth_bot, everything()) %>% 
              arrange(depth)) ## 45 seconds
save(CTD_interp_clim, file = "data/CTD_interp_clim.Rdata")
save(CTD_interp_clim, file = "./shiny/CTD_project/CTD_interp_clim.Rdata")

# Annual values
system.time(CTD_interp_yearly <- CTD_pre_interp %>%
              select(-month, -depth_to, -depth_bot) %>% 
              mutate(depth2 = depth) %>%
              group_by(year, depth) %>% 
              nest() %>%
              mutate(res = purrr::map(data, interpp_CTD)) %>% 
              select(-data) %>% 
              unnest() %>% 
              left_join(bathy_mask_filter, by = c("lon", "lat")) %>% 
              rename(depth = depth.x, depth_bot = depth.y) %>% 
              mutate(depth_to = abs(depth - depth_bot)) %>% 
              select(year, lon, lat, depth, depth_to, depth_bot, everything()) %>% 
              arrange(depth)) ## 77 seconds
save(CTD_interp_yearly, file = "data/CTD_interp_yearly.Rdata")
save(CTD_interp_yearly, file = "./shiny/CTD_project/CTD_interp_yearly.Rdata")

system.time(CTD_interp_monthly <- CTD_pre_interp %>%
              mutate(depth2 = depth) %>%
              select(-depth_to, -depth_bot) %>% 
              group_by(year, month, depth) %>%
              nest() %>%
              mutate(res = purrr::map(data, interpp_CTD)) %>%
              select(-data) %>%
              unnest() %>% 
              left_join(bathy_mask_filter, by = c("lon", "lat")) %>% 
              rename(depth = depth.x, depth_bot = depth.y) %>% 
              mutate(depth_to = abs(depth - depth_bot)) %>% 
              select(year, month, lon, lat, depth, depth_to, depth_bot, everything()) %>% 
              arrange(depth)) ## 233 seconds
save(CTD_interp_monthly, file = "data/CTD_interp_monthly.Rdata")
save(CTD_interp_monthly, file = "./shiny/CTD_project/CTD_interp_monthly.Rdata")

# 3. Soap-film smoothing --------------------------------------------------

## NB: This section still requires a good deal of work
# 
# # Create bathy mask and temperature data frames
# foutline <- sa0
# fdepth <- filter(ALL_CTD, month == "Jan", depth == 0)
# foutline_sub <- foutline[foutline$lat < (max(fdepth$lat)+0.5) & foutline$lon < (max(fdepth$lon)+0.5),]
# foutline_sub <- rbind(foutline_sub, foutline_sub[1,])
# # Visualise
# ggplot(foutline, aes(x = lon, y = lat)) +
#   geom_path() +
#   geom_path(data = foutline_sub, colour = "red", size = 0.5) +
#   geom_point(data = fdepth, aes(x = lon, y = lat, colour = temp)) +
#   coord_fixed() + ylab("Northing") + xlab("Easting") +
#   scale_color_viridis()
# 
# ## Run an additive model
# library(mgcv)
# crds <- foutline_sub[,2:3] # These coords were meant to be pulled from a shape file...
# tprs <- gam(temp ~ s(lon, lat, k = 60), data = fdepth, method = "REML")
# summary(tprs)
# # Adjsut values so it can be plotted
# grid.x <- with(tprs$var.summary,
#                seq(min(c(lon, crds[,1])), max(c(lon, crds[,1])), by = 0.1))
# grid.y <- with(tprs$var.summary,
#                seq(min(c(lat, crds[,2])), max(c(lat, crds[,2])), by = 0.1))
# pdata <- with(tprs$var.summary, expand.grid(lon = grid.x, lat = grid.y))
# names(pdata) <- c("lon","lat")
# ##predictions
# pdata <- transform(pdata, temp = predict(tprs, pdata, type = "response"))
# tmp <- pdata                         # temporary version...
# # take <- with(tmp, Depth > 0)        # getting rid of > 0 depth points
# # tmp$Depth[take] <- NA
# # Visualise the results
# ggplot(foutline, aes(x = lon, y = lat)) + # lol. This looks terrible...
#   geom_raster(data = tmp, aes(x = lon, y = lat, fill = temp)) +
#   geom_path() +
#   geom_point(data = fdepth, aes(x = lon, y = lat), size = 0.5) +
#   coord_fixed() + ylab("Northing") + xlab("Easting") +
#   scale_fill_viridis(na.value = NA)
# 
# # Now we get to the soap-filter
# bound <- list(list(lon = crds$lon, lat = crds$lat))
# # N <- 10 # not using
# gx <- seq(min(crds[,1]), max(crds[,1]), len = 300)
# gy <- seq(min(crds[,2]), max(crds[,2]), len = 400)
# gp <- expand.grid(gx, gy)
# names(gp) <- c("lon","lat")
# knots <- gp[with(gp, inSide(bound, lon, lat)), ]
# names(knots) <- c("lon", "lat")
# 
# # Making a plan
# knots$lon <- round(knots$lon,1)
# knots$lat <- round(knots$lat,1)
# knots <- unique(knots)
# ki <- knnx.index(as.matrix(knots), as.matrix(crds), k = 1)
# knots <- knots[-ki,]
# 
# # Freestyle
# # fs.test(foutline[,2:3], fdepth[,2:3])
# bound <- list(list(lon = foutline$lon, lat = foutline$lat))
# knots <- fdepth[with(fdepth, inSide(bound, lon, lat)),]
# knots <- fdepth[with(fdepth, inSide(foutline[,2:3], lon, lat)),]
# knots <- knots[with(knots, inSide(bound, lon, lat)),]
# 
# knots <- fdepth[with(fdepth, inSide(bound, lon, lat)), ]
# inlake <- with(pdata, inSide(bound, lon, lat))
# pdata <- pdata[inlake, ]
# 
# ggplot(foutline, aes(x = lon, y = lat)) +
#   geom_raster(data = pdata, aes(x = lon, y = lat, fill = temp)) +
#   geom_path() +
#   # geom_point(data = knots, aes(x = lon, y = lat), size = 0.5) +
#   geom_point(data = fdepth, aes(x = lon, y = lat), size = 0.1) +
#   coord_fixed() + ylab("Northing") + xlab("Easting") +
#   scale_fill_viridis(na.value = NA)
# 
# 
# 
# cheeky <- data.frame(lon = 20, lat = -35)
# 
# # The gam for interpolating
# m2 <- gam(temp ~ s(lon, lat, bs = "so", xt = list(bnd = bound)),
#           data = knots, method = "REML", knots = cheeky)
# 
# 
# ## Comparison
# inlake <- with(pdata, inSide(bound, lon, lat))
# pdata <- pdata[inlake, ]
# 
# pdata2 <- transform(rbind(pdata, pdata2),
#                     Model = rep(c("TPRS", "Soap-film"),
#                                 times = c(nrow(pdata), nrow(pdata2))))
# 
# ## let's drop the NAs from the Soap-film too...
# take <- with(pdata2, !is.na(Depth))
# pdata2 <- pdata2[take, ]
# 
# poutline <- transform(rbind(foutline, foutline),
#                       Model = rep(c("TPRS", "Soap-film"), each = nrow(foutline)))
# names(poutline)[1:2] <- c("os_x", "os_y")
# 
# ggplot(poutline, aes(x = os_x, y = os_y)) +
#   geom_raster(data = pdata2, aes(x = os_x, y = os_y, fill = Depth)) +
#   geom_path() +
#   geom_point(data = fdepth, aes(x = os_x, y = os_y), size = 0.5) +
#   coord_fixed() +
#   ylab("Northing") + xlab("Easting") +
#   scale_fill_viridis(na.value = NA) +
#   facet_wrap( ~ Model) +
#   theme(legend.position = "top", legend.key.width = unit(2.5, "cm"))
# 


# 4. Clean up -------------------------------------------------------------
rm(bathy_mask_filter, CTD_interp_yearly, CTD_interp_monthly, 
   CTD_interp_clim, CTD_pre_interp, interpp_col, interpp_CTD)


