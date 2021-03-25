library(gulf.data)
library(gulf.graphics)
library(gulf.spatial)

# Load last year's stations:
stations <- read.scsset(year = 2020, valid = 1, survey = "regular")

# Assign towIDs from previous year:
mif <- read.gulf.spatial("mif")
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp$longitude
   mif[[i]]$latitude  <- tmp$latitude
   ix <- which(in.polygon(as.polygon(mif[[i]]$longitude, mif[[i]]$latitude), lon(stations), lat(stations)))
   if (length(ix) == 1) mif[[i]]$tow.id <- stations$tow.id[ix] else mif[[i]]$tow.id <- ""
}

# Assign missing tow labels:
ix <- which(unlist(lapply(mif, function(x) x$tow.id)) == "")
s <- which(!(substr(stations$tow.id, 1, 5) %in% substr(unlist(lapply(mif, function(x) x$tow.id)), 1, 5)))
for (i in 1:length(ix)){
   ux <- mean(mif[[ix[i]]]$longitude[1:4])
   uy <- mean(mif[[ix[i]]]$latitude[1:4])
   if (length(s) > 0){
      d <- distance(ux, uy, lon(stations)[s], lat(stations)[s])[1, ]
      mif[[ix[i]]]$tow.id <- stations$tow.id[s[which.min(d)]]
      s <- s[-which.min(d)]
   }
}
ix <- which(!((1:length(mif)) %in% as.numeric(substr(unlist(lapply(mif, function(x) x$tow.id)), 3, 5))))
iy <- which(unlist(lapply(mif, function(x) x$tow.id)) == "")
for (i in 1:length(ix)) mif[[iy[i]]]$tow.id <- paste0("GP", gsub(" ", "0", formatC(ix[i], width = 3)), "F")
for (i in 1:length(mif)) mif[[i]]$tow.id <- paste0(substr(mif[[i]]$tow.id, 1, 5), "F")

# Load survey polygon:
gulf <- read.gulf.spatial("scs.bounds")

# Initialize survey station coordinate table:
tab <- data.frame(id = 1:length(mif), tow.id = unlist(lapply(mif, function(x) x$tow.id)))
ix  <- match(substr(tab$tow.id, 3, 5), substr(stations$tow.id, 3, 5))
tab$longitude <- longitude(stations)[ix]
tab$latitude  <- latitude(stations)[ix]

# Identify which stations lie outside the survey polygon:
ix <- in.polygon(as.polygon(gulf$longitude, gulf$latitude), tab$longitude, tab$latitude)
tab$longitude[!ix] <- NA
tab$latitude[!ix]  <- NA
for (i in 1:length(mif)) mif[[i]]$label <- mif[[i]]$tow.id
ix  <- which(is.na(tab$longitude))
tmp <- scs(grid = mif[ix], alternates = 0)
tab$longitude[ix]   <- tmp$longitude
tab$latitude[ix]    <- tmp$latitude
tab$new.station     <- "No"
tab$new.station[ix] <- "Yes"

# Determine if grids contain their assigned points:
ix <- rep(NA, length(mif))
for (i in 1:length(mif)) ix[i] <- !in.polygon(as.polygon(mif[[i]]$longitude, mif[[i]]$latitude), tab$longitude[i], tab$latitude[i])

# Re-generate stations that were not in their proper grids:
tmp <- scs(grid = mif[ix], alternates = 0)
tab$longitude[ix]   <- tmp$longitude
tab$latitude[ix]    <- tmp$latitude
tab$new.station[ix] <- "Yes"

# Generate alternate stations:
for (i in 1:3){
   tmp <- scs(grid = mif, alternates = 0)
   res <- data.frame(tmp$longitude, tmp$latitude)
   names(res) <- paste0(c("longitude.alt", "latitude.alt"), i)
   tab <- cbind(tab, res)
}

# Groundfish sampling stations:
k <- 100
tab$fish.sample <- "No"
tab$fish.sample[sample(1:nrow(tab), k)] <- "Yes"

vars <- c("tow.id", "longitude", "latitude", "new.station", "fish.sample",
          "longitude.alt1", "latitude.alt1", "longitude.alt2", "latitude.alt2", "longitude.alt3", "latitude.alt3")
tab <- tab[vars]

# Order tows using 2020 survey:
ix <- order(match(substr(tab$tow.id, 3, 5), substr(read.scsset(2020)$tow.id, 3, 5)))
tab <- tab[ix, ]
mif <- mif[ix]
rownames(tab) <- NULL

# Load original stations from the 2013 survey:
n <- 100  # Number of original random stations from 2013 to substitute.
y <- read.scsset(2013)
y <- y[substr(y$tow.id, 6, 6) == "F", ]
y <- y[-grep("FR", y$tow.id), ]
y <- y[-grep("A1", y$tow.id), ]

# Identify which stations will be replaced by 2013 originals:
ix <- seq(sample(1:3, 1), nrow(tab), by = 3)
ix <- ix[-sample(1:length(ix), length(ix) - n)]
tab$station <- "regular"
for (i in 1:length(ix)){
   iy <- which(in.polygon(as.polygon(mif[[ix[i]]]$longitude, mif[[ix[i]]]$latitude), longitude(y), latitude(y)))
   if (length(iy) > 1) iy <- iy[1]
   tab$station[ix[i]] <- "fixed 2013"
   tab$new.station[ix[i]] <- "Yes"
   tab[ix[i], c("longitude.alt1", "latitude.alt1", "longitude.alt2", "latitude.alt2", "longitude.alt3", "latitude.alt3")] <- NA
   if (length(iy) == 1){
      tab$longitude[ix[i]] <- longitude(y)[iy]
      tab$latitude[ix[i]]  <- latitude(y)[iy]
   }else{
      tab$longitude[ix[i]] <- NA
      tab$latitude[ix[i]]  <- NA
   }
}

# Re-order fields:
tab <- tab[c("tow.id", "longitude", "latitude", "station", setdiff(names(tab), c("tow.id", "longitude", "latitude", "station")))]
rownames(tab) <- NULL

# Fill-in missing 2013 stations:
ix  <- which(is.na(tab$longitude))
tmp <- scs(grid = mif[ix], alternates = 0)
tab$longitude[ix]   <- tmp$longitude
tab$latitude[ix]    <- tmp$latitude
tab$new.station[ix] <- "Yes"

# Map points:
clg()
map.new()
ix <- tab$station == "fixed 2013"
for (i in 1:length(mif)) polygon(mif[[i]]$longitude, mif[[i]]$latitude, lwd = 1, border = "grey60")
points(tab$longitude[-ix], tab$latitude[-ix], pch = 21, bg = "grey")
points(tab$longitude[ix], tab$latitude[ix], pch = 21, bg = "red")
map("coastline")
box()

lines(gulf$longitude, gulf$latitude)

# Determine if grids contain their assigned points:
ix <- rep(NA, length(mif))
for (i in 1:length(mif)){
   ix[i] <- !in.polygon(as.polygon(mif[[i]]$longitude, mif[[i]]$latitude), tab$longitude[i], tab$latitude[i])
   if (ix[i]) lines(mif[[i]]$longitude, mif[[i]]$latitude, col = "red", lwd = 2)
}

# Write table:
write.table(tab, file = "Snow Crab Survey Stations 2021.csv", row.names = FALSE, col.names = TRUE, sep = ",")
