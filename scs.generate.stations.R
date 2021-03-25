library("gulf.data")
library("gulf.spatial")

# Load last year's stations:
stations <- read.scsset(year = 2019, valid = 1)
stations <- stations[substr(stations$tow.id,2,2) != "C", ]

# Read grid scheme from previous year:
mif <- read.mif(locate(package = "gulf.spatial", file = "scs.grids.mif"))
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp$longitude
   mif[[i]]$latitude <- tmp$latitude
}

# Load survey polygon:
gulf <- read.csv(locate(package = "gulf.spatial", file = "scs.bounds"), header = TRUE, stringsAsFactors = FALSE)


tab <- data.frame(tow.id = unlist(lapply(mif, function(x) x$tow.id)))
tab$longitude <- NA
tab$latitude <- NA
# Get complete survey grid scheme:
for (i in 1:length(mif)){
   # Assign survey stations to grids:
   p <- as.polygon(mif[[i]]$longitude, mif[[i]]$latitude)
   index <- which(in.polygon(p, -abs(lon(stations)), lat(stations)))
   if (length(index) > 0){
      ii <- which(as.numeric(substr(stations$tow.id, 3, 5)) == mif[[i]]$tow.id)
      tab$longitude[i] <- -abs(lon(stations)[ii])
      tab$latitude[i]  <- lat(stations)[ii]
   }
}

# Identify which stations lie outside the survey polygon:
index <- in.polygon(as.polygon(gulf$longitude, gulf$latitude), tab$longitude, tab$latitude)
tab$longitude[!index] <- NA
tab$latitude[!index] <- NA
index <- is.na(tab$longitude)
for (i in 1:length(mif)) mif[[i]]$label <- mif[[i]]$tow.id
tmp <- scs.stations(grid = mif[index], alternates = 0)
tab$longitude[index] <- tmp$longitude
tab$latitude[index] <- tmp$latitude
tab$new.station <- "No"
tab$new.station[index] <- "Yes"

# Generate alternates:
for (i in 1:3){
   tmp <- scs.stations(grid = mif, alternates = 0)
   res <- data.frame(tmp$longitude, tmp$latitude)
   names(res) <- paste0(c("longitude.alt", "latitude.alt"), i)
   mif.tab <- cbind(mif.tab, res)
}

# Determine fishing zone:
mif.tab$zone <- fishing.zone(mif.tab$longitude, mif.tab$latitude, species = 2526)

# Groundfish sampling stations:
k <- 100
mif.tab$fish.sample <- "No"
mif.tab$fish.sample[sample(1:nrow(mif.tab), k)] <- "Yes"

clg()
gulf.map()
x <- mif.tab
for (i in 1:length(mif)) polygon(mif[[i]]$longitude, mif[[i]]$latitude, lwd = 2, col = NA)
points(x$longitude, x$latitude, pch = 21, bg = "red")
index <- (x$new.station == "Yes") # stations$tow.id %in%
points(stations$longitude[!index], stations$latitude[!index], pch = 21, bg = "green")
polygon(poly$longitude, poly$latitude, border = "red", lwd = 2)
points(x$longitude[x$water.sample], x$latitude[x$water.sample], pch = 22, bg = "purple", cex = 1.2)
points(x$longitude[x$fish.sample == "Yes"], x$latitude[x$fish.sample == "Yes"], pch = 22, bg = "orange", cex = 1.5)

data(fishing.zone.polygons)
p <- fishing.zone.polygons
p <- subset(p, species = 2526, region = "gulf")
#plot.polygon(as.polygon(p), border = "red", lwd = 2)
for (i in 1:length(p)){
   for (j in 1:length(p[[i]])){
      if ("x" %in% names(p[[i]][[j]])) lines(p[[i]][[j]]$x, p[[i]][[j]]$y, col = "blue", lwd = 2)
   }
}
index <- is.na(mif.tab$zone)
points(mif.tab$longitude[index], mif.tab$latitude[index], pch = 21, bg = "yellow", cex = 2)

vars <- c("tow.id", "longitude", "latitude", "new.station", "zone", "fish.sample",
           "longitude.alt1", "latitude.alt1", "longitude.alt2", "latitude.alt2", "longitude.alt3", "latitude.alt3")
mif.tab <- mif.tab[vars]

# Minor 'tow.id' fixes:
mif.tab$tow.id <- paste0(substr(mif.tab$tow.id, 1, 5), "F")

mif.tab <- sort(mif.tab, by = "tow.id")
rownames(mif.tab) <- NULL

write.table(mif.tab, file = "Snow Crab Survey Stations 2020.csv", row.names = FALSE, col.names = TRUE, sep = ",")
