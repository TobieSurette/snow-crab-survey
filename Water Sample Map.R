

x <- read.table("U:/Snow Crab/Survey 2018/Survey Station Generation/Snow Crab Survey Stations 2018.csv", sep = ",", header = TRUE)

xx <- x[x$water.sample == "Yes", ]

gulf.map(sea = TRUE, land = FALSE, xlim = c(-62.00, -60),  ylim = c(45.90, 48))
map.place.names(sea = FALSE)

points(x$longitude, x$latitude, pch = 21, bg = "red", cex = 1.5)
map.fishing.zones(species = 2526)
coastline(col = "grey80", border = "grey50")

text(x$longitude, x$latitude, x$tow.id, pos = 3, cex = 0.75)
box()

index <- xx$tow.id %in% c("GP304F", "GP305F", "GP318F")

text(xx$longitude[index], xx$latitude[index], xx$tow.id[index], pos = 3, cex = 0.8)

dir("W:/Crab/Offshore Crab Common/Fishing Year 2017/Trawl Data/South Western Gulf/Stations/")


