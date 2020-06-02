rm(list=ls())
graphics.off()
library("gulf")

#source("C:/R packages/gulf/R/fmt.scset.R")
#source("C:/R packages/gulf/R/oracle.read.scset.R")
#source("C:/R packages/gulf/R/select.sc.statio ns.R")
#source("C:/R packages/gulf/R/grid.str.R")
#source("C:/R packages/gulf/R/DFOgrid.str.R")

setwd("U:/Snow Crab/Survey 2018/Survey Station Generation")

tab <- read.csv("MIF table.csv", header = TRUE)
tab$tow.id <- substr(tab$tow.id, 3, 5)

# Load survey polygon:
data(survey.polygons)
p <- subset(p, label = "sc.survey")
poly <- data.frame(longitude = p[[1]]$x, latitude = p[[1]]$y)
      
# Load last year's stations:
stations <- read.scset(year = 2018, valid = 1)

# Check that grid identifiers are unique:
if (any(duplicated(stations$tow.id))) print("There are duplicate tow ID numbers.")

# Read grid scheme from previous year:
mif <- read.mif("grids2017.mif", mid = FALSE)
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp$longitude
   mif[[i]]$latitude <- tmp$latitude
}
n <- length(mif) # Number of stations, which defines the sampling grid scheme.]

tow.id <- c('GP002','GP001','GP004','GP003','GP006','GP005','GP008','GP007','GP009','GP011','GP010','GP012','GP013','GP016','GP019','GP018','GP015','GP017',
            'GP014','GP026','GP024','GP025','GP023','GP028','GP022','GP030','GP031','GP021','GP020','GP027','GP036','GP044','GP032','GP040','GP042','GP041',
            'GP039','GP043','GP037','GP029','GP034','GP033','GP038','GP035','GP049','GP046','GP048','GP050','GP047','GP052','GP051','GP045','GP053','GP055',
            'GP058','GP059','GP057','GP056','GP054','GP077','GP068','GP061','GP076','GP067','GP066','GP070','GP063','GP069','GP062','GP060','GP071','GP073',
            'GP065','GP074','GP064','GP072','GP094','GP084','GP088','GP080','GP090','GP079','GP091','GP093','GP095','GP092','GP087','GP082','GP075','GP089',
            'GP086','GP083','GP078','GP085','GP081','GP099','GP113','GP106','GP105','GP097','GP104','GP108','GP102','GP111','GP110','GP100','GP101','GP114',
            'GP103','GP112','GP096','GP107','GP109','GP098','GP133','GP128','GP123','GP127','GP125','GP124','GP116','GP117','GP132','GP119','GP131','GP135',
            'GP129','GP118','GP126','GP130','GP115','GP134','GP121','GP122','GP120','GP154','GP141','GP147','GP149','GP150','GP151','GP146','GP140','GP148',
            'GP137','GP145','GP136','GP144','GP143','GP153','GP152','GP138','GP139','GP142','GP162','GP170','GP159','GP158','GP163','GP164','GP161','GP160',
            'GP167','GP171','GP155','GP168','GP172','GP157','GP169','GP165','GP166','GP156','GP189','GP174','GP184','GP176','GP179','GP177','GP188','GP182',
            'GP186','GP173','GP175','GP178','GP183','GP185','GP180','GP181','GP187','GP192','GP197','GP202','GP198','GP191','GP190','GP196','GP199','GP194',
            'GP205','GP195','GP193','GP204','GP200','GP201','GP203','GP214','GP220','GP206','GP215','GP210','GP221','GP212','GP219','GP211','GP213','GP216',
            'GP209','GP218','GP217','GP207','GP208','GP231','GP234','GP226','GP233','GP229','GP225','GP232','GP222','GP224','GP228','GP223','GP236','GP230',
            'GP227','GP237','GP235','GP238','GP241','GP240','GP248','GP245','GP246','GP244','GP239','GP242','GP247','GP243','GP249','GP251','GP250','GP262',
            'GP264','GP254','GP256','GP265','GP259','GP263','GP252','GP255','GP253','GP258','GP257','GP261','GP260','GP269','GP271','GP270','GP266','GP276',
            'GP267','GP272','GP274','GP268','GP275','GP279','GP281','GP278','GP273','GP282','GP277','GP284','GP280','GP289','GP286','GP283','GP288','GP291',
            'GP285','GP287','GP290','GP293','GP294','GP301','GP298','GP292','GP299','GP302','GP296','GP297','GP300','GP303','GP295','GP305','GP306','GP304',
            'GP309','GP307','GP311','GP308','GP310','GP315','GP313','GP312','GP316','GP319','GP318','GP322','GP327','GP317','GP314','GP321','GP328','GP323',
            'GP325','GP326','GP324','GP320','GP330','GP336','GP332','GP329','GP333','GP337','GP338','GP334','GP335','GP331','GP340','GP339','GP343','GP345',
            'GP346','GP342','GP344','GP341','GP349','GP348','GP347','GP351','GP350','GP352','GP355','GP354','GP353')
for (i in 1:length(mif)) mif[[i]]$tow.id <- tow.id[i]

# Update MID file:
#file <- "U:/Snow Crab/Trawl Survey/2017/Generate Survey Stations 2017/grids2017.mid"
#for (i in 1:length(mif)){
#  if (i == 1) append <- FALSE else append <- TRUE
#  cat(paste0('FALSE,"', mif[[i]]$tow.id, '",', mean(mif[[i]]$longitude[1:4]), ',', mean(mif[[i]]$latitude[1:4]), '\n'), file = file, append = append)
#}

mif.tab <- data.frame(grid = 1:length(mif), tow.id = unlist(lapply(mif, function(x) x$tow.id)))
mif.tab$longitude <- NA
mif.tab$latitude <- NA
# Get complete survey grid scheme:
for (i in 1:length(mif)){   
   # Assign survey stations to grids:
   p <- as.polygon(mif[[i]]$longitude, mif[[i]]$latitude)
   index <- which(in.polygon(p, longitude(stations), latitude(stations)))
   if (length(index) > 0){
      gpnum <- substr(mif[[i]]$tow.id, 3, 5)
      ii <- which(substr(stations$tow.id, 3, 5) == gpnum)
      mif.tab$longitude[i] <- longitude(stations)[ii]
      mif.tab$latitude[i]  <- latitude(stations)[ii] 
   }
}
# Identify which stations lie outside the survey polygon:
index <- in.polygon(as.polygon(poly$longitude, poly$latitude), mif.tab$longitude, mif.tab$latitude)
mif.tab$longitude[!index] <- NA
mif.tab$latitude[!index] <- NA
index <- is.na(mif.tab$longitude)
for (i in 1:length(mif)) mif[[i]]$label <- mif[[i]]$tow.id
tmp <- select.sc.stations(grid = mif[index], alternates = 0)
mif.tab$longitude[index] <- tmp$longitude
mif.tab$latitude[index] <- tmp$latitude
mif.tab$new.station <- "No"
mif.tab$new.station[index] <- "Yes"

# Generate alternates:
for (i in 1:3){
   tmp <- select.sc.stations(grid = mif, alternates = 0)
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

write.table(mif.tab, file = "Snow Crab Survey Stations 2019.csv", row.names = FALSE, col.names = TRUE, sep = ",")

