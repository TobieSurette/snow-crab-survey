library(gulf.data)
library(gulf.spatial)

years <- setdiff(1988:2020, NULL)

radius <- 0.75
jpeg <- TRUE

data <- read.scsset(year = years, valid = 1, survey = "regular")
data$station <- station(lon(data), lat(data), distance.tol = radius*2)

M <- matrix(NA, nrow = max(data$station), ncol = length(years))
rownames(M) <- 1:max(data$station)
colnames(M) <- years

for (i in 1:length(years)){
   t <- table(data$station[year(data) == years[i]])
   M[names(t), i] <- 1
}

# Survey station history figure:
pdf(file = "results/figures/Survey Station History.pdf", height = 7, width = 7)
image(years, as.numeric(rownames(M)), z = t(M), xlab = "", ylab = "", ylim = c(1, 2000), yaxs = "i", col = "grey30")
grid()
mtext("Year", 1, 2.5, cex = 1.5)
mtext("Station number", 2, 2.5, cex = 1.5)
box()
text(2011.9, 1330, "Survey redesign 2012 -", pos = 2, cex = 0.75)
text(2012.9, 1650, "Survey redesign 2013 -", pos = 2, cex = 0.75)
text(2005.9, 1135, "Partial redistribution 2006 -", pos = 2, cex = 0.75)
dev.off()

# Where the bad tows occurred during random shuffle years:
if (jpeg){
   jpeg(file = "U:/Snow Crab/Stock Assessment 2019/Map of tow success.jpg", width = 8 * 480, height = 8 * 480, res = 8 * 75)
}else{
   windows()
}
gulf.map(sea = TRUE, land = FALSE)
points(longitude(bad), latitude(bad), pch  = 21, bg = "red", cex = 0.5, col = "red")
points(longitude(data), latitude(data), pch  = 21, bg = "black", cex = 0.25, col = "black")
legend("topright", legend = c("Successful", "Rejected"), bg = "white", pch = 21, pt.bg = c("black", "red"), col = c("black", "red"), pt.cex = 1.5, cex = 1.1)
map.fishing.zones(species = 2526)
coastline(col = "grey80")
if (jpeg) dev.off()

# Proportion of survey trawlable area:
data <- read.scset(year = 2007:2019)
data <- data[substr(data$tow.id, 2, 2) != "C", ]
data <- data[!((data$year > 2000) & (data$month == 6)), ]

index <- substr(data$tow.id, 6, 10) == "F"
t <- table(data$year[index], data$valid[index])

if (jpeg){
   jpeg(file = "U:/Snow Crab/Stock Assessment 2019/Proportion Successful Tows.jpg", width = 8 * 480, height = 8 * 480, res = 8 * 75)
}else{
   windows()
}
p <- t[, 1] / apply(t, 1, sum)
plot(c(2006.5, 2019.5), c(0, 0.25), type = "n", xlab = "", ylab = "", xaxs = "i", yaxs = "i")
grid()
dbarplot(p, width = 1, col = "grey90", add = TRUE)
p[!(names(p) %in% as.character(2012:2013))] <- 0
dbarplot(p, width = 1, add = TRUE, col = "grey70")

mtext("Year", 1, 2.5, cex = 1.4)
mtext("Probability", 2, 2.5, cex = 1.4)
legend("topleft", legend = c("Fixed & new stations", "All new stations"), pch = 22, pt.cex = 3, pt.bg = c("grey90", "grey70"))

if (jpeg) dev.off()

# Calculate number of times a station has moved:
years <- 2013:2019
data <- read.scset(year = years)
data <- data[substr(data$tow.id, 2, 2) != "C", ]

data$tow.id <- gsub("A1R", "A1", data$tow.id)
data$tow.id <- gsub("FA1", "A1", data$tow.id)

M <- matrix(NA, nrow = 355, ncol = length(years))
rownames(M) <- 1:355
colnames(M) <- years
for (i in 1:length(years)){
    x <- data[data$year == years[i] & data$valid == 1, ]
    x$order <- NA
    x$order[substr(x$tow.id, 6, 10) == "F"] <- 0
    x$order[substr(x$tow.id, 6, 10) == "FR"] <- 0
    x$order[substr(x$tow.id, 6, 10) == "A1"] <- 1
    x$order[substr(x$tow.id, 6, 10) == "A2"] <- 2
    x$order[substr(x$tow.id, 6, 10) == "A3"] <- 3
    index <- as.numeric(substr(x$tow.id, 3, 5))
    print(sort(index))
    M[index, i] <- x$order

    y <- data[data$year == years[i] & as.numeric(substr(data$tow.id, 3, 5)) %in% which(is.na(M[,i])), ]
    y <- y[substr(y$tow.id, 6, 10) != "FR", ]
    t <- table(substr(y$tow.id, 3, 5))
    M[as.numeric(names(t)), i] <- t-1
}
n <- apply(M, 1, sum)
dbarplot(table(n), width = 1)
mtext("Number of station changes", 1, 2.5, cex = 1.4)
mtext("Frequency", 2, 2.5, cex = 1.4)

# Plot survey grids:
# Read grid scheme from previous year:
mif <- read.mif("U:/Snow Crab/Trawl Survey/2017/Generate Survey Stations 2017/grids2017.mif", mid = FALSE)
for (i in 1:length(mif)){
   tmp <- km2deg(mif[[i]]$x, mif[[i]]$y)
   mif[[i]]$longitude <- tmp$longitude
   mif[[i]]$latitude <- tmp$latitude
}

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
for (i in 1:length(mif)) mif[[i]]$tow.id <- substr(tow.id[i], 3, 5)
mif <- mif[order(as.numeric(substr(tow.id, 3, 5)))]

if (jpeg){
   jpeg(file = "U:/Snow Crab/Stock Assessment 2019/Map of relocated stations.jpg", width = 8 * 480, height = 8 * 480, res = 8 * 75)
}else{
   windows()
}
gulf.map(sea = TRUE)
for (i in 1:length(mif)){
   x <- mif[[i]]
   polygon(x$longitude, x$latitude, col = grey(1-n[i]/max(n)))
   if (n[i] > 7) col <- "white" else col <- "black"
   text(mean(x$longitude[1:4]), mean(x$latitude[1:4]), n[i], cex = 0.5, col = col)
}

if (jpeg) dev.off()

