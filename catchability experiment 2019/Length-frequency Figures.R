library(gulf)

jpeg <- TRUE
sex <- 2
confidence.intervals <- FALSE
species <- c(10, 40, 42)

s <- read.scset(year = 2019, valid = 1)

s <- summary(s, category = c("COM", "MM", "MI", "FM", "FI"))

jm <- s[substr(s$tow.id, 2, 2) == "C", ]
av <- s[substr(s$tow.id, 3, 5) %in%  substr(jm$tow.id, 3, 5) & substr(s$tow.id, 2, 2) != "C", ]

dist <- distance(longitude(jm), latitude(jm), longitude(av), latitude(av), pairwise = FALSE)
names(dist) <- substr(jm$tow.id, 3, 5)
#Touchdown and Liftoff - Tilt 2019.csv
z <- read.csv("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/GP Swept Area - Tilt 2019.csv")
jm$swept.area <- z$swept.area[match(jm$tow.id, z$tow.id)]
jm$swept.area.sd <- z$swept.area.sd[match(jm$tow.id, z$tow.id)]
jm$swept.area.passive <- z$swept.area.passive[match(jm$tow.id, z$tow.id)]
jm$swept.area.passive.sd <- z$swept.area.passive.sd[match(jm$tow.id, z$tow.id)]
av$swept.area <- z$swept.area[match(av$tow.id, z$tow.id)]
av$swept.area.sd <- z$swept.area.sd[match(av$tow.id, z$tow.id)]
av$swept.area.passive <- z$swept.area.passive[match(av$tow.id, z$tow.id)]
av$swept.area.passive.sd <- z$swept.area.passive.sd[match(av$tow.id, z$tow.id)]

# Define set of species to be collated:
#species <- c(10, 40, 42, 201, 304, 340, 1823, 2521, 2527, 4210, 6115, 6121, 6123, 6400)
#name.str <- c("cod", "plaice", "yellowtail", "thorny", "mailed.sculpin", "alligatorfish", "sea.potato", "coarctatus", 
#              "araneus", "whelk", "mudstar", "purple.sunstar", "spiny.sunstar", "sea.urchin")

# Load by-catch data:
cat <- read.csv("W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-catch/SCS2019C.csv", header = TRUE, stringsAsFactors = FALSE)
cat <- cat[(cat$tow.id %in% av$tow.id) | (cat$tow.id %in% jm$tow.id), ]

# Add missing count data from length-frequency data:
len <- read.csv("W:/Crab/Offshore Crab Common/Fishing Year 2019/Trawl Data/South Western Gulf/By-catch/SCS2019L.csv", header = TRUE, stringsAsFactors = FALSE)
len <- len[(len$tow.id %in% av$tow.id) | (len$tow.id %in% jm$tow.id), ]

b <- read.scbio(year = 2019)
b$cw <- round(b$carapace.width)
b$maturity <- is.mature(b, prob = TRUE)
b <- b[!is.na(b$maturity), ]
b$maturity <- as.numeric(b$maturity >= 0.5)
sum(match(b$tow.id,  read.scset(year = 2019)$tow.id))


res <- freq(b[b$tow.id %in% av$tow.id | b$tow.id %in% jm$tow.id,], by = c("sex", "maturity", "tow.id"))
res <- res[-nrow(res), ]

fvars <- as.character(0:141)

av[fvars] <- 0
jm[fvars] <- 0
avi <- av
avm <- av
jmi <- jm
jmm <- jm

tmp <- res[substr(res$tow.id,2,2) == "P" &  res$sex == sex & res$maturity == 0, ]
avi[match(tmp$tow.id, av$tow.id), fvars] <- tmp[fvars] 
tmp <- res[substr(res$tow.id,2,2) == "P" &  res$sex == sex & res$maturity == 1, ]
avm[match(tmp$tow.id, av$tow.id), fvars] <- tmp[fvars] 

tmp <- res[substr(res$tow.id,2,2) == "C" &  res$sex == sex & res$maturity == 0, ]
jmi[match(tmp$tow.id, jm$tow.id), fvars] <- tmp[fvars] 
tmp <- res[substr(res$tow.id,2,2) == "C" &  res$sex == sex & res$maturity == 1, ]
jmm[match(tmp$tow.id, jm$tow.id), fvars] <- tmp[fvars] 

res <- cbind(apply(1000000 * jmi[fvars] / repvec(jm$swept.area, ncol = length(fvars)), 2, mean),
             apply(1000000 * jmm[fvars] / repvec(jm$swept.area, ncol = length(fvars)), 2, mean),
             apply(1000000 * avi[fvars] / repvec(av$swept.area, ncol = length(fvars)), 2, mean),
             apply(1000000 * avm[fvars] / repvec(av$swept.area, ncol = length(fvars)), 2, mean))
      
cols <- c("grey30", "grey80")
if (sex == 1) sex.str <- "Male" else sex.str <- "Female"
if (jpeg){
   jpeg(paste0("U:/Snow Crab/Stock Assessment 2019/Comparative Analyses/Length Frequency ", sex.str, ".jpg"), width = 8 * 480, height = 8 * 480, res = 8 * 75) 
}else{ 
   windows()
}
m <- rbind(0, cbind(0, kronecker(matrix(1:2, ncol = 1), matrix(1, nrow = 4, ncol = 6)), 0), 0)
layout(m)
par(mar = c(0,0,0,0))
if (sex == 1) xlim <- c(0, 140) else xlim <- c(0, 90)

# Jean Mathieu:
plot(xlim, c(0, 1.1*max(apply(res[, 1:2],1,sum), apply(res[, 3:4],1,sum))), type = "n", xaxs = "i", xlab = "", xaxt = "n", yaxs = "i")
grid()
dbarplot(res[, 1:2], as.numeric(rownames(res)), add = TRUE, width = 1, legend = FALSE, col = cols, border = "grey50")
mtext(expression(paste("Density (#/km"^"2", ")")), 2, 2.5, at = 0, cex = 1.25) 
if (confidence.intervals){
   mu <- apply(1000000 * (jmi[fvars] + jmm[fvars]) / repvec(jm$swept.area, ncol = length(fvars)) , 2, mean)
   sigma <- apply(1000000 * (jmi[fvars] + jmm[fvars]) / repvec(jm$swept.area, ncol = length(fvars)) , 2, sd)
   lci <- mu - 1.96 * sigma / sqrt(40)
   uci <- mu + 1.96 * sigma / sqrt(40)
   #lines(as.numeric(fvars), mu, col = "red", lwd = 2)
   lines(as.numeric(fvars), lci, col = "red", lwd = 1, lty = "dashed")
   lines(as.numeric(fvars), uci, col = "red", lwd = 1, lty = "dashed")
}

if (sex == 1) lines(c(95, 95), par("usr")[3:4], lwd = 1, lty = "dashed", col = "red")
box()
legend("topleft", legend = c("Immature", "Mature"), pch = 22, pt.bg = cols, pt.cex = 3, cex = 1.5, bg = "white")
text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]), 
     par("usr")[3] + 0.94 * diff(par("usr")[3:4]), "Jean Mathieu", cex = 1.5)
    
# Avalon:
plot(xlim, c(0, 1.1*max(apply(res[, 1:2],1,sum), apply(res[, 3:4],1,sum))), type = "n", xaxs = "i", xlab = "", xaxt = "n", yaxs = "i")
grid()
dbarplot(res[, 3:4], as.numeric(rownames(res)), add = TRUE, width = 1, legend = FALSE, col = cols, border = "grey50")
if (confidence.intervals){
   mu <- apply(1000000 * (avi[fvars] + avm[fvars]) / repvec(av$swept.area, ncol = length(fvars)) , 2, mean)
   sigma <- apply(1000000 * (avi[fvars] + avm[fvars]) / repvec(av$swept.area, ncol = length(fvars)) , 2, sd)
   lci <- mu - 1.96 * sigma / sqrt(40)
   uci <- mu + 1.96 * sigma / sqrt(40)
   #lines(as.numeric(fvars), mu, col = "red", lwd = 2)
   lines(as.numeric(fvars), lci, col = "red", lwd = 1, lty = "dashed")
   lines(as.numeric(fvars), uci, col = "red", lwd = 1, lty = "dashed")
}
axis(1)
mtext("Carapace width (mm)", 1, 2.25, cex = 1.25)
text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]), 
     par("usr")[3] + 0.94 * diff(par("usr")[3:4]), "Avalon Voyager II", cex = 1.5)
if (sex == 1) lines(c(95, 95), par("usr")[3:4], lwd = 1, lty = "dashed", col = "red")
box()     

if (jpeg) dev.off()

len <- len[len$species %in% species, ]
len$length[len$length.unit == "cm"] <- round(10*(len$length[len$length.unit == "cm"] + runif(sum(len$length.unit == "cm"))))

av <- av[setdiff(names(av), fvars)]
jm <- jm[setdiff(names(jm), fvars)]


clg()
for (i in 1:length(species)){
   avt <- av
   jmt <- jm
   res <- freq.default(round(len[len$species == species[i], "length"]/5)*5, by = len[len$species == species[i], "tow.id", drop = FALSE])
   fvars <- setdiff(names(res), "tow.id")
   
   jmt[fvars] <- 0
   index <- match(jmt$tow.id, res$tow.id)
   jmt[which(!is.na(index)), fvars] <- res[index[which(!is.na(index))], fvars]
   
   avt[fvars] <- 0
   index <- match(avt$tow.id, res$tow.id)
   avt[which(!is.na(index)), fvars] <- res[index[which(!is.na(index))], fvars]   
   
   tmp <- cbind(apply(1000000 * jmt[fvars] / repvec(jm$swept.area, ncol = length(fvars)), 2, mean),
                apply(1000000 * avt[fvars] / repvec(av$swept.area, ncol = length(fvars)), 2, mean))
   


   if (species[i] == 10){ xlim <- c(0, 50); v <- seq(0, 1200, by = 200); ylim <- c(0, 1200)}
   if (species[i] == 40){ xlim <- c(0, 40); v <- seq(0, 1400, by = 200); ylim <- c(0, 1400)}
   if (species[i] == 42){ xlim <- c(0, 30); v <- seq(0, 400, by = 100); ylim <- c(0, 400) }
   
   if (jpeg){
      jpeg(paste0("U:/Snow Crab/Stock Assessment 2019/Comparative Analyses/Length Frequency ", species.str(species[i], source = "gulf"), ".jpg"), width = 8 * 480, height = 8 * 480, res = 8 * 75) 
   }else{ 
      windows()
   }

   m <- rbind(0, cbind(0, kronecker(matrix(1:2, ncol = 1), matrix(1, nrow = 4, ncol = 6)), 0), 0)
   layout(m)
   par(mar = c(0,0,0,0))

   plot(xlim, ylim, type = "n", xaxs = "i", xlab = "", xaxt = "n", yaxs = "i", yaxt = "n")

   axis(2, at = v)
   grid()
   dbarplot(tmp[, 1], as.numeric(rownames(tmp)) / 10, add = TRUE, width = 1, legend = FALSE, col = cols, border = "grey50")          
   text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]), 
        par("usr")[3] + 0.94 * diff(par("usr")[3:4]), "Jean Mathieu", cex = 1.5)
   mtext(expression(paste("Density (#/km"^"2", ")")), 2, 2.5, at = 0, cex = 1.25) 
   box()
     
   plot(xlim, ylim, type = "n", xaxs = "i", xlab = "", xaxt = "n", yaxs = "i", yaxt = "n")
   grid()
   dbarplot(tmp[, 2], as.numeric(rownames(tmp)) / 10, add = TRUE, width = 1, legend = FALSE, col = cols, border = "grey50")  
   axis(1)
   axis(2, at = v[-length(v)])
   text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]), 
        par("usr")[3] + 0.94 * diff(par("usr")[3:4]), "Avalon Voyager II", cex = 1.5)   
   box()
   mtext("Length (cm)", 1, 2.75, cex = 1.25)
   
   if (jpeg) dev.off()
}


