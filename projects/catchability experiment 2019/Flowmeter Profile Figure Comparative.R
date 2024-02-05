
jpeg <- TRUE

x <- read.flowmeter(year = 2019)

tows <- unique(x$tow.id)

stations <- unique(substr(x$tow.id, 3, 5))

# Read revised start-end times:
res <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", 2019, ".csv"), stringsAsFactors = FALSE)
touchdown.tilt <- res$touchdown
names(touchdown.tilt) <- res$tow.id
liftoff.tilt <- res$liftoff
names(liftoff.tilt) <- res$tow.id
 
# Read set data and import revised times:
s <- read.scset(year = 2019, valid = 1)   
s <- s[substr(s$tow.id, 3, 5) %in% substr(s$tow.id[substr(s$tow.id, 2, 2) == "C"], 3, 5), ]
s$vessel <- "Avalon Voyager II"
s$vessel[substr(s$tow.id, 2, 2) == "C"] <- "Jean Mathieu"
s$captain <- "Ghislain Bourgeois"
s$captain[(s$day < 24) & (s$vessel == "Jean Mathieu")] <- "Denis Eloquin"  
s$captain[(s$day >= 24) & (s$vessel == "Avalon Voyager II")] <- "Denis Eloquin" 
s$start.time <- touchdown.tilt[s$tow.id] 
s$liftoff.time <- liftoff.tilt[s$tow.id]   
s$end.time <- s$end.time.logbook
         
s <- s[s$tow.id %in% tows, ]

      
smooth <- function(y, n = 3){
   v <- rep(NA, length(y))
   for (i in (n+1):(length(y)-n+1)) v[i] <- mean(y[(i-n):(i+n)])
   return(v)
}

clg()
if (jpeg){
   jpeg(file = "U:/Snow Crab/Stock Assessment 2019/Comparative Analyses/Flowmeter Comparative.jpg", width = 8.5*480, height = 11*480, res = 8*75)
}else{
   windows(height = 11, width = 8.5)
}


m <- kronecker(matrix(1:12, ncol = 2), matrix(1, nrow = 4, ncol = 7))
m <- rbind(0, cbind(0, 0, m, 0, 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))

for (i in 1:12){#nrow(s)){
   yp <- x[substr(x$tow.id, 3, 5) %in% stations[i] & substr(x$tow.id, 2, 2) == "P", ]
   yc <- x[substr(x$tow.id, 3, 5) %in% stations[i] & substr(x$tow.id, 2, 2) == "C", ]
 
   plot(c(0, 400), c(0, 2.5), type = "n", xaxs = "i", yaxs = "i", xaxt = "n", yaxt = "n")
   grid()
   if (nrow(yp) > 0){
      start.time <- 0
      ii <- which(s$tow.id == unique(yp$tow.id))
      end.time <-  time2sec(end.time(s[ii, ]), start.time(s[ii, ]))
      liftoff.time <- time2sec(time.default(paste0(as.character(date(s[ii, ])), " ", s$liftoff[ii], " AST")), start.time(s[ii, ]))

      yp$time <- time2sec(time(yp), start.time(s[ii, ]))
      yp <- yp[yp$time >= 0 & yp$time <= liftoff.time, ]
      speed <- smooth(yp$"Speed.(m/s)", 5)
    
      index <- yp$time >= 0 & yp$time <= end.time
      lines(yp$time[index], speed[index], lwd = 2, col = "lightskyblue1")

      index <- yp$time >= end.time & yp$time <= liftoff.time
      lines(yp$time[index], speed[index], lwd = 2, col = "blue")  
   } 
   
   if (nrow(yc) > 0){
      start.time <- 0
      ii <- which(s$tow.id == unique(yc$tow.id))
      end.time <-  time2sec(end.time(s[ii, ]), start.time(s[ii, ]))
      liftoff.time <- time2sec(time.default(paste0(as.character(date(s[ii, ])), " ", s$liftoff[ii], " AST")), start.time(s[ii, ]))

      yc$time <- time2sec(time(yc), start.time(s[ii, ]))
      yc <- yc[yc$time >= 0 & yc$time <= liftoff.time, ]
      speed <- smooth(yc$"Speed.(m/s)", 5)
    
      index <- yc$time >= 0 & yc$time <= end.time
      lines(yc$time[index], speed[index], lwd = 2, col = "lightpink1")

      index <- yc$time >= end.time & yc$time <= liftoff.time
      lines(yc$time[index], speed[index], lwd = 2, col = "red")  
   }  
   
   text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]), par("usr")[3] + 0.90 * diff(par("usr")[3:4]), stations[i], cex = 1.5)
   
   if (i == 1) axis(2, at = seq(0, 2.5, by = 0.5))  
   if (i %in% 2:6) axis(2, at = seq(0, 2.0, by = 0.5))
   
   if (i == 6)  axis(1, at = 100*(1:3))
   if (i == 12) axis(1)
   if (i == 3)  mtext("Current speed (m/s)", 2, 2.5, cex = 1.4, at = 0)
   if (i == 12)  mtext("Time from touchdown (s)", 1, 3.5, cex = 1.3, at = 0)
   box()
}

if (jpeg) dev.off()  

 