library(gulf)
library(mgcv)

years <- 2012:2013

m <- kronecker(matrix(1:length(years)), matrix(1, nrow = 4, ncol = 6))
m <- rbind(0, cbind(0, 0, m, 0), 0, 0)

clg()
windows(width = 5, height = 8.5)
layout(m)
par(mar = c(0,0,0,0))
# Plot vessel tracks by the origin:
angles <- rep(NA, nrow(s))
for (j in 1:length(years)){
   # Read revised start-end times:
   res <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Inits ", years[j], ".csv"), stringsAsFactors = FALSE)
   touchdown.tilt <- res$touchdown
   names(touchdown.tilt) <- res$tow.id
   liftoff.tilt <- res$liftoff
   names(liftoff.tilt) <- res$tow.id
 
   # Read set data and import revised times:
   s <- read.scset(year = years[j], valid = 1)
   s <- s[substr(s$tow.id, 2, 2) != "C", ]
   s$start.time <- touchdown.tilt[s$tow.id] 
   s$liftoff.time <- liftoff.tilt[s$tow.id] 
   plot(c(-500, 500), c(-500, 500), xlab = "", ylab = "", type = "n", xaxt = "n")
   grid()
   
   if (years[j] == 2012){
      s <- s[!(s$tow.id %in% c("GP009FR")), ]
   }
   
   s <- s[s$start.time != "        " & s$liftoff.time != "        " , ]
   for (i in 1:nrow(s)){
      cat(paste0(i, ") ", s$tow.id[i], "\n"))
      if (years[j] <= 2012){
         file <- netmind.file.str(year = years[j], tow.id = s$tow.id[i]) 
         n <- read.netmind(file, offset = 0)
         n$doormaster <- n$doorspread
         e <- n 
         if (!is.null(e)){
            e$time <- time2min(time(e), start.time(s[i, ]))
            e$time <- e$time - 180
         }
      }else{
         e <- read.esonar(year = years[j], tow.id = s$tow.id[i])
         if (!is.null(e)) e$time <- time2min(time(e), start.time(s[i, ]))
      }
      
      if (!is.null(e)){
         if (max(diff(e$time)) <= 5){
            end.time <- time2min(end.time(s[i, ]), start.time(s[i, ]))
            liftoff.time <- time2min(time.default(paste0(as.character(date(s[i, ])), " ", s$liftoff[i], " AST")), start.time(s[i, ]))
       
            # Plot trawling time:
            index <- which(e$time >= 0 & e$time <= end.time)
            if (length(index) > 0){
               tmp <- deg2km(e$longitude[index], e$latitude[index])
               origin <- c(tmp$x[1], tmp$y[1])
               tmp$x <- (tmp$x-origin[1])*1000
               tmp$y <- (tmp$y-origin[2])*1000
               lines(tmp$x, tmp$y, col = "grey50")
            }
      
            # Plot extra-bottom time:
            index <- which(e$time >= end.time & e$time <= liftoff.time)
            if (length(index) > 0){
               tmp <- deg2km(e$longitude[index], e$latitude[index])
               tmp$x <- (tmp$x-origin[1])*1000
               tmp$y <- (tmp$y-origin[2])*1000
               lines(tmp$x, tmp$y, col = "red") 
               #text(mean(tmp$x), mean(tmp$y), i)  
            }
         }
      }
   }
   points(0, 0, pch = 21, bg = "grey", cex = 1.25)
   if (j == 2) mtext("y (meters)", 2, 2.9, cex = 1.5)   
   text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]), par("usr")[3] + 0.93 * diff(par("usr")[3:4]), years[j], cex = 1.5)
}
mtext("x (meters)", 1, 3, cex = 1.5)
axis(1)