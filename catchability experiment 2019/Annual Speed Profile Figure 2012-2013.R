library(gulf)

source("C:/gulf package/gulf/R/liftoff.scset.R")
years <- 2012:2013 

clg()
xlim <- c(-60, 60) # In seconds.
ylim <- c(0, 3.0)
reference <- "end.time"
windows(height = 11, width = 8.5)
m <- kronecker(matrix(1:length(years)), matrix(1, nrow = 4, ncol = 6))
m <- rbind(0, cbind(0, m, 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))
for (j in 1:length(years)){
   print(years[j])
   
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
   s$end.time <- s$end.time.logbook
   if (years[j] == 2012) s <- s[!(s$tow.id %in% c("GP009FR")), ]
   s <- s[s$start.time != "        " & s$liftoff.time != "        " , ]
   
   # Compile vessel speed stats:
   speed <- matrix(0, nrow = 40, ncol = length(xlim[1]:xlim[2]))
   rownames(speed) <- as.character(seq(0.1, 4.0, by = 0.1))
   colnames(speed) <- xlim[1]:xlim[2]
   for (i in 1:nrow(s)){
      cat(paste0(i, ") ", s$tow.id[i], "\n"))
      if (years[j] <= 2012){
         file <- netmind.file.str(year = years[j], tow.id = s$tow.id[i]) 
         n <- read.netmind(file, offset = 0)
         n$doormaster <- n$doorspread
         e <- n 
      }else{
         e <- read.esonar(year = years[j], tow.id = s$tow.id[i])
      }      
        
      if (!is.null(e)){
         if (reference == "touchdown") e$time <- time2min(time(e), start.time(s[i, ]))
         if (reference == "end.time")  e$time <- time2min(time(e), time.default(paste0(as.character(date(s[i, ])), " ", s$end.time[i], " AST")))
         if (reference == "liftoff")   e$time <- time2min(time(e), time.default(paste0(as.character(date(s[i, ])), " ", s$liftoff[i], " AST")))
         if (years[j] <= 2012) e$time <- e$time - 180
      
         index <- e$time >= (xlim[1] / 60) & e$time <= (xlim[2] / 60)
         tt <- e$time[index]
         ss <- e$speed[index]
         if (length(tt) > 0){
            for (k in 1:length(tt)){
               ii <- as.character(ss[k])
               jj <- as.character(round(tt[k]*60))
               if ((ii %in% rownames(speed)) & (jj %in% colnames(speed))) speed[ii, jj] <- speed[ii, jj] + 1
            }
         }
      }
   }
   
   # Image plot for speed:
   plot(xlim, ylim, type = "n", xaxs = "i", xaxt = "n", yaxt = "n", yaxs = "i", xlab = "", ylab = "")
   if (j == 1) axis(2, seq(ylim[1], ylim[2], by = 0.5)) else axis(2, at = seq(ylim[1], ylim[2]-0.5, by = 0.5))
   if (j == 2) mtext("Speed (knots)", 2, 2.5, cex = 1.5)
   grid()   
   image(as.numeric(colnames(speed)), as.numeric(rownames(speed)), t(speed), 
         col = grey(seq(1, 0, len = 100)), add = TRUE)
   index <- which(speed > 0, arr.ind = TRUE)
   v <- cbind(as.numeric(rownames(speed)[index[, 1]]), as.numeric(colnames(speed)[index[, 2]]))
   v <- v[(v[,1] <= 1.7) | (v[,1] >= 2.5), ]
   #points(v[,2], v[,1], pch = 21, bg = "grey75", cex = 0.5, col = "grey60")

   prc <- function(x,  p){
      if (all(is.na(x))) return(NA*p)
      return(approx(x, as.numeric(names(x)), p)$y)
   }
   res <- NULL
   for (i in 1:ncol(speed)){
      res <- rbind(res, prc(cumsum(speed[, i] / sum(speed[, i])), p = c(0.025, 0.25, .5, 0.75, 0.975)))
   }  
   lines(as.numeric(dimnames(speed)[[2]]), res[,1], lwd = 1, lty = "dotted", col = "red")
   lines(as.numeric(dimnames(speed)[[2]]), res[,5], lwd = 1, lty = "dotted", col = "red")
   lines(as.numeric(dimnames(speed)[[2]]), res[,2], lwd = 2, lty = "dashed", col = "red")
   lines(as.numeric(dimnames(speed)[[2]]), res[,4], lwd = 2, lty = "dashed", col = "red")
   lines(as.numeric(dimnames(speed)[[2]]), res[,3], lwd = 2, lty = "solid", col = "red")   

   text(par("usr")[1] + 0.9 * diff(par("usr")[1:2]), par("usr")[3] + 0.93 * diff(par("usr")[3:4]), years[j], cex = 1.5)
   if ((xlim[1] < 0) & (xlim[2] > 0)) lines(c(0, 0), par("usr")[3:4], lwd = 2, col = "red")
   box()
}
axis(1)
mtext("Time (s)", 1, 3, cex = 1.5)

