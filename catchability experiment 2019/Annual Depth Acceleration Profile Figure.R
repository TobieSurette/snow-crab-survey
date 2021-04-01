library(gulf)

source("C:/gulf package/gulf/R/liftoff.scset.R")
years <- 2017:2019 

clg()
xlim <- c(-120, 120) # In seconds.
type = "depth" 
ylim <- c(-0.03, 0.02, 0.001)
reference <- "touchdown"
windows(height = 11, width = 8.5)
m <- kronecker(matrix(1:3), matrix(1, nrow = 4, ncol = 6))
m <- rbind(0, cbind(0, m, 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))
for (j in 1:length(years)){
   print(years[j])
   
   # Read revised start-end times:
   res <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", years[j], ".csv"), stringsAsFactors = FALSE)
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
   
   # Compile wingspread stats:
   w <- matrix(0, nrow = length(seq(ylim[1], ylim[2], by = ylim[3])), ncol = length(xlim[1]:xlim[2]))
   rownames(w) <- as.character(round(seq(ylim[1], ylim[2], by = ylim[3]),3))
   colnames(w) <- xlim[1]:xlim[2]
   for (i in 1:nrow(s)){
      if (i %% 10 == 0) print(i)
      o <- read.star.oddi(year = years[j], tow.id = s$tow.id[i], type = type)      
      if (!is.null(o)){
         if (reference == "touchdown") o$time <- time2min(time(o), start.time(s[i, ]))
         if (reference == "end.time")  o$time <- time2min(time(o), time.default(paste0(as.character(date(s[i, ])), " ", s$end.time[i], " AST")))
         if (reference == "liftoff")   o$time <- time2min(time(o), time.default(paste0(as.character(date(s[i, ])), " ", s$liftoff[i], " AST")))
            
         e <- read.esonar(year = years[j], tow.id = s$tow.id[i])
         depth <- NULL
         if (!is.null(e)){
            if (reference == "touchdown") e$time <- time2min(time(e), start.time(s[i, ]))   
            if (reference == "end.time")  e$time <- time2min(time(e), time.default(paste0(as.character(date(s[i, ])), " ", s$end.time[i], " AST")))
            if (reference == "liftoff")   e$time <- time2min(time(e), time.default(paste0(as.character(date(s[i, ])), " ", s$liftoff[i], " AST")))
            depth <- mean(-depth(e$longitude[e$time >= 0 & e$time <= 1], e$latitude[e$time >= 0 & e$time <= 1]))
            if (is.na(depth)) depth <- NULL
         }
         if (is.null(depth)){
            depth <- -depth(longitude(s[i, ]), latitude(s[i, ]))
         }
         
         # Convert pressure to estimated depth:
         o$depth <- (o$pressure / mean(o$pressure[o$time >= 0 & o$time <= 1])) * depth - depth  
         
         # Extract subset of data:
         index <- which((o$time >= (xlim[1] / 60)) & (o$time <= (xlim[2] / 60)))
         tt <- o$time[index]
         pp <- round(o$depth[index], 1)
         
         # Calculate speed and acceleration:
         k <- 7  # Smoothing window width in seconds.
         n <- length(tt)
         vv <- NA * tt
         aa <- NA * tt
         ttt <- round(tt * 60)
         jj <- j
         for (j in 1:(n-k)){
            model <- lm(pp[j:(j+k)] ~ I(ttt[j:(j+k)]))
            vv[j+(k-1)/2] <- as.numeric(coef(model)[2])
         }
         for (j in 1:(n-k)){
            model <- lm(vv[j:(j+k)] ~ I(ttt[j:(j+k)]))
            aa[j+(k-1)/2] <- as.numeric(coef(model)[2])
         }
         j <- jj 
         #print(range(aa, na.rm = TRUE))
         aa <- round(aa, 3)

         if (length(tt) > 0){
            for (k in 1:length(tt)){
               if (!is.na(aa[k])){
                  ii <- as.character(aa[k])
                  jj <- as.character(round(tt[k]*60))
                  if ((ii %in% rownames(w)) & (jj %in% colnames(w))) w[ii, jj] <- w[ii, jj] + 1
               }
            }
         }
      }
   }
   
   # Image plot for speed:
   step <- 0.01 
   plot(xlim[1:2], ylim[1:2], type = "n", xaxs = "i", xaxt = "n", yaxt = "n", yaxs = "i", xlab = "", ylab = "")
   if (j == 1) axis(2, seq(ylim[1], ylim[2], by = step)) else axis(2, at = seq(ylim[1], ylim[2]-step, by = step))
   
  # if (j == 2) mtext(expression(paste("Vertical acceleration(m"^"2", "/s)")), 2, 2.5, cex = 1.5)    
   if (j == 2) mtext(expression(paste("Vertical acceleration (m/s"^"2", ")")), 2, 2.5, cex = 1.5)
   
   grid()   
   image(as.numeric(colnames(w)), as.numeric(rownames(w)), t(w), col = grey(seq(1, 0, len = 100)), add = TRUE)
   index <- which(w > 0, arr.ind = TRUE)
   v <- cbind(as.numeric(rownames(w)[index[, 1]]), as.numeric(colnames(w)[index[, 2]]))
   v <- v[(v[,1] <= 1.7) | (v[,1] >= 2.5), ]
   #points(v[,2], v[,1], pch = 21, bg = "grey75", cex = 0.5, col = "grey60")

   prc <- function(x,  p){
      if (sum(x) < 10) return(NA*p)
      x <- cumsum(x / sum(x))
      if (all(is.na(x))) return(NA*p)
      if (sd(x) == 0) return(NA*p)
      return(approx(x, as.numeric(names(x)), p)$y)
   }
   res <- NULL
   for (i in 1:ncol(w)){
      res <- rbind(res, prc(w[, i], p = c(0.025, 0.25, .5, 0.75, 0.975)))
   }  
   rownames(res) <- dimnames(w)[[2]]
   # Group by 5 seconds:
   res <- aggregate(res, by = list(time = round(as.numeric(dimnames(w)[[2]]) / 5) * 5), mean)
   lines(res$time, res[,2], lwd = 1, lty = "dotted", col = "red")
   lines(res$time, res[,6], lwd = 1, lty = "dotted", col = "red")
   lines(res$time, res[,3], lwd = 2, lty = "dashed", col = "red")
   lines(res$time, res[,5], lwd = 2, lty = "dashed", col = "red")
   lines(res$time, res[,4], lwd = 2, lty = "solid", col = "red")   

   text(par("usr")[1] + 0.9 * diff(par("usr")[1:2]), par("usr")[3] + 0.93 * diff(par("usr")[3:4]), years[j], cex = 1.5)
   if ((xlim[1] < 0) & (xlim[2] > 0)) lines(c(0, 0), par("usr")[3:4], lwd = 2, col = "red")
   box()
}
axis(1)
mtext("Time (s)", 1, 3, cex = 1.5)

