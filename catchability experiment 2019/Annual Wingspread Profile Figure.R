library(gulf)

source("C:/gulf package/gulf/R/liftoff.scset.R")
years <- 2017:2019 

clg()
xlim <- c(0, 300) # In seconds.
variable <- "headline"
if (variable == "wingspread") ylim <- c(0, 15, 0.1)
if (variable == "headline") ylim <- c(0, 3, 0.1)
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
   rownames(w) <- as.character(round(seq(ylim[1], ylim[2], by = ylim[3]),1))
   colnames(w) <- xlim[1]:xlim[2]
   for (i in 1:nrow(s)){
      if (i %% 10 == 0) print(i)
      e <- read.esonar(year = years[j], tow.id = s$tow.id[i])
      if (!is.null(e)){
         if (reference == "touchdown") e$time <- time2min(time(e), start.time(s[i, ]))
         if (reference == "end.time")  e$time <- time2min(time(e), time.default(paste0(as.character(date(s[i, ])), " ", s$end.time[i], " AST")))
         if (reference == "liftoff")   e$time <- time2min(time(e), time.default(paste0(as.character(date(s[i, ])), " ", s$liftoff[i], " AST")))
         if (variable == "wingspread") index <- which((e$time >= (xlim[1] / 60)) & (e$time <= (xlim[2] / 60)) & !is.na(e$doormaster))
         if (variable == "headline")   index <- which((e$time >= (xlim[1] / 60)) & (e$time <= (xlim[2] / 60)) & !is.na(e$headline))
         tt <- e$time[index]
         if (variable == "wingspread") ss <- round(e$doormaster[index], 1)
         if (variable == "headline") ss <- round(e$headline[index], 1) 
         if (length(tt) > 0){
            for (k in 1:length(tt)){
               ii <- as.character(ss[k])
               jj <- as.character(round(tt[k]*60))
               if ((ii %in% rownames(w)) & (jj %in% colnames(w))) w[ii, jj] <- w[ii, jj] + 1
            }
         }
      }
   }
   
   # Image plot for speed:
   if (variable == "wingspread") step <- 3
   if (variable == "headline")  step <- 0.5 
   plot(xlim[1:2], ylim[1:2], type = "n", xaxs = "i", xaxt = "n", yaxt = "n", yaxs = "i", xlab = "", ylab = "")
   if (j == 1) axis(2, seq(ylim[1], ylim[2], by = step)) else axis(2, at = seq(ylim[1], ylim[2]-step, by = step))
   
   if (j == 2){
      if (variable == "wingspread")  mtext("Wing spread (m)", 2, 2.5, cex = 1.5)
      if (variable == "headline") mtext("Headline height (m)", 2, 2.5, cex = 1.5)
   }
   grid()   
   image(as.numeric(colnames(w)), as.numeric(rownames(w)), t(w), col = grey(seq(1, 0, len = 100)), add = TRUE)
   index <- which(w > 0, arr.ind = TRUE)
   v <- cbind(as.numeric(rownames(w)[index[, 1]]), as.numeric(colnames(w)[index[, 2]]))
   v <- v[(v[,1] <= 1.7) | (v[,1] >= 2.5), ]
   #points(v[,2], v[,1], pch = 21, bg = "grey75", cex = 0.5, col = "grey60")

   prc <- function(x,  p){
      if (all(is.na(x))) return(NA*p)
      return(approx(x, as.numeric(names(x)), p)$y)
   }
   res <- NULL
   for (i in 1:ncol(w)){
      res <- rbind(res, prc(cumsum(w[, i] / sum(w[, i])), p = c(0.025, 0.25, .5, 0.75, 0.975)))
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


