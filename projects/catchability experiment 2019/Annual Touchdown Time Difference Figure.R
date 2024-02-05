library(gulf)

years <- 2017:2019

m <- kronecker(matrix(1:3), matrix(1, nrow = 4, ncol = 6))
m <- rbind(0, cbind(0, m, 0), 0, 0)

clg()
windows(height = 11, width = 8.5)
layout(m)
par(mar = c(0,0,0,0))
for (i in 1:length(years)){
   s <- read.scset(year = years[i], valid = 1)
   s <- s[substr(s$tow.id, 2, 2) != "C", ]

   res <- read.csv(paste0("U:/Snow Crab/Stock Assessment 2019/Touchdown and Swept Area Results/Touchdown and Liftoff - Tilt ", years[i], ".csv"), stringsAsFactors = FALSE)
   touchdown.tilt <- res$touchdown
   names(touchdown.tilt) <- res$tow.id
   touchdown.tilt <- touchdown.tilt[s$tow.id] 
 
   delta <- as.numeric(difftime(start.time(s), time.default(paste0(as.character(date(s)), " ", touchdown.tilt, " AST")), units = "secs"))
   plot(c(-20, 80), c(0, 30), type = "n", xaxs = "i", xaxt = "n", yaxt = "n", yaxs = "i", xlab = "", ylab = "")
   if (i == 1) axis(2) else axis(2, at = seq(0, 25, by = 5))
   if (i == 2) mtext("Frequency", 2, 2.5, cex = 1.5)
   grid()
   dbarplot(table(delta), width = 1, add = TRUE)
   lines(c(0,0), par("usr")[3:4], lwd = 2, col = "red")
   lines(rep(mean(delta[delta >= -20 & delta <= 80]), 2), par("usr")[3:4], lwd = 2, col = "red", lty = "dashed")
   text(par("usr")[1] + 0.5 * diff(par("usr")[1:2]), par("usr")[3] + 0.93 * diff(par("usr")[3:4]), years[i], cex = 1.5)
   box()
   
   print(mean(delta[delta >= -20 & delta <= 80]))
   print(sd(delta[delta >= -20 & delta <= 80]))
}
axis(1)
mtext("Time difference(s)", 1, 3, cex = 1.5)


