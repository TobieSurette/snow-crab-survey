library(gulf)

source("C:/gulf package/gulf/R/liftoff.scset.R")
jpeg <- TRUE

clg()
xlim <- c(0, 300) # In seconds.
ylim <- c(0, 3.0)
reference <- "start.time"

if (jpeg){
   jpeg(file = "U:/Snow Crab/Stock Assessment 2019/Comparative Analyses/Trawl Tracks Comparative.jpg", width = 8*480, height = 8*480, res = 8*75)
}else{
   windows(height = 8.5, width = 8.5)
}
m <- kronecker(matrix(1:4, ncol = 2), matrix(1, nrow = 4, ncol = 4))
m <- rbind(0, cbind(0, m, 0), 0, 0)
layout(m)
par(mar = c(0,0,0,0))

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
      
counter <- 1
for (captain in c("Ghislain Bourgeois", "Denis Eloquin")){
   for (vessel in c("Jean Mathieu", "Avalon Voyager II")){ 
     # plot(c(0, 5), c(0.5, 1.5), type = "n", xaxs = "i", xaxt = "n", yaxt = "n", yaxs = "i", xlab = "", ylab = "")
     # if (j == 1) axis(2, seq(ylim[1], ylim[2], by = 0.5)) else axis(2, at = seq(ylim[1], ylim[2]-0.5, by = 0.5))
     # if (j == 2) mtext("Speed (knots)", 2, 2.5, cex = 1.5)
      #grid()  
      ss <- s[(s$vessel == vessel) & (s$captain == captain), ]
      cols <- rainbow(nrow(ss))
      plot(c(-500, 500), c(-500, 500), xlab = "", ylab = "", type = "n", xaxt = "n", yaxt = "n")
      grid()       
      for (i in 1:nrow(ss)){
         e <- read.esonar(year = 2019, tow.id = ss$tow.id[i])
         if (!is.null(e)){
            e$time <- time2min(time(e), start.time(ss[i, ]))
 
            if (max(diff(e$time)) <= 5){
               end.time <- time2min(end.time(ss[i, ]), start.time(ss[i, ]))
               liftoff.time <- time2min(time.default(paste0(as.character(date(ss[i, ])), " ", ss$liftoff[i], " AST")), start.time(ss[i, ]))
       
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
                  text(1.07 * tmp$x[nrow(tmp)], 1.07 * tmp$y[nrow(tmp)], substr(ss$tow.id[i], 3, 5), cex = 0.5)
                  #text(mean(tmp$x), mean(tmp$y), i)  
               }
            }
         }
      }
         
      if (counter == 1) axis(2) 
      if (counter %in% c(2,4)) axis(1)
      if (counter == 2) axis(2, seq(ylim[1], ylim[2]-0.5, by = 0.5))    
      if (counter == 1) mtext("y (meters)", 2, 3, cex = 1.3, at = -500)
      if (counter == 2) mtext("x (meters)", 1, 3.5, cex = 1.3, at = 500)
      if (counter == 3) mtext("Jean Mathieu", 4, 2.0, cex = 1.3, srt = 180, at = 0) 
      if (counter == 4) mtext("Avalon Voyager II", 4, 2.0, cex = 1.3, srt = 180, at = 0) 
      if (counter == 1) mtext("Ghislain Bourgeois", 3, 1.0, cex = 1.3, at = 0) 
      if (counter == 3) mtext("Denis Eloquin", 3, 1.0, cex = 1.3,  at = 0) 

      counter <- counter + 1 
      grid()  
      box()

      points(0, 0, pch = 21, bg = "grey", cex = 1.0)
            
      str <- paste0(captain, " on ", vessel)
      #text(par("usr")[1] + 0.5*diff(par("usr")[1:2]), par("usr")[3] + 0.075*diff(par("usr")[3:4]), str, cex = 1.25)
   }
}

if (jpeg) dev.off()     
