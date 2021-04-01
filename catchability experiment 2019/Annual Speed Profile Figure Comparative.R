library(gulf)

source("C:/gulf package/gulf/R/liftoff.scset.R")
jpeg <- TRUE

clg()
xlim <- c(0, 300) # In seconds.
ylim <- c(0, 3.0)
reference <- "start.time"

if (jpeg){
   jpeg(file = "U:/Snow Crab/Stock Assessment 2019/Comparative Analyses/Speed Profile Plots Comparative.jpg", width = 8*480, height = 8*480, res = 8*75)
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
    
      # Compile vessel speed stats:
      speed <- matrix(0, nrow = 40, ncol = length(xlim[1]:xlim[2]))
      rownames(speed) <- as.character(seq(0.1, 4.0, by = 0.1))
      colnames(speed) <- xlim[1]:xlim[2]
   
      for (i in 1:nrow(ss)){
         e <- read.esonar(year = 2019, tow.id = ss$tow.id[i])
         if (!is.null(e)){   
            e$time <- time2min(time(e), start.time(ss[i, ]))
            start.time <- 0
            end.time <-  time2min(end.time(ss[i, ]), start.time(ss[i, ]))
            liftoff.time <- time2min(time.default(paste0(as.character(date(ss[i, ])), " ", ss$liftoff[i], " AST")), start.time(ss[i, ]))
            e <- e[e$time <= liftoff.time, ]
            if (reference == "end.time")  e$time <- e$time - end.time
            if (reference == "liftoff")   e$time <- e$time - liftoff.time
         
            if (nrow(e) > 0){               
               index <- e$time >= (xlim[1] / 60) & e$time <= (xlim[2] / 60)
               ttt <- e$time[index]
               sss <- e$speed[index]
               if (length(ttt) > 0){
                  for (k in 1:length(ttt)){
                     ii <- as.character(sss[k])
                     jj <- as.character(round(ttt[k]*60))
                     if ((ii %in% rownames(speed)) & (jj %in% colnames(speed))) speed[ii, jj] <- speed[ii, jj] + 1
                  }
               }
                              
            }
         }
      }
      # Image plot for speed:
      plot(xlim, c(1, 3), type = "n", xaxs = "i", xaxt = "n", yaxt = "n", yaxs = "i", xlab = "", ylab = "",)
      #if (j == 1) axis(2, seq(ylim[1], ylim[2], by = 0.5)) else axis(2, at = seq(ylim[1], ylim[2]-0.5, by = 0.5))
      if (j == 2) mtext("Speed (knots)", 2, 2.5, cex = 1.5)
      grid()   
      image(as.numeric(colnames(speed)), as.numeric(rownames(speed)), t(speed), 
            col = grey(seq(1, 0, len = 100)), add = TRUE)
      index <- which(speed > 0, arr.ind = TRUE)
      v <- cbind(as.numeric(rownames(speed)[index[, 1]]), as.numeric(colnames(speed)[index[, 2]]))
      v <- v[(v[,1] <= 1.7) | (v[,1] >= 2.5), ]
   
      if (counter == 1) axis(2, seq(ylim[1], ylim[2], by = 0.5)) 
      if (counter %in% c(2,4)) axis(1)
      if (counter == 2) axis(2, seq(ylim[1], ylim[2]-0.5, by = 0.5))    
      if (counter == 1) mtext("Vessel speed (knots)", 2, 3, cex = 1.3, at = 1)
      if (counter == 2) mtext("Time from touchdown(s)", 1, 3.5, cex = 1.3, at = 300)
      if (counter == 3) mtext("Jean Mathieu", 4, 2.0, cex = 1.3, srt = 180, at = 2) 
      if (counter == 4) mtext("Avalon Voyager II", 4, 2.0, cex = 1.3, srt = 180, at = 2) 
      if (counter == 1) mtext("Ghislain Bourgeois", 3, 1.0, cex = 1.3, at = 150) 
      if (counter == 3) mtext("Denis Eloquin", 3, 1.0, cex = 1.3,  at = 150) 

      counter <- counter + 1 
      grid()  
      box()

      lines(par("usr")[1:2], c(2,2), lwd = 2, col = "red")
            
      str <- paste0(captain, " on ", vessel)
      #text(par("usr")[1] + 0.5*diff(par("usr")[1:2]), par("usr")[3] + 0.075*diff(par("usr")[3:4]), str, cex = 1.25)
   }
}

if (jpeg) dev.off()     
