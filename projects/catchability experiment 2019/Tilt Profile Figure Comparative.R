library(gulf)

source("C:/gulf package/gulf/R/liftoff.scset.R")
source("C:/gulf package/gulf/R/star.oddi.path.str.R")
source("C:/gulf package/gulf/R/star.oddi.file.str.R")
source("C:/gulf package/gulf/R/read.star.oddi.R")
 
jpeg <- TRUE

clg()
xlim <- c(0, 300) # In seconds.
ylim <- c(0, 3.0)
reference <- "touchdown"

if (jpeg){
   jpeg(file = "U:/Snow Crab/Stock Assessment 2019/Comparative Analyses/Tilt Profile Comparative.jpg", width = 8*480, height = 8*480, res = 8*75)
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
      
smooth <- function(y, n = 3){
   v <- rep(NA, length(y))
   for (i in (n+1):(length(y)-n+1)) v[i] <- mean(y[(i-n):(i+n)])
   return(v)
}

counter <- 1
files <- star.oddi.file.str(year = 2019) 
for (captain in c("Ghislain Bourgeois", "Denis Eloquin")){
   for (vessel in c("Jean Mathieu", "Avalon Voyager II")){ 
      ss <- s[(s$vessel == vessel) & (s$captain == captain), ]
      cols <- rainbow(nrow(ss))
      plot(c(0, 300), c(-25, 25), xlab = "", ylab = "", type = "n", xaxt = "n", yaxt = "n", xaxs = "i", yaxs = "i")
      grid()       
      xx <- yy <- rep(NA, nrow(ss))
      for (i in 1:nrow(ss)){
         # Read Star Oddi:
         o <- read.star.oddi(year = 2019, tow.id = ss$tow.id[i], type = "tilt")      
         start.time <- 0
         end.time <-  time2min(end.time(ss[i, ]), start.time(ss[i, ]))
         liftoff.time <- time2min(time.default(paste0(as.character(date(ss[i, ])), " ", ss$liftoff[i], " AST")), start.time(ss[i, ]))
         if (!is.null(o)){
            o$time <- time2min(time(o), start.time(ss[i, ]))
         
            # Extract subset of data:
            index <- (o$time >= 0) & (o$time <= end.time)
            tt <- o$time[index]
            #dd <- smooth(o$tilt.x[index], n = 20)  
            dd <- smooth(o$tilt.x[index], n = 10)
            
            if (length(tt) > 10) lines(tt * 60, dd, lwd = 1.5, col = "grey")

            xx[i] <- par("usr")[1] + (0.25 + 0.6*i/nrow(ss)) * diff(par("usr")[1:2])
            yy[i] <- dd[which.min(abs((tt*60)-xx))[1]] 
         }
      }
    
      text(xx, yy, substr(ss$tow.id,3,5), cex = 0.9)
      
      if (counter == 1) axis(2) 
      if (counter %in% c(2,4)) axis(1)
      if (counter == 2) axis(2) 
      if (counter == 1) mtext("Tilt angle (degrees)", 2, 3, cex = 1.3, at = -25)
      if (counter == 2) mtext("Time from touchdown (s)", 1, 3.5, cex = 1.3, at = 300)
      if (counter == 3) mtext("Jean Mathieu", 4, 2.0, cex = 1.3, srt = 180, at = 0) 
      if (counter == 4) mtext("Avalon Voyager II", 4, 2.0, cex = 1.3, srt = 180, at = 0) 
      if (counter == 1) mtext("Ghislain Bourgeois", 3, 1.0, cex = 1.3, at = 150) 
      if (counter == 3) mtext("Denis Eloquin", 3, 1.0, cex = 1.3,  at = 150) 

      counter <- counter + 1 

      box()
   }
}

if (jpeg) dev.off()     
