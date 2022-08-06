#Test revision when file opened via GitHub Desktop

#Multiple Linear Regression analysis - South Fork Coquille River
#DO and DOPerSat = f(Time of day, temperature, river flow, nutrients)
#Exploratory Data Analysis EDA all seasons
#REGRESSION MODLELING FOR FALL SPAWNING PERIOD

#Revised September 2021 update to revise Observed and Load Capacity temperature 
#boxplot to reduce non-summer temperatures less than summer temperatures
#Observed T and T minus 3.8 C in summer, 1.9 C other seasons

#v3 - log transform TOC

#quick youtube vid on MLR models: https://www.youtube.com/watch?v=q1RD5ECsSB0

#"The general rule of thumb (based on stuff in Frank Harrell's book, Regression Modeling Strategies) 
# is that if you expect to be able to detect reasonable-size effects with reasonable power, you 
# need 10-20 observations per parameter (covariate) estimated."
#https://stats.stackexchange.com/questions/29612/minimum-number-of-observations-for-multiple-linear-regression?utm_medium=organic&utm_source=google_rich_qa&utm_campaign=google_rich_qa

library(tidyr) #"An evolution of 'reshape2'. It's designed specifically for data tidying 
# (not general reshaping or aggregating) and works well with 'dplyr' data pipelines."
library(car) # Companion to Applied Regression - includes some() function
library(MASS)
library(lattice); citation("lattice")
#multi-panel plots via "ggpairs:
library(ggplot2)
#plotmatrix(dDO) + geom_smooth() ## Note: The plotmatrix() function has been replaced by 
# the ggpairs() function from the GGally package 
library(GGally)

### make multi-panel plot by using "pairs"
# This is from my ESR 550 class Y. Pan Winter 2009
##create histograms for each variable
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="cyan", ...)
}
## put (absolute) correlations on the upper panels, with size proportional to the correlations.
panel.cor <- function(x, y, digits=2, prefix="", cex.cor)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex * r)
}

### CI.LINES Function that draws 95% confidence intervals for a regression
#     via The R Book, 2nd Ed., Crawley, p. 474
#     REGRESSIONS - LINEAR MODEL - m <- lm(y ~ x); m
ci.lines <- function(model, colorci){
  xm <- sapply(model[[12]][2],mean)
  n <- sapply(model[[12]][2],length)
  ssx <- sum(model[[12]][2]^2)-sum(model[[12]][2])^2/n
  s.t <- qt(0.975,(n-2))
  xv <- seq(min(model[[12]][2]),max(model[[12]][2]),length=100)
  yv <- coef(model)[1]+coef(model)[2]*xv
  se <- sqrt(summary(model)[[6]]^2*(1/n+(xv-xm)^2/ssx))
  ci <- s.t*se
  uyv <- yv+ci
  lyv <- yv-ci
  lines(xv,uyv,lty=2,col=colorci)
  lines(xv,lyv,lty=2,col=colorci)
}

#setwd("I:/TMDL_WR/South_Coast/Coquille/Data/11486_SFCoquilleR/") 
#setwd("//Deqhq1/tmdl/TMDL_WR/South_Coast/Coquille/Data/11486_SFCoquilleR/")
setwd("C:/Coquille/Data/11486_SFCoquilleR/")

dfile <- "SFCoquilleR_11486.csv" 

#outpath <- "//Deqhq1/tmdl/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots/"
#outpath <- "//Deqhq1/tmdl/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots2/"
#outpath <- "//Deqhq1/tmdl/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots3/"
#outpath <- "//Deqhq1/tmdl/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots4/"
#outpath <- "//Deqhq1/tmdl/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots5/"
#outpath <- "C:/R/Rscripts/Bloom_Rscripts/MLRandCorrelation/Plots/"
outpath <- "C:/R/Rscripts/Bloom_Rscripts/MLRandCorrelation/Plots3/"

d1A <- read.csv(dfile); d1A 

str(d1A)

head(d1A, n=8L)

#GATHER command wants date in POSIXct format
d1A$DateTimect <- as.POSIXct(d1A$DateTime_PST, format="%m/%d/%Y %H:%M" ,tz="America/Los_Angeles")
#You can obtain things like hour and min from POSIXlt format
d1A$DateTimelt <- as.POSIXlt(d1A$DateTime_PST, format="%m/%d/%Y %H:%M" ,tz="America/Los_Angeles")
d1A$Month <- d1A$DateTimelt$mon + 1 #since Jan is mon 0
#d1A$DateTimelt$mon
#d1A$DateTimelt$hour
#d1A$DateTimelt$min

#Summer Only 
#NOV23 d1 <- subset(d1A,  (Month == 6 | Month == 7 | Month == 8 | Month == 9))
#NOV23 head(d1)

#Select only necessary columns and place in order that worksfor TIDYR function SPREAD
#Include DateTimelt in order to extract month and julian day - consider adding month or julian day as an explanatory varible
#NOV23 str(d1)
#NOV23 d2 <- data.frame(d1[9], d1[16], d1[17], d1[8], d1[10]) #Parameter, DateTimect, DateTimelt, HourPST, Result_est - get clean variable names 
#NOV23 head(d2); str(d2)
#SPREAD via TIDYR package takes two columns (key & value) and spreads in to multiple columns, it turns "long" data frame into a "wide"
#data frame (values for all parameters in seperate columnes)
#NOV23 d2_wide <- spread(data = d2, key = Parameter, value = Result_est)
#NOV23 head(d2_wide, n=10L)
#NOV23 str(d2_wide)

#key parameters include HourPST, DO, DOPerSat, Temperature, TP, OrthophosphateP, TOC, QatSFCoqPowers, (and, for NF Coq, Alkalinity)
#NOV23 d2_wide$Jday <- d2_wide$DateTimelt$yday +1 #0-365: day of the year - add 1
#NOV23 head(d2_wide)
#NOV23 d2_wide$Month <- d2_wide$DateTimelt$mon +1 #0-11: month - add 1
#NOV23 some(d2_wide)

#Use data for all months for exploratory data analysis and monthly box plots - A for All
#d2A <- data.frame(d1A[9], d1A[16], d1A[8], d1A[10], d1A[18]) #Parameter, DateTimect, HourPST, Result_est, Month - get clean variable names 
#str(d2A)
#d2A_wide <- spread(data = d2A, key = Parameter, value = Result_est)
#Select only necessary columns and place in order that worksfor TIDYR function SPREAD
#SPREAD via TIDYR package takes two columns (key & value) and spreads in to multiple columns, it makes "long" data wide
#Include DateTimelt in order to extract month and julian day - consider adding month or julian day as an explanatory varible
#d2A <- data.frame(d1A[9], d1A[16], d1A[17], d1A[8], d1A[11]) #Parameter, DateTimect, DateTimelt, HourPST, Result_est

d2A <- d1A[,c(9,16,17,8,10)]
str(d2A); head(d2A)
d2A_wide <- spread(data = d2A, key = Parameter, value = Result_est)
head(d2A_wide)
d2A_wide$Month <- d2A_wide$DateTimelt$mon + 1 #0-11: months after the first of the year - add 1 
d2A_wide$Jday <- d2A_wide$DateTimelt$yday +1 #0-365: day of the year - add 1
d2A_wide$Time <- d2A_wide$HourPST/24

#DO SATURATION
#DO Percent Saturation (DOPerSat) is a value derived by technicians at the time samples are collected
#For this dataset, there are two records with NAs for DOPerSat
#DO saturation can be calculated via a number of equations, one of which is Duke and Masch, 1973:
#DOSat = (14.62-(0.3898*T)+(0.006969*T^2)-(0.00005897*T^3))*(1-(0.00000697*EL-Ft))^5.167
#DOSat can also be derivied via a ratio of field measured DO and field derived DOPerSat
#but this ratio can't be used for the two records with missing field derived DOPerSat.
d2A_wide$DOSat <- d2A_wide$DO / (d2A_wide$DOPerSat/100);d2A_wide$DOSat
#A comparison of the ratio method to calc via Duke and Masch, 1973 shows similar results.
#DOSat - DOSaturation
#DOSat = (14.62-(0.3898*T)+(0.006969*T^2)-(0.00005897*T^3))*(1-(0.00000697*EL-Ft))^5.167
#(Duke and Masch, 1973)
ElevFt <- 0
d2A_wide$DOSatC <- (14.62-(0.3898*d2A_wide$Temperature)+(0.006969*d2A_wide$Temperature^2)-(0.00005897*d2A_wide$Temperature^3))*(1-(0.00000697*ElevFt))^5.167
d2A_wide$DOPerSatC <-  d2A_wide$DO/d2A_wide$DOSatC * 100
d2A_wide$DOPerSat/d2A_wide$DOPerSatC
head(d2A_wide, n=3L)
xxx <- data.frame(d2A_wide$DO, d2A_wide$DOPerSat, d2A_wide$DOPerSatC, d2A_wide$DOSat, d2A_wide$DOSatC); head(xxx)
mean(d2A_wide$DOPerSat, na.rm = TRUE)
xxx <- subset(d2A_wide, DOPerSat != "NA"); str(xxx)
mean(xxx$DOPerSat, na.rm = TRUE)
mean(xxx$DOPerSatC, na.rm = TRUE)
mean(xxx$DOPerSat, na.rm = TRUE) - mean(xxx$DOPerSatC, na.rm = TRUE)
#difference between means via two methods for DOPerSat is < 0.03%
mean(xxx$DOSat, na.rm = TRUE)
mean(xxx$DOSatC, na.rm = TRUE)
mean(xxx$DOSat, na.rm = TRUE) - mean(xxx$DOSatC, na.rm = TRUE)
#Difference between means via two methods for DOSat is < 0.01 mg/L 

#Exploratory Data Analysis
#1. Plot of all DO vs time
#2. Plot of all DOPerSat vs time
#3. Plot of all Temperature vs time
#4. Plot of all pH vs time
#5. Monthly box plot of DO
#6. Monthly box plot of DOPerSat
#7. Monthly box plot of Temperature
#8. Monthly box plot of pH
#
#PLOTS OF DO, TIME OF SAMPLING, TEMPERATURE, ETC. VS. TIME
str(d2A_wide)
#Summer subset for time-series plots
head(d2A_wide_Summer)
tail(d2A_wide_Fall)

d2A_wide_Summer <- subset(d2A_wide,  (Month == 6 | Month == 7 | Month == 8 | Month == 9)); head(d2A_wide_Summer)

d2A_wide_Fall <- subset(d2A_wide,  (Month == 10 | Month == 11 | Month == 12)); head(d2A_wide_Fall)

#Mean and median summer values of interest
head(d2A_wide_Summer)
mean(d2A_wide_Summer$TOC, na.rm = TRUE)
median(d2A_wide_Summer$TOC, na.rm = TRUE)
mean(d2A_wide_Summer$TP, na.rm = TRUE)
median(d2A_wide_Summer$TP, na.rm = TRUE)
mean(d2A_wide_Summer$BOD5, na.rm = TRUE)
median(d2A_wide_Summer$BOD5, na.rm = TRUE)
mean(d2A_wide_Summer$AmmoniaN , na.rm = TRUE)
median(d2A_wide_Summer$AmmoniaN , na.rm = TRUE)
mean(d2A_wide_Summer$TKN, na.rm = TRUE)
median(d2A_wide_Summer$TKN, na.rm = TRUE)
mean(d2A_wide_Summer$NO23N, na.rm = TRUE)
median(d2A_wide_Summer$NO23N, na.rm = TRUE)

#Mean and median fall values of interest
head(d2A_wide_Fall)
mean(d2A_wide_Fall$TOC, na.rm = TRUE)
median(d2A_wide_Fall$TOC, na.rm = TRUE) 
mean(d2A_wide_Fall$TP, na.rm = TRUE)
median(d2A_wide_Fall$TP, na.rm = TRUE) 
mean(d2A_wide_Fall$BOD5, na.rm = TRUE)
median(d2A_wide_Fall$BOD5, na.rm = TRUE) 
mean(d2A_wide_Fall$TKN, na.rm = TRUE)
median(d2A_wide_Fall$TKN, na.rm = TRUE) 



#Titles
stations <- "11486_SFCoquilleR"
stations2  <- "SF Coquille R - 11486"

# Plots of results over time ----------------------------------------------

#par(mfrow=c(2,1))
png(file=paste(outpath,stations,"_","DOvTime.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$DO,  main=paste("Measured Dissolved Oxygen vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(6,14), 
     xlab="Year", ylab = "DO mg/L",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$DO, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$DO, pch=20, cex=1.5, col="blue", bg="blue")
#axTicks(1)
abline(h = 14, lty = "dotted", lwd=1, col="gray" )
abline(h = 13, lty = "dotted", lwd=1, col="gray" )
abline(h = 12, lty = "dotted", lwd=1, col="gray" )
abline(h = 11, lty = "dotted", lwd=1, col="gray" )
abline(h = 10, lty = "dotted", lwd=1, col="gray" )
abline(h = 9, lty = "dotted", lwd=1, col="gray" )
abline(h = 8, lty = "dotted", lwd=1, col="gray" )
abline(h = 7, lty = "dotted", lwd=1, col="gray" )
abline(h = 6, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
#grid(nx=5,col = "gray", lty = "dotted", lwd = 1)
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
crity <- c(8,8)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
#legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=14.3, bty="n",  legend = "Nov-May", 
       pch=21, cex=1.3, col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=13.8, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
dev.off()

png(file=paste(outpath,stations,"_","HourPSTvTime.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$HourPST, main=paste("Sampling Time vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(6,18), 
     xlab="Year", ylab = "Sampling Time",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$HourPST, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$HourPST, pch=20, cex=1.5, col="blue", bg="blue")
abline(h = 18, lty = "dotted", lwd=1, col="gray" )
abline(h = 16, lty = "dotted", lwd=1, col="gray" )
abline(h = 14, lty = "dotted", lwd=1, col="gray" )
abline(h = 12, lty = "dotted", lwd=1, col="gray" )
abline(h = 10, lty = "dotted", lwd=1, col="gray" )
abline(h = 8, lty = "dotted", lwd=1, col="gray" )
abline(h = 6, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
crity <- c(12,12)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=18.4, bty="n",  legend = "Nov-May",
       pch=21, cex=1.3,  col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=17.7, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
dev.off()

png(file=paste(outpath,stations,"_","TvTime.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$Temperature, main=paste("Measured Temperature vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(0,30), 
     xlab="Year", ylab = "Celsius",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$Temperature, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$Temperature, pch=20, cex=1.5, col="blue", bg="blue")
abline(h = 30, lty = "dotted", lwd=1, col="gray" )
abline(h = 25, lty = "dotted", lwd=1, col="gray" )
abline(h = 20, lty = "dotted", lwd=1, col="gray" )
abline(h = 15, lty = "dotted", lwd=1, col="gray" )
abline(h = 10, lty = "dotted", lwd=1, col="gray" )
abline(h = 5, lty = "dotted", lwd=1, col="gray" )
abline(h = 0, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
crity <- c(18,18)
lines(critx,crity, type="l", lty=2, lwd=2, col="blue")
crity <- c(13,13)
lines(critx,crity, type="l", lty=2, lwd=2, col="gray")
#legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=31, bty="n",  legend = "Nov-May", 
       pch=21, cex=1.3, col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=29, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
legend(x=as.POSIXct("2014-01-01 00:00:00 PDT"), y=31, bty="n",  legend = "18C criterion", 
       cex=1.3,lty=2, lwd=2, col="blue")
legend(x=as.POSIXct("2014-01-01 00:00:00 PDT"), y=29, bty="n",  legend = "13C criterion", 
       cex=1.3,lty=2, lwd=2, col="gray")
dev.off()

png(file=paste(outpath,stations,"_","pHvTime.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$pH, main=paste("Measured pH vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(6,9), 
     xlab="Year", ylab = "pH su",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$pH, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$pH, pch=20, cex=1.5, col="blue", bg="blue")
abline(h = 9, lty = "dotted", lwd=1, col="gray" )
abline(h = 8.5, lty = "dotted", lwd=1, col="gray" )
abline(h = 8, lty = "dotted", lwd=1, col="gray" )
abline(h = 7.5, lty = "dotted", lwd=1, col="gray" )
abline(h = 7, lty = "dotted", lwd=1, col="gray" )
abline(h = 6.5, lty = "dotted", lwd=1, col="gray" )
abline(h = 6, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
crity <- c(8.5,8.5)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(6.5,6.5)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=9.1, bty="n",  legend = "Nov-May", 
       pch=21, cex=1.3, col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=8.9, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
legend(x=as.POSIXct("2013-01-01 00:00:00 PDT"), y=9.1, bty="n",  legend = "6.5-8.5 criteria", 
       cex=1.3,lty=2, lwd=2, col="black")
#legend(x=as.POSIXct("2016-01-01 00:00:00 PDT"), y=29, bty="n",  legend = "13C crit", 
#       cex=1.3,lty=2, lwd=2, col="gray")
dev.off()

?pch
#par(mfrow=c(2,1))
d2A_wide$TP
png(file=paste(outpath,stations,"_","TPvTime_LC16_log.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$TP,  main=paste("Measured Total Phosphorus vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(0.005,0.5), 
     log="y", xlab="Year", ylab = "TP mg/L",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$TP, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$TP, pch=20, cex=2.2, col="blue", bg="blue")
#axTicks(1)
#abline(h = 14, lty = "dotted", lwd=1, col="gray" )
#abline(h = 13, lty = "dotted", lwd=1, col="gray" )
#abline(h = 12, lty = "dotted", lwd=1, col="gray" )
#abline(h = 11, lty = "dotted", lwd=1, col="gray" )
#abline(h = 10, lty = "dotted", lwd=1, col="gray" )
abline(h = .4, lty = "dotted", lwd=1, col="gray" )
abline(h = .3, lty = "dotted", lwd=1, col="gray" )
abline(h = .2, lty = "dotted", lwd=1, col="gray" )
abline(h = .1, lty = "dotted", lwd=1, col="gray" )
abline(h = .05, lty = "dotted", lwd=1, col="gray" )
abline(h = .01, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
#grid(nx=5,col = "gray", lty = "dotted", lwd = 1)
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
crity <- c(0.0163,0.0163)
lines(critx,crity, type="l", lty=2, lwd=2, col="red")
mrlx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
mrly <- c(0.01,0.01) #MRL for TP
lines(mrlx,mrly, type="l", lty=2, lwd=2, col="black")
#legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=0.62, bty="n",  legend = "Nov-May", 
       pch=21, cex=1.3, col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=0.46, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
legend(x=as.POSIXct("2008-01-01 00:00:00 PDT"), y=0.62, bty="n",  legend = "16.3 ug/L Loading Capacity", 
       lty=2, cex=1.3,lwd=2, col="red")
legend(x=as.POSIXct("2008-01-01 00:00:00 PDT"), y=0.46, bty="n",  legend = "10 ug/L MRL", 
       lty=2, cex=1.3,lwd=2, col="black")
dev.off()

png(file=paste(outpath,stations,"_","TPvTime_LC17_log.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$TP,  main=paste("Measured Total Phosphorus vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(0.005,0.5), 
     log="y", xlab="Year", ylab = "TP mg/L",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$TP, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$TP, pch=20, cex=2.2, col="blue", bg="blue")
abline(h = .4, lty = "dotted", lwd=1, col="gray" )
abline(h = .3, lty = "dotted", lwd=1, col="gray" )
abline(h = .2, lty = "dotted", lwd=1, col="gray" )
abline(h = .1, lty = "dotted", lwd=1, col="gray" )
abline(h = .05, lty = "dotted", lwd=1, col="gray" )
abline(h = .01, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
#grid(nx=5,col = "gray", lty = "dotted", lwd = 1)
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
#crity <- c(0.0163,0.0163)
crity <- c(0.017,0.017)
lines(critx,crity, type="l", lty=2, lwd=2, col="red")
mrlx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
mrly <- c(0.01,0.01) #MRL for TP
lines(mrlx,mrly, type="l", lty=2, lwd=2, col="black")
#legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=0.62, bty="n",  legend = "Nov-May", 
       pch=21, cex=1.3, col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=0.46, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
legend(x=as.POSIXct("2008-01-01 00:00:00 PDT"), y=0.62, bty="n",  legend = "17 ug/L Loading Capacity", 
       lty=2, cex=1.3,lwd=2, col="red")
legend(x=as.POSIXct("2008-01-01 00:00:00 PDT"), y=0.46, bty="n",  legend = "10 ug/L MRL", 
       lty=2, cex=1.3,lwd=2, col="black")

dev.off()

png(file=paste(outpath,stations,"_","TPvTime_LC11and17_log.png",sep=""), width = 1000, height = 400)
plot(d2A_wide$DateTimect, d2A_wide$TP,  main=paste("Measured Total Phosphorus vs. Time","-",stations2), 
     pch=21, col="black", bg="gray",
     xlim=as.POSIXct(c("1980-01-01 00:00:00 PDT","2020-01-01 00:00:00 PDT")), ylim=c(0.005,0.5), 
     log="y", xlab="Year", ylab = "TP mg/L",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
lines(d2A_wide$DateTimect, d2A_wide$TP, type="l", lty=3, col="gray")
points(d2A_wide_Summer$DateTimect, d2A_wide_Summer$TP, pch=20, cex=2.2, col="blue", bg="blue")
abline(h = .4, lty = "dotted", lwd=1, col="gray" )
abline(h = .3, lty = "dotted", lwd=1, col="gray" )
abline(h = .2, lty = "dotted", lwd=1, col="gray" )
abline(h = .1, lty = "dotted", lwd=1, col="gray" )
abline(h = .05, lty = "dotted", lwd=1, col="gray" )
abline(h = .01, lty = "dotted", lwd=1, col="gray" )
abline(v = 315561600, lty = "dotted", lwd=1, col="gray" )  #1980-01-01 Number of secs since 1970-01-01
abline(v = 631180800, lty = "dotted", lwd=1, col="gray" )  #1990-01-01
abline(v = 946713600, lty = "dotted", lwd=1, col="gray" )  #2000-01-01
abline(v = 1262332800, lty = "dotted", lwd=1, col="gray" ) #2010-01-01
abline(v = 1577865600, lty = "dotted", lwd=1, col="gray" ) #2020-01-01
critx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
#crity <- c(0.0163,0.0163)
crity <- c(0.017,0.017)
lines(critx,crity, type="l", lty=2, lwd=2, col="red")
crity <- c(0.011,0.011)
lines(critx,crity, type="l", lty=2, lwd=2, col="red")
mrlx <- as.POSIXct(c("1977-01-01 00:00:00 PDT","2023-01-01 00:00:00 PDT"))
mrly <- c(0.01,0.01) #MRL for TP
lines(mrlx,mrly, type="l", lty=2, lwd=2, col="black")
#legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=0.62, bty="n",  legend = "Nov-May", 
       pch=21, cex=1.3, col="black", pt.bg="gray")
legend(x=as.POSIXct("1979-01-01 00:00:00 PDT"), y=0.46, bty="n",  legend = "Jun-Sep \"Summer\"", 
       pch=20, cex=1.3, pt.cex=2.0, col="blue", bg="blue")
legend(x=as.POSIXct("2008-01-01 00:00:00 PDT"), y=0.62, bty="n",  legend = "17 ug/L Loading Capacity", 
       lty=2, cex=1.3,lwd=2, col="red")
legend(x=as.POSIXct("2008-01-01 00:00:00 PDT"), y=0.46, bty="n",  legend = "11 ug/L Loading Capacity", 
       lty=2, cex=1.3,lwd=2, col="red")
legend(x=as.POSIXct("2008-01-01 00:00:00 PDT"), y=0.34, bty="n",  legend = "10 ug/L MRL", 
       lty=2, cex=1.3,lwd=2, col="black")
dev.off()

# Derive Month_order factor for Box Plots ---------------------------------
#BOX PLOTS
head(d2A_wide)
Month_orderA <- factor(d2A_wide$Month,levels=c(1:12), ordered=TRUE) #all months plotted, including those w/ no data
Month_order <- factor(d2A_wide$Month,levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) #all months plotted, including those w/ no data

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_DO.png",sep=""), width = 665, height = 500) #dev.off()
#plot(DO~Month_order,ylim=c(6,max(DO,na.rm=TRUE)+0.5),ylab="DO mg/L",xlab="Month",
#     notch=TRUE,
#     main=paste("Dissolved Oxygen","-", stations2),lty=1,las=1,varwidth = TRUE,
#     cex.axis=1.2, cex.lab=1.2) 
# if notch is TRUE, a notch is drawn in each side of the boxes. If the notches of two plots do not overlap 
# this is 'strong evidence' that the two medians differ
plot(d2A_wide$DO~Month_order,ylim=c(6,max(d2A_wide$DO,na.rm=TRUE)+0.5),ylab="DO mg/L",xlab="Month",
     main=paste("Dissolved Oxygen","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
#?boxplot #alternative, produces same plot
#boxplot(DO~Month_order, ylim=c(6,max(DO,na.rm=TRUE)+0.5),ylab="DO mg/L",xlab="Month",
#        main=paste("Dissolved Oxygen","-", stations2),lty=1,las=1,varwidth = TRUE,
#        cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
#CRITERIA: 
#  SUMMER_MAX	18
#  USE_CODE	3
#  SPN_FINAL	October 15-May 15
critx <- c(5,10) #REARING AND MIGRATION
crity <- c(8.,8.) #30-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(6.5,6.5) #7-day min mean criterion"
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(6.,6.) #Absolute min criterion
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(0,5) #SPAWNING
crity <- c(11.,11.) #7-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(9.,9.) #if IGDO >= 8.0 then 7-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(10,13) #Spawning Criteria
crity <- c(11.,11.) # 7-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(9.,9.) #if IGDO >= 8.0 then 7-day mean minimum as defined in OAR 340-41-006
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 7.5,labels = "Oct 15 to May 15 - Spawning",cex = 0.9, adj=c(0,0))
text(0.5, 7.25,labels = "7-day mean min = 11.0",cex = 0.9, adj=c(0,0))
text(0.5, 7.0,labels = " (if IGDO>8 then 9.0)",cex = 0.9, adj=c(0,0))
text(0.2, 6.65,labels = "May 15 to Oct 15 - Rearing/Migration",cex = 0.9, adj=c(0,0))
text(0.5, 6.4,labels = "30-day mean min = 8.0",cex = 0.9, adj=c(0,0))
text(0.5, 6.15,labels = "7-day min mean = 6.5",cex = 0.9, adj=c(0,0))
text(0.5, 5.9,labels = "Absolute min = 6.0",cex = 0.9, adj=c(0,0))
dev.off()

#Derive monthly mean DOs
str(d2A_wide)
d2A_wide$DO
tapply(d2A_wide$DO, d2A_wide$Month, mean, na.rm = TRUE)
#        1         2         3         4         5         6         7         8         9        10        11        12 
#11.854167 10.800000 11.610714 11.200000 10.104762  8.987500  8.225000  8.050000  8.541935 10.200000 11.043478 11.466667 

#Derive monthly 90th percentile DO
tapply(d2A_wide$DO, d2A_wide$Month, quantile, p=0.9, na.rm = TRUE)
#    1     2     3     4     5     6     7     8     9    10    11    12 
#12.51 11.60 12.00 11.36 11.20  9.57  8.90  8.53  9.20 10.96 11.56 11.85 

#Derive monthly median DO
tapply(d2A_wide$DO, d2A_wide$Month, quantile, p=0.5, na.rm = TRUE)
#    1     2     3     4     5     6     7     8     9    10    11    12 
#11.95 10.80 11.70 11.20 10.20  8.85  8.25  8.00  8.50 10.00 11.10 11.45 


#
#PERCENT DO SATURATION
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_DOPerSat.png",sep=""), width = 665, height = 500)
#plot(DOPerSat~Month_order,ylim=c(min(DOPerSat,na.rm=TRUE),max(DOPerSat,na.rm=TRUE)+0.5),ylab="DO % Sat",xlab="month",
#     main=paste("DO Percent Saturation","-", stations2),lty=1,las=1,varwidth = TRUE) # boxplot - min set to min
plot(d2A_wide$DOPerSat~Month_order,ylim=c(min(d2A_wide$DOPerSat,na.rm=TRUE)-5,max(d2A_wide$DOPerSat,na.rm=TRUE)+10),
     ylab="DO % Sat",xlab="Month",
     main=paste("DO Percent Saturation","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(5,10) #REARING AND MIGRATION
crity <- c(90.,90.) #90% sat applies if 30-day mean minimum < 8.0
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(0,5) #SPAWNING
crity <- c(95.,95.) #95% sat applies if 7-day mean minimum < 11.0/9.0
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(10,13) #Spawning Criteria
crity <- c(95.,95.)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 118,labels = "Oct 15 to May 15 - Spawning",cex = 0.9, adj=c(0,0))
text(0.5, 116.5,labels = "If DO<11.0 then 95% sat applies",cex = 0.9, adj=c(0,0))
text(0.2, 114,labels = "May 15 to Oct 15 - Rearing/Migration",cex = 0.9, adj=c(0,0))
text(0.5, 112.5,labels = "If DO<8.0 then 90% sat applies",cex = 0.9, adj=c(0,0))
dev.off()

#TEMPERATURE
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_Temperature.png",sep=""), width = 665, height = 500)
plot(d2A_wide$Temperature~Month_order,ylim=c(min(d2A_wide$Temperature,na.rm=TRUE)-5,max(d2A_wide$Temperature,na.rm=TRUE)+5),
     ylab="Celsius",xlab="Month",
     main=paste("Temperature","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(5,10) #REARING AND MIGRATION
crity <- c(18.,18.) 
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(0,5) #SPAWNING
crity <- c(13.,13.) #SPAWNING
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(10,13) #Spawning Criteria
crity <- c(13.,13.)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 30,labels = "May 15 to Oct 15 - Rearing/Migration BBNC = 18C",cex = 0.9, adj=c(0,0))
text(0.2, 28.5,labels = "Oct 15 to May 15 - Spawning BBNC = 13C",cex = 0.9, adj=c(0,0))
dev.off()



#Loading Capacity Calcs
d2A_wide_LC <- d2A_wide
d2A_wide_Summer_LC <- d2A_wide_Summer

#BOX PLOTS for loading capacity
Month_order_LC <- factor(d2A_wide_LC$Month,levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) #all months plotted, including those w/ no data

#Temperature reduced 3.8 C
d2A_wide_LC$Tminus3 <- d2A_wide_LC$Temperature - 3.8 #Use for loading capacity T to meet DO criteria plot
quantile(d2A_wide_LC$Temperature,c(0.10, .25, .50, .75, .9), na.rm = TRUE)
quantile(d2A_wide_LC$Temperature,c(0.10, .25, .50, .63, .75, .9), na.rm = TRUE) #37% exceed 18C currently
quantile(d2A_wide_LC$Tminus3,    c(0.10, .25, .50, .75, .9), na.rm = TRUE)
quantile(d2A_wide_LC$Tminus3,    c(0.10, .25, .50, .75, .89), na.rm = TRUE) #11% exceed 18C if T reduced 3.8C

#Temperature reduced 3.8 C during summer months and 1.9 C during non-summer months
head(d2A_wide_LC)
d2A_wide_LC$Tminus3b <- ifelse(d2A_wide_LC$Month < 6, d2A_wide_LC$Temperature - 3.8/2,
                               ifelse(d2A_wide_LC$Month < 10, d2A_wide_LC$Temperature - 3.8,
                                      d2A_wide_LC$Temperature - 3.8/2))

d2A_wide_Summer_LC$Tminus3 <- d2A_wide_Summer_LC$Temperature - 3.8 #Use for loading capacity T to meet DO criteria plot
quantile(d2A_wide_Summer_LC$Temperature,c(0.10, .25, .50, .75, .9), na.rm = TRUE) #90% exceed 18C during summer
quantile(d2A_wide_Summer_LC$Tminus3,    c(0.10, .25, .50, .72, .9), na.rm = TRUE) #28% exceed 18C during summer if T reduced 3.8C

#Derive monthly mean temperatures
str(d2A_wide)
d2A_wide$Temperature
tapply(d2A_wide$Temperature, d2A_wide$Month, mean, na.rm = TRUE)
#1         2         3         4         5         6         7         8         9        10        11        12 
#7.483333  7.000000  9.025000 11.200000 14.895238 19.187500 22.591667 20.750000 19.283871 13.600000 10.273913  8.966667 

tapply(d2A_wide_LC$Temperature, d2A_wide_LC$Month, mean, na.rm = TRUE)
#1         2         3         4         5         6         7         8         9        10        11        12 
#7.483333  7.000000  9.025000 11.200000 14.895238 19.187500 22.591667 20.750000 19.283871 13.600000 10.273913  8.966667 

tapply(d2A_wide_LC$Tminus3b, d2A_wide_LC$Month, mean, na.rm = TRUE)
#1         2         3         4         5         6         7         8         9        10        11        12 
#5.583333  5.100000  7.125000  9.300000 12.995238 15.387500 18.791667 16.950000 15.483871 11.700000  8.373913  7.066667 

#Derive monthly 90th percentile temperatures
tapply(d2A_wide$Temperature, d2A_wide$Month, quantile, p=0.9, na.rm = TRUE)
#1     2     3     4     5     6     7     8     9    10    11    12 
#9.14  8.20 10.33 11.36 18.80 22.80 24.94 21.73 21.50 15.00 11.44 10.00 


#TEMPERATURE and temperature minus 3.8 C
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_Temperature_LC.png",sep=""), width = 665, height = 500)
plot(d2A_wide_LC$Temperature~Month_order_LC,col="red",ylim=c(min(d2A_wide_LC$Temperature,na.rm=TRUE)-5,max(d2A_wide_LC$Temperature,na.rm=TRUE)+5),
     ylab="Celsius",xlab="Month",
     main=paste("Temperature","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
boxplot(d2A_wide_LC$Tminus3~Month_order_LC,col="blue",add=TRUE,ylim=c(min(d2A_wide_LC$Temperature,na.rm=TRUE)-5,max(d2A_wide_LC$Temperature,na.rm=TRUE)+5),
        ylab="Celsius",xlab="Month",
        main=paste("Temperature","-", stations2),lty=1,las=1,varwidth = TRUE,
        cex.axis=1.2, cex.lab=1.2) 
mtext("Observed Temperature and Observed Temperature minus 3.8 C",side = 3, line=0.5, cex=1.0, outer=FALSE)
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(5,10) #REARING AND MIGRATION
crity <- c(18.,18.) 
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(0,5) #SPAWNING
crity <- c(13.,13.) 
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(10,13) #Spawning Criteria
crity <- c(13.,13.)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 30,labels = "May 15 to Oct 15 - Rearing/Migration BBNC = 18C",cex = 0.9, adj=c(0,0))
text(0.2, 28.5,labels = "Oct 15 to May 15 - Spawning BBNC = 13C",cex = 0.9, adj=c(0,0))
dev.off()

#TEMPERATURE and temperature minus 3.8 C during summer and 1.9 C other months
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_Temperature_LC2.png",sep=""), width = 665, height = 500)
plot(d2A_wide_LC$Temperature~Month_order_LC,col="red",ylim=c(min(d2A_wide_LC$Temperature,na.rm=TRUE)-5,max(d2A_wide_LC$Temperature,na.rm=TRUE)+5),
     ylab="Celsius",xlab="Month",
     main=paste("Temperature","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) #Current Temperature
boxplot(d2A_wide_LC$Tminus3b~Month_order_LC,col="blue",add=TRUE,ylim=c(min(d2A_wide_LC$Temperature,na.rm=TRUE)-5,max(d2A_wide_LC$Temperature,na.rm=TRUE)+5),
        ylab="Celsius",xlab="Month",
        main=paste("Temperature","-", stations2),lty=1,las=1,varwidth = TRUE,
        cex.axis=1.2, cex.lab=1.2) 
mtext("Current T and T minus 3.8 C in summer, 1.9 C other seasons",side = 3, line=0.5, cex=1.0, outer=FALSE)
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(5,10) #REARING AND MIGRATION
crity <- c(18.,18.) 
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(0,5) #SPAWNING
crity <- c(13.,13.) 
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
critx <- c(10,13) #Spawning Criteria
crity <- c(13.,13.)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 30,labels = "May 15 to Oct 15 - Rearing/Migration BBNC = 18C",cex = 0.9, adj=c(0,0))
text(0.2, 28.5,labels = "Oct 15 to May 15 - Spawning BBNC = 13C",cex = 0.9, adj=c(0,0))
text(0.2, 27,labels = "T reduced 3.8C summer and 1.9C fall-spring",cex = 0.9, adj=c(0,0))
dev.off()


#Derive monthly mean temperatures
str(d2A_wide)
tapply(d2A_wide$Temperature, d2A_wide$Month, mean, na.rm = TRUE)

#Derive monthly mean TOC
tapply(d2A_wide$TOC, d2A_wide$Month, mean, na.rm = TRUE)


#PH
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_pH.png",sep=""), width = 665, height = 500)
plot(d2A_wide$pH~Month_order,ylim=c(min(d2A_wide$pH,na.rm=TRUE)-.5,max(d2A_wide$pH,na.rm=TRUE)+0.5),
     ylab="pH su",xlab="Month",
     main=paste("pH","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
#Water Quality Standards and Policies for South Coast Basin
#pH (Hydrogen ion concentration) pH values may not fall outside the following ranges:
#  Estuarine and fresh waters: 6.5-8.5.
critx <- c(0,13)
crity <- c(8.5,8.5)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
crity <- c(6.5,6.5)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.2, 6.08,labels = "pH may not fall outside range 6.5 to 8.5",cex = 0.9, adj=c(0,0))
text(0.2, 6.0,labels = "(South Coast Basin - Estuarine and fresh waters)",cex = 0.9, adj=c(0,0))
dev.off()

#OrthophosphateP
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_OrthophosphateP.png",sep=""), width = 665, height = 500)
plot(d2A_wide$OrthophosphateP~Month_order,
     ylim=c(min(d2A_wide$OrthophosphateP,na.rm=TRUE)-0.0,max(d2A_wide$OrthophosphateP,na.rm=TRUE)+0.01),
     ylab="mg/L",xlab="Month",
     main=paste("Orthophosphate, as P","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(0,13)
crity <- c(0.005,0.005)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 0.028,labels = "0.005 mg/L (5 ug/L) is est. conc. at",cex = 0.9, adj=c(0,0))
text(0.5, 0.027,labels = "which P limitation may be significant",cex = 0.9, adj=c(0,0))
dev.off()

#Derive monthly mean OrthophosphateP
tapply(d2A_wide$OrthophosphateP, d2A_wide$Month, mean, na.rm = TRUE)
#          1           2           3           4           5           6           7           8           9          10          11          12 
#0.007318182 0.006000000 0.006923077 0.004750000 0.003690476 0.004571429 0.003791667 0.003562500 0.003550000 0.008666667 0.005868421 0.010600000 
 

#TP
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TP.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TP~Month_order,ylim=c(min(d2A_wide$TP,na.rm=TRUE)-0.0,max(d2A_wide$TP,na.rm=TRUE)+0.0),
     ylab="mg/L",xlab="Month",
     main=paste("Total Phosphorus","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()

#Derive monthly mean TP
tapply(d2A_wide$TP, d2A_wide$Month, mean, na.rm = TRUE)


png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TP2.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TP~Month_order,ylim=c(min(d2A_wide$TP,na.rm=TRUE)-0.0, 0.3),
     ylab="mg/L",xlab="Month",
     main=paste("Total Phosphorus","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
text(0.5, 0.27,labels = "one high TP value of 0.83 mg/L not shown",cex = 0.9, adj=c(0,0))
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TP3.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TP~Month_order,ylim=c(min(d2A_wide$TP,na.rm=TRUE)-0.0, 0.3),
     ylab="mg/L",xlab="Month",
     main=paste("Total Phosphorus","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
mrlx <- c(0,13)
mrly <- c(0.01,0.01)
lines(mrlx,mrly, type="l", lty=2, lwd=2, col="black")
text(0.5, 0.28,labels = "one high TP value of 0.83 mg/L not shown",cex = 0.9, adj=c(0,0))
text(0.5, 0.26,labels = "0.01 mg/L (10 ug/L) is minimum reporting level",cex = 0.9, adj=c(0,0))
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TP_log.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TP~Month_order,ylim=c(min(d2A_wide$TP,na.rm=TRUE)-0.0, 0.3),
     log="y",ylab="mg/L",xlab="Month",
     main=paste("Total Phosphorus","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
text(0.5, 0.27,labels = "one high TP value of 0.83 mg/L not shown",cex = 0.9, adj=c(0,0))
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TP_LC16_log.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TP~Month_order,ylim=c(min(d2A_wide$TP,na.rm=TRUE)-0.0, 0.3),
     log="y",ylab="mg/L",xlab="Month",
     main=paste("Total Phosphorus","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
mrlx <- c(0,13)
mrly <- c(0.01,0.01)
lines(mrlx,mrly, type="l", lty=2, lwd=2, col="black")
LCx <- c(6,10)
LCy <- c(0.0163,0.0163)
lines(LCx,LCy, type="l", lty=2, lwd=2, col="red")
text(0.5, 0.28,labels = "one high TP value of 0.83 mg/L not shown",cex = 0.9, adj=c(0,0))
text(0.5, 0.24,labels = "0.010 mg/L (10 ug/L) is minimum reporting level",cex = 0.9, adj=c(0,0))
text(0.5, 0.20,labels = "0.0163 mg/L (16.3 ug/L) is pH criteria based Loading Capacity",cex = 0.9, adj=c(0,0))
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TP_LC17_log.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TP~Month_order,ylim=c(min(d2A_wide$TP,na.rm=TRUE)-0.0, 0.3),
     log="y",ylab="mg/L",xlab="Month",
     main=paste("Total Phosphorus","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
mrlx <- c(0,13)
mrly <- c(0.01,0.01)
lines(mrlx,mrly, type="l", lty=2, lwd=2, col="black")
LCx <- c(6,10)
LCy <- c(0.017,0.017)
lines(LCx,LCy, type="l", lty=2, lwd=2, col="red")
text(0.5, 0.28,labels = "one high TP value of 0.83 mg/L not shown",cex = 0.9, adj=c(0,0))
text(0.5, 0.24,labels = "0.010 mg/L (10 ug/L) is minimum reporting level",cex = 0.9, adj=c(0,0))
#text(0.5, 0.20,labels = "0.0163 mg/L (16.3 ug/L) is pH criteria based Loading Capacity",cex = 0.9, adj=c(0,0))
text(0.5, 0.20,labels = "0.017 mg/L (17 ug/L) is pH criteria based Loading Capacity",cex = 0.9, adj=c(0,0))
dev.off()

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TP_LC11and17_log.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TP~Month_order,ylim=c(min(d2A_wide$TP,na.rm=TRUE)-0.0, 0.3),
     log="y",ylab="mg/L",xlab="Month",
     main=paste("Total Phosphorus","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
mrlx <- c(0,13)
mrly <- c(0.01,0.01)
lines(mrlx,mrly, type="l", lty=2, lwd=2, col="black")
LCx <- c(6,10)
LCy <- c(0.017,0.017)
lines(LCx,LCy, type="l", lty=2, lwd=2, col="red")
LCy <- c(0.011,0.011)
lines(LCx,LCy, type="l", lty=2, lwd=2, col="red")
text(0.5, 0.28,labels = "one high TP value of 0.83 mg/L not shown",cex = 0.9, adj=c(0,0))
text(0.5, 0.24,labels = "0.010 mg/L (10 ug/L) is minimum reporting level",cex = 0.9, adj=c(0,0))
#text(0.5, 0.20,labels = "0.0163 mg/L (16.3 ug/L) is pH criteria based Loading Capacity",cex = 0.9, adj=c(0,0))
text(0.5, 0.20,labels = "0.017 mg/L (17 ug/L) is pH criteria based Loading Capacity",cex = 0.9, adj=c(0,0))
text(0.5, 0.17,labels = "0.011 mg/L (17 ug/L) is 30% reduction based Loading Capacity",cex = 0.9, adj=c(0,0))
dev.off()



png(file=paste(outpath,stations,"_","MonthlyBoxPlot_Chlorophylla.png",sep=""), width = 665, height = 500)
plot(d2A_wide$Chlorophylla~Month_order,ylim=c(min(d2A_wide$Chlorophylla,na.rm=TRUE)-0.0,15),
     ylab="mg/L",xlab="Month",
     main=paste("Chlorophyll a","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(0,13)
crity <- c(15,15)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 14.5,labels = "15 ug/L is action level",cex = 0.9, adj=c(0,0))
dev.off()

#Derive monthly mean Chlorophylla
tapply(d2A_wide$Chlorophylla, d2A_wide$Month, mean, na.rm = TRUE)
#  1         2         3         4         5         6         7         8         9        10        11        12 
#NaN       NaN       NaN       NaN 0.5888889 1.0428571 0.8229167 0.4000000 0.9214286 0.6000000 0.2000000       NaN 


png(file=paste(outpath,stations,"_","MonthlyBoxPlot_BOD5.png",sep=""), width = 665, height = 500)
plot(d2A_wide$BOD5~Month_order,ylim=c(min(d2A_wide$BOD5,na.rm=TRUE)-0.0,max(d2A_wide$BOD5,na.rm=TRUE)+0.0),
     ylab="mg/L",xlab="Month",
     main=paste("BOD5","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()

#Derive monthly mean BOD5
tapply(d2A_wide$BOD5, d2A_wide$Month, mean, na.rm = TRUE)
#        1         2         3         4         5         6         7         8         9        10        11        12 
#1.0739130 1.1000000 0.9464286 1.4000000 0.7200000 0.5714286 0.6187500 0.3071429 0.7016667 0.7333333 1.1090909 1.0666667 
 
#Derive monthly mean COD
tapply(d2A_wide$COD, d2A_wide$Month, mean, na.rm = TRUE)
#       1        2        3        4        5        6        7        8        9       10       11       12 
#4.000000 2.500000 4.184211 2.500000 5.136364 3.142857 4.156250 2.812500 3.636364 5.166667 8.107143 8.833333 


png(file=paste(outpath,stations,"_","MonthlyBoxPlot_TOC.png",sep=""), width = 665, height = 500)
plot(d2A_wide$TOC~Month_order,ylim=c(min(d2A_wide$TOC,na.rm=TRUE)-0.0,max(d2A_wide$TOC,na.rm=TRUE)+0.0),
     ylab="mg/L",xlab="Month",
     main=paste("TOC","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()

#Derive monthly mean TOC
tapply(d2A_wide$TOC, d2A_wide$Month, mean, na.rm = TRUE)

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_AmmoniaN.png",sep=""), width = 665, height = 500)
plot(d2A_wide$AmmoniaN~Month_order,ylim=c(min(d2A_wide$AmmoniaN,na.rm=TRUE)-0.0,max(d2A_wide$AmmoniaN,na.rm=TRUE)+0.0),
     ylab="mg/L",xlab="Month",
     main=paste("Ammonia, as N","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()

#Derive monthly mean AmmoniaN
tapply(d2A_wide$AmmoniaN, d2A_wide$Month, mean, na.rm = TRUE)



#Alkalinity
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_Alkalinity.png",sep=""), width = 665, height = 500)
plot(d2A_wide$Alkalinity~Month_order,ylim=c(min(d2A_wide$Alkalinity,na.rm=TRUE)-5,max(d2A_wide$Alkalinity,na.rm=TRUE)+5),
     ylab="Alkalinity as CaCO3 mg/L",xlab="Month",
     main=paste("Alkalinity as Calcium Carbonate","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()

#Flow
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_QatSFCoqPowers.png",sep=""), width = 665, height = 500)
plot(d2A_wide$QatSFCoqPowers~Month_order,ylim=c(min(d2A_wide$QatSFCoqPowers,na.rm=TRUE)-5,max(d2A_wide$QatSFCoqPowers,na.rm=TRUE)+5),
     ylab="cfs",xlab="Month",
     main=paste("SF Coquille R at Powers Discharge","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()
#Derive monthly means (for report)
xf <- data.frame(d2A_wide$QatSFCoqPowers,Month_orderA)
tapply(d2A_wide$QatSFCoqPowers,Month_orderA, mean, simplify = TRUE)
tapply(d2A_wide$QatSFCoqPowers,Month_orderA, median, simplify = TRUE)

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_QatSFCoqPowers_Log.png",sep=""), width = 665, height = 500)
plot(d2A_wide$QatSFCoqPowers~Month_order,ylim=c(min(d2A_wide$QatSFCoqPowers,na.rm=TRUE)-5,max(d2A_wide$QatSFCoqPowers,na.rm=TRUE)+5),
     log="y", ylab="cfs",xlab="Month",
     main=paste("SF Coquille R at Powers Discharge","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
dev.off()


#Derive dissolved inorganic nitrogen for observations where both ammonia and nitrate/nitrite measured
d2A_wide_DIN_1 <- d2A_wide #Calculate dissolved inorganic nitrogen in order to generate box plot
d2A_wide_DIN_2 <- subset(d2A_wide_DIN_1, AmmoniaN != "NA"); str(d2A_wide_DIN_2)
d2A_wide_DIN <- subset(d2A_wide_DIN_2, NO23N != "NA"); str(d2A_wide_DIN)
d2A_wide_DIN$DIN <- d2A_wide_DIN$AmmoniaN + d2A_wide_DIN$NO23N; d2A_wide_DIN$DIN
#Plot DIN
head(d2A_wide_DIN)
#detach()
#attach(d2A_wide_DIN) # d2A_wide_DIN$
Month_orderAN <- factor(d2A_wide_DIN$Month,levels=c(1:12), ordered=TRUE)#all months plotted, including those w/ no data
Month_orderN <- factor(d2A_wide_DIN$Month,levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) #all months plotted, including those w/ no data
png(file=paste(outpath,stations,"_","MonthlyBoxPlot_DIN.png",sep=""), width = 665, height = 500)
plot(d2A_wide_DIN$DIN~Month_orderN,ylim=c(min(d2A_wide_DIN$DIN,na.rm=TRUE)-0.0, 0.6),
     ylab="DIN mg/L",xlab="Month",
     main=paste("Dissolved Inorganic N","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(0,13)
crity <- c(0.05,0.05)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 0.02,labels = "0.05 mg/L (50 ug/L) is est. conc. below",cex = 0.9, adj=c(0,0))
text(0.5, 0.00,labels = "which N limitation may be significant",cex = 0.9, adj=c(0,0))
dev.off()

#Derive monthly mean NO23N
tapply(d2A_wide$NO23N, d2A_wide$Month, mean, na.rm = TRUE)
#          1           2           3           4           5           6           7           8           9          10          11          12 
#0.173808696 0.250000000 0.138167857 0.092800000 0.036323810 0.028571429 0.007345833 0.004375000 0.007741935 0.066666667 0.174130435 0.303333333 
# 


#Generate box plot for DIN to PO4P ratio
d2A_wide_NtoP <- subset(d2A_wide_DIN, OrthophosphateP != "NA"); str(d2A_wide_NtoP)
d2A_wide_NtoP$NtoPratio <- d2A_wide_NtoP$DIN / d2A_wide_NtoP$OrthophosphateP
d2A_wide_NtoP$NtoPratio

Month_orderAN <- factor(d2A_wide_NtoP$Month,levels=c(1:12), ordered=TRUE)#all months plotted, including those w/ no data
Month_orderN <- factor(d2A_wide_NtoP$Month,levels=c(1:12), labels=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"),ordered=TRUE) #all months plotted, including those w/ no data

png(file=paste(outpath,stations,"_","MonthlyBoxPlot_NtoP.png",sep=""), width = 665, height = 500)
plot(d2A_wide_NtoP$NtoPratio~Month_orderN,ylim=c(0.0, 100.),
     ylab="DIN:PO4P ratio",xlab="Month",
     main=paste("Dissolved Inorganic N to Dissolved Orthophosphate P","-", stations2),lty=1,las=1,varwidth = TRUE,
     cex.axis=1.2, cex.lab=1.2) 
grid(col = "lightgray", lty = "dotted", lwd = 1)
critx <- c(0,13)
crity <- c(7,7)
lines(critx,crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 10,labels = "Ratio >> 7 P limited",cex = 0.9, adj=c(0,0))
text(0.5, 3,labels = "Ratio << 7 N limited",cex = 0.9, adj=c(0,0))
dev.off()

#SPAWNING PERIOD ANALYSIS -  OCT 15 THRU DEC 31
#before Oct 15 tends to be more like summer - low DO in early Oct
d2A_wide_F <- subset(d2A_wide, d2A_wide$Jday > 287)
str(d2A_wide_F) #Fall spawing dataset


# View(d2A_wide_F)
#Fall spawning period data has no missing AmmoniaN or NO23N values so can calculate DIN for all records
d2A_wide_F$DIN <- d2A_wide_F$AmmoniaN + d2A_wide_F$NO23N ; d2A_wide_F$DIN
colnames(d2A_wide_F)

#Mean TOC for Oct 15 thru Dec 31
d2A_wide_F$TOC
mean(d2A_wide_F$TOC, na.rm = TRUE) 
median(d2A_wide_F$TOC, na.rm = TRUE) 

#Mean TP for Summer - Jun Jul Aug Sep

d2A_wide_Summer$TP
#Few non-detects for TP, MRL 0.01, values 0.005 are 1/2 MRL
xxx <- mean(d2A_wide_Summer$TP, na.rm = TRUE); xxx
0.7*xxx
median(d2A_wide_Summer$TP, na.rm = TRUE) 
quantile(d2A_wide_Summer$TP,c(0.10, .25, .50, .75, .9), na.rm = TRUE)
quantile(d2A_wide_Summer$TP,c(0.10, .25, .50, .54, .55, .9), na.rm = TRUE)
#slightly less than half of values during summer exceed 16.3 ug/L

#Mean TP for 2013-09-18 15:24:00 and before

# dDOspawnF2 <- subset(dDOspawnF, dDOspawnF$Temperature < 15.0); str(dDOspawnF2)
str(d2A_wide_Summer)
xxx <- subset(d2A_wide_Summer, d2A_wide_Summer$DateTimect < "2013-09-19 00:00:00")
d2A_wide_SummerX <- d2A_wide_Summer
d2A_wide_SummerX$DATE <- as.Date(d2A_wide_Summer$DateTimect)
d2A_wide_SummerX2 <- subset(d2A_wide_SummerX, DATE < "2011-09-19")
str(d2A_wide_SummerX2)
mean(d2A_wide_SummerX2$TP, na.rm = TRUE)
quantile(d2A_wide_SummerX2$TP,c(0.10, .25, .50, .75, .9), na.rm = TRUE)

d2A_wide_SummerX2$TP_LC <- 0.7 * d2A_wide_SummerX2$TP
mean(d2A_wide_SummerX2$TP_LC, na.rm = TRUE)
quantile(d2A_wide_SummerX2$TP_LC,c(0.10, .25, .50, .75, .9), na.rm = TRUE)

#d1A$DateTimelt$min

as.Date(d2A_wide_Summer$DateTimect)


d2A_wide_Summer$OrthophosphateP
#MRL generally 0.05 mg/L for OrthophosphateP, values 0.025 mg/L are 1/2 MRL, many non-detects
mean(d2A_wide_Summer$OrthophosphateP, na.rm = TRUE) 
median(d2A_wide_Summer$OrthophosphateP, na.rm = TRUE) 
quantile(d2A_wide_Summer$OrthophosphateP,c(0.10, .25, .50, .75, .9), na.rm = TRUE)



#DO HourPST Temperature QatSFCoqPowers BOD5 OrthophosphateP TP AmmoniaN NitrateNitriteN TOC TSS Turbidity Jday      
#dDOspawnFW <-  d2A_wide_F[,c(9, 26, 17, 16, 5, 14, 21, 4, 13, 19, 20, 22, 23, 25)] #added Month and Jday as potential explanatory variables
#DO Time Temperature QatSFCoqPowers BOD5 OrthophosphateP TP DIN TOC TSS Turbidity Jday      
dDOspawnF <-        d2A_wide_F[,c(10,25,17,16,6,14,20,29,19,21,22,24)] 
str(dDOspawnF) # F for Fall
#Also add DOPerSat
dDOPERSATspawnF <-  d2A_wide_F[,c(11,25,17,16,6,14,20,29,19,21,22,24)] 
str(dDOPERSATspawnF) # F for Fall 

#DELETE ONE HIGH T VALUE 15.0 C
#Remove it since more like summer and not representative of fall condition being analyzed
dDOspawnF2 <- subset(dDOspawnF, dDOspawnF$Temperature < 15.0); str(dDOspawnF2)
dDOPERSATspawnF2 <- subset(dDOPERSATspawnF, dDOPERSATspawnF$Temperature < 15.0)

#ggpairs of DO HourPST Temperature QatSFCoqPowers BOD5 OrthophosphateP TP DIN TOC TSS Turbidity Jday      
png(file=paste(outpath,stations,"_","ggpairs_DO_spawnF2.png",sep=""), width = 650, height = 650)
ggpairs(data=dDOspawnF2, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()

#ggpairs of DOPerSat HourPST Temperature QatSFCoqPowers BOD5 OrthophosphateP TP DIN TOC TSS Turbidity Jday      
png(file=paste(outpath,stations,"_","ggpairs_DOPerSat_spawnF2.png",sep=""), width = 650, height = 650)
ggpairs(data=dDOPERSATspawnF2, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()

#TRANSFORMATIONS
#DO
#One high T record removed
par(mfrow=c(2,2))
hist(dDOspawnF2$DO, main = "Histogram of DO")
qqPlot(dDOspawnF2$DO, dist="norm", main = "Normal QQ Plot of DO" ) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDOspawnF2$DO) #if p-value > 0.05 then normally distributed - NORMALLY DISTRIBUTED
bc <- boxcox(DO~., data=dDOspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- 0.9
hist(dDOspawnF2$DO^lambda, main = c("Normal QQ Plot of DO^",lambda))
qqPlot(dDOspawnF2$DO^lambda, dist="norm", main = c("Normal QQ Plot of DO^",lambda)) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDOspawnF2$DO^lambda) #p-value > 0.05, normally distributed
lambda <- -1
hist(dDOspawnF2$DO^lambda, main = c("Normal QQ Plot of DO^",lambda))
qqPlot(dDOspawnF2$DO^lambda, dist="norm", main = c("Normal QQ Plot of DO^",lambda)) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDOspawnF2$DO^lambda) #p-value > 0.05, normally distributed
#PLOT OPTION 1
png(file=paste(outpath,stations,"_","DO_SpawnOct15toDec31_HistandQQ.png",sep=""), width = 650, height = 350)
par(mfrow=c(1,2))
hist(dDOspawnF2$DO, main = "Histogram of DO")
qqPlot(dDOspawnF2$DO, dist="norm", main = "Normal QQ Plot of DO" ) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDOspawnF2$DO) #if p-value > 0.05, then normally distributed
dev.off()
#PLOT OPTION 2
png(file=paste(outpath,stations,"_","DO_SpawnOct15toDec31_HistandQQ_v2.png",sep=""), width = 650, height = 350)
par(mfrow=c(1,2))
x2 <- c(9.75,10,10.25,10.5,10.75,11,11.25,11.5,11.75,12,12.25); x2
hist(dDOspawnF2$DO, breaks = x2, main = "Histogram of DO")
qqPlot(dDOspawnF2$DO, dist="norm", main = "Normal QQ Plot of DO" ) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDOspawnF2$DO) #if p-value > 0.05, then normally distributed
dev.off()
#PLOT OPTION 3 - DON'T GENERATE THIS ONE - KEEP FOR REFERENCE
png(file=paste(outpath,stations,"_","DO_SpawnOct15toDec31_HistandQQ_trans.png",sep=""), width = 650, height = 650)
par(mfrow=c(2,2))
hist(dDOspawnF2$DO, main = "Histogram of DO")
qqPlot(dDOspawnF2$DO, dist="norm", main = "Normal QQ Plot of DO" ) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDOspawnF2$DO) #if p-value > 0.05, then normally distributed
lambda <- -1
hist(dDOspawnF2$DO^lambda, main = c("Normal QQ Plot of DO^",lambda))
qqPlot(dDOspawnF2$DO^lambda, dist="norm", main = c("Normal QQ Plot of DO^",lambda)) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDOspawnF2$DO^lambda) #p-value > 0.05, normally distributed
dev.off()
#DO is normally distributed - DO NOT TRANSFORM

#Check DO percent saturation
#dDOPERSATspawnF2
#DOPerSat
par(mfrow=c(2,2))
hist(dDOPERSATspawnF2$DOPerSat, main = "Histogram of DOPerSat")
qqPlot(dDOPERSATspawnF2$DOPerSat, dist="norm", main = "Normal QQ Plot of DOPerSat" ) #Quantile-Comparison Plots - via "car" package
shapiro.test(dDOPERSATspawnF2$DOPerSat) #if p-value > 0.05 then normally distributed - NORMALLY DISTRIBUTED
png(file=paste(outpath,stations,"_","DOPerSat_SpawnOct15toDec31_HistandQQ_v2.png",sep=""), width = 650, height = 350)
par(mfrow=c(1,2))
hist(dDOPERSATspawnF2$DOPerSat, main = "Histogram of DOPerSat")
qqPlot(dDOPERSATspawnF2$DOPerSat, dist="norm", main = "Normal QQ Plot of DOPerSat" ) #Quantile-Comparison Plots - via "car" package
dev.off()
bc <- boxcox(DOPerSat~., data=dDOPERSATspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- 2
hist(dDOPERSATspawnF2$DOPerSat^lambda, main = c("Normal QQ Plot of DOPerSat^",lambda))
qqPlot(dDOPERSATspawnF2$DOPerSat^lambda, dist="norm", main = c("Normal QQ Plot of DOPerSat^",lambda))
shapiro.test(dDOPERSATspawnF2$DOPerSat^lambda) #if p-value > 0.05 then normally distributed - NORMALLY DISTRIBUTED
#Transforming DOPerSat does not significantly improve normality
#Do not transform DOPerSat

#QatSFCoqPowers
#with high T record removed
par(mfrow=c(2,2))
hist(dDOspawnF2$QatSFCoqPowers, main = "Histogram of QatSFCoqPowers")
qqPlot(dDOspawnF2$QatSFCoqPowers, dist="norm", main = "Normal QQ Plot of QatSFCoqPowers" ) 
shapiro.test(dDOspawnF2$QatSFCoqPowers) #if p-value > 0.05, then normally distributed
bc <- boxcox(QatSFCoqPowers~., data=dDOspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda # lambda close to 0 so log transform
# trans X = X^Lambda
# Lambda     Transformation             Transformation
#  0         Log                        log(Y)
#  0.33      cube root                  Y^0.33
#  0.5       square root                Y^.5
#  1         linear (no transformation) Y
# -1         reciprical                 1/Y
#bc <- boxcox(DO~., data=dDOspawnF2, lambda=seq(0,2,by=.1))
#lambda <- 0 #LOG
#PLOT 
png(file=paste(outpath,stations,"_","Q_SpawnOct15toDec31_HistandQQ_trans.png",sep=""), width = 650, height = 650)
par(mfrow=c(2,2))
hist(dDOspawnF2$QatSFCoqPowers, main = "Histogram of Q")
qqPlot(dDOspawnF2$QatSFCoqPowers, dist="norm", main = "Normal QQ Plot of Q" )
shapiro.test(dDOspawnF2$QatSFCoqPowers) #if p-value > 0.05 then normally distributed - NOT NORMALLY DISTRIBUTE
hist(log(dDOspawnF2$QatSFCoqPowers), main = "Normal QQ Plot of log Q")
qqPlot(log(dDOspawnF2$QatSFCoqPowers), dist="norm", main = "Normal QQ Plot of log Q") #Quantile-Comparison Plots - via "car" package
shapiro.test(log(dDOspawnF2$QatSFCoqPowers)) #if p-value > 0.05 then normally distributed - NORMALLY DISTRIBUTED
dev.off()
#LOG TRANSFORM QatSFCoqPowers (already transformed)
dDOspawnF2$transQ <- log(dDOspawnF2$QatSFCoqPowers)
dDOPERSATspawnF2$transQ <- log(dDOPERSATspawnF2$QatSFCoqPowers)

#BOD5
par(mfrow=c(2,2))
hist(dDOspawnF2$BOD5, main = "Histogram of QatSFCoqPowers")
qqPlot(dDOspawnF2$BOD5, dist="norm", main = "Normal QQ Plot of QatSFCoqPowers" ) 
shapiro.test(dDOspawnF2$BOD5) #if p-value > 0.05, then normally distributed
bc <- boxcox(BOD5~., data=dDOspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- .30
hist(dDOspawnF$BOD5^lambda, main = c("Normal QQ Plot of BOD5^",lambda))
qqPlot(dDOspawnF$BOD5^lambda, dist="norm", main = c("Normal QQ Plot of BOD5^",lambda))
shapiro.test(dDOspawnF$BOD5^lambda)
#no improvement
#DO NOT TRANSFORM BOD5

#OrthophosphateP
par(mfrow=c(2,2))
hist(dDOspawnF2$OrthophosphateP, main = "Histogram of PO4P")
qqPlot(dDOspawnF2$OrthophosphateP, dist="norm", main = "Normal QQ Plot of PO4P" ) 
shapiro.test(dDOspawnF2$OrthophosphateP) #if p-value > 0.05, then normally distributed
bc <- boxcox(OrthophosphateP~., data=dDOspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- .6
hist(dDOspawnF2$OrthophosphateP^lambda, main = c("Normal QQ Plot of PO4P^",lambda))
qqPlot(dDOspawnF2$OrthophosphateP^lambda, dist="norm", main = c("Normal QQ Plot of PO4P^",lambda))
shapiro.test(dDOspawnF2$OrthophosphateP^lambda)
lambda <- .5
hist(dDOspawnF2$OrthophosphateP^lambda, main = c("Normal QQ Plot of PO4P^",lambda))
qqPlot(dDOspawnF2$OrthophosphateP^lambda, dist="norm", main = c("Normal QQ Plot of PO4P^",lambda))
shapiro.test(dDOspawnF2$OrthophosphateP^lambda)
hist(log(dDOspawnF2$OrthophosphateP), main = "Normal QQ Plot of log PO4P")
qqPlot(log(dDOspawnF2$OrthophosphateP), dist="norm", main = "Normal QQ Plot of log PO4P")
shapiro.test(log(dDOspawnF2$OrthophosphateP))
#Cannot be transformed to normality
#Several records lack PO4P values
#DO NOT TRANSFORM OrthophosphateP

#TP
par(mfrow=c(2,2))
hist(dDOspawnF2$TP, main = "Histogram of TP")
qqPlot(dDOspawnF2$TP, dist="norm", main = "Normal QQ Plot of TP" ) 
shapiro.test(dDOspawnF2$TP) #if p-value > 0.05, then normally distributed
bc <- boxcox(TP~., data=dDOspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- .5
hist(dDOspawnF2$TP^lambda, main = c("Normal QQ Plot of TP^",lambda))
qqPlot(dDOspawnF2$TP^lambda, dist="norm", main = c("Normal QQ Plot of TP^",lambda))
shapiro.test(dDOspawnF2$TP^lambda)
lambda <- .33
hist(dDOspawnF2$TP^lambda, main = c("Normal QQ Plot of TP^",lambda))
qqPlot(dDOspawnF2$TP^lambda, dist="norm", main = c("Normal QQ Plot of TP^",lambda))
shapiro.test(dDOspawnF2$TP^lambda)
hist(log(dDOspawnF2$TP), main = "Normal QQ Plot of log TP")
qqPlot(log(dDOspawnF2$TP), dist="norm", main = "Normal QQ Plot of log TP")
shapiro.test(log(dDOspawnF2$TP))
#Log transforms to normality
#Note one particulary high TP value
dDOspawnF2$TP
dDOspawnF2b <- subset(dDOspawnF2, dDOspawnF2$TP < 0.83) #Remove high value
dDOspawnF2b$TP
par(mfrow=c(2,2))
hist(dDOspawnF2b$TP, main = "Histogram of TP")
qqPlot(dDOspawnF2b$TP, dist="norm", main = "Normal QQ Plot of TP" ) 
shapiro.test(dDOspawnF2b$TP) #if p-value > 0.05, then normally distributed
bc <- boxcox(TP~., data=dDOspawnF2b)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- .5
hist(dDOspawnF2b$TP^lambda, main = c("Normal QQ Plot of TP^",lambda))
qqPlot(dDOspawnF2b$TP^lambda, dist="norm", main = c("Normal QQ Plot of TP^",lambda))
shapiro.test(dDOspawnF2b$TP^lambda)
lambda <- .33
hist(dDOspawnF2b$TP^lambda, main = c("Normal QQ Plot of TP^",lambda))
qqPlot(dDOspawnF2b$TP^lambda, dist="norm", main = c("Normal QQ Plot of TP^",lambda))
shapiro.test(dDOspawnF2b$TP^lambda)
hist(log(dDOspawnF2b$TP), main = "Normal QQ Plot of log TP")
qqPlot(log(dDOspawnF2b$TP), dist="norm", main = "Normal QQ Plot of log TP")
shapiro.test(log(dDOspawnF2b$TP))
#PLOT 
png(file=paste(outpath,stations,"_","TP_SpawnOct15toDec31_HistandQQ_trans.png",sep=""), width = 650, height = 650)
par(mfrow=c(2,2))
hist(dDOspawnF2$TP, main = "Histogram of TP")
qqPlot(dDOspawnF2$TP, dist="norm", main = "Normal QQ Plot of TP" )
shapiro.test(dDOspawnF2$TP) #if p-value > 0.05 then normally distributed - NOT NORMALLY DISTRIBUTE
hist(log(dDOspawnF2$TP), main = "Normal QQ Plot of log TP")
qqPlot(log(dDOspawnF2$TP), dist="norm", main = "Normal QQ Plot of log TP")
shapiro.test(log(dDOspawnF2$TP)) #if p-value > 0.05 then normally distributed - NORMALLY DISTRIBUTED
dev.off()
#Do not remove high TP value
#LOG TRANSFORM TP
dDOspawnF2$transTP <- log(dDOspawnF2$TP)
dDOPERSATspawnF2$transTP <- log(dDOPERSATspawnF2$TP)

#DIN
par(mfrow=c(2,2))
hist(dDOspawnF2$DIN, main = "Histogram of DIN")
qqPlot(dDOspawnF2$DIN, dist="norm", main = "Normal QQ Plot of DIN" ) 
shapiro.test(dDOspawnF2$DIN) #if p-value > 0.05, then normally distributed
bc <- boxcox(DIN~., data=dDOspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- .7
hist(dDOspawnF2$DIN^lambda, main = c("Normal QQ Plot of DIN^",lambda))
qqPlot(dDOspawnF2$DIN^lambda, dist="norm", main = c("Normal QQ Plot of DIN^",lambda))
shapiro.test(dDOspawnF2$DIN^lambda)
lambda <- .8
hist(dDOspawnF2$DIN^lambda, main = c("Normal QQ Plot of DIN^",lambda))
qqPlot(dDOspawnF2$DIN^lambda, dist="norm", main = c("Normal QQ Plot of DIN^",lambda))
shapiro.test(dDOspawnF2$DIN^lambda)
hist(log(dDOspawnF2$DIN), main = "Normal QQ Plot of log DIN")
qqPlot(log(dDOspawnF2$DIN), dist="norm", main = "Normal QQ Plot of log DIN")
shapiro.test(log(dDOspawnF2$DIN))
#DIN normally distributed
#Normality improved with Lambda=0.7
#0.7 close to 1 so DO NOT TRANSFORM DIN

#TOC
par(mfrow=c(2,2))
hist(dDOspawnF2$TOC, main = "Histogram of TOC")
qqPlot(dDOspawnF2$TOC, dist="norm", main = "Normal QQ Plot of TOC" ) 
shapiro.test(dDOspawnF2$TOC) #if p-value > 0.05, then normally distributed
bc <- boxcox(TOC~., data=dDOspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- .5
par(mfrow=c(2,2))
hist(dDOspawnF2$TOC^lambda, main = c("Normal QQ Plot of TOC^",lambda))
qqPlot(dDOspawnF2$TOC^lambda, dist="norm", main = c("Normal QQ Plot of TOC^",lambda))
shapiro.test(dDOspawnF2$TOC^lambda)
lambda <- .5
hist(dDOspawnF2$TOC^lambda, main = c("Normal QQ Plot of TOC^",lambda))
qqPlot(dDOspawnF2$TOC^lambda, dist="norm", main = c("Normal QQ Plot of TOC^",lambda))
shapiro.test(dDOspawnF2$TOC^lambda)
hist(log(dDOspawnF2$TOC), main = "Normal QQ Plot of log TOC")
qqPlot(log(dDOspawnF2$TOC), dist="norm", main = "Normal QQ Plot of log TOC")
shapiro.test(log(dDOspawnF2$TOC))
png(file=paste(outpath,stations,"_","TOC_SpawnOct15toDec31_HistandQQ_trans.png",sep=""), width = 650, height = 650)
par(mfrow=c(2,2))
hist(dDOspawnF2$TOC, main = "Histogram of TOC")
qqPlot(dDOspawnF2$TOC, dist="norm", main = "Normal QQ Plot of TOC" ) 
shapiro.test(dDOspawnF2$TOC) #if p-value > 0.05, then normally distributed
hist(log(dDOspawnF2$TOC), main = "Normal QQ Plot of log TOC")
qqPlot(log(dDOspawnF2$TOC), dist="norm", main = "Normal QQ Plot of log TOC")
shapiro.test(log(dDOspawnF2$TOC))
dev.off()
#TOC is not normally distributed.  Log Transformation improves p-value
#TOC affected by whole number reporting
#LOG TRANSFORM TOC
dDOspawnF2$transTOC <- log(dDOspawnF2$TOC)
dDOPERSATspawnF2$transTOC <- log(dDOPERSATspawnF2$TOC)

#TSS
par(mfrow=c(2,2))
hist(dDOspawnF2$TSS, main = "Histogram of TSS")
qqPlot(dDOspawnF2$TSS, dist="norm", main = "Normal QQ Plot of TSS" ) 
shapiro.test(dDOspawnF2$TSS) #if p-value > 0.05, then normally distributed
bc <- boxcox(TSS~., data=dDOspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- .2
hist(dDOspawnF2$TSS^lambda, main = c("Normal QQ Plot of TSS^",lambda))
qqPlot(dDOspawnF2$TSS^lambda, dist="norm", main = c("Normal QQ Plot of TSS^",lambda))
shapiro.test(dDOspawnF2$TSS^lambda)
hist(log(dDOspawnF2$TSS), main = "Normal QQ Plot of log TSS")
qqPlot(log(dDOspawnF2$TSS), dist="norm", main = "Normal QQ Plot of log TSS")
shapiro.test(log(dDOspawnF2$TSS))
#log transformation helps
#LOG TRANSFORM TSS  
dDOspawnF2$transTSS <- log(dDOspawnF2$TSS)
dDOPERSATspawnF2$transTSS <- log(dDOPERSATspawnF2$TSS)

#Turbidity
par(mfrow=c(2,2))
hist(dDOspawnF2$Turbidity, main = "Histogram of Turbidity")
qqPlot(dDOspawnF2$Turbidity, dist="norm", main = "Normal QQ Plot of Turbidity" ) 
shapiro.test(dDOspawnF2$Turbidity) #if p-value > 0.05, then normally distributed
bc <- boxcox(Turbidity~., data=dDOspawnF2)
lambda <- bc$x[which.max(bc$y)]; lambda
lambda <- .3
hist(dDOspawnF2$Turbidity^lambda, main = c("Normal QQ Plot of Turbidity^",lambda))
qqPlot(dDOspawnF2$Turbidity^lambda, dist="norm", main = c("Normal QQ Plot of Turbidity^",lambda))
shapiro.test(dDOspawnF2$Turbidity^lambda)
hist(log(dDOspawnF2$Turbidity), main = "Normal QQ Plot of log Turbidity")
qqPlot(log(dDOspawnF2$Turbidity), dist="norm", main = "Normal QQ Plot of log Turbidity")
shapiro.test(log(dDOspawnF2$Turbidity))
#LOG TRANSFORM TSS
dDOspawnF2$transTurbidity <- log(dDOspawnF2$Turbidity)
dDOPERSATspawnF2$transTurbidity <- log(dDOPERSATspawnF2$Turbidity)


colnames(dDOspawnF2)
colnames(dDOPERSATspawnF2)

#DO Time Temperature transQ transTP OrthophosphateP DIN BOD5 transTOC transTSS transTurbidity Jday
dDOspawnF3 <- dDOspawnF2[,c(1,2,3,13,14,6,8,5,15,16,17,12)]; head(dDOspawnF3)
dDOPERSATspawnF3 <-  dDOPERSATspawnF2[,c(1,2,3,13,14,6,8,5,15,16,17,12)]; head(dDOPERSATspawnF3)

#ggpairs of DO HourPST Temperature transQ transTP OrthophosphateP DIN BOD5 TOC transTSS transTurbidity Jday      
png(file=paste(outpath,stations,"_","ggpairs_DO_spawnFtrans.png",sep=""), width = 650, height = 650)
ggpairs(data=dDOspawnF3, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()
png(file=paste(outpath,stations,"_","ggpairs_DOSAT_spawnFtrans.png",sep=""), width = 650, height = 650)
ggpairs(data=dDOPERSATspawnF3, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()

#DO VS EXPLANATORY VARIABLES
#DO correlates most highly with T and Flow
#also with julian day Jday since correlates with T and Flow
#DOSAT vs Q or Turbidity 
#Note high correlations between flow and turbidity, TSS, and TP

#MODEL mod.DOvTFW2 METHOD 1 MODEL - Oct 15 to Dec 31 - Dataset w/o TOC NAs but w/ high T and low TOC record
#1 PARM MODEL - DO v Temperature
mod.DOvTF <- lm(DO~Temperature, 
                  data=dDOspawnF3)
modelname <- "mod_DOvTF" #Fall-Winter for plot title
s=summary(mod.DOvTF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOvTF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
#Plot DO vs Temperature
png(file=paste(outpath,stations,"_","ObservedDOandModelDOvT_Fall_Oct15toDec31.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnF3$Temperature, dDOspawnF3$DO,  main="Observed DO and 1-parm Model vs. Temperature",
     pch=21, col="black", bg="gray",
     xlim=c(6,13), ylim=c(9,13), xlab = "Temperature, C", ylab = "DO mg/L",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
abline(mod.DOvTF,col="red") #abline(intercept, slope)
ci.lines(mod.DOvTF, colorci="blue")
text(10.0, 9.1,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
dev.off()
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnF3$DO, fitted(mod.DOvTF), main="Model mod.DOvTF DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#
#Correlate DO = f(T) model residuals with remaining parameters
s$residuals
d7 <- cbind(s$residuals,dDOspawnF3); d7
str(d7)
d8 <- d7[,c(1,3,5,6,7,8,9,10,11,12,13)]; head(d8)
names(d8) <- c("Residuals","Time","transQ","transTP","OrthophosphateP","DIN","BOD5","transTOC","transTSS","transTurbidity", "Jday")
head(d8)    
png(file=paste(outpath,"Model_",modelname,"_pairs_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d8, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d8, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()
# Highest correlation with Turbidity, TSS, and Flow
dDOspawnF3$transTurbidity #one NA
dDOspawnF3$transTSS #one NA
dDOspawnF3$transQ #no NA

#2 PARM MODEL - METHOD 1
#Model of DO = f(T,Q)
str(dDOspawnF3)
mod.DOvTQF <- lm(DO~Temperature+transQ, 
                    data=dDOspawnF3) 
modelname <- "mod_DOvTQF" #for plot title
s=summary(mod.DOvTQF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOvTQF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnF3$DO, fitted(mod.DOvTQF), main="Model mod.DOvTQF DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#CONTINUE TO SEE WHAT ELSE CORRELATES
s$residuals
d9 <- cbind(s$residuals,dDOspawnF3); d9
str(d9)
#1  s$residuals
#2  DO         
#3  Time       
#4  Temperature
#5  transQ
#6  transTP            
#7  OrthophosphateP
#8  DIN
#9  BOD5
#10  TOC            
#11 transTSS       
#12 transTurbidity      
d10 <- d9[,c(1,3,6,7,8,9,10,11,12,13)]; head(d10)
names(d10) <- c("Residuals","Time","transTP","OrthophosphateP","DIN","BOD5","transTOC","transTSS","transTurbidity","Jday")
head(d10)
png(file=paste(outpath,"Model_",modelname,"_pairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d10, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d10, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()
warnings()
#Highest correlation is with TOC, then time
#DO = f(T,Q,TOC)
#Remove record which lacks TOC
dDOspawnF3$transTOC
dDOspawnF4 <- subset(dDOspawnF3, transTOC != "NA"); str(dDOspawnF4)
dDOspawnF4

#3 PARM MODEL - METHOD 1 - MODEL A: transQ = log Q (natural log), transTOC = log TOC
mod.DOvTQTOCF <- lm(DO~Temperature+transQ+transTOC, 
                   data=dDOspawnF4)            #MODEL A
modelname <- "mod_DOvTQTOCF" #for plot title
s=summary(mod.DOvTQTOCF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOvTQTOCF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnF4$DO, fitted(mod.DOvTQTOCF), main="Model mod.DOvTQTOCF DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#CONTINUE TO SEE IF ANYTHING ELSE CORRELATES
s$residuals
d11 <- cbind(s$residuals,dDOspawnF4); d11
str(d11)
#1  s$residuals
#2  DO         
#3  Time       
#4  Temperature
#5  transQ
#6  transTP            
#7  OrthophosphateP
#8  DIN
#9  BOD5
#10 transTOC            
#11 transTSS       
#12 transTurbidity      
#13 Jday
d12 <- d11[,c(1,3,6,7,8,9,11,12,13)]; head(d12)
names(d12) <- c("Residuals","Time","transTP","OrthophosphateP","DIN","BOD5","transTSS","transTurbidity","Jday")
head(d12)
png(file=paste(outpath,"Model_",modelname,"_pairs_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d12, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
#Time very weak correlation
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d12, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()
#
#BOD5 correlation highest
dDOspawnF4$BOD5
#Remove record which lacks BOD5 
dDOspawnF5 <- subset(dDOspawnF4, BOD5 != "NA"); str(dDOspawnF5)
dDOspawnF5

#4 PARM MODEL - METHOD 1 - MODEL A2: transQ = log Q (natural log), transTOC = log TOC
mod.DOvTQTOCBODF <- lm(DO~Temperature+transQ+transTOC+BOD5, 
                    data=dDOspawnF5) #Model A2
str(dDOspawnF5) # 28 obs -> 28/4 = 7 rather small
modelname <- "mod_DOvTQTOCBODF" #for plot title
s=summary(mod.DOvTQTOCBODF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOvTQTOCBODF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnF5$DO, fitted(mod.DOvTQTOCBODF), main="Model mod.DOvTQTOCBODF DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#CONTINUE TO SEE IF ANYTHING ELSE CORRELATES
s$residuals
d13 <- cbind(s$residuals,dDOspawnF5); d13
str(d13)
#1  s$residuals
#2  DO         
#3  Time       
#4  Temperature
#5  transQ
#6  transTP            
#7  OrthophosphateP
#8  DIN
#9  BOD5
#10 TOC            
#11 transTSS       
#12 transTurbidity      
d14 <- d13[,c(1,3,6,7,8,11,12,13)]; head(d14)
names(d14) <- c("Residuals","Time","transTP", "OrthophosphateP", "DIN", "transTSS", "transTurbidity","Jday")
head(d14)
png(file=paste(outpath,"Model_",modelname,"_pairs_4parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d14, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
#Time very weak correlation
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_4parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d14, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()

mod.DOvTQTOCBODJdayF <- lm(DO~Temperature+transQ+transTOC+BOD5+Jday, 
                       data=dDOspawnF5)
modelname <- "mod_DOvTQTOCBODJdayF" #for plot title
s=summary(mod.DOvTQTOCBODJdayF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOvTQTOCBODJdayF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnF5$DO, fitted(mod.DOvTQTOCBODJdayF), main="Model mod.DOvTQTOCBODJdayF DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()


#REPEAT WITH 2 PARM MODEL DO = f(T,turbidity)
#2 PARM MODEL - METHOD 1
str(dDOspawnF3)
dDOspawnF3$transTurbidity
dDOspawnFt4 <- subset(dDOspawnF3, transTurbidity != "NA"); str(dDOspawnFt4)
dDOspawnFt4

mod.DOvTTurbidityF <- lm(DO~Temperature+transTurbidity, 
                 data=dDOspawnFt4)
modelname <- "mod_DOvTTurbidityF" #for plot title
s=summary(mod.DOvTTurbidityF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOvTTurbidityF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnFt4$DO, fitted(mod.DOvTTurbidityF), main="Model mod.DOvTTurbidityF DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#CONTINUE TO SEE WHAT ELSE CORRELATES
s$residuals
d9 <- cbind(s$residuals,dDOspawnFt4); d9
str(d9)
#1  s$residuals
#2  DO         
#3  Time       
#4  Temperature
#5  transQ
#6  transTP            
#7  OrthophosphateP
#8  DIN
#9  BOD5
#10  TOC            
#11 transTSS       
#12 transTurbidity      
d10 <- d9[,c(1,3,5,6,7,8,9,10,11)]; head(d10)
names(d10) <- c("Residuals","Time","transQ","transTP", "OrthophosphateP", "DIN", "BOD5", "TOC", "transTSS")
head(d10)
png(file=paste(outpath,"Model_",modelname,"_pairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d10, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d10, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()
warnings()

mod.DOvTimeTTurbidityF <- lm(DO~Time+Temperature+transTurbidity, 
                    data=dDOspawnFt4)
modelname <- "mod_DOvTimeTTurbidityF" #for plot title
s=summary(mod.DOvTimeTTurbidityF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOvTimeTTurbidityF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnFt4$DO, fitted(mod.DOvTimeTTurbidityF), main="Model mod.DOvTimeTTurbidityF DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#CONTINUE TO SEE IF ANYTHING ELSE CORRELATES
s$residuals
d11 <- cbind(s$residuals,dDOspawnFt4); d11
str(d11)
#1  s$residuals
#2  DO         
#3  Time       
#4  Temperature
#5  transQ
#6  transTP            
#7  OrthophosphateP
#8  DIN
#9  BOD5
#10 TOC            
#11 transTSS       
#12 transTurbidity      
d12 <- d11[,c(1,5,6,7,8,9,10,11)]; head(d12)
names(d12) <- c("Residuals","transQ","transTP","OrthophosphateP","DIN","BOD5","TOC","transTSS")
head(d12)
png(file=paste(outpath,"Model_",modelname,"_pairs_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d12, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d12, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()
#highest correlation with TOC
dDOspawnFt4$TOC
#Remove one TOC NA record
dDOspawnFt5 <- subset(dDOspawnFt4, TOC != "NA"); str(dDOspawnF5)
dDOspawnFt5

mod.DOvTimeTTurbidityTOCF <- lm(DO~Time+Temperature+transTurbidity+TOC, 
                             data=dDOspawnFt5)
modelname <- "mod_DOvTimeTTurbidityTOCF" #for plot title
s=summary(mod.DOvTimeTTurbidityTOCF); s #Time P-value 0.263524 > 0.05-0.1 NOT STATISTICALLY SIGNIFICANT 

#Generate via STEP
#NOTE ONE HIGH T VALUE 15.0 C
#Remove it since more like summer and not representative of fall condition being analyzed
dDOspawnFS2 <- subset(dDOspawnF, dDOspawnF$Temperature < 15.0)
dDOPERSATspawnFS2 <- subset(dDOPERSATspawnF, dDOPERSATspawnF$Temperature < 15.0)
#Remove record which lacks TOC 
dDOspawnFS3 <- subset(dDOspawnFS2, TOC != "NA"); str(dDOspawnFS3)
dDOPERSATspawnFS3 <- subset(dDOPERSATspawnFS2, TOC != "NA"); str(dDOspawnFS3)


mod.megaF <- lm(DO~Time+Temperature+QatSFCoqPowers+TOC+
                     Time*Temperature+Time*QatSFCoqPowers+Time*TOC+
                     Temperature*QatSFCoqPowers+Temperature*TOC+
                     QatSFCoqPowers*TOC+
                     I(Time^2)+I(Temperature^2)+I(QatSFCoqPowers^2)+I(TOC^2), 
                   data=dDOspawnFS3)
s=summary(mod.megaF); s
mod.stepF <- step(mod.megaF) #MODEL B
s=summary(mod.stepF); s
slope <- coefficients(mod.stepF)[2]; slope
intercept <- coefficients(mod.stepF)[1]; intercept
confint(mod.stepF) # 95% condfidence interval for the intercept and slope
coef(mod.stepF) # p. 359 extracting info from model objects
summary(mod.stepF)
resid(mod.stepF)
# DO ~ Time + Temperature + QatSFCoqPowers + I(Temperature^2) + I(QatSFCoqPowers^2) + I(TOC^2)
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
modelname <- "mod_stepF"
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnFS3$DO, fitted(mod.stepF), main="Model mod.stepF DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#FINAL Model via STEP:
#lm(formula = DO ~ Time + Temperature + QatSFCoqPowers + I(Temperature^2) + 
#       I(QatSFCoqPowers^2) + I(TOC^2), data = dDOspawnFS3)
#Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          1.817e+01  2.762e+00   6.579 1.29e-06 ***
#  Time                 1.837e+00  6.729e-01   2.730   0.0122 *  
#  Temperature         -1.546e+00  6.037e-01  -2.562   0.0178 *  
#  QatSFCoqPowers       3.436e-04  1.209e-04   2.841   0.0095 ** 
#  I(Temperature^2)     7.174e-02  3.133e-02   2.290   0.0320 *  
#  I(QatSFCoqPowers^2) -2.883e-08  1.246e-08  -2.313   0.0305 *  
#  I(TOC^2)            -1.051e-02  5.683e-03  -1.849   0.0779 .  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 0.2923 on 22 degrees of freedom
#Multiple R-squared:  0.7002,	Adjusted R-squared:  0.6184 
#F-statistic: 8.564 on 6 and 22 DF,  p-value: 7.219e-05

#DO~Temperature+Q+TOC
mod.megaF2 <- lm(DO~Temperature+QatSFCoqPowers+TOC+
                   Temperature*QatSFCoqPowers+Temperature*TOC+
                   QatSFCoqPowers*TOC+
                   I(Temperature^2)+I(QatSFCoqPowers^2)+I(TOC^2), 
                data=dDOspawnFS3)
s=summary(mod.megaF2); s
mod.stepF2 <- step(mod.megaF2)
s=summary(mod.stepF2); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
modelname <- "mod_stepF2"
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepF2)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnFS3$DO, fitted(mod.stepF2), main="Model mod.stepF2 DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#Remove terms with p-value > 0.10
#Results in 2 parameter model DO ~ Temperature + QatSFCoqPowers
mod.megaF3 <- lm(DO~Temperature+QatSFCoqPowers+TOC+
                   Temperature*QatSFCoqPowers+Temperature*TOC+
                   QatSFCoqPowers*TOC+
                   I(QatSFCoqPowers^2)+I(TOC^2), 
                 data=dDOspawnFS3)
s=summary(mod.megaF3); s
mod.stepF3 <- step(mod.megaF3)
s=summary(mod.stepF3); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
modelname <- "mod_stepF3"
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepF3)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnFS3$DO, fitted(mod.stepF3), main="Model mod.stepF3 DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#Since TOC not an explanatory variable, repeat with only flow and temperature and
#using datafile with includes record with TOC NA - dDOspawnFS2
mod.megaF4 <- lm(DO~Temperature+QatSFCoqPowers+
                   Temperature*QatSFCoqPowers+
                   I(Temperature^2)+I(QatSFCoqPowers^2),
                 data=dDOspawnFS2)
s=summary(mod.megaF4); s
mod.stepF4 <- step(mod.megaF4)
s=summary(mod.stepF4); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
modelname <- "mod_stepF4"
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepF4)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnFS2$DO, fitted(mod.stepF4), main="Model mod.stepF4 DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
 
#Repeat with Time Temperature QatSFCoqPowers
mod.megaF5 <- lm(DO~Time+Temperature+QatSFCoqPowers+
                  Time*Temperature+Time*QatSFCoqPowers+
                  Temperature*QatSFCoqPowers+
                  I(Time^2)+I(Temperature^2)+I(QatSFCoqPowers^2), 
                data=dDOspawnFS2)
mod.stepF5 <- step(mod.megaF5)
s=summary(mod.stepF5); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
modelname <- "mod_stepF5"
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepF5)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnFS2$DO, fitted(mod.stepF5), main="Model mod.stepF5 DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#Repeat with Time Temperature QatSFCoqPowers
#and with terms with high p-values step-wise removed
mod.megaF6 <- lm(DO~Time+Temperature+QatSFCoqPowers+
                   Time*QatSFCoqPowers+
                   Temperature*QatSFCoqPowers+
                   I(Time^2)+I(Temperature^2), 
                 data=dDOspawnFS2)
mod.stepF6 <- step(mod.megaF6)
s=summary(mod.stepF6); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
#Coefficients:
#                     Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)       1.833e+01  2.916e+00   6.286 1.41e-06 ***
#  Time              1.622e+00  6.319e-01   2.566  0.01666 *  
#  Temperature      -1.480e+00  6.218e-01  -2.380  0.02526 *  
#  QatSFCoqPowers    6.877e-05  2.289e-05   3.004  0.00597 ** 
#  I(Temperature^2)  6.544e-02  3.188e-02   2.053  0.05073 .  
#---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 0.315 on 25 degrees of freedom
#Multiple R-squared:  0.608,	Adjusted R-squared:  0.5453 
#F-statistic: 9.693 on 4 and 25 DF,  p-value: 7.092e-05
modelname <- "mod_stepF6"
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.stepF6)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOspawnFS2$DO, fitted(mod.stepF6), main="Model mod.stepF6 DO vs. Observed",xlim = c(10,12.5), ylim=c(10,12.5), 
     xlab = "Observed DO", ylab = "Model Calculated DO")
abline(a=0,b=1) 
text(11.8, 10.2,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(11.8, 10.1,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#DOPerSat VS EXPLANATORY VARIABLES
#DOSAT vs Q or Turbidity 
#Note high correlations between flow and turbidity, TSS, and TP

#MODEL mod.DOvTFW2 METHOD 1 MODEL - Oct 15 to Dec 31
#1 PARM MODEL - DO v flow
mod.DOPERSATvQF <- lm(DOPerSat~transQ, 
                data=dDOPERSATspawnF3)
modelname <- "mod_DOPERSATvQF" #Fall-Winter for plot title
s=summary(mod.DOPERSATvQF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATvQF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
#Plot DO vs flow
png(file=paste(outpath,stations,"_","ObservedDOPERSATandModelDOSATvT_Fall_Oct15toDec31.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnF3$transQ, dDOPERSATspawnF3$DOPerSat,  main="Observed DOPerSat and 1-parm Model vs. log Q",
     pch=21, col="black", bg="gray",
     xlim=c(3,10), ylim=c(90,110), xlab = "log QatSFCoqPowers", ylab = "DOPerSat %",
     cex=1.3, cex.main=1.5, cex.axis=1.2, cex.lab=1.5)
abline(mod.DOPERSATvQF,col="red") #abline(intercept, slope)
ci.lines(mod.DOPERSATvQF, colorci="blue")
text(8.0, 90,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
dev.off()
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDOPERSAT.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnF3$DOPerSat, fitted(mod.DOPERSATvQF), main="Model mod.DOPERSATvQF DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#
#Correlate DOPerSat = f(Q) model residuals with remaining parameters
s$residuals
d7 <- cbind(s$residuals,dDOPERSATspawnF3); d7
str(d7)
d8 <- d7[,c(1,3,4,6,7,8,9,10,11,12,13)]; head(d8)
names(d8) <- c("Residuals","Time","Temperature","transTP","OrthophosphateP","DIN","BOD5","transTOC","transTSS","transTurbidity", "Jday")
head(d8)    
png(file=paste(outpath,"Model_",modelname,"_pairs_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d8, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_1parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d8, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()

# Highest correlation with Time and TOC
dDOPERSATspawnF3$Time 
dDOPERSATspawnF3$transTOC #one NA

#2 PARM MODEL - METHOD 1 - NOTE VERY LOW R^2
#Model of DOPerSat = f(Q,TOC)
#Remove record which lacks TOC
dDOPERSATspawnF4 <- subset(dDOPERSATspawnF3, transTOC != "NA"); str(dDOPERSATspawnF4)
mod.DOPERSATvQTOCF <- lm(DOPerSat~transQ+transTOC, 
                 data=dDOPERSATspawnF4) 
modelname <- "mod_DOPERSATvQTOCF" #for plot title
s=summary(mod.DOPERSATvQTOCF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATvQTOCF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnF4$DOPerSat, fitted(mod.DOPERSATvQTOCF), main="Model mod.DOPERSATvQTOCF DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#CONTINUE TO SEE WHAT ELSE CORRELATES
s$residuals
d9 <- cbind(s$residuals,dDOPERSATspawnF4); d9
str(d9)
#1  s$residuals
#2  DO         
#3  Time       
#4  Temperature
#5  transQ
#6  transTP            
#7  OrthophosphateP
#8  DIN
#9  BOD5
#10 transTOC            
#11 transTSS       
#12 transTurbidity     
#13 Jday
d10 <- d9[,c(1,3,4,6,7,8,9,11,12,13)]; head(d10)
names(d10) <- c("Residuals","Time","Temperature","transTP","OrthophosphateP","DIN","BOD5","transTSS","transTurbidity","Jday")
head(d10)
png(file=paste(outpath,"Model_",modelname,"_pairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d10, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d10, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()
warnings()
#Highest correlation is with BOD5

str(dDOPERSATspawnF4)
dDOPERSATspawnF5 <- subset(dDOPERSATspawnF4, BOD5 != "NA"); str(dDOPERSATspawnF5)

#DOPERSAT MODEL A - USE THIS 3-parm model DOPerSat~transQ+transTOC+BOD5
mod.DOPERSATvQTOCBODF <- lm(DOPerSat~transQ+transTOC+BOD5, 
                      data=dDOPERSATspawnF5) #DOPERSAT MODEL A
modelname <- "mod_DOPERSATvQTOCBODF" #for plot title
s=summary(mod.DOPERSATvQTOCBODF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATvQTOCBODF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnF5$DOPerSat, fitted(mod.DOPERSATvQTOCBODF), main="Model mod.DOPERSATvQTOCBODF DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#CONTINUE TO SEE WHAT ELSE CORRELATES
s$residuals
d11 <- cbind(s$residuals,dDOPERSATspawnF5); d11
str(d11)
d12 <- d11[,c(1,3,4,6,7,8,11,12,13)]; head(d12)
names(d12) <- c("Residuals","Time","Temperature","transTP","OrthophosphateP","DIN","transTSS","transTurbidity","Jday")
head(d12)
png(file=paste(outpath,"Model_",modelname,"_pairs_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d12, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_3parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d12, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()
#Highest correlation is with Temperature

mod.DOPERSATvTQTOCBODF <- lm(DOPerSat~Temperature+transQ+transTOC+BOD5, 
                         data=dDOPERSATspawnF5) 
modelname <- "mod_DOPERSATvTQTOCBODF" #for plot title
s=summary(mod.DOPERSATvTQTOCBODF); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
#Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)  80.8976     6.1047  13.252 2.98e-12 ***
#  Temperature   0.7811     0.4811   1.623 0.118122    
#  transQ        1.5135     0.3652   4.144 0.000393 ***
#  transTOC     -3.4634     1.0168  -3.406 0.002421 ** 
#  BOD5          2.6127     1.1752   2.223 0.036310 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 2.321 on 23 degrees of freedom
#Multiple R-squared:  0.5336,	Adjusted R-squared:  0.4525 
#F-statistic: 6.579 on 4 and 23 DF,  p-value: 0.001106
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATvTQTOCBODF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnF5$DOPerSat, fitted(mod.DOPERSATvTQTOCBODF), main="Model mod.DOPERSATvTQTOCBODF DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#TEMPERATURE p-value > 0.10
#Don't use this - use 3-parm model DOPerSat~transQ+transTOC+BOD5


#2 PARM MODEL - METHOD 1 
#Model of DOPerSat = f(Q,Time)
mod.DOPERSATvTimeQF <- lm(DOPerSat~Time+transQ, 
                      data=dDOPERSATspawnF3) 
modelname <- "mod_DOPERSATvTimeQF" #for plot title
s=summary(mod.DOPERSATvTimeQF); s #P-VALUE FOR TIME = 0.0889 
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATvTimeQF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnF3$DOPerSat, fitted(mod.DOPERSATvTimeQF), main="Model mod.DOPERSATvTimeQF DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#CONTINUE TO SEE WHAT ELSE CORRELATES
s$residuals
d9 <- cbind(s$residuals,dDOPERSATspawnF3); d9
str(d9)
d10 <- d9[,c(1,4,6,7,8,9,10,11,12,13)]; head(d10)
names(d10) <- c("Residuals","Temperature","transTP","OrthophosphateP","DIN","BOD5","TOC","transTSS","transTurbidity","Jday")
head(d10)
png(file=paste(outpath,"Model_",modelname,"_pairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
pairs(d10, lower.panel=panel.smooth, diag.panel=panel.hist, upper.panel=panel.cor)
dev.off()
png(file=paste(outpath,"Model_",modelname,"_ggpairs_2parm_ResidualsvsEtc.png",sep=""), width = 650, height = 650)
ggpairs(data=d10, upper = list(continuous = "cor", combo = "box_no_facet"),
        lower = list(continuous = "smooth", combo = "dot_no_facet"))
dev.off()
warnings()
#Highest correlation is TOC

#3 PARM MODEL 
#Model of DOPerSat = f(Time,Q,TOC)
dDOPERSATspawnF4 <- subset(dDOPERSATspawnF3, transTOC != "NA"); str(dDOPERSATspawnF4)

mod.DOPERSATvTimeQTOCF <- lm(DOPerSat~Time+transQ+transTOC, 
                       data=dDOPERSATspawnF4) 
modelname <- "mod_DOPERSATvTimeQTOCF" #for plot title
s=summary(mod.DOPERSATvTimeQTOCF); s #P-VALUE FOR TIME > 0.05-0.10
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATvTimeQTOCF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnF4$DOPerSat, fitted(mod.DOPERSATvTimeQTOCF), main="Model mod.DOPERSATvTimeQTOCF DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#THIS MODEL IS NOT STATISTICALLY SIGNIFICANT DUE TO P-VALUE FOR TIME > 0.05-0.10

#3 PARM MODEL 
#Model of DOPerSat = f(Time,T,Q)
mod.DOPERSATvTimeTQF <- lm(DOPerSat~Time+Temperature+transQ, 
                          data=dDOPERSATspawnF3) 
modelname <- "mod_DOPERSATvTimeTQF" #for plot title
s=summary(mod.DOPERSATvTimeTQF); s #P-VALUE FOR TIME > 0.05-0.10
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATvTimeTQF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnF4$DOPerSat, fitted(mod.DOPERSATvTimeTQF), main="Model mod.DOPERSATvTimeTQF DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()
#THIS MODEL IS NOT STATISTICALLY SIGNIFICANT DUE TO P-VALUE FOR Temperature > 0.05-0.10

#STEP-WISE DELETION MODEL VIA "STEP"
head(dDOPERSATspawnFS3) #Lacks high T record and NA TOC record
mod.DOPERSATmegaF <- lm(DOPerSat~Time+Temperature+QatSFCoqPowers+TOC+
                      Time*Temperature+Time*QatSFCoqPowers+Time*TOC+
                      Temperature*QatSFCoqPowers+Temperature*TOC+
                       QatSFCoqPowers*TOC+
                      I(Time^2)+I(Temperature^2)+I(QatSFCoqPowers^2)+I(TOC^2), 
                    data=dDOPERSATspawnFS3) 
s=summary(mod.DOPERSATmegaF); s
#if p-value > 0.05 then statistically significant
mod.DOPERSATstepF <- step(mod.DOPERSATmegaF) #MODEL B
modelname <- "mod_DOPERSATstepF" #for plot title
s=summary(mod.DOPERSATstepF); s
#  lm(formula = DOPerSat ~ Time + Temperature + QatSFCoqPowers + 
#      I(Temperature^2) + I(QatSFCoqPowers^2) + I(TOC^2), data = dDOPERSATspawnFS3)
#  Coefficients:
#                       Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          1.349e+02  2.524e+01   5.344  2.3e-05 ***
#  Time                 1.701e+01  6.148e+00   2.767  0.01125 *  
#  Temperature         -1.106e+01  5.516e+00  -2.005  0.05739 .  
#  QatSFCoqPowers       3.121e-03  1.105e-03   2.825  0.00987 ** 
#  I(Temperature^2)     6.247e-01  2.862e-01   2.183  0.04002 *  
#  I(QatSFCoqPowers^2) -2.616e-07  1.139e-07  -2.298  0.03146 *  
#  I(TOC^2)            -9.790e-02  5.192e-02  -1.885  0.07265 .  
#Residual standard error: 2.671 on 22 degrees of freedom
#Multiple R-squared:  0.4721,	Adjusted R-squared:  0.3281 
#F-statistic: 3.279 on 6 and 22 DF,  p-value: 0.01856
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATstepF)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnFS3$DOPerSat, fitted(mod.DOPERSATstepF), main="Model mod.DOPERSATstepF DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#STEP-WISE DELETION MODEL VIA "STEP"
dDOPERSATspawnFS4 <- subset(dDOPERSATspawnFS3, BOD5 != "NA"); 
str(dDOPERSATspawnFS4) #Lacks high T record and NA TOC and BOD5 records
mod.DOPERSATmega2F <- lm(DOPerSat~Temperature+QatSFCoqPowers+TOC+BOD5+
                       Temperature*QatSFCoqPowers+Temperature*TOC+Temperature*BOD5+
                       QatSFCoqPowers*TOC+QatSFCoqPowers*BOD5+TOC*BOD5+
                       I(Temperature^2)+I(QatSFCoqPowers^2)+I(TOC^2)+I(BOD5^2), 
                     data=dDOPERSATspawnFS4) 
s=summary(mod.DOPERSATmega2F); s
mod.DOPERSATstep2F <- step(mod.DOPERSATmega2F) 
modelname <- "mod_DOPERSATstep2F" #for plot title
s=summary(mod.DOPERSATstep2F); s
#Get rid of  Temperature:BOD5 I(TOC^2) I(Temperature^2) QatSFCoqPowers:BOD5
mod.DOPERSATmega2F <- lm(DOPerSat~Temperature+QatSFCoqPowers+TOC+BOD5+
                        Temperature*QatSFCoqPowers+Temperature*TOC+
                        QatSFCoqPowers*TOC+TOC*BOD5+
                        I(QatSFCoqPowers^2)+I(BOD5^2), 
                      data=dDOPERSATspawnFS4) 
s=summary(mod.DOPERSATmega2F); s
mod.DOPERSATstep2F <- step(mod.DOPERSATmega2F)
s=summary(mod.DOPERSATstep2F); s
#lm(formula = DOPerSat ~ Temperature + QatSFCoqPowers + TOC + 
#     BOD5 + I(QatSFCoqPowers^2) + I(BOD5^2) + Temperature:TOC, 
#   data = dDOPERSATspawnFS4)
#  Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          9.138e+01  8.871e+00  10.301 1.92e-09 ***
#  Temperature         -4.392e-01  7.887e-01  -0.557  0.58380    
#  QatSFCoqPowers       3.395e-03  9.643e-04   3.521  0.00215 ** 
#  TOC                 -6.951e+00  3.424e+00  -2.030  0.05585 .  
#  BOD5                 1.965e+01  5.570e+00   3.528  0.00211 ** 
#  I(QatSFCoqPowers^2) -3.076e-07  9.950e-08  -3.092  0.00575 ** 
#  I(BOD5^2)           -7.445e+00  2.525e+00  -2.948  0.00795 ** 
#  Temperature:TOC      5.917e-01  3.295e-01   1.795  0.08770 .  
#  ---
##Residual standard error: 2.148 on 20 degrees of freedom
#Multiple R-squared:  0.6528,	Adjusted R-squared:  0.5313 
#F-statistic: 5.372 on 7 and 20 DF,  p-value: 0.001403
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATstep2F)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnFS4$DOPerSat, fitted(mod.DOPERSATstep2F), main="Model mod.DOPERSATstep2F DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#STEP-WISE DELETION MODEL VIA "STEP"
#same explanatory variables as mod.DOPERSATvQTOCBODF <- lm(DOPerSat~transQ+transTOC+BOD5, 
#                            data=dDOPERSATspawnF5) 
dDOPERSATspawnFS4 <- subset(dDOPERSATspawnFS3, BOD5 != "NA"); 
str(dDOPERSATspawnFS4) #Lacks high T record and NA TOC and BOD5 records
mod.DOPERSATmega3F <- lm(DOPerSat~QatSFCoqPowers+TOC+BOD5+
                           QatSFCoqPowers*TOC+QatSFCoqPowers*BOD5+TOC*BOD5+
                           I(QatSFCoqPowers^2)+I(TOC^2)+I(BOD5^2), 
                         data=dDOPERSATspawnFS4) 
s=summary(mod.DOPERSATmega3F); s
mod.DOPERSATstep3F <- step(mod.DOPERSATmega3F) 
modelname <- "mod_DOPERSATstep3F" #for plot title
s=summary(mod.DOPERSATstep3F); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
#DOPerSat ~ QatSFCoqPowers + TOC + BOD5 + I(QatSFCoqPowers^2) + 
#           I(TOC^2) + I(BOD5^2) + QatSFCoqPowers:TOC
#Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          8.469e+01  3.391e+00  24.976  < 2e-16 ***
#  QatSFCoqPowers       2.413e-03  8.075e-04   2.988  0.00727 ** 
#  TOC                  1.476e+00  1.130e+00   1.307  0.20614    
#  BOD5                 1.781e+01  5.558e+00   3.204  0.00445 ** 
#  I(QatSFCoqPowers^2) -1.717e-07  8.123e-08  -2.114  0.04728 *  
#  I(TOC^2)            -2.561e-01  1.505e-01  -1.701  0.10444    
#  I(BOD5^2)           -6.598e+00  2.537e+00  -2.600  0.01712 *  
#  QatSFCoqPowers:TOC  -1.478e-04  9.415e-05  -1.570  0.13220    
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 2.165 on 20 degrees of freedom
#Multiple R-squared:  0.6472,	Adjusted R-squared:  0.5237 
#F-statistic: 5.242 on 7 and 20 DF,  p-value: 0.001616
#NOTE: several high p-value terms remain
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATstep3F)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnFS4$DOPerSat, fitted(mod.DOPERSATstep3F), main="Model mod.DOPERSATstep3F DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#same explanatory variables as mod.DOPERSATvQTOCBODF <- lm(DOPerSat~transQ+transTOC+BOD5, 
#                            data=dDOPERSATspawnF5) 
#Eliminate terms in above with p-value > 0.10
dDOPERSATspawnFS4 <- subset(dDOPERSATspawnFS3, BOD5 != "NA"); 
str(dDOPERSATspawnFS4) #Lacks high T record and NA TOC and BOD5 records
mod.DOPERSATmega4F <- lm(DOPerSat~QatSFCoqPowers+TOC+BOD5+
                           QatSFCoqPowers*BOD5+TOC*BOD5+
                           I(QatSFCoqPowers^2)+I(TOC^2)+I(BOD5^2), 
                         data=dDOPERSATspawnFS4) 
s=summary(mod.DOPERSATmega4F); s
mod.DOPERSATstep4F <- step(mod.DOPERSATmega4F) 
modelname <- "mod_DOPERSATstep4F" #for plot title
s=summary(mod.DOPERSATstep4F); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
#Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          8.681e+01  3.049e+00  28.470  < 2e-16 ***
#  QatSFCoqPowers       2.062e-03  7.811e-04   2.640  0.01496 *  
#  BOD5                 1.883e+01  5.696e+00   3.307  0.00321 ** 
#  I(QatSFCoqPowers^2) -1.784e-07  8.337e-08  -2.140  0.04371 *  
#  I(TOC^2)            -1.099e-01  5.009e-02  -2.195  0.03904 *  
#  I(BOD5^2)           -7.146e+00  2.597e+00  -2.752  0.01163 *  
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 2.234 on 22 degrees of freedom
#Multiple R-squared:  0.5868,	Adjusted R-squared:  0.4929 
#F-statistic: 6.249 on 5 and 22 DF,  p-value: 0.0009463
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATstep4F)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnFS4$DOPerSat, fitted(mod.DOPERSATstep4F), main="Model mod.DOPERSATstep4F DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()

#Repeat with low Q value removed
#same explanatory variables as mod.DOPERSATvQTOCBODF <- lm(DOPerSat~transQ+transTOC+BOD5, 
#                            data=dDOPERSATspawnF5) 
dDOPERSATspawnFS4 <- subset(dDOPERSATspawnFS3, BOD5 != "NA")
dDOPERSATspawnFS5 <- subset(dDOPERSATspawnFS4, QatSFCoqPowers > 30.2)
str(dDOPERSATspawnFS5) #Lacks high T record, NA TOC and BOD5 records, and low Q record
mod.DOPERSATmega5F <- lm(DOPerSat~QatSFCoqPowers+TOC+BOD5+
                           QatSFCoqPowers*TOC+QatSFCoqPowers*BOD5+TOC*BOD5+
                           I(QatSFCoqPowers^2)+I(TOC^2)+I(BOD5^2), 
                         data=dDOPERSATspawnFS5) 
s=summary(mod.DOPERSATmega5F); s
mod.DOPERSATstep5F <- step(mod.DOPERSATmega5F) 
modelname <- "mod_DOPERSATstep5F" #for plot title
s=summary(mod.DOPERSATstep5F); s
rsquared <- round(s[[8]],2); rsquared
StdError <- round(s[[6]],2); StdError #Residual standard error
#DOPerSat ~ QatSFCoqPowers + TOC + BOD5 + I(QatSFCoqPowers^2) + I(BOD5^2), 
# data = dDOPERSATspawnFS5)
#Coefficients:
#                        Estimate Std. Error t value Pr(>|t|)    
#  (Intercept)          8.749e+01  2.785e+00  31.415  < 2e-16 ***
#  QatSFCoqPowers       1.778e-03  6.908e-04   2.574 0.017694 *  
#  TOC                 -8.087e-01  3.250e-01  -2.488 0.021319 *  
#  BOD5                 2.088e+01  4.998e+00   4.177 0.000426 ***
#  I(QatSFCoqPowers^2) -1.539e-07  7.362e-08  -2.091 0.048905 *  
#  I(BOD5^2)           -8.071e+00  2.265e+00  -3.564 0.001833 ** 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#Residual standard error: 1.957 on 21 degrees of freedom
#Multiple R-squared:  0.6603,	Adjusted R-squared:  0.5794 
#F-statistic: 8.163 on 5 and 21 DF,  p-value: 0.0002071
png(file=paste(outpath,"Model_",modelname,"_Diagnostics.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(2,2))
plot(mod.DOPERSATstep4F)
dev.off()
shapiro.test(s$residuals) #if p-value > 0.05 then normally distributed
png(file=paste(outpath,"Model_",modelname,"_CalculatedvsObservedDO.png",sep=""), width = 500, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATspawnFS5$DOPerSat, fitted(mod.DOPERSATstep5F), main="Model mod.DOPERSATstep5F DOPerSat vs. Observed",
     xlim = c(90,110), ylim=c(90,110), 
     xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
abline(a=0,b=1) 
text(105, 91,labels = paste("R-squared =", rsquared),cex = 0.9, adj=c(0,0))
text(105, 90,labels = paste("Std Error =", StdError),cex = 0.9, adj=c(0,0))
dev.off()



#MODEL SENSITIVITES VIA DSENS 25th to 75th %tiles ------------------------
#For DO consider one Method 1 models and one Method 2 model:
#  3 PARM MODEL - METHOD 1 - MODEL A: mod.DOvTQTOCF DO~Temperature+transQ+transTOC
#  4 PARM MODEL - METHOD 2 - MODEL B: mod.stepF DO ~ Time + Temperature + QatSFCoqPowers + I(Temperature^2) + I(QatSFCoqPowers^2) + I(TOC^2), data = dDOspawnFS3

colnames(d2A_wide_F)
#DO Time Temperature QatSFCoqPowers TOC
dDOspawnF6 <-       d2A_wide_F[,c(10,25,17,16,19,6)]; head(dDOspawnF6)
dDOPERSATspawnF6 <- d2A_wide_F[,c(11,25,17,16,19,6)]; head(dDOPERSATspawnF6)
#Remove record which lacks TOC 
dDOspawnF7 <- subset(dDOspawnF6, TOC != "NA"); str(dDOspawnF7)
dDOPERSATspawnF7 <- subset(dDOPERSATspawnF6, TOC != "NA"); str(dDOPERSATspawnF7)
#Also remove high T record
dDOspawnF8 <- subset(dDOspawnF7, Temperature < 15.0); str(dDOspawnF8)
dDOPERSATspawnF8 <- subset(dDOPERSATspawnF7, Temperature < 15.0); str(dDOPERSATspawnF8)

#Transform Q
dDOspawnF8$transQ <- log(dDOspawnF8$QatSFCoqPowers)
dDOspawnF8$transTOC <- log(dDOspawnF8$TOC)
dDOPERSATspawnF8$transQ <- log(dDOPERSATspawnF8$QatSFCoqPowers)
dDOPERSATspawnF8$transTOC <- log(dDOPERSATspawnF8$TOC)

dDOspawnF8$DO
mean(dDOspawnF8$DO)
# 11.14138
quantile(dDOspawnF8$DO,c(.10,.25, .50, .75,.9)) #observed current
#10.5 10.8 11.2 11.4 11.7 
mean(dDOspawnF8$Time)
# 0.530842
quantile(dDOspawnF8$Time,c(.10,.25, .50, .75,.9)) 
# 0.4154167 0.4513750 0.5138750 0.6250000 0.6508417
mean(dDOspawnF8$Temperature)
# 10.07931
quantile(dDOspawnF8$Temperature,c(.10,.25, .50, .75,.9)) #observed current
# 9.00  9.00 10.10 10.70 11.26 
mean(dDOspawnF8$QatSFCoqPowers)
# 1833.11
geometric <- function (x) exp(mean(log(x))) #Crawley 2007 p.49
geometric(dDOspawnF8$QatSFCoqPowers)
# 630.178
quantile(dDOspawnF8$QatSFCoqPowers,c(.10,.25, .50, .75,.9)) #observed current
# 71.52  129.00  869.00 1320.00 6154.00 
mean(dDOspawnF8$TOC)
# 2.841034
quantile(dDOspawnF8$TOC,c(.10,.25, .50, .75,.9)) #observed current
# 1.0 2.0 2.0 4.0 4.4 

# Input table which has median, 25th and 75th values for explanatory variables
#dsens <- read.csv("I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Data/Data_Mean25th75th_Sens.csv")
dsens <- read.csv("I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Data/Data_Mean25th75th_Sens_4.csv")
dsens

#
#Transformations
dsens$transQ <- log(dsens$QatSFCoqPowers)
dsens$transTOC <- log(dsens$TOC)
dsens

#FUTURE SCENARIO A - 3 parameter model - Via method 1 mod.DOvTQTOCF
#mod.DOvTQTOCF
#DO ~ Temperature + transQ + transTOC, data=dDOspawnF4 #MODEL A: transQ = log Q (natural log), transTOC = log TOC
DO.pred.DOvTQTOCF<- predict(mod.DOvTQTOCF, newdata=dsens); DO.pred.DOvTQTOCF #Koch 3-parm
summary(mod.DOvTQTOCF, digits = 8)
coef(mod.DOvTQTOCF) # p. 359 extracting info from model objects, this has greater precision than summary

#FUTURE SCENARIO B - 3 parameter step model GENERATED BY STEP - mod.step
# DO ~ Time + Temperature + QatSFCoqPowers + I(Temperature^2) + I(QatSFCoqPowers^2) + I(TOC^2), data = dDOspawnFS3
DO.pred.stepF <- predict(mod.stepF, newdata=dsens); DO.pred.stepF
s=summary(mod.stepF); s
coef(mod.stepF)

dsens4 <- cbind(dsens, DO.pred.DOvTQTOCF, DO.pred.stepF)
dsens4
write.table(dsens4,paste(outpath,"ModelSensitivities4.csv"),sep=",", row.names = FALSE) #short column headers

#MODEL CALCULATED IMPACTS OF TEMPERATURE AND TOC REDUCTIONS
Tchange = -4.0 #degree C reduction - additive
TOCchange = 1.0 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TRed4/" #Temperature reduction of 4C

#MODEL CALCULATED IMPACTS OF TEMPERATURE AND TOC REDUCTIONS
Tchange = -3.0 #degree C reduction - additive
TOCchange = 1.0 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TRed3/" #Temperature reduction of 3C

Tchange = -2.75 #degree C reduction - additive
TOCchange = 0.8 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TRed275TOCRed20/" #Temperature reduction of 2.5C and 30% TOC reduction

Tchange = -2.5 #degree C reduction - additive
TOCchange = 0.8 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TRed25TOCRed20/" 
#Temperature reduction of 2.5C and 20% TOC reduction

Tchange = -2.5 #degree C reduction - additive
TOCchange = 0.75 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TRed25TOCRed25/" 
#Temperature reduction of 2.5C and 25% TOC reduction

Tchange = -2.5 #degree C reduction - additive
TOCchange = 1.0 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TRed25/" 
#Temperature reduction of 2.5C and 25% TOC reduction

Tchange = -2.25 #degree C reduction - additive
TOCchange = 0.7 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TRed225TOCRed30/" 
#Temperature reduction of 2.5C and 30% TOC reduction

#Tchange = -2.5 #degree C reduction - additive
#TOCchange = 0.7 #1.0 is no reduction, for 30% use 0.7 - ratio 
#Qchange = 1.2 #1.0 is no reduction - ratio (flow change not an option for Fall)
#outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TRed25TOCRed30Q20/" 
#Temperature reduction of 2.5C and 30% TOC reduction and 10% Flow increase

Tchange = -2.0 #degree C reduction - additive
TOCchange = 0.7 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TRed2TOCRed30/" 
#Temperature reduction of 2C and 30% TOC reduction

Tchange = 0.0 #degree C reduction - additive
TOCchange = 0.8 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TOCRed20/" 
#Temperature reduction of zero and 20% TOC reduction

Tchange = 0.0 #degree C reduction - additive
TOCchange = 0.75 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TOCRed25/" 
#Temperature reduction of zero and 25% TOC reduction

Tchange = 0.0 #degree C reduction - additive
TOCchange = 0.7 #1.0 is no reduction, for 30% use 0.7 - ratio 
Qchange = 1.0 #1.0 is no reduction - ratio (flow change not an option for Fall)
outpath <- "I:/TMDL_WR/South_Coast/Coquille/MLR_Models/SFCoquille/Plots_spawn_TOCRed30/" 
#Temperature reduction of zero and 30% TOC reduction

#METHOD 1 MODEL - SPAWNING MODEL A - FUTURE SCENARIO
# mod.DOvTQTOCF
# DO~Temperature+transQ+transTOC
# data=dDOspawnF4  DATA SET WITH T > 15 RECORD REMOVED  n=29
# transQ = log Q (natural log)
mtitle <- "Model \"mod.DOvTQTOCF\" DO=f(T,logQ,logTOC)"; mtitle
modelname <- "mod_DOvTQTOCF"
dDOF <- dDOspawnF8 #F for future scenario
#Derive "regulatory DO" (DO values > saturation set to saturation)
dDOF$DOSat <- (14.62-(0.3898*dDOF$Temperature)+(0.006969*dDOF$Temperature^2)-(0.00005897*dDOF$Temperature^3))*(1-(0.00000697*ElevFt))^5.167
dDOF$DOPerSat <-  dDOF$DO/dDOF$DOSat * 100 #DO Percent Saturation
dDOF$DOReg <- ifelse(dDOF$DO<=dDOF$DOSat,dDOF$DO,dDOF$DOSat) # "Regulatory DO"
dDOF$DO.Cpred <- predict(mod.DOvTQTOCF, newdata=dDOF) #predicted DO for Current conditions
head(dDOF) #
#Derive correction factors based on observed current vs predicted current 
dDOF$DO.ErrRat <- dDOF$DO / dDOF$DO.Cpred #error correction Ratio   Obs / Model calculated
dDOF$DO.CpredCa <- dDOF$DO.ErrRat * dDOF$DO.Cpred #predicted Current DO corrected via correction ratio
dDOF$Temperature <- dDOspawnF8$Temperature + Tchange
TOCred <- (1.0 - TOCchange) * 100; TOCred #for plot name
dDOF$TOC <- TOCchange * dDOspawnF8$TOC
dDOF$transTOC <- log(dDOF$TOC)
dDOF$QatSFCoqPowers <- Qchange * dDOspawnF8$QatSFCoqPowers
dDOF$transQ <- log(dDOF$QatSFCoqPowers)
dDOF$DO.Fpred <- predict(mod.DOvTQTOCF, newdata=dDOF) #predicted DO for future conditions
dDOF$DO.FpredCa <- dDOF$DO.ErrRat * dDOF$DO.Fpred #predicted Future DO corrected via correction ratio
#Derive DO Saturation for future T
dDOF$DOSat.F <- (14.62-(0.3898*dDOF$Temperature)+(0.006969*dDOF$Temperature^2)-(0.00005897*dDOF$Temperature^3))*(1-(0.00000697*ElevFt))^5.167
dDOF$DOReg.FpredCa <- ifelse(dDOF$DO.FpredCa<=dDOF$DOSat.F,dDOF$DO.FpredCa,dDOF$DOSat.F) # "Regulatory DO" for future temperature
xxx <- data.frame(dDOF$DOSat.F,dDOF$DO.FpredCa,dDOF$DOReg.FpredCa); xxx
head(dDOF)
q.obs <- round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2); q.obs #observed current DO
q.DOReg.obs <- round(quantile(dDOF$DOReg,c(.05,.10, .25, .5)),2); q.DOReg.obs #observed current Regulatory DO
q.Cpred <- round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2); q.Cpred #predicted Current
q.CpredCa <- round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2); q.CpredCa #predicted Current with correction; same as q.obs
q.Fpred <- round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2); q.Fpred #predicted Future
q.FpredCa <- round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2); q.FpredCa #predicted Future with correction
q.DOReg.FpredCa <- round(quantile(dDOF$DOReg.FpredCa,c(.05,.10, .25, .5)),2); q.DOReg.FpredCa #predicted Future Regulatory DO with correction
mean(dDOF$DO)
mean(dDOF$DO.CpredCa) #should be same as observed mean
mean(dDOF$DOReg)
dDOF$DO.FpredCa
mean(dDOF$DO.FpredCa)
mean(dDOF$DOReg.FpredCa)
mean(dDOF$DO.FpredCa) - mean(dDOF$DO) #improvement in mean DO
mean(dDOF$DOReg.FpredCa) - mean(dDOF$DOReg) #improvement in mean "Regulatory DO"
#PLOT 1 - Plot model calculated current condition DO vs observed current condition
#         and model calculated future  condition DO vs observed current condition 
png(file=paste(outpath,modelname,"_Plot1_","CalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOF$DO, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(10.0,12.5), ylim=c(10.0,12.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO (uncorrected)",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,11,11)
DO.crity <- c(11,11,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(11.1, 10.2,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                               " 5th %tile=", q.Fpred[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.1,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                               " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.5,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(11.1, 10.4,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=6.4, y=10.45, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 2 - Same data as Plot 1 but vs. Time of Day - Model calculated DO for current and future conditions (uncorrected)
png(file=paste(outpath,modelname,"_Plot2_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", 
     cex.main=1.0, bg="gray", xlim = c(0.2,0.8), ylim=c(10.0,13.0), xlab = "Time of Day", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO (uncorrected)",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(11,11)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 10.3,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                              " 5th %tile=", q.Fpred[1]), cex = 0.8, adj=c(0,0))
text(0.5, 10.15,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(0.5, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                              " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 10.3,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 10.15,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=13.05, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=12.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 3 - Plot CORRECTED model calculated current condition DO vs observed current condition
#         and CORRECTED model calculated future  condition DO vs observed current condition 
#Plot shows how corrected predicted values for current conditions match observed
png(file=paste(outpath,modelname,"_Plot3_","CalculatedCorrectedCurrentDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=19, col="black", bg="gray", 
     cex.main=1.0, xlim = c(10.0,12.5), ylim=c(10.0,12.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and \"Corrected\" Calculated Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Cpred, cex=0.7, col="red", bg="red")
#abline(a=0,b=1) 
text(11.1, 10.3,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.15,labels = paste("Corrected Model Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                                " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                               " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
legend(x=10.1, y=12.3, bty="n", legend = "Corrected calculated DO", pch=21, cex=0.9, col="black", bg = "gray")
legend(x=10.1, y=12.5, bty="n", legend = "Calculated DO", pch=21, cex=0.9, col="red", bg = "red")
dev.off()
#PLOT 4 - Next plot corrected calculated DO for future conditions vs current observed DO
#         and corrected calculated DO for current conditions vs current observed DO
png(file=paste(outpath,modelname,"_Plot4_","CorrectedCalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(10.0,12.5), ylim=c(10.0,12.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("\"Corrected\" Model Calculated Future and Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,11,11)
DO.crity <- c(11,11,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(11.1, 10.25,labels = paste("Corrected Calculated Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                                " 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.125,labels = paste("Corrected Calculated Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                                 " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                               " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.5,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(11.1, 10.375,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=10, y=12.35, bty="n", legend = "Corrected calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
legend(x=10, y=12.5, bty="n",  legend = "Corrected calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
dev.off()
#PLOT 5 - Plot vs Time of Day - CORRECTED Model calculated DO for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot5_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(10.0,13.0), xlab = "Time of Day", ylab = "Corrected Model Calculated DO")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(11,11)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.4, 10.3,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                              " 10th %tile=", q.FpredCa[2]," 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.4, 10.15,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                               " 10th %tile=", q.CpredCa[2]," 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.4, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                              " 10th %tile=", q.obs[2]," 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 10.3,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 10.15,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=13.05, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=12.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 6 - Plot Regulatory DO vs Time of Day - CORRECTED Model calculated Regulatory DO for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot6_","CalculatedCurrentandFutureDORegvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DOReg, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(10.0,13.0), xlab = "Time of Day", ylab = "Regulatory DO")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DOReg.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(11,11)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.38, 10.15,labels = paste("Corrected Calc Future Reg DO:  Mean = ", round(mean(dDOF$DOReg.FpredCa),2),
                              " 10th %tile=", q.DOReg.FpredCa[2]," 5th %tile=", q.DOReg.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.38, 10.,labels = paste("Observed Current Reg DO:   Mean =", round(mean(dDOF$DOReg),2),
                              " 10th %tile=", q.DOReg.obs[2]," 5th %tile=", q.DOReg.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 10.15,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 10.,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=13.05, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=12.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()


#METHOD 2 MODEL - SPAWNING MODEL B - 3 parameter step model GENERATED BY STEP - FUTURE SCENARIO
# DO ~ Time + Temperature + QatSFCoqPowers + I(Temperature^2) + I(QatSFCoqPowers^2) + I(TOC^2), data = dDOspawnFS3
# mod.stepF
mtitle <- "Model \"mod.stepF\" DO=f(T,Q,TOC)"; mtitle
modelname <- "mod_stepF"
dDOF <- dDOspawnF8 #F for future scenario
dDOF$DO.Cpred <- predict(mod.stepF, newdata=dDOF) #predicted DO for Current conditions
head(dDOF) #
#Derive correction factors based on observed current vs predicted current 
dDOF$DO.ErrRat <- dDOF$DO / dDOF$DO.Cpred #error correction Ratio   Obs / Model calculated
dDOF$DO.CpredCa <- dDOF$DO.ErrRat * dDOF$DO.Cpred #predicted Current DO corrected via correction ratio
dDOF$Temperature <- dDOspawnF8$Temperature + Tchange
TOCred <- (1.0 - TOCchange) * 100; TOCred #for plot name
dDOF$TOC <- TOCchange * dDOspawnF8$TOC
dDOF$QatSFCoqPowers <- Qchange * dDOspawnF8$QatSFCoqPowers
dDOF$transQ <- log(dDOF$QatSFCoqPowers)
dDOF$DO.Fpred <- predict(mod.stepF, newdata=dDOF) #predicted DO for future conditions
dDOF$DO.FpredCa <- dDOF$DO.ErrRat * dDOF$DO.Fpred #predicted Future DO corrected via correction ratio
head(dDOF)
q.obs <- round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2); q.obs #observed current
q.Cpred <- round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2); q.Cpred #predicted Current
q.CpredCa <- round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2); q.CpredCa #predicted Current with correction; same as q.obs
q.Fpred <- round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2); q.Fpred #predicted Future
q.FpredCa <- round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2); q.FpredCa #predicted Future with correction
dDOF$DO.FpredCa
mean(dDOF$DO)
mean(dDOF$DO.FpredCa)
mean(dDOF$DO.FpredCa) - mean(dDOF$DO)
#PLOT 1 - Plot model calculated current condition DO vs observed current condition
#         and model calculated future  condition DO vs observed current condition 
png(file=paste(outpath,modelname,"_Plot1_","CalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOF$DO, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(10.0,12.5), ylim=c(10.0,12.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO (uncorrected)",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,11,11)
DO.crity <- c(11,11,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(11.1, 10.2,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                               " 5th %tile=", q.Fpred[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.1,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                               " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.5,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(11.1, 10.4,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=6.4, y=10.45, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 2 - Same data as Plot 1 but vs. Time of Day - Model calculated DO for current and future conditions (uncorrected)
png(file=paste(outpath,modelname,"_Plot2_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", 
     cex.main=1.0, bg="gray", xlim = c(0.2,0.8), ylim=c(10.0,13.0), xlab = "Time of Day", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO (uncorrected)",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(11,11)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 10.3,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                              " 5th %tile=", q.Fpred[1]), cex = 0.8, adj=c(0,0))
text(0.5, 10.15,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(0.5, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                              " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 10.3,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 10.15,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=13.05, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=12.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 3 - Plot CORRECTED model calculated current condition DO vs observed current condition
#         and CORRECTED model calculated future  condition DO vs observed current condition 
#Plot shows how corrected predicted values for current conditions match observed
png(file=paste(outpath,modelname,"_Plot3_","CalculatedCorrectedCurrentDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=19, col="black", bg="gray", 
     cex.main=1.0, xlim = c(10.0,12.5), ylim=c(10.0,12.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and \"Corrected\" Calculated Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Cpred, cex=0.7, col="red", bg="red")
#abline(a=0,b=1) 
text(11.1, 10.3,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.15,labels = paste("Corrected Model Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                                " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                               " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
legend(x=10.1, y=12.3, bty="n", legend = "Corrected calculated DO", pch=21, cex=0.9, col="black", bg = "gray")
legend(x=10.1, y=12.5, bty="n", legend = "Calculated DO", pch=21, cex=0.9, col="red", bg = "red")
dev.off()
#PLOT 4 - Next plot corrected calculated DO for future conditions vs current observed DO
#         and corrected calculated DO for current conditions vs current observed DO
png(file=paste(outpath,modelname,"_Plot4_","CorrectedCalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(10.0,12.5), ylim=c(10.0,12.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("\"Corrected\" Model Calculated Future and Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,11,11)
DO.crity <- c(11,11,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(11.1, 10.25,labels = paste("Corrected Calculated Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                                " 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.125,labels = paste("Corrected Calculated Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                                 " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                               " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.5,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(11.1, 10.375,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=10, y=12.35, bty="n", legend = "Corrected calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
legend(x=10, y=12.5, bty="n",  legend = "Corrected calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
dev.off()
#PLOT 5 - Plot vs Time of Day - CORRECTED Model calculated DO for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot5_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(10.0,13.0), xlab = "Time of Day", ylab = "Corrected Model Calculated DO")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(11,11)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.4, 10.3,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                              " 10th %tile=", q.FpredCa[2]," 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.4, 10.15,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                               " 10th %tile=", q.CpredCa[2]," 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.4, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                              " 10th %tile=", q.obs[2]," 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 10.3,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 10.15,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=13.05, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=12.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()

#METHOD 2 MODEL - SPAWNING MODEL C - FUTURE SCENARIO
# mod.stepF6
# DO ~ Time + Temperature + QatSFCoqPowers + I(Temperature^2), 
# data = dDOspawnFS2
mtitle <- "Model \"mod.stepF6\" DO=f(Time,T,Q)"; mtitle
modelname <- "mod_stepF6"
dDOF <- dDOspawnF6 #F for future scenario, dDOspawnF6 includes TOC record and high T record
dDOF$DO.Cpred <- predict(mod.stepF6, newdata=dDOF) #predicted DO for Current conditions
head(dDOF) #
#Derive correction factors based on observed current vs predicted current 
dDOF$DO.ErrRat <- dDOF$DO / dDOF$DO.Cpred #error correction Ratio   Obs / Model calculated
dDOF$DO.CpredCa <- dDOF$DO.ErrRat * dDOF$DO.Cpred #predicted Current DO corrected via correction ratio
dDOF$Temperature <- dDOspawnF6$Temperature + Tchange
#TOCred <- (1.0 - TOCchange) * 100; TOCred #for plot name
#dDOF$TOC <- TOCchange * dDOspawnF8$TOC
dDOF$QatSFCoqPowers <- Qchange * dDOspawnF6$QatSFCoqPowers
#dDOF$transQ <- log(dDOF$QatSFCoqPowers)
dDOF$DO.Fpred <- predict(mod.stepF6, newdata=dDOF) #predicted DO for future conditions
dDOF$DO.FpredCa <- dDOF$DO.ErrRat * dDOF$DO.Fpred #predicted Future DO corrected via correction ratio
head(dDOF)
q.obs <- round(quantile(dDOF$DO,c(.05,.10, .25, .5)),2); q.obs #observed current
q.Cpred <- round(quantile(dDOF$DO.Cpred,c(.05,.10, .25, .5)),2); q.Cpred #predicted Current
q.CpredCa <- round(quantile(dDOF$DO.CpredCa,c(.05,.10, .25, .5)),2); q.CpredCa #predicted Current with correction; same as q.obs
q.Fpred <- round(quantile(dDOF$DO.Fpred,c(.05,.10, .25, .5)),2); q.Fpred #predicted Future
q.FpredCa <- round(quantile(dDOF$DO.FpredCa,c(.05,.10, .25, .5)),2); q.FpredCa #predicted Future with correction
dDOF$DO.FpredCa
mean(dDOF$DO)
mean(dDOF$DO.FpredCa)
mean(dDOF$DO.FpredCa) - mean(dDOF$DO)
#PLOT 1 - Plot model calculated current condition DO vs observed current condition
#         and model calculated future  condition DO vs observed current condition 
png(file=paste(outpath,modelname,"_Plot1_","CalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOF$DO, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(10.0,12.5), ylim=c(10.0,12.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO (uncorrected)",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,11,11)
DO.crity <- c(11,11,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(11.1, 10.2,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                               " 5th %tile=", q.Fpred[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.1,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                               " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.5,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(11.1, 10.4,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=6.4, y=10.7, bty="n",  legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=6.4, y=10.45, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 2 - Same data as Plot 1 but vs. Time of Day - Model calculated DO for current and future conditions (uncorrected)
png(file=paste(outpath,modelname,"_Plot2_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.Cpred, main=mtitle, pch=21, col="black", 
     cex.main=1.0, bg="gray", xlim = c(0.2,0.8), ylim=c(10.0,13.0), xlab = "Time of Day", ylab = "Model Calculated DO")
mtext("Model Calculated Current and Future DO (uncorrected)",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(11,11)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 10.3,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOF$DO.Fpred),2),
                              " 5th %tile=", q.Fpred[1]), cex = 0.8, adj=c(0,0))
text(0.5, 10.15,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(0.5, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                              " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 10.3,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 10.15,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=13.05, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=12.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 3 - Plot CORRECTED model calculated current condition DO vs observed current condition
#         and CORRECTED model calculated future  condition DO vs observed current condition 
#Plot shows how corrected predicted values for current conditions match observed
png(file=paste(outpath,modelname,"_Plot3_","CalculatedCorrectedCurrentDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=19, col="black", bg="gray", 
     cex.main=1.0, xlim = c(10.0,12.5), ylim=c(10.0,12.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("Model Calculated Current and \"Corrected\" Calculated Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.Cpred, cex=0.7, col="red", bg="red")
#abline(a=0,b=1) 
text(11.1, 10.3,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOF$DO.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.15,labels = paste("Corrected Model Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                                " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                               " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
legend(x=10.1, y=12.3, bty="n", legend = "Corrected calculated DO", pch=21, cex=0.9, col="black", bg = "gray")
legend(x=10.1, y=12.5, bty="n", legend = "Calculated DO", pch=21, cex=0.9, col="red", bg = "red")
dev.off()
#PLOT 4 - Next plot corrected calculated DO for future conditions vs current observed DO
#         and corrected calculated DO for current conditions vs current observed DO
png(file=paste(outpath,modelname,"_Plot4_","CorrectedCalculatedCurrentandFutureDO.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$DO, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(10.0,12.5), ylim=c(10.0,12.5), xlab = "Observed DO", ylab = "Model Calculated DO")
mtext("\"Corrected\" Model Calculated Future and Current DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$DO, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DO.critx <- c(6.3,11,11)
DO.crity <- c(11,11,6.3)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(11.1, 10.25,labels = paste("Corrected Calculated Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                                " 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.125,labels = paste("Corrected Calculated Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                                 " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                               " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.5,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(11.1, 10.375,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=10, y=12.35, bty="n", legend = "Corrected calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
legend(x=10, y=12.5, bty="n",  legend = "Corrected calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
dev.off()
#PLOT 5 - Plot vs Time of Day - CORRECTED Model calculated DO for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot5_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOF$Time, dDOF$DO.CpredCa, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(10.0,13.0), xlab = "Time of Day", ylab = "Corrected Model Calculated DO")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOF$Time, dDOF$DO.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DO.critx <- c(0.18,0.82)
DO.crity <- c(11,11)
lines(DO.critx,DO.crity, type="l", lty=2, lwd=2, col="black")
text(0.4, 10.3,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOF$DO.FpredCa),2),
                              " 10th %tile=", q.FpredCa[2]," 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.4, 10.15,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOF$DO.CpredCa),2),
                               " 10th %tile=", q.CpredCa[2]," 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.4, 10.0,labels = paste("Observed Current DO:   Mean =", round(mean(dDOF$DO),2),
                              " 10th %tile=", q.obs[2]," 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 10.3,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 10.15,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=13.05, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=12.9, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()


#mod.DOPERSATvQTOCBODF - SPAWNING DOPERSAT MODEL A3 - USE THIS
#DOPerSat~transQ+transTOC+BOD5
#data=dDOPERSATspawnF5  28 records (TOC and BOD5 NAs removed)
#DOPerSat Time Temperature transQ    transTP OrthophosphateP    DIN BOD5  transTOC transTSS transTurbidity Jday
dDOPERSATF <- dDOPERSATspawnF5 #F for future scenario
mtitle <- "Model \"mod.DOPERSATvQTOCBODF\" DO=f(Q,TOC,BOD5)"; mtitle
modelname <- "mod_DOPERSATvQTOCBODF"
dDOPERSATF$DOPerSat.Cpred <- predict(mod.DOPERSATvQTOCBODF, newdata=dDOPERSATF) #predicted DO for Current conditions
str(dDOPERSATF) #
#Derive correction factors based on observed current vs predicted current 
dDOPERSATF$DOPerSat.ErrRat <- dDOPERSATF$DOPerSat / dDOPERSATF$DOPerSat.Cpred #error correction Ratio   Obs / Model calculated
dDOPERSATF$DOPerSat.CpredCa <- dDOPERSATF$DOPerSat.ErrRat * dDOPERSATF$DOPerSat.Cpred #predicted Current DO corrected via correction ratio
dDOPERSATF$Temperature <- dDOPERSATspawnF5$Temperature + Tchange
dDOPERSATF$transQ <- log(Qchange * exp(dDOPERSATspawnF5$transQ)); dDOPERSATF$transQ  #apply flow change
TOCred <- (1.0 - TOCchange) * 100; TOCred #for plot text
dDOPERSATF$transTOC <- log(TOCchange * exp(dDOPERSATspawnF5$transTOC)); dDOPERSATF$transTOC #apply TOC change
dDOPERSATF$DOPerSat.Fpred <- predict(mod.DOPERSATvQTOCBODF, newdata=dDOPERSATF) #predicted DO for future conditions
dDOPERSATF$DOPerSat.FpredCa <- dDOPERSATF$DOPerSat.ErrRat * dDOPERSATF$DOPerSat.Fpred #predicted Future DO corrected via correction ratio
head(dDOPERSATF)
dDOPERSATF$DOPerSat
q.obs <- round(quantile(dDOPERSATF$DOPerSat,c(.05,.10, .25, .5)),2); q.obs #observed current
q.Cpred <- round(quantile(dDOPERSATF$DOPerSat.Cpred,c(.05,.10, .25, .5)),2); q.Cpred #predicted Current
q.CpredCa <- round(quantile(dDOPERSATF$DOPerSat.CpredCa,c(.05,.10, .25, .5)),2); q.CpredCa #predicted Current with correction; same as q.obs
q.Fpred <- round(quantile(dDOPERSATF$DOPerSat.Fpred,c(.05,.10, .25, .5)),2); q.Fpred #predicted Future
q.FpredCa <- round(quantile(dDOPERSATF$DOPerSat.FpredCa,c(.05,.10, .25, .5)),2); q.FpredCa #predicted Future with correction
dDOPERSATF$DOPerSat
mean(dDOPERSATF$DOPerSat)
dDOPERSATF$DOPerSatRc <- dDOPERSATF$DOPerSat #Calculated regulatory mean 
dDOPERSATF$DOPerSatR <- ifelse(dDOPERSATF$DOPerSatRc>100,100,dDOPERSATF$DOPerSatRc); dDOPERSATF$DOPerSatR #regulatory DOPerSat
mean(dDOPERSATF$DOPerSatR) #Mean Min (Regulatory Avg)
dDOPERSATF$DOPerSat.FpredCa
mean(dDOPERSATF$DOPerSat.FpredCa)
mean(dDOPERSATF$DOPerSat.FpredCa)-mean(dDOPERSATF$DOPerSat)
dDOPERSATF$DOPerSatRc.FpredCa <- dDOPERSATF$DOPerSat.FpredCa 
dDOPERSATF$DOPerSatR.FpredCa <- ifelse(dDOPERSATF$DOPerSatRc.FpredCa>100,100,dDOPERSATF$DOPerSatRc.FpredCa); dDOPERSATF$DOPerSatR.FpredCa #regulatory DOPerSat
mean(dDOPERSATF$DOPerSatR.FpredCa) #Mean Min (Regulatory Avg)
data.frame(dDOPERSATF$DOPerSat,dDOPERSATF$DOPerSat.FpredCa,dDOPERSATF$DOPerSatR,dDOPERSATF$DOPerSatR.FpredCa)
#PLOT 1 - Plot model calculated current condition DOPerSat vs observed current condition
#         and model calculated future  condition DOPerSat vs observed current condition 
png(file=paste(outpath,modelname,"_Plot1_","CalculatedCurrentandFutureDOPerSat.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATF$DOPerSat, dDOPERSATF$DOPerSat.Cpred, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(90,110), ylim=c(90,110), xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
mtext("Model Calculated Current and Future DOPerSat (uncorrected)",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOPERSATF$DOPerSat, dDOPERSATF$DOPerSat.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DOPerSat.critx <- c(6.3,95,95)
DOPerSat.crity <- c(95,95,6.3)
lines(DOPerSat.critx,DOPerSat.crity, type="l", lty=2, lwd=2, col="black")
text(97, 91.5,labels = paste("Model Calc Future DOPerSat:  Mean = ", round(mean(dDOPERSATF$DOPerSat.Fpred),2),
                             " 5th %tile=", q.Fpred[1]), cex = 0.8, adj=c(0,0))
text(97, 90.75,labels = paste("Model Calc Current DOPerSat: Mean =", round(mean(dDOPERSATF$DOPerSat.Cpred),2),
                              " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(97, 90,labels = paste("Observed Current DOPerSat:   Mean =", round(mean(dDOPERSATF$DOPerSat),2),
                           " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.5,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(97, 92.5,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=90, y=110, bty="n",  legend = "Calculated future DOPerSat", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=90, y=109, bty="n", legend = "Calculated current DOPerSat", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 2 - Same data as Plot 1 but vs. Time of Day - Model calculated DOPerSat for current and future conditions (uncorrected)
png(file=paste(outpath,modelname,"_Plot2_","CalculatedCurrentandFutureDOvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOPERSATF$Time, dDOPERSATF$DOPerSat.Cpred, main=mtitle, pch=21, col="black", 
     cex.main=1.0, bg="gray", xlim = c(0.2,0.8), ylim=c(90,110), xlab = "Time of Day", ylab = "Model Calculated DOPerSat")
mtext("Model Calculated Current and Future DOPerSat (uncorrected)",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOPERSATF$Time, dDOPERSATF$DOPerSat.Fpred, cex=0.7, pch = 24, col="blue", bg="blue")
DOPerSat.critx <- c(0.18,0.82)
DOPerSat.crity <- c(95,95)
lines(DOPerSat.critx,DOPerSat.crity, type="l", lty=2, lwd=2, col="black")
text(0.5, 91.5,labels = paste("Model Calc Future DO:  Mean = ", round(mean(dDOPERSATF$DOPerSat.Fpred),2),
                              " 5th %tile=", q.Fpred[1]), cex = 0.8, adj=c(0,0))
text(0.5, 90.75,labels = paste("Model Calc Current DO: Mean =", round(mean(dDOPERSATF$DOPerSat.Cpred),2),
                               " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(0.5, 90,labels = paste("Observed Current DO:   Mean =", round(mean(dDOPERSATF$DOPerSat),2),
                            " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 10.3,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 91.5,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=110, bty="n", legend = "Calculated future DOPerSat", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=109, bty="n", legend = "Calculated current DOPerSat", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()
#PLOT 3 - Plot CORRECTED model calculated current condition DOPerSat vs observed current condition
#         and CORRECTED model calculated future  condition DOPerSat vs observed current condition 
#Plot shows how corrected predicted values for current conditions match observed
png(file=paste(outpath,modelname,"_Plot3_","CalculatedCorrectedCurrentDOPerSat.png",sep=""), width = 665, height = 500) #dev.off()
par(mfrow=c(1,1))
plot(dDOPERSATF$DOPerSat, dDOPERSATF$DOPerSat.CpredCa, main=mtitle, pch=19, col="black", bg="gray", 
     cex.main=1.0, xlim = c(90,110), ylim=c(90,110), xlab = "Observed DOPerSat", ylab = "Corrected Model Calculated DOPerSat")
mtext("Model Calculated Current and \"Corrected\" Calculated Current DOPerSat",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOPERSATF$DOPerSat, dDOPERSATF$DOPerSat.Cpred, cex=0.7, col="red", bg="red")
#abline(a=0,b=1) 
text(97, 91.5,labels = paste("Model Calc Current DOPerSat: Mean =", round(mean(dDOPERSATF$DOPerSat.Cpred),2),
                             " 5th %tile=", q.Cpred[1]), cex = 0.8, adj=c(0,0))
text(97, 90.75,labels = paste("Corrected Model Calc Current DOPerSat: Mean =", round(mean(dDOPERSATF$DOPerSat.CpredCa),2),
                              " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(97, 90,labels = paste("Observed Current DOPerSat:   Mean =", round(mean(dDOPERSATF$DOPerSat),2),
                           " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
legend(x=90, y=110, bty="n", legend = "Corrected calculated DOPerSat", pch=21, cex=0.9, col="black", bg = "gray")
legend(x=90, y=109, bty="n", legend = "Calculated DOPerSat", pch=21, cex=0.9, col="red", bg = "red")
dev.off()
#PLOT 4 - Next plot corrected calculated DOPerSat for future conditions vs current observed DO
#         and corrected calculated DOPerSat for current conditions vs current observed DO
png(file=paste(outpath,modelname,"_Plot4_","CorrectedCalculatedCurrentandFutureDOPerSat.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOPERSATF$DOPerSat, dDOPERSATF$DOPerSat.CpredCa, main=mtitle, pch=21, col="black", bg="gray", 
     cex.main=1.0, xlim = c(90,110), ylim=c(90,110), xlab = "Observed DOPerSat", ylab = "Model Calculated DOPerSat")
mtext("\"Corrected\" Model Calculated Future and Current DOPerSat",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOPERSATF$DOPerSat, dDOPERSATF$DOPerSat.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
abline(a=0,b=1) 
DOPerSat.critx <- c(6.3,95,95)
DOPerSat.crity <- c(95,95,6.3)
lines(DOPerSat.critx,DOPerSat.crity, type="l", lty=2, lwd=2, col="black")
text(97,91.5,labels = paste("Corrected Calculated Future DO:  Mean = ", round(mean(dDOPERSATF$DOPerSat.FpredCa),2),
                            " 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(97,90.75,labels = paste("Corrected Calculated Current DO: Mean =", round(mean(dDOPERSATF$DOPerSat.CpredCa),2),
                             " 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(97,90,labels = paste("Observed Current DO:   Mean =", round(mean(dDOPERSATF$DOPerSat),2),
                          " 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(11.1, 10.5,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(97, 92.5,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=90, y=110, bty="n", legend = "Corrected calculated current DOPerSat", pch=21, cex=0.8, col="black", pt.bg = "gray")
legend(x=90, y=109, bty="n",  legend = "Corrected calculated future DOPerSat", pch=24, cex=0.8, col="blue", pt.bg = "blue")
dev.off()
#PLOT 5 - Plot vs Time of Day - CORRECTED Model calculated DOPerSat for current conditions and future conditions
png(file=paste(outpath,modelname,"_Plot5_","CalculatedCurrentandFutureDOPerSatvTime.png",sep=""), width = 665, height = 500) #dev.off()
plot(dDOPERSATF$Time, dDOPERSATF$DOPerSat.CpredCa, main=mtitle, pch=21, col="black", bg="gray",
     cex.main=1.0, xlim = c(0.2,0.8), ylim=c(90,110), xlab = "Time of Day", ylab = "Corrected Model Calculated DOPerSat")
mtext("\"Corrected\" Model Calculated Current and Future DO",side = 3, line=0.5, cex=0.9, outer=FALSE)
points(dDOPERSATF$Time, dDOPERSATF$DOPerSat.FpredCa, cex=0.7, pch = 24, col="blue", bg="blue")
DOPerSat.critx <- c(0.18,0.82)
DOPerSat.crity <- c(95,95)
lines(DOPerSat.critx,DOPerSat.crity, type="l", lty=2, lwd=2, col="black")
text(0.4, 91.5,labels = paste("Corrected Calc Future DO:  Mean = ", round(mean(dDOPERSATF$DOPerSat.FpredCa),2),
                              " 10th %tile=", q.FpredCa[2]," 5th %tile=", q.FpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.4, 90.75,labels = paste("Corrected Calc Current DO: Mean =", round(mean(dDOPERSATF$DOPerSat.CpredCa),2),
                               " 10th %tile=", q.CpredCa[2]," 5th %tile=", q.CpredCa[1]), cex = 0.8, adj=c(0,0))
text(0.4, 90,labels = paste("Observed Current DO:   Mean =", round(mean(dDOPERSATF$DOPerSat),2),
                            " 10th %tile=", q.obs[2]," 5th %tile=", q.obs[1]), cex = 0.8, adj=c(0,0))
text(0.2, 10.3,labels = paste("Temperature change = ", Tchange, "C"), cex = 0.8, adj=c(0,0))
text(0.2, 91.5,labels = paste("TOC reduction = ", TOCred, "%"), cex = 0.8, adj=c(0,0))
legend(x=0.18, y=110, bty="n", legend = "Calculated future DO", pch=24, cex=0.8, col="blue", pt.bg = "blue")
legend(x=0.18, y=109, bty="n", legend = "Calculated current DO", pch=21, cex=0.8, col="black", pt.bg = "gray")
dev.off()

