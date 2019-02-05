## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = F)
library(here)


## ------------------------------------------------------------------------
baggage = read.csv(here("HW1", "Baggage.csv"), header=T, stringsAsFactors = F)

indus_med = read.csv(here("HW1","IndustryMedians.csv"),header=T)
head(baggage)

## ------------------------------------------------------------------------
baggage$Date = as.Date(paste0("02/",baggage$Date),"%d/%m/%Y")
baggage$Month = factor(baggage$Month,
                       labels=c("Jan","Feb","Mar","Apr","May",
                                "Jun","Jul","Aug","Sep",
                                "Oct","Nov","Dec"))
baggage$Airline = as.character(baggage$Airline)

## ------------------------------------------------------------------------
airlines = unique(baggage$Airline)
for(i in 1:length(airlines)){
    airline = airlines[i]
    data = baggage[baggage$Airline == airline,]
    res = aggregate(data["Baggage"], by=list(Month = data$Month), sum)
    plot(x=as.integer(res$Month),y=res$Baggage,type="o",xaxt="n",xlab="Month", ylab="Number of Baggage Complaints")
    axis(1,at = seq(1,12),labels = levels(res$Month))
    title(paste(airline,"Baggage Complaints (2004-2010)"))
    
}

## ------------------------------------------------------------------------
airlines = unique(baggage$Airline)
airline = airlines[1]
data = baggage[baggage$Airline == airline,]
res = aggregate(data["Baggage"], by=list(Month = data$Month, Year = data$Year), sum)
years = unique(data$Year)
plot_dat = res[res$Year == years[1],]

#bottom,left,top,right margin
par(mar=c(7.1, 4.1, 3.1, 4.9), xpd=TRUE)

plot(x=as.integer(plot_dat$Month),y=plot_dat$Baggage,type="o",xaxt="n",xlab="", ylab="Number of Baggage Complaints",lty=1, col=1, pch = 1,ylim = c(min(res$Baggage),max(res$Baggage)))
axis(1,at = seq(1,12),labels = levels(res$Month))
title(paste(airline,"Baggage Complaints"))   
for(j in 1:length(years)){
    plot_dat = res[res$Year == years[j],]
    lines(x=as.integer(plot_dat$Month),y=plot_dat$Baggage,type="o",lty=j, col=j,pch=j)
    
}
legend("topright", inset=c(-0.2,0), legend=years, pch=1:length(years),lty=1:length(years),col=1:length(years), title="Years")


## ------------------------------------------------------------------------
airline = airlines[2]
data = baggage[baggage$Airline == airline,]
res = aggregate(data["Baggage"], by=list(Month = data$Month, Year = data$Year), sum)
years = unique(data$Year)
plot_dat = res[res$Year == years[1],]

#bottom,left,top,right margin
par(mar=c(7.1, 4.1, 3.1, 4.9), xpd=TRUE)

plot(x=as.integer(plot_dat$Month),y=plot_dat$Baggage,type="o",xaxt="n",xlab="",
     ylab="Number of Baggage Complaints",lty=1, col=1, pch = 1,ylim = c(min(res$Baggage),max(res$Baggage)))
axis(1,at = seq(1,12),labels = levels(res$Month))
title(paste(airline,"Baggage Complaints"))   
for(j in 1:length(years)){
    plot_dat = res[res$Year == years[j],]
    lines(x=as.integer(plot_dat$Month),y=plot_dat$Baggage,type="o",lty=j, col=j,pch=j)
    
}
legend("topright", inset=c(-0.2,0), legend=years, pch=1:length(years),
       lty=1:length(years),col=1:length(years), title="Years")


## ------------------------------------------------------------------------
airline = airlines[3]
data = baggage[baggage$Airline == airline,]
res = aggregate(data["Baggage"], by=list(Month = data$Month, Year = data$Year), sum)
years = unique(data$Year)
plot_dat = res[res$Year == years[1],]

#bottom,left,top,right margin
par(mar=c(7.1, 4.1, 3.1, 4.9), xpd=TRUE)

plot(x=as.integer(plot_dat$Month),y=plot_dat$Baggage,type="o",xaxt="n",xlab="",
     ylab="Number of Baggage Complaints",lty=1, col=1, pch = 1,ylim = c(min(res$Baggage),max(res$Baggage)))
axis(1,at = seq(1,12),labels = levels(res$Month))
title(paste(airline,"Baggage Complaints"))   
for(j in 1:length(years)){
    plot_dat = res[res$Year == years[j],]
    lines(x=as.integer(plot_dat$Month),y=plot_dat$Baggage,type="o",lty=j, col=j,pch=j)
    
}
legend("topright", inset=c(-0.2,0), legend=years, pch=1:length(years),
       lty=1:length(years),col=1:length(years), title="Years")


## ------------------------------------------------------------------------
# Maybe do this on the log scale?
airlines = unique(baggage$Airline)
airline = airlines[1]

total_aggregated = aggregate(baggage["Baggage"], by=list(Date = baggage$Date,Airline=baggage$Airline), sum)

res = total_aggregated[baggage$Airline == airline,]
res_ts = ts(res$Baggage, frequency = 12, start = 2004)
tsp = attributes(res_ts)$tsp
dates = seq(as.Date("2004-01-02"), by = "month", along = res_ts)

#bottom,left,top,right margin
par(mar=c(7.1, 4.1, 3.1, 4.9), xpd=TRUE)

plot(res_ts,type="o",xaxt="n",xlab="Month", 
     ylab="Number of Baggage Complaints",lty=1, col=1, pch = 1,
     ylim= c(0,max(total_aggregated$Baggage)))+ axis(1, at = seq(tsp[1], tsp[2], along = res_ts),
                                                     labels = format(dates, "%Y-%m"))
title("Baggage Complaints for all 3 Airlines (2004-2010)")
for(i in 2:length(airlines)){
    airline = airlines[i]
    res = total_aggregated[baggage$Airline == airline,]
    res_ts = ts(res$Baggage, frequency = 12, start = 2004)
    lines(res_ts,type="o",lty=i, col=i,pch=i)
}
legend("topright", inset=c(-0.075,0), legend=airlines, 
       pch=1:length(airlines),lty=1:length(airlines),col=1:length(airlines), title="Airlines")


## ------------------------------------------------------------------------
mean_scheduled = rep(0, length(unique(airlines)))
names(mean_scheduled) = unique(airlines)
for(i in 1:length(unique(airlines)))
{
  mean_scheduled[i] = mean(baggage[baggage$Airline == airlines[i],6])
}

## ------------------------------------------------------------------------
mean_enplaned = rep(0, length(unique(airlines)))
names(mean_enplaned) = unique(airlines)
for(i in 1:length(unique(airlines)))
{
  mean_enplaned[i] = mean(baggage[baggage$Airline == airlines[i],8])
}

## ------------------------------------------------------------------------
mean_scheduled

## ------------------------------------------------------------------------
mean_enplaned

## ------------------------------------------------------------------------
baggage$Baggage_perc = baggage$Baggage / baggage$Enplaned * 100

mean_kpi = rep(0, length(unique(airlines)))
names(mean_kpi) = unique(airlines)
for(i in 1:length(unique(airlines)))
{
  mean_kpi[i] = mean(baggage[baggage$Airline == airlines[i],9])
}

## ------------------------------------------------------------------------
for(i in 1: length(unique(airlines)))
  print(paste(unique(airlines), round(mean_kpi*100,2),"%")[i])


## ------------------------------------------------------------------------

airlines = unique(baggage$Airline)
airline = airlines[1]

perc_aggregated = aggregate(baggage["Baggage_perc"], by=list(Date = baggage$Date,Airline=baggage$Airline), sum)

res = perc_aggregated[perc_aggregated$Airline == airline,]
res_ts = ts(res$Baggage_perc, frequency = 12, start = 2004)
tsp = attributes(res_ts)$tsp
dates = seq(as.Date("2004-01-02"), by = "month", along = res_ts)


par(mar=c(7.1, 4.1, 3.1, 8.9), xpd=TRUE)

plot(res_ts,type="o",xaxt="n",xlab="Month", ylab="Number of Baggage %",lty=1, col=1, pch = 1,ylim= c(0,max(perc_aggregated$Baggage_perc)))+ axis(1, at = seq(tsp[1], tsp[2], along = res_ts), labels = format(dates, "%Y-%m"))
title("Baggage % for all 3 Airlines (2004-2010)")
for(i in 2:length(airlines)){
    airline = airlines[i]
    res = perc_aggregated[perc_aggregated$Airline == airline,]
    res_ts = ts(res$Baggage_perc, frequency = 12, start = 2004)
    lines(res_ts,type="o",lty=i, col=i,pch=i)
}
legend("topright", inset=c(-0.375,0), legend=airlines, pch=1:length(airlines),lty=1:length(airlines),col=1:length(airlines), title="Airlines")


## ------------------------------------------------------------------------

airlines = unique(baggage$Airline)
airline = airlines[1]
perc_aggregated = aggregate(baggage["Baggage_perc"], by=list(Date = baggage$Date,Airline=baggage$Airline), sum)
res = perc_aggregated[perc_aggregated$Airline == airline,]



res = perc_aggregated[baggage$Airline == airline,]
res_ts = ts(res$Baggage_perc, frequency = 12, start = 2004)
tsp = attributes(res_ts)$tsp
dates = seq(as.Date("2004-01-02"), by = "month", along = res_ts)


# par(mar=c(7.1, 4.1, 3.1, 8.9), xpd=TRUE)

plot(res_ts,type="o",xaxt="n",xlab="Month", ylab="Number of Baggage %",lty=1, col=1, pch = 1,ylim=c(0,max(res$Baggage_perc)))+ axis(1, at = seq(tsp[1], tsp[2], along = res_ts), labels = format(dates, "%Y-%m"))
title(paste0("Baggage % for ", airline,  " (2004-2010)"))


## ------------------------------------------------------------------------
airline = airlines[2]
perc_aggregated = aggregate(baggage["Baggage_perc"], by=list(Date = baggage$Date,Airline=baggage$Airline), sum)
res = perc_aggregated[perc_aggregated$Airline == airline,]



res = perc_aggregated[baggage$Airline == airline,]
res_ts = ts(res$Baggage_perc, frequency = 12, start = 2004)
tsp = attributes(res_ts)$tsp
dates = seq(as.Date("2004-01-02"), by = "month", along = res_ts)


# par(mar=c(7.1, 4.1, 3.1, 8.9), xpd=TRUE)

plot(res_ts,type="o",xaxt="n",xlab="Month", ylab="Number of Baggage %",lty=1, col=1, pch = 1,ylim=c(0,max(res$Baggage_perc)))+ axis(1, at = seq(tsp[1], tsp[2], along = res_ts), labels = format(dates, "%Y-%m"))
title(paste0("Baggage % for ", airline,  " (2004-2010)"))


## ------------------------------------------------------------------------
airline = airlines[3]
perc_aggregated = aggregate(baggage["Baggage_perc"], by=list(Date = baggage$Date,Airline=baggage$Airline), sum)
res = perc_aggregated[perc_aggregated$Airline == airline,]



res = perc_aggregated[baggage$Airline == airline,]
res_ts = ts(res$Baggage_perc, frequency = 12, start = 2004)
tsp = attributes(res_ts)$tsp
dates = seq(as.Date("2004-01-02"), by = "month", along = res_ts)


# par(mar=c(7.1, 4.1, 3.1, 8.9), xpd=TRUE)

plot(res_ts,type="o",xaxt="n",xlab="Month", ylab="Number of Baggage %",lty=1, col=1, pch = 1,ylim=c(0,max(res$Baggage_perc)))+ axis(1, at = seq(tsp[1], tsp[2], along = res_ts), labels = format(dates, "%Y-%m"))
title(paste0("Baggage % for ", airline,  " (2004-2010)"))


## ------------------------------------------------------------------------

airlines = unique(baggage$Airline)
airline = airlines[1]
perc_averaged= aggregate(baggage["Baggage_perc"], by=list(Date = baggage$Month,Airline=baggage$Airline), mean)
res = perc_averaged[perc_averaged$Airline == airline,]
plot(x=1:12, y=res$Baggage_perc, type="o",xlab="Month", ylab="Month Average of Baggage %",lty=1, col=1, pch = 1,ylim= c(0,max(perc_averaged$Baggage_perc)))

title("Monthly Averages of Baggage % for all 3 Airlines (2004-2010)")
for(i in 2:length(airlines)){
    airline = airlines[i]
    res = perc_averaged[perc_averaged$Airline == airline,]
    lines(res$Baggage_perc,type="o",lty=i, col=i,pch=i)
}
legend("topright", inset=c(-0.375,0), legend=airlines, pch=1:length(airlines),lty=1:length(airlines),col=1:length(airlines), title="Airlines")

## ------------------------------------------------------------------------
airlines = unique(baggage$Airline)
for(i in 1:length(airlines))
{
  airline = airlines[i]
  data = baggage[baggage$Airline == airline,]
  res = aggregate(data["Baggage_perc"], by=list(Month = data$Month, Year = data$Year), sum)
  years = unique(data$Year)
  plot_dat = res[res$Year == years[1],]
  
  #bottom,left,top,right margin
  par(mar=c(7.1, 4.1, 3.1, 8.9), xpd=TRUE)
  
  plot(x=as.integer(plot_dat$Month),y=plot_dat$Baggage_perc,type="o",xaxt="n",xlab="", ylab="Baggage % ",lty=3, col=3, pch = 3,ylim = c(min(res$Baggage_perc),max(res$Baggage_perc)))
  axis(1,at = seq(1,12),labels = levels(res$Month))
  title(paste(airline,"Baggage %"))   
  for(j in 2:length(years)){
    plot_dat = res[res$Year == years[j],]
    lines(x=as.integer(plot_dat$Month),y=plot_dat$Baggage_perc,type="o",lty=j+2, col=j+2,pch=j+2)
    
  }
  # add average line
  perc_averaged= aggregate(res["Baggage_perc"], by=list(Date = res$Month), mean)
  lines(x=as.integer(plot_dat$Month),y=perc_averaged$Baggage_perc, type="o",lty=1, col=1,pch=1)
  
  
  legend("topright", inset=c(-0.2,0), legend=c("Average", "Regression", years), lty=1:(length(years)+2),col=1:(length(years)+2), title="Years")
  
  # add regression line
  lm_perc = lm(perc_averaged$Baggage_perc~as.integer(plot_dat$Month))
  clip(min(as.integer(plot_dat$Month))-0.48, max(as.integer(plot_dat$Month))-0.9, min(res$Baggage),max(res$Baggage_perc))
  abline(lm_perc, lty = 2, col=2)
  
}

## ------------------------------------------------------------------------
airline = airlines[2]
data = baggage[baggage$Airline == airline,]
res = aggregate(data["Baggage_perc"], by=list(Month = data$Month, Year = data$Year), sum)
years = unique(data$Year)
plot_dat = res[res$Year == years[1],]

#bottom,left,top,right margin
par(mar=c(7.1, 4.1, 3.1, 8.9), xpd=TRUE)

plot(x=as.integer(plot_dat$Month),y=plot_dat$Baggage_perc,type="o",
   xaxt="n",xlab="", ylab="Baggage % ",lty=2, col=2, pch = 2,
   ylim = c(min(res$Baggage_perc),max(res$Baggage_perc)))
axis(1,at = seq(1,12),labels = levels(res$Month))
title(paste(airline,"Baggage %"))   
for(j in 1:length(years)){
  plot_dat = res[res$Year == years[j],]
  lines(x=as.integer(plot_dat$Month),
        y=plot_dat$Baggage_perc,type="o",lty=j+2, col=j+2,pch=j+2)
    
}
# add average line
perc_averaged= aggregate(res["Baggage_perc"], by=list(Date = res$Month), mean)
lines(x=as.integer(plot_dat$Month),y=perc_averaged$Baggage_perc, type="o",lty=1, col=1,pch=1)


legend("topright", inset=c(-0.43,0), legend=c("Average", "Regression", years), lty=1:(length(years)+2),col=1:(length(years)+2), title="Years")

# add regression line
lm_perc = lm(perc_averaged$Baggage_perc~as.integer(plot_dat$Month))
clip(min(as.integer(plot_dat$Month))-0.48, 
   max(as.integer(plot_dat$Month))-0.9, 
   min(res$Baggage),max(res$Baggage_perc))
abline(lm_perc, lty = 2, col=2)



## ------------------------------------------------------------------------
airline = airlines[3]
data = baggage[baggage$Airline == airline,]
res = aggregate(data["Baggage_perc"], by=list(Month = data$Month, Year = data$Year), sum)
years = unique(data$Year)
plot_dat = res[res$Year == years[1],]

#bottom,left,top,right margin
par(mar=c(7.1, 4.1, 3.1, 8.9), xpd=TRUE)

plot(x=as.integer(plot_dat$Month),y=plot_dat$Baggage_perc,type="o",
   xaxt="n",xlab="", ylab="Baggage % ",lty=2, col=2, pch = 2,
   ylim = c(min(res$Baggage_perc),max(res$Baggage_perc)))
axis(1,at = seq(1,12),labels = levels(res$Month))
title(paste(airline,"Baggage %"))   
for(j in 1:length(years)){
  plot_dat = res[res$Year == years[j],]
  lines(x=as.integer(plot_dat$Month),
        y=plot_dat$Baggage_perc,type="o",lty=j+2, col=j+2,pch=j+2)
    
}
# add average line
perc_averaged= aggregate(res["Baggage_perc"], by=list(Date = res$Month), mean)
lines(x=as.integer(plot_dat$Month),y=perc_averaged$Baggage_perc, type="o",lty=1, col=1,pch=1)


legend("topright", inset=c(-0.43,0), legend=c("Average", "Regression", years), lty=1:(length(years)+2),col=1:(length(years)+2), title="Years")

# add regression line
lm_perc = lm(perc_averaged$Baggage_perc~as.integer(plot_dat$Month))
clip(min(as.integer(plot_dat$Month))-0.48, 
   max(as.integer(plot_dat$Month))-0.9, 
   min(res$Baggage),max(res$Baggage_perc))
abline(lm_perc, lty = 2, col=2)



## ----case 2 load data----------------------------------------------------
x <- read.table(here("HW1", "CEOcompensation.txt"), header = T, sep = "\t", quote = "\"", row.names = 1)

## ----question 1----------------------------------------------------------
num.ceo.f <- length(x$CEO[x$Gender == "F"])
print(paste("The number of female CEO's is", num.ceo.f))

## ----question 2----------------------------------------------------------
age.ceo.min <- min(x$Age)
print(paste("The age of the youngest CEO is", age.ceo.min))

## ----question 3----------------------------------------------------------
age.ceo.max <- max(x$Age)
print(paste("The age of the oldest CEO is", age.ceo.max))

## ----question 4----------------------------------------------------------
age.ceo.avg <- round(mean(x$Age), 2)
print(paste("The average age of a CEO is", age.ceo.avg))

## ----question 5----------------------------------------------------------
tot.2008.sal <- sum(x$X2008.Salary)
print(paste("The total CEO 2008 salary is", paste0(tot.2008.sal,"0"), "million"))

## ----question 6----------------------------------------------------------
## Here we claim that a CEO joined the company as a CEO if the number of years she was at the company equals the number of years she has been CEO.

yearCheck <- sum(x$Years.as.company.CEO == x$Years.with.company)
print(paste(yearCheck, "CEO's joined a company as a CEO"))

## ----question 7----------------------------------------------------------
beforeCEO <- round(mean(x$Years.with.company - x$Years.as.company.CEO), 2)
print(paste("The average amount of time a CEO worked for a company before becoming a CEO is", beforeCEO, "years"))

## ----question 8----------------------------------------------------------
numCEO <- aggregate(x$CEO, list(Industry = x$Industry), FUN = length)
industry.max.ceo <- numCEO[[1]][which(numCEO[[2]] == max(numCEO[[2]]))]
print(paste("The industry with the largest number of CEO's is", industry.max.ceo))

## ----question 9----------------------------------------------------------
totCompensation <- round(mean(x$X2008.Salary + 
                                  x$X2008.Bonus + x$X2008.Other +
                                  x$X2008.Stock.gains), 2)
print(paste("The average CEO 2008 Compensation is", totCompensation, "million"))

## ----question 10---------------------------------------------------------
ceoCompensation <- x$X2008.Salary + x$X2008.Bonus + x$X2008.Other + x$X2008.Stock.gains
maxCompensation <- which(ceoCompensation == max(ceoCompensation))
print(paste("The CEO with the largest compensation amount in 2008 is", x$CEO[maxCompensation]))

## ----question 11---------------------------------------------------------
ceoCompensation <- x$X2008.Salary + x$X2008.Bonus + x$X2008.Other + x$X2008.Stock.gains
print(paste("The corresponding amount is", max(ceoCompensation), "million"))

## ----question 12---------------------------------------------------------
ceoCompensation <- x$X2008.Salary + x$X2008.Bonus + x$X2008.Other + x$X2008.Stock.gains
secondMaxCompensation <- which(ceoCompensation == tail(sort(ceoCompensation), 2)[1])
print(paste("The industry with the second largest total CEO compensation is",
            x$Industry[secondMaxCompensation]))

## ----question 13---------------------------------------------------------
ageGroups <- vector(mode="numeric", length=nrow(x))
ageGroups[which(x$Age >= 45 & x$Age < 50)] <- 1
ageGroups[which(x$Age >= 50 & x$Age < 55)] <- 2
ageGroups[which(x$Age >= 55 & x$Age < 60)] <- 3
ageGroups[which(x$Age >= 60 & x$Age < 70)] <- 4
ageGroups[which(x$Age >= 70)] <- 5
x$ageGroups <- ageGroups

groupByIndustry <- aggregate(x$X2008.Salary, list(ageGroups = x$ageGroups, 
                               industry = x$Industry), mean)
groupByAge <- as.data.frame(as.matrix(aggregate(x$X2008.Salary, list(ageGroups = x$ageGroups), mean)))

print(paste("The age group that has the highest average salary is group", 
            groupByAge$ageGroups[which(groupByAge$x == max(groupByAge$x))],
            "which corresponds to [70 or more)"))

print(paste("This corresponded to the industry of", 
            groupByIndustry$industry[which(groupByIndustry$x == max(groupByIndustry$x))],
            "where the salary was", max(groupByIndustry$x), "million"))

## Import median data
y <- read.csv("IndustryMedians.csv")

## Calculate percent difference for each CEO
z <- cbind.data.frame(ceoCompensation, industry=x$Industry)
  
x$percentDiff <- sapply(1:nrow(z), function(i) {
  compensation <- z$ceoCompensation[i]
  indMed <- y$Total.compensation[which(y$Industry == z$industry[i])]
  return( (compensation - indMed) / indMed*100)
})

## ------------------------------------------------------------------------
## Look at the percent difference for each CEO
print(round(x$percentDiff,3))
hist(x$percentDiff, main = "Percent Difference for each CEO", xlab = "Percent Difference")

## ----question 14---------------------------------------------------------
numLarger <- length(which(x$percentDiff > 100))
cat(paste("The number of CEO's that recieved 100% or larger
          compensation relative to their respective median compensation is", numLarger))

## ----question 15---------------------------------------------------------
y$totalMedianCompensation <- y$Salary + y$Bonus + y$Other + y$Stock.Gains

cat(paste("There are a total of",
            length(which(y$totalMedianCompensation != y$Total.compensation)), 
            "where the total median compensation formula
does not match our given total median compensation. This means the formula is not always true."))

