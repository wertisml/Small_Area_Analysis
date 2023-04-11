library(data.table)
library(dplyr)
library(dlnm) 
library(mixmeta)
library(tsModel) 
library(splines) 
library(lubridate)

setwd("~/Gasparini/Files/Regions")

Data <- fread("Sheps_Temp_Regions.csv")

Data <- Data %>%
   mutate(month = month(Date)) %>%
  filter(Zip != 28668, Zip != 28652, Zip != 28629, Zip != 28672, Zip != 28720, 
         Zip != 28733, Zip != 28735, Zip != 28662, Zip != 28663, Zip != 28749,
         Zip != 28702, Zip != 28757,
         Zip != 28282, Zip != 28244, Zip != 27110, Zip != 27340, Zip != 28007,
         Zip != 28102, Zip != 28089, Zip != 28280, Zip != 27201, Zip != 27556,
         Zip != 28109, Zip != 27582, Zip != 27109,
         Zip != 28308, Zip != 27531, Zip != 27861, Zip != 27841, Zip != 27881,
         Zip != 27916, Zip != 27950, Zip != 27943, Zip != 27978, Zip != 27985,
         Zip != 28310, Zip != 28520, Zip != 28524, Zip != 28552, Zip != 28589,
         Zip != 28587, Zip != 27927, Zip != 28375, Zip != 28528, Zip != 28533,
         Zip != 28537, Zip != 27842, Zip != 27872, Zip != 27964, Zip != 27965, 
         Zip != 27968, Zip != 28424, Zip != 28577, Zip != 28583, Zip != 27960,
         Zip != 28342, Zip != 28543, Zip != 28547, Zip != 28581, Zip != 27926) %>%
  filter(sex == "M") %>%
  filter(Age == 4) %>%
  filter(race == 3) %>%
  filter(Region == "Mountains") %>%
  arrange(Zip) %>%
  mutate(loc = cumsum(c(1,as.numeric(diff(Zip))!=0))) %>%
  rename(Outcome = Mental_Health,
         temp = TAVG) %>%
  dplyr::select(Date, temp, RH, Outcome, Zip, loc, Region) 

Data$Outcome <- as.numeric(Data$Outcome)

Data <- Data[complete.cases(Data),]

#==============================================================================#
# DLNM of mental health - temperature
#==============================================================================#

tmeanparlist <- tmeansumlist <- TAVGlist <- predictlist <- vector("list", length(unique(Data$Zip)))

# RUN THE LOOP
#length(unique(Data$Zip))
for(i in 1:length(unique(Data$Zip))) {
  
  # PRINT CITY
  cat(unique(Data$Zip)[i],"")
  
  data <- Data %>%
    filter(loc == i)
  
  #============================================================================#
  # ANALYSIS OF TEMPERATURE - Mental Health (SUMMER-ONLY)
  #============================================================================#
  
  range <- round(range(data$temp, na.rm = T),0)
  knots <- range[1] + (range[2]-range[1])/4*1:3
  
  # DEFINE THE CROSS-BASIS FOR TEMPERATURE FROM THE EXPOSURE HISTORY MATRIX
  # NB: USE group TO IDENTIFY LACK OF CONTINUITY IN SERIES BY MSOA AND YEAR
  argvar <- list(fun="ns", knots=knots)
  arglag <- list(fun="ns", knots=1)
  
  cbtmean <- crossbasis(data$temp, lag=7, argvar=argvar, arglag=arglag)
  
  # RUN THE MODEL ON AGGREGATED DATA
  model <- glm(Outcome ~ cbtmean + ns(yday(Date), df=4):factor(year(Date)) +
                   wday(Date, label = TRUE), data=data, family=quasipoisson)
  
  redpred <- crossreduce(cbtmean, model, cen=mean(data$temp, na.rm=T)) 
  
  #plot(redpred, ylim=c(0.5,1.5), xlab="Temperature (C)", ylab="RR", main=paste0(unique(data$Zip)))
  
  # STORE PARAMETERS (COEF + VECTORIZED VCOV)
  ncoef <- length(coef(redpred))
  par <- c(coef(redpred), vechMat(vcov(redpred)))
  names(par) <- c(paste0("coef", seq(ncoef)),
                  paste0("vcov", seq(ncoef*(ncoef+1)/2)))
  tmeanpar <- data.frame(Data[i, c("Region", "Zip", "loc", "Zip")],
                         t(par), row.names=i)
  tmeanparlist[[i]] <- tmeanpar
  TAVGlist[[i]] <- redpred$cen

  #============================================================================#
  # TEMPERATURE DISTRIBUTION (SUMMER ONLY)
  #============================================================================#
  
  # DEFINE PERCENTILES
  per <- c(1,2,2.5, 3:97, 97.5, 98,99,100)/100
  tmeansumlist[[i]] <- quantile(data$temp, per, na.rm=T)

} 

#==============================================================================#
# PREPARE AND STORE
#==============================================================================#

# RBIND COEF/VCOV TOGETHER IN DATAFRAMES
tmeanpar <- do.call(rbind, tmeanparlist)
TAVG <- do.call(rbind, TAVGlist)

# Create state-average summer temperature distribution
avgtmeansum <- data.frame(perc=names(tmeansumlist[[1]]), 
                          tmean=apply(do.call(cbind, tmeansumlist), 1, mean))

# Write off the data sets
setwd("~/Gasparini/Files/Regions")
fwrite(tmeanpar, "tmeanpar.csv")
fwrite(avgtmeansum, "avgtmeansum.csv")
fwrite(TAVG, "tmean.csv")

