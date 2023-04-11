library(tidyverse)
library(dlnm) 
library(mixmeta)
library(tsModel) 
library(splines) 
library(lubridate)
library(gnm)
library(scales)

setwd("~/Gasparini/Files/Regions")

filter_data <- function(Sex = NULL, age = NULL, Race = NULL, region = NULL, outcome_column = NULL) {
  
  # Generate file name based on input
  file_name <- paste0("Sheps_Temp_Regions", 
                      if (!is.null(Sex)) paste0("_Sex") else "",
                      if (!is.null(age)) paste0("_Age") else "", 
                      if (!is.null(Race)) paste0("_Race") else "", ".parquet")
  
  filtered_data <- open_dataset(file_name) %>%
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
           Zip != 28342, Zip != 28543, Zip != 28547, Zip != 28581, Zip != 27926,
           Zip != 28553, Zip != 27956) %>%
    arrange(Zip) %>%
    collect() %>%
    mutate(loc = cumsum(c(1,as.numeric(diff(Zip))!=0)),
           doy = yday(Date),
           year = year(Date),
           month = month(Date),
           dow = wday(Date)) %>%
    rename(Outcome = !!outcome_column,
           temp = TAVG) %>%
    select(Date, temp, RH, Outcome, Zip, loc, Region, doy, year, month, dow, 
           if (!is.null(Sex)) paste0("sex"),
           if (!is.null(age)) paste0("Age"), 
           if (!is.null(Race)) paste0("race")) %>%
    as.data.table()
  
  if (!is.null(Sex)) {filtered_data <- filtered_data %>%filter(sex == Sex)}
  
  if (!is.null(age)) {filtered_data <- filtered_data %>%filter(Age == age)}
  
  if (!is.null(Race)) {filtered_data <- filtered_data %>%filter(race == Race)}
  
  if (!is.null(region)) {filtered_data <- filtered_data %>%filter(Region == region)}
  
  return(filtered_data)
} 

Data <- filter_data(Sex = "F",                          # This can be either M or F       
                    #age = 4,                            # This can be any value between 1 & 4 1 = 0-25, 2 = 26- 46, 3 = 47-65, 4 = 66+       
                    #Race = 3,                           # This can be any value between 1-5      
                    region = "Coast",                # This can be Mountains, Piedmont, or Coast
                    outcome_column = "Anxiety"     # This can be Mental_Health, Substance, Mood, Anxiety  
)


Data$Outcome <- as.numeric(Data$Outcome)

Data <- Data[complete.cases(Data),]

#==============================================================================#
# Build DLNM and pooled model
#==============================================================================#

source("/home/students/wertisml/Gasparini/Code/Regions/small_area_calculation.R")

#==============================================================================#
# RR Results
#==============================================================================#

redpred <- crossreduce(cbtmean, modfull, cen=mean(Data$temp, na.rm=T))
lines <- quantile(Data$temp, c(2.5,50,97.5)/100, na.rm=T)
pred <- crosspred(cbtmean, modfull, cen = mean(Data$temp, na.rm=T),
                  at=c(lines[1],redpred$cen,lines[3]))
#plot(pred)
data.frame(pred[14:16])

#==============================================================================#
# Plot data  
#==============================================================================#

# Take a screen shot of each plot run and get it named for what its showing
redpred <- crossreduce(cbtmean, modfull, cen=mean(Data$temp, na.rm=T))
lines <- quantile(Data$temp, c(2.5,50,97.5)/100, na.rm=T)
col <- c("red", "gray50")

# Set the resolution to 600 dpi
res <- 600

# Get the current plot dimensions
w <- par("fin")[1] * res
h <- par("fin")[2] * res

# Create a new device with the desired resolution
png(filename = paste0("North_Carolina_Anxiety", ".png"), width = w, height = h, res = res)

#Plot the DLNM
plot(cpfull, "overall", ylim=c(0.5,1.5), ylab="RR", main = "Anxiety",
     col=col[1], lwd=1.5,
     xlab=expression(paste("Temperature ("*degree,"C)")), 
     ci.arg=list(col=alpha(col[2], 0.2))) # Set the color of the 95% CI to grey
abline(v=c(lines[1], lines[3]), lty=2, col=grey(0.8))
abline(v = redpred$cen, lty = 2)

# Save the plot
dev.off()

#==============================================================================#
# Ploting temperature frequencies
#==============================================================================#

bar <- barplot(table(round(Data$temp)),
               border = "black",
               col = "white",
               xlab = "Temperature (C)",
               ylab = "") 

par(new = TRUE)
plot(bar, NA, axes = FALSE, xlab = "", ylab = "")
axis(side = 4)
mtext("Count Totals", side = 4, line = 3)



