#==============================================================================#
# POOLING COMPLEX MULTI-PARAMETER ASSOCIATIONS
#==============================================================================#

# LOAD PACKAGES
library(mixmeta) ; library(dlnm) ; library(scales)
library(dplyr)

# LOAD COEF/VCOV FROM FIRST-STAGE MODELS
setwd("~/Gasparini/Files/Regions")
tmeanpar <- read.csv(file="tmeanpar.csv")
coef <- as.matrix(tmeanpar[,grep("coef", names(tmeanpar))])
vcov <- as.matrix(tmeanpar[,grep("vcov", names(tmeanpar))])

# LINK WITH CENSUS DATA
cityind <- tmeanpar[,1:4,]

#==============================================================================#
# RUN THE MODELS
#==============================================================================#

# MODEL WITH NO META-PREDICTOR
model0 <- mixmeta(coef~1, vcov, data=cityind, method="ml")

# SUMMARY AND HETEROGENEITY TEST
summary(model0)
qtest(model0)

#==============================================================================#
# PLOT THE AVERAGE EXPOSURE-RESPONSE RELATIONSHIPS
#==============================================================================#

# LOAD AVERAGE TEMPERATURE DISTRIBUTION ACROSS CITIES
avgtmeansum <- read.csv("avgtmeansum.csv")
TAVG <- read.csv("tmean.csv")
tmean <- avgtmeansum$tmean

# DEFINE SPLINE TRANSFORMATION ORIGINALLY USED IN FIRST-STAGE MODELS
bvar <- onebasis(tmean, df=4, fun="bs") #same degrees as in the first stage 

# DEFINE THE CENTERING POINT (AT POINT OF MINIMUM RISK)
cen <- sum(TAVG)/nrow(TAVG)

# PREDICT THE ASSOCIATION
cp <- crosspred(bvar, coef=coef(model0), vcov=vcov(model0), model.link="log",
                at=tmean, cen=cen)

# PLOTTING LABELS
# PLOT
plot(cp, ylim=c(0.75,1.3), xlab="Temperature (C)", ylab="RR", main="Mood")
abline(v=cen, lty=2)
abline(v=c(tmean[3], tmean[99]), lty=3, col=grey(0.8)) 

# RR locations
pred <- crosspred(bvar, coef=coef(model0), vcov=vcov(model0), model.link="log",
                  at=c(tmean[3], cen, tmean[99]), cen=cen)

data.frame(pred[14:16])
