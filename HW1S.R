rm(list=ls())

setwd("/Users/magal/Desktop/regression/R scripts/HW1")
getwd()
mydata <- read.delim("/Users/magal/Desktop/regression/All_Data/P007-8.txt")


#Turn NDIR into a dummy variable called NDIR dummy where any value at or above 
# the mean of NDIR is coded 1, and anything below the mean is coded 0
mydata$NDIR
mean(mydata$NDIR)
ndir_mean <- mean(mydata$NDIR)
mydata$NDIR_dummy <-0
mydata$NDIR_dummy[which(mydata$NDIR>ndir_mean)] <- 1

mydata$NDIR_dummy

#Split Taxes into an ordinal variable, tax_ord, of four equally size bins
library(Hmisc)
mydata$Taxes
mydata$tax_ord <- as.numeric(cut2(mydata$Taxes, g=4))
mydata$tax_ord

#Cross-tabulate ndir dummy tax ord (hint: table())
table(mydata$NDIR_dummy, mydata$tax_ord)

#Draw a grouped barplot exploring ndir dummy ??? tax ord
barplot(counts, main="My Title", xlab="tax groups", ylab="Number of states", beside=TRUE)
# Same plot but with color
library(CGPfunctions)
library(ggplot2)
library(dplyr)
PlotXTabs(mydata,NDIR_dummy,tax_ord)
  

# Using the original NDIR variable, compute the mean by tax ord group
aggdata <- mydata[,c("NDIR","tax_ord")]
aggregate(aggdata$NDIR, list(aggdata$tax_ord), mean)


#Draw a boxplot exploring NDIR ??? tax ord
boxplot(NDIR~tax_ord, data=mydata, main="My Title", xlab="my x label", ylab="my y label")


#Test the interactive hypothesis that the effect of tax ord is different in the South than the other Regions.
#splitting data fram between south and other
library(plyr)
mydata$Region
mydata$rs <-mydata$Region
mydata$rs <- revalue(mydata$rs, c("Midwest"="other", 
                                  "Northeast"="other",
                                  "West"="other",
                                  "South"="South"))
mydata$rs
spdt <- mydata[,c("NDIR_dummy","NDIR","tax_ord","rs")]
df<-spdt 
datasplit<-split(df, mydata$rs)
datasplit

datasplit$South$NDIR_dummy
datasplit$South$NDIR_dummy

table(datasplit$South$NDIR_dummy, datasplit$South$tax_ord)
table(datasplit$other$NDIR_dummy, datasplit$other$tax_ord)

#Construct a 3-way table, similar to Treiman's Table 3.3, where the dependent variable is NDIR, 
#and the independent variables are tax ord and Region, with categories collapsed into South and Other


aggregate(datasplit$South$NDIR, list(datasplit$South$tax_ord), mean)
aggregate(datasplit$other$NDIR, list(datasplit$other$tax_ord), mean)






