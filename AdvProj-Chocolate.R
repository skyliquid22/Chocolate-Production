#Libraries
library(MASS)
library(foreign)
library(car)
library(zoo)
library(xts)
library(lubridate)
library(plyr)
library(reshape2)
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(randomForest)
library(readxl)
library(doBy)
library(data.table)
library(chron)
library(dplyr)
library(itertools)
##########Methods##############
# some functions which are handy for plotting heatmaps of correlation matrices
#
# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

####
# function to detect local maxima
#
which.peaks <- function(x,partial=TRUE,decreasing=FALSE){
  if (decreasing){
    if (partial){
      which(diff(c(FALSE,diff(x)>0,TRUE))>0)
    }else {
      which(diff(diff(x)>0)>0)+1
    }
  }else {
    if (partial){
      which(diff(c(TRUE,diff(x)>=0,FALSE))<0)
    }else {
      which(diff(diff(x)>=0)<0)+1
    }
  }
}
########
# function to add zero output for days without production
#
addLevel <- function(x, newlevel=NULL) {
  if(is.factor(x)) {
    if (is.na(match(newlevel, levels(x))))
      return(factor(x, levels=c(levels(x), newlevel)))
  }
  return(x)
}
#########

#########
#function to add values from another column to given column, where null
#
fill_na <- function(col_to_fill, col_for_replace){
  for(val in as.list(enumerate(col_to_fill))){
    if(is.na(val$value)){
      col_to_fill[val$index] <- col_for_replace[val$index]
    }
  }
  return(col_to_fill)
}


############END#################

#~~#
#Importing Data
#~~#

Production_Orders_from_Access <- read_excel("Data/Production Orders from Access.xlsx")
View(Production_Orders_from_Access)

Choco.full <- Production_Orders_from_Access

#The time related variables have been parsed by R with dates
#and, as all the dates are set in the 1800's
#They are obviously very wrong
#Stripping the date from the time variables
Choco.full$`Basic Start time` <- format(Choco.full$`Basic Start time`, "%H:%M:%S")
Choco.full$`Basic Finish Time` <- format(Choco.full$`Basic Finish Time`, "%H:%M:%S")
Choco.full$`Scheduled Start Time` <- format(Choco.full$`Scheduled Start Time`, "%H:%M:%S")
Choco.full$`Scheduled Finish Time` <- format(Choco.full$`Scheduled Finish Time`, "%H:%M:%S")
Choco.full$`Actual Start Time` <- format(Choco.full$`Actual Start Time`, "%H:%M:%S")
Choco.full$`Actual Confirmed Finish Time` <- format(Choco.full$`Actual Confirmed Finish Time`, "%H:%M:%S")

#~~#
#Subsetting Data
#~~#

#Reducing to -000s materials
Choco.00s <- subset(Choco.full, grepl("^.+(000)$", Material))
View(Choco.00s)

#Reducing to my plants - PL01 and GB30
Choco.Plants <- subset(Choco.00s, grepl("(PL01|GB30)$", Plant))
View(Choco.Plants)

#Writing Subset for Tableau Analysis
#RAN
write.csv(Choco.Plants, file = "SubsetData.csv")

#~~#
#Dropping Un-needed data sets
#~~#
rm(Choco.full)
rm(Choco.00s)

#~~#
#Cleaning the Data
#~~#
#There are variables for both "Actual Finish Date"
#and "Actual confirmed finish date"
#These are non-identical, and although they are usually missing in the same row
#they aren't always
#I'll be filling those where the confirmed is missing with the non confirmed date
#and then dropping the "actual finish date" variable
fill_na(Choco.Plants$`Actual Confirmed Finish Date`, Choco.Plants$`Actual finish date`)


#Searching for missing values 
sum(is.na(Choco.Plants))
#25788 missings
#most of these are in the "Del Flag" column
#a flag boolean for deletions, in a data set with very few deletions

## Reviewing high interest clumns for missings
sum(is.na(Choco.Plants$`Order Number`))
#no missings

sum(is.na(Choco.Plants$Plant))
#none

sum(is.na(Choco.Plants$`Prod Line`))
#none

sum(is.na(Choco.Plants$Material))
#none

#Still 7459 missings, almost all were in Del Flag
sum(is.na(Choco.Plants$`Actual start date`) + is.na(Choco.Plants$`Actual finish date`))
#Here are another 4939, 2520 remaining

sum(is.na(Choco.Plants$`Actual Confirmed Finish Date`))
#2520 missings, found them

#So our columns with missings are
#DelFlag, Actual Start Date, Actual finish Date
#and Actual Confirmed Finish date

#double check
sum(is.na(Choco.Plants$`Del Flag`)+is.na(Choco.Plants$`Actual start date`) + 
      is.na(Choco.Plants$`Actual finish date`) +is.na(Choco.Plants$`Actual Confirmed Finish Date`))
#Yepo, here they all are

#Treating missings in Del Flag

#This seems to be simple boolean flag, filled only when observed
#so that's how I'm ging to fix it
#DO NOT RE-RUN
for(i in 1:nrow(Choco.Plants)){
  if(is.na(Choco.Plants$`Del Flag`[i]) == TRUE) {
    Choco.Plants$`Del Flag`[i] <- 0
  } else {
    Choco.Plants$`Del Flag`[i] <- 1
  }
}

sum(is.na(Choco.Plants$`Del Flag`))
#No more NA's in Del Flag

#All other NAs are in date columns
#I'll fill them with the scheduled start and finish dates

##Filling NAs in Actual Start Date
#And Adding a boolean flag for filled observations
Choco.Plants$Imputed_ASD
for(i in 1:nrow(Choco.Plants)){
  if(is.na(Choco.Plants$`Actual start date`[i])){
    Choco.Plants$Imputed_ASD[i] <- 1
  } else{
    Choco.Plants$Imputed_ASD[i] <- 0
  }
}
Choco.Plants$`Actual start date` <- fill_na(Choco.Plants$`Actual start date`, Choco.Plants$`Scheduled Start Date`)

##Filling NAs in "Actual Finish Data"
###And adding boolean flag for filled observations
Choco.Plants$Imputed_AFD
for(i in 1:nrow(Choco.Plants)){
  if(is.na(Choco.Plants$`Actual finish date`[i])){
    Choco.Plants$Imputed_AFD[i] <- 1
  } else{
    Choco.Plants$Imputed_AFD[i] <- 0
  }
}
Choco.Plants$`Actual finish date` <- fill_na(Choco.Plants$`Actual finish date`, Choco.Plants$`Scheduled Finish Date`)

## Filling NAs in "Actual Confirmed Finish Date"
### And creating boolean flag for filled positions
Choco.Plants$Imputed_ACFD
for(i in 1:nrow(Choco.Plants)){
  if(is.na(Choco.Plants$`Actual Confirmed Finish Date`[i])){
    Choco.Plants$Imputed_ACFD[i] <- 1
  } else{
    Choco.Plants$Imputed_ACFD[i] <- 0
  }
}
Choco.Plants$`Actual Confirmed Finish Date` <- fill_na(Choco.Plants$`Actual Confirmed Finish Date`, Choco.Plants$`Scheduled Finish Date`)

#UoM is "constant", marking all units as KG, so it should be ignored
Choco.Plants$UoM <- NULL

#~~#
#Adding Variables for Analysis
#~~#

#Date Variables
Choco.Plants$Date <- (Choco.Plants$`Basic Start Date`)
Choco.Plants$day <- weekdays(Choco.Plants$Date)

#creating weekdays vector
weekdays1 <- c('Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Frietag')
Choco.Plants$wDay <- factor((weekdays(Choco.Plants$`Basic Start Date`) %in% weekdays1), levels=c(TRUE, FALSE), labels=c('Weekday', 'Weekend'))

#Changing Variable names so R doesn't HATE me
Choco.Plants$Order.Number <- Choco.Plants$`Order Number`
Choco.Plants$Prod.Line <- Choco.Plants$`Prod Line`
Choco.Plants$Total.Order.Quant <- Choco.Plants$`Total order quantity`
Choco.Plants$Del.Flag <- Choco.Plants$`Del Flag`
Choco.Plants$Basic.Start.Date <- Choco.Plants$`Basic Start Date`

Choco.Plants$`Order Number` <- NULL
Choco.Plants$`Prod Line` <- NULL
Choco.Plants$`Total order quantity` <- NULL
Choco.Plants$`Del Flag` <- NULL
Choco.Plants$`Basic Start Date` <- NULL

#Splitting Into two sets, Set PL01 and GB30

Choco.PL01 <- subset(Choco.Plants, grepl("PL01", Plant))
Choco.GB30 <- subset(Choco.Plants, grepl("GB30", Plant))

Choco.PL01 <- as.data.frame(Choco.PL01)
Choco.GB30 <- as.data.frame(Choco.GB30)

#Oh hey look
#Plant PL01 only has one production line
#So there is that

#Writing data for plant subsets
write.csv(Choco.GB30, file = "Choco_GB30.csv")
write.csv(Choco.PL01, file = "Choco_PL01.csv")


#~~#
#Creating Frequency Count summaries
#~#

#Daily Orders 
#Plant GB30
Choco.sumGB <- summaryBy(Order.Number ~ Prod.Line + Date + day, data = Choco.GB30, FUN=c(length))

#Plant PL01
#Note that Prod line is dropped form the summary
#PL01 only has the one line
Choco.sumPL <- summaryBy(Order.Number ~ Date + day, data= Choco.PL01, FUN = c(length))

#Orders by Weekday
Choco.sumGBWD <- summaryBy(Order.Number ~ day + Prod.Line, data = Choco.GB30,
                           FUN = c(length))

Choco.sumPLWD <- summaryBy(Order.Number ~ day, data= Choco.PL01,
                           FUN = c(length))

#Materials By Day/Prodline
Choco.sumGBMAT <- summaryBy(Material ~ day + Prod.Line, data=Choco.GB30,
                            FUN = c(length))

Choco.sumPLMAT <- summaryBy(Material ~day, data= Choco.PL01, FUN = c(length))

#~~#
#Plotting
#~~#

#Plotting Daily Orders
ggplot(Choco.sumGB, aes(x=Date, y=Order.Number.length, colour=Prod.Line)) + 
  geom_line() +
  xlab("Date") + ylab ("# of Orders") +
  ggtitle("Daily Number of Orders by Production Line")+
  theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6))

ggplot(Choco.sumPL, aes(x=Date, y=Order.Number.length)) + 
  geom_line() +
  xlab("Date") + ylab ("# of Orders") +
  ggtitle("Daily Number of Orders")+
  theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6))

#Creating a Facet Plot of Daily Orders
#Only for GB30
ggplot(Choco.sumGB, aes(x=as.Date(Date), y=Order.Number.length)) + 
  geom_line(colour="chocolate") +facet_wrap(~Prod.Line)+
  xlab("Date") + ylab ("# of Orders") +
  ggtitle("Daily Number of Orders by Production Line")+
  scale_x_date(labels = function(x) format(x, "%b-%y"))+
  theme(axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6))

#Huge Volatility on line LLD
#Can't be a summary line
#I already checked for repeat order numbers
#and there are none

#Boxplot of Orders by Line

#GB30
ggplot(Choco.sumGB, aes(x=Prod.Line, y=Order.Number.length)) + geom_boxplot()
#PL01
ggplot(Choco.sumPL, aes(x="PL01", y=Order.Number.length)) + geom_boxplot()

#~~#
#correlation
#~~#
cor(Choco.GB30)

#~~#
#Attempt at getting planning types
General <- read_excel("General Material Master Data per SKU (1).xlsx")

gen.1 <- General

General.00s <- subset(gen.1, grepl("^.+(000)$", Material))
General.Plants <- subset(General.00s, grepl("(PL01|GB30)$", General.00s$`Plant BOM owner`))
View(General.Plants)
joined <- merge(General.Plants, Choco.Plants, by = "Material")
sum(is.na(joined))

Small.Gen <- General.Plants[,c(1, 7, 10, 11, 13)]

joined.2 <- merge(Choco.Plants, Small.Gen, by = "Material")
sum(is.na(joined.2))
sum(is.na(Choco.Plants))
#not perfect, lost some materials that aren't covered in small Gen
#lets try one more thing

Small.Gen.2 <- General[,c(1, 7, 10, 11, 13)]
joined.3 <- merge(Choco.Plants, Small.Gen.2, by = "Material")
nrow(joined.3)
#got them all now
#use joined.3

write.csv(joined.3, file = "Joined3.csv")
sum(is.na(joined.3$`Planning Type`))
sum(is.na(Small.Gen.2))

joined.3$`Planning Type` <- na.omit(joined.3$`Planning Type`)


write.csv(Join.Final, file = "FinalData.csv")

Join.Final <- read.csv("FinalData.csv")

#join.finale has no missings, so use it in tableau
#setting join.Final to run over the fiscal year

Join.Final <- Join.Final[Join.Final$Date > as.Date("2016-08-31"), ]
Join.Final <- Join.Final[Join.Final$Date < as.Date("2017-09-01"), ]
#~~#

#Still needs worked on
#issues right now:
#I need to be doing this for the plants individually
#and for their max days for those plants
#otherwise the difference in the plant averages
#and in the high production versus low production
#will really mess up my capacity

#Getting reference capacity
line.sum <- summaryBy(Total.Order.Quant  ~  Date + Prod.Line, data=Choco.Plants,
                        FUN=c(sum,mean,length))

line.cap <- summaryBy(Total.Order.Quant.sum  ~  Prod.Line, data=line.sum,
                        FUN=c(sum,mean,length))

# reference capacity for each line cluster (average of Tuesday till Thursdfays is taken as reference capacity)
#
#
cap.ref <- df.twt.lcm$Qty.KG.sum.mean
sum(cap.ref)
names(cap.ref) <- df.twt.lcm$Line.Cluster


#~~#

#Correlations
#Segment producation by day
#then see if any day's production is correlated with another
weeknum <- strftime(Choco.Plants$Date, format = "%V")
Choco.Plants$Week.Number <- weeknum


test <- summaryBy(Total.Order.Quant ~ day+ Date, data = Choco.GB30,
                           FUN = c(sum))




Choco.sumPLWD <- summaryBy(Order.Number ~ day, data= Choco.PL01,
                           FUN = c(length))

#Segmenting production by day
Choco.PlantsFri <- subset(Join.Final, grepl("Friday", day))
Choco.PlantsMon <- subset(Join.Final, grepl("Monday", day))
Choco.PlantsTues <- subset(Join.Final, grepl("Tuesday", day))
Choco.PlantsWed <- subset(Join.Final, grepl("Wednesday", day))
Choco.PlantsThurs <- subset(Join.Final, grepl("Thursday", day))
Choco.PlantsSat <- subset(Join.Final, grepl("Saturday", day))
Choco.PlantsSun <- subset(Join.Final, grepl("Sunday", day))

#Segmeting by line, on top of day

#Monday
MonL1 <- subset(Choco.PlantsMon, grepl("L1", Prod.Line))
MonL2 <- subset(Choco.PlantsMon, grepl("L2", Prod.Line))
MonL3 <- subset(Choco.PlantsMon, grepl("L3", Prod.Line))
MonL4 <- subset(Choco.PlantsMon, grepl("L4", Prod.Line))
MonL5 <- subset(Choco.PlantsMon, grepl("L5", Prod.Line))
MonLLD <- subset(Choco.PlantsMon, grepl("LLD", Prod.Line))

#Tuesday
TuesL1 <- subset(Choco.PlantsTues, grepl("L1", Prod.Line))
TuesL2 <- subset(Choco.PlantsTues, grepl("L2", Prod.Line))
TuesL3 <- subset(Choco.PlantsTues, grepl("L3", Prod.Line))
TuesL4 <- subset(Choco.PlantsTues, grepl("L4", Prod.Line))
TuesL5 <- subset(Choco.PlantsTues, grepl("L5", Prod.Line))
TuesLLD <- subset(Choco.PlantsTues, grepl("LLD", Prod.Line))

#Wednesday
WedL1 <- subset(Choco.PlantsWed, grepl("L1", Prod.Line))
WedL2 <- subset(Choco.PlantsWed, grepl("L2", Prod.Line))
WedL3 <- subset(Choco.PlantsWed, grepl("L3", Prod.Line))
WedL4 <- subset(Choco.PlantsWed, grepl("L4", Prod.Line))
WedL5 <- subset(Choco.PlantsWed, grepl("L5", Prod.Line))
WedLLD <- subset(Choco.PlantsWed, grepl("LLD", Prod.Line))

#thursday
ThursL1 <- subset(Choco.PlantsThurs, grepl("L1", Prod.Line))
ThursL2 <- subset(Choco.PlantsThurs, grepl("L2", Prod.Line))
ThursL3 <- subset(Choco.PlantsThurs, grepl("L3", Prod.Line))
ThursL4 <- subset(Choco.PlantsThurs, grepl("L4", Prod.Line))
ThursL5 <- subset(Choco.PlantsThurs, grepl("L5", Prod.Line))
ThursLLD <- subset(Choco.PlantsThurs, grepl("LLD", Prod.Line))

#Friday
FriL1 <- subset(Choco.PlantsFri, grepl("L1", Prod.Line))
FriL2 <- subset(Choco.PlantsFri, grepl("L2", Prod.Line))
FriL3 <- subset(Choco.PlantsFri, grepl("L3", Prod.Line))
FriL4 <- subset(Choco.PlantsFri, grepl("L4", Prod.Line))
FriL5 <- subset(Choco.PlantsFri, grepl("L5", Prod.Line))
FriLLD <- subset(Choco.PlantsFri, grepl("LLD", Prod.Line))

#saturday
SatL1 <- subset(Choco.PlantsSat, grepl("L1", Prod.Line))
SatL2 <- subset(Choco.PlantsSat, grepl("L2", Prod.Line))
SatL3 <- subset(Choco.PlantsSat, grepl("L3", Prod.Line))
SatL4 <- subset(Choco.PlantsSat, grepl("L4", Prod.Line))
SatL5 <- subset(Choco.PlantsSat, grepl("L5", Prod.Line))
SatLLD <- subset(Choco.PlantsSat, grepl("LLD", Prod.Line))

#Sunday
SunL1 <- subset(Choco.PlantsSun, grepl("L1", Prod.Line))
SunL2 <- subset(Choco.PlantsSun, grepl("L2", Prod.Line))
SunL3 <- subset(Choco.PlantsSun, grepl("L3", Prod.Line))
sunL4 <- subset(Choco.PlantsSun, grepl("L4", Prod.Line))
SunL5 <- subset(Choco.PlantsSun, grepl("L5", Prod.Line))
SunLLD <- subset(Choco.PlantsSun, grepl("LLD", Prod.Line))

#all are now seperated over line and day
#for GB30
#Seperating only over line
L1 <- subset(Join.Final, grepl("L1", Prod.Line))
L2 <- subset(Join.Final, grepl("L2", Prod.Line))
L3 <- subset(Join.Final, grepl("L3", Prod.Line))
L4 <- subset(Join.Final, grepl("L4", Prod.Line))
L5 <- subset(Join.Final, grepl("L5", Prod.Line))
LLD <- subset(Join.Final, grepl("LLD", Prod.Line))
L00 <- subset(Join.Final, grepl("L00", Prod.Line))

#comparing mean production
library("ggpubr")
ggboxplot(Join.Final, x = "day", y = "Total.Order.Quant", color = "Prod.Line")

day.prodline.anova <- aov(Total.Order.Quant ~ day + Prod.Line +day:Prod.Line, data=Join.Final)
summary(day.prodline.anova)
TukeyHSD(day.prodline.anova, which = "day")

#Comparing mean producition on lines
#L1
L1.anova <- aov(Total.Order.Quant ~ day, data = L1)
summary(L1.anova)
#yes different
TukeyHSD(L1.anova, which = "day")
#only Wednesday and friday are significantly different fromone another
ggboxplot(L1, x = "day", y = "Total.Order.Quant", color = "day")

#L2
L2.anova <- aov(Total.Order.Quant ~ day, data = L2)
summary(L2.anova)
#not significant at the 0.05 level

#L3
L3.anova <- aov(Total.Order.Quant ~ day, data = L3)
summary(L3.anova)
#not significant on the 0.05 level

#L4
L4.anova <- aov(Total.Order.Quant ~ day, data = L4)
summary(L4.anova)
#not significant at the 0.05 level

#L5
L5.anova <- aov(Total.Order.Quant ~ day, data = L5)
summary(L5.anova)
#significant, p.vale of 0.0139
TukeyHSD(L5.anova, which = "day")
#wed-Fri and Wed-Sun are stat sig different
#0.056 (barely) and 0.019
ggboxplot(L5, x = "day", y = "Total.Order.Quant", color = "day")


#LLD
LLD.anova <- aov(Total.Order.Quant ~ day, data = LLD)
summary(LLD.anova)
#VERY stat sig, p.val < 0.0001
TukeyHSD(LLD.anova, which = "day")
#many different pairs
#re-run for p-values
ggboxplot(LLD, x = "day", y = "Total.Order.Quant", color = "day")

#L00
L00.anova <- aov(Total.Order.Quant ~ day, data = L00)
summary(L00.anova)
#significant
TukeyHSD(L00.anova, which = "day")
ggboxplot(LLD, x = "day", y = "Total.Order.Quant", color = "day")
#sunday from Mon, thurs, tues, weds, sat & fri

#Planning types average production by weekday
JF.PL01 <- subset(Join.Final, grepl("PL01", Plant))
JF.GB30 <- subset(Join.Final, grepl("GB30", Plant))

#GB30
Plan.anova <- aov(Total.Order.Quant ~day + Planning.Type + day:Planning.Type, data = JF.GB30)
summary(Plan.anova)
#not sig
TukeyHSD(Plan.anova, which = "Planning.Type")

#Pl01
Plan.anova2 <- aov(Total.Order.Quant ~ day+ Planning.Type + day:Planning.Type, data = JF.Pl01)
summary(Plan.anova2)
#significant
TukeyPL01 <- TukeyHSD(Plan.anova2, which = "day:Planning.Type")
#ugh
TukeyHSD(Plan.anova2, which = 'Planning.Type')

ggboxplot(JF.Pl01, x = "day", y = "Total.Order.Quant", color = "Planning.Type")


#Tree creation
#adding month variable
library(rpart)
Join.Final$Month <- month(Join.Final$Date)
Join.Final$week <- strftime(Join.Final$Date, format = "%V")



GB30tree <- rpart(Total.Order.Quant~Month+day + wDay + week + Prod.Line + Planning.Type + Plant,
              data=JF.GB30,
              method="anova")

PL01tree <- rpart(Total.Order.Quant~Month+day + wDay + week + Prod.Line + Planning.Type + Plant,
                  data=JF.PL01,
                  method="anova")

####### Tree results
#GB30
printcp(GB30tree)
plotcp(GB30tree) 
summary(GB30tree)
plot(GB30tree)
text(GB30tree)
fancyRpartPlot(GB30tree)

#PL01
printcp(PL01tree)
plotcp(PL01tree) 
summary(PL01tree)
plot(PL01tree)
text(PL01tree)
fancyRpartPlot(PL01tree)

#Creating trees with the full data set
GB30tree.full <- rpart(Total.Order.Quant~,
                  data=JF.GB30,
                  method="anova")
JF.PL01$Marketing.Group <- JF.PL01$`Marketing Goup`
JF.PL01$Recipe.Subtype <- JF.PL01$`Recipe Subtype`
PL01tree.full <- rpart(Total.Order.Quant~Material+day+wDay+Del.Flag+Recipe.Subtype+Marketing.Group+Planning.Type+Month+week,
                  data=JF.PL01,
                  method="anova")


fancyRpartPlot(GB30tree.full)
fancyRpartPlot(PL01tree.full)

#Correlations
PL01.Cor <- JF.PL01
GB.Cor <- JF.GB30

#PL01
PL01.Cor$Material <- as.numeric(as.factor(PL01.Cor$Material))
PL01.Cor$Plant <- NULL
PL01.Cor$`Basic Start time`<-NULL
PL01.Cor$Date <- as.numeric(as.factor(PL01.Cor$Date))
PL01.Cor$day <- as.numeric(as.factor(PL01.Cor$day))
PL01.Cor$Del.Flag <- as.numeric(PL01.Cor$Del.Flag)
PL01.Cor$wDay <- as.numeric(as.factor(PL01.Cor$wDay))
PL01.Cor$Order.Number <- NULL
PL01.Cor$Prod.Line <- NULL #This will be kept for GB30, but here there is no variability in the variable
PL01.Cor$Basic.Start.Date <- NULL
PL01.Cor$Brand <- NULL #same as prod line, but felete for both
PL01.Cor$`Recipe Subtype`<- as.numeric(as.factor(PL01.Cor$`Recipe Subtype`))
PL01.Cor$`Marketing Goup` <- as.numeric(as.factor(PL01.Cor$`Marketing Goup`))
PL01.Cor$Planning.Type <- as.numeric(as.factor(PL01.Cor$Planning.Type))
PL01.Cor$week <- as.numeric(PL01.Cor$week)

#cormat
cormat.PL <- round(cor(PL01.Cor),2)
#upper_tri.PL <- get_upper_tri(cormat.PL)
melted_cormat.PL <- melt(cormat.PL, na.rm = TRUE)

#plot cormat w/ heat map
ggheatmap.PL <- ggplot(melted_cormat.PL, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

print(ggheatmap.PL)

#GB30
#data needs to be numeric
GB.Cor$Material <- as.numeric(as.factor(GB.Cor$Material))
GB.Cor$Plant <- NULL
GB.Cor$`Basic Start time`<-NULL
GB.Cor$Date <- as.numeric(as.factor(GB.Cor$Date))
GB.Cor$day <- as.numeric(as.factor(GB.Cor$day))
GB.Cor$wDay <- as.numeric(as.factor(GB.Cor$wDay))
GB.Cor$Order.Number <- NULL
GB.Cor$Prod.Line <- as.numeric(as.factor(GB.Cor$Prod.Line))
GB.Cor$Del.Flag <- as.numeric(GB.Cor$Del.Flag)
GB.Cor$Basic.Start.Date <- NULL
GB.Cor$Brand <- NULL #no variability
GB.Cor$`Recipe Subtype` <- as.numeric(as.factor(GB.Cor$`Recipe Subtype`))
GB.Cor$`Marketing Goup` <- as.numeric(as.factor(GB.Cor$`Marketing Goup`))
GB.Cor$Planning.Type <- as.numeric(as.factor(GB.Cor$Planning.Type))
GB.Cor$week <- as.numeric(GB.Cor$week)

#cormat
cormat.GB <- round(cor(GB.Cor),2)
#upper_tri.PL <- get_upper_tri(cormat.PL)
melted_cormat.GB <- melt(cormat.GB, na.rm = TRUE)

#plot cormat w/ heat map
ggheatmap.GB <- ggplot(melted_cormat.GB, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Correlation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

print(ggheatmap.GB)

#new trees, informed by correlations
GB30tree1 <- rpart(Total.Order.Quant~Month+day + wDay + week + Prod.Line + Planning.Type + Plant + Del.Flag + Marketing.Group + Recipe.Subtype,
                  data=JF.GB30,
                  method="anova")

PL01tree1 <- rpart(Total.Order.Quant~Month+day + wDay + week + Prod.Line + Planning.Type + Plant + Recipe.Subtype,
                  data=JF.PL01,
                  method="anova")


fancyRpartPlot(GB30tree1)
fancyRpartPlot(PL01tree1)

printcp(GB30tree1)
plotcp(GB30tree) 
summary(GB30tree)
plot(GB30tree)
text(GB30tree)
fancyRpartPlot(GB30tree)