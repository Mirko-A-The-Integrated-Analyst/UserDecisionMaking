# Investigating Online User Decision Process Project ----

# Project Showcase Notice ----
#This repository contains code and materials for the purpose of showcasing the project. Please note that sensitive and official parts of the project have been either partially or totally removed.
#Purpose
#The content provided here is intended to demonstrate the capabilities, design patterns, and methodologies employed in the project. It is not meant for production use and may lack certain functionalities that are part of the full, official version.
#Limitations
#- **Sensitive Information**: Any sensitive information, including but not limited to private keys, credentials, and personal data, has been removed or anonymized.
#- **Partial Functionality**: Some sections of the code may have been modified or omitted to ensure the security and privacy of the underlying system or data. As such, the repository may not represent the full functionality of the original project.
#- **Showcase Only**: The provided code and documents are intended for viewing and demonstration purposes only.

rm(list=ls())
library("foreign")
library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(plyr)
library(readr)
library(reshape)

# Initialize an empty list to store datasets
dataset_list <- list()

# Define the filenames
filenames <- c(
  "InternalUseOnly.csv", "InternalUseOnly.csv.csv", "InternalUseOnly.csv", "InternalUseOnly.csv",
  "InternalUseOnly.csv", "InternalUseOnly.csv.csv", "InternalUseOnly.csv", "InternalUseOnly.csv",
  "InternalUseOnly.csv", "InternalUseOnly.csv.csv", "InternalUseOnly.csv", "InternalUseOnly.csv",
  "InternalUseOnly.csv", "InternalUseOnly.csv.csv", "InternalUseOnly.csv", "InternalUseOnly.csv"
)

# Loop through the filenames to read each dataset
for (filename in filenames) {
  # Construct the file path
  filepath <- file.path("//InternalUseOnly", filename)
  # Read the dataset
  dataset <- read.csv(filepath, header = TRUE)
  # Store the dataset in the list
  dataset_list[[filename]] <- dataset
  # Print names, structure, and summary of each dataset
  print(names(dataset))
  print(str(dataset))
  print(summary(dataset))
}

# Combine all datasets into a single dataset
dataFinal <- do.call(rbind.fill, dataset_list)

dim(dataFinal)
names(dataFinal)
str(dataFinal)
summary(dataFinal)


dataFinal$nameR <-dataFinal$Name
class(dataFinal$nameR)

dataFinal$categoryR <-dataFinal$Category
table(dataFinal$categoryR)
class(dataFinal$categoryR)

dataFinal$countryR <-dataFinal$Country
table(dataFinal$countryR)
class(dataFinal$countryR)

dataFinal$backersR <-dataFinal$Backers
class(dataFinal$backersR)

dataFinal$fundedR <-dataFinal$Funded
class(dataFinal$fundedR)

dataFinal$goalR <-dataFinal$Goal
class(dataFinal$goalR)

dataFinal$percentage_fundedR <-dataFinal$Percentage_funded
class(dataFinal$percentage_fundedR)
summary(dataFinal$percentage_fundedR)

dataFinal$durationR <-dataFinal$Duration
class(dataFinal$durationR)
summary(dataFinal$durationR)

dataFinal$start_deliveryR <-dataFinal$Start_delivery
class(dataFinal$start_deliveryR)
summary(dataFinal$start_deliveryR)

dataFinal$total_durationR <-dataFinal$Total_duration
class(dataFinal$total_durationR)

dataFinal$level_pledgesR <-dataFinal$Level_pledges
class(dataFinal$level_pledgesR)

dataFinal$new.backersR <-dataFinal$New_backers
class(dataFinal$new.backersR)

dataFinal$existing.backersR <-dataFinal$Existing_backers
class(dataFinal$existing.backersR)

dataFinal$updateR <-dataFinal$Update
class(dataFinal$updateR)

dataFinal$yearR <-dataFinal$End_year
class(dataFinal$yearR)

dataFinal$currencyR <-dataFinal$Currency
class(dataFinal$currencyR)
table(dataFinal$currencyR)

dataFinal$first_videoR <-dataFinal$First_video
class(dataFinal$first_videoR)

dataFinal$projects_launchedR <-dataFinal$Projects_launched
class(dataFinal$projects_launchedR)

dataFinal$projects_backedR <-dataFinal$Projects_backed
class(dataFinal$projects_backedR)

names(dataFinal)
colnames(dataFinal)

myvars1 <- c("nameR","categoryR", "countryR", "backersR", "fundedR", "goalR", "percentage_fundedR", 
             "durationR","start_deliveryR","total_durationR", "level_pledgesR", "new.backersR", "existing.backersR", "updateR", "yearR",
             "currencyR","first_videoR","projects_launchedR", "projects_backedR")
dataFinal1 <- dataFinal[,myvars1]
dim(dataFinal1)
names(dataFinal1)
str(dataFinal1)
summary(dataFinal1)

save(list=c("dataFinal1"),file="dataFinal1.RData")
load("dataFinal1.RData")
names(dataFinal1)

# Subset dataset 
##### Dollar
table(dataFinal1$currencyR)
summary(dataFinal1$goalR)
table(dataFinal1$yearR)
# outliers
dataFinal.D <- subset(dataFinal1, currencyR == "$" & goalR > 100 & goalR < 1000000 & yearR >2010 & yearR <2020) 
dim(dataFinal.D)
table(dataFinal.D$currencyR)
summary(dataFinal.D$goalR)
table(dataFinal.D$yearR)

#Save the new data set
save(list=c("dataFinal.D"),file="dataFinal.Dollar.Data")
load("dataFinal.Dollar.Data")

######################################### Cleaning ######################################### 
load("dataFinal.Dollar.Data")
# file = dataFinal.D
dim(dataFinal.D)
names(dataFinal.D)
table(dataFinal.D$currencyR)
summary(dataFinal.D$goalR)
table(dataFinal.D$yearR)

table(dataFinal.D$categoryR)
class(dataFinal.D$categoryR)
dataFinal.D$categoryrGroup <- fct_collapse(dataFinal.D$categoryR,
                                           Art = c("Art","Ceramics","Conceptual Art","Digital Art","Illustration","Installations","Mixed Media",
                                                   "Painting","Performance Art","Public Art","Sculpture","Social Practice","Textiles","Video Art"),
                                           Comics = c("Comics","Anthologies","Comic Books","Events","Graphic Novels","Webcomics"),
                                           Crafts = c("Crafts","Candles","Crochet","DIY","Embroidery","Glass","Knitting","Pottery","Printing","Quilts","Stationery",
                                                      "Taxidermy","Weaving","Woodworking"),
                                           Dance = c("Dance","Performances","Residencies","Spaces","education.training","Workshops"),
                                           Design = c("Design","Architecture","Civic Design","Graphic Design","Interactive Design","Product Design","Toys","Typography"),
                                           Fashion = c("Accessories","Fashion","Apparel","Childrenswear","Couture","Footwear","Jewelry","Pet Fashion", "Ready-to-wear"),
                                           FilmVideo = c("Film & Video", "Action","Animation","Comedy","Documentary","Drama","Experimental","Family","Fantasy","Festivals",
                                                         "Horror","Movie Theaters","Music Videos","Narrative Film","Romance","Science Fiction","Shorts","Television","Thrillers","Webseries"),
                                           Food = c("Food", "Bacon","Community Gardens","Cookbooks","Drinks","Events","Farmer's Markets", "Farms","Food Trucks","Restaurants","Small Batch","Spaces","Vegan"),
                                           Games = c("Games", "Gaming Hardware","Live Games","Mobile Games","Playing Cards","Puzzles", "Tabletop Games","Video Games"),
                                           Journalism = c("Journalism", "Audio","Photo","Print","Video","Web"),
                                           Music = c("Music","Blues","Chiptune","Classical Music","Comedy","Country & Folk","Electronic Music", "Faith","Hip-Hop","Indie Rock",
                                                     "Jazz","Kids","Latin","Metal","Pop","Punk","R&B","Rock","World Music"),
                                           Photography = c("Photography", "Animals","Fine Art","Nature","People","Photobooks","Places"),
                                           Publishing  = c("Publishing", "Academic","Anthologies","Art Books","Calendars","Children's Books","Comedy","Fiction","Letterpress","Literary Journals", 
                                                           "Literary Spaces","Nonfiction","Periodicals","Poetry","Radio & Podcasts","Translations","Young Adult","Zines"),
                                           Technology = c("Technology", "3D Printing","Apps","Camera Equipment","DIY Electronics","Fabrication Tools","Flight","Gadgets","Hardware","Makerspaces",
                                                          "Robots","Software","Sound", "Space Exploration","Wearables","Web"),
                                           Theater = c("Theater", "Comedy","Experimental","Festivals","Immersive","Musical","Plays","Spaces"))
table(dataFinal.D$categoryrGroup)
class(dataFinal.D$categoryrGroup)
summary(dataFinal.D$categoryrGroup)
dataFinal.D$categoryrGroupF <- factor(dataFinal.D$categoryrGroup)
table(dataFinal.D$categoryrGroupF)
class(dataFinal.D$categoryrGroupF)
str(dataFinal.D$categoryrGroupF)
dataFinal.D$categoryGroupFO <- order(dataFinal.D$categoryrGroupF)
table(dataFinal.D$categoryrGroupFO)

by(dataFinal.D$backersR, dataFinal.D$categoryrGroup, summary)
by(dataFinal.D$durationR, dataFinal.D$categoryrGroup, summary)
by(dataFinal.D$start_deliveryR, dataFinal.D$categoryrGroup, summary)

# Descriptibe statistics
dataFinal.D$Art<- ifelse(dataFinal.D$categoryrGroup == "Art", 1, 0)
dataFinal.D$Comics<- ifelse(dataFinal.D$categoryrGroup =="Comics", 1, 0)
dataFinal.D$Crafts<- ifelse(dataFinal.D$categoryrGroup =="Crafts", 1, 0)
dataFinal.D$Dance<- ifelse(dataFinal.D$categoryrGroup == "Dance", 1, 0)
dataFinal.D$Design<- ifelse(dataFinal.D$categoryrGroup =="Design", 1, 0)
dataFinal.D$Fashion<- ifelse(dataFinal.D$categoryrGroup =="Fashion", 1, 0)
dataFinal.D$FilmVideo<- ifelse(dataFinal.D$categoryrGroup == "FilmVideo", 1, 0)
dataFinal.D$Food<- ifelse(dataFinal.D$categoryrGroup =="Food", 1, 0)
dataFinal.D$Games<- ifelse(dataFinal.D$categoryrGroup =="Games", 1, 0)
dataFinal.D$Journalism<- ifelse(dataFinal.D$categoryrGroup == "Journalism", 1, 0)
dataFinal.D$Music<- ifelse(dataFinal.D$categoryrGroup =="Music", 1, 0)
dataFinal.D$Photography<- ifelse(dataFinal.D$categoryrGroup =="Photography", 1, 0)
dataFinal.D$Publishing<- ifelse(dataFinal.D$categoryrGroup == "Publishing", 1, 0)
dataFinal.D$Technology<- ifelse(dataFinal.D$categoryrGroup =="Technology", 1, 0)
dataFinal.D$Theater<- ifelse(dataFinal.D$categoryrGroup =="Theater", 1, 0)

summary(dataFinal.D$backersR)
backers.Histo <- ggplot(dataFinal.D, aes(backersR)) + labs(legend.position = "none") + geom_histogram(aes(y=..density..), colour="black", fill="white") + labs(x="backersR", y = "Density")
backers.Histo
summary(dataFinal.D$fundedR)
dataFinal.D$fundedR.log <-log(dataFinal.D$fundedR)
summary(dataFinal.D$fundedR.log)
summary(dataFinal.D$goalR)
summary(log(dataFinal.D$goalR))
dataFinal.D$goalR.log <-log(dataFinal.D$goalR)
summary(dataFinal.D$percentage_fundedR)
dataFinal.D$percentage_fundedR100 <-dataFinal.D$percentage_fundedR*100
summary(dataFinal.D$percentage_fundedR100)
summary(dataFinal.D$durationR)
sd(dataFinal.D$durationR)
summary(dataFinal.D$start_deliveryR)
summary(sqrt(dataFinal.D$start_deliveryR))
summary(dataFinal.D$total_durationR)
summary(dataFinal.D$level_pledgesR)

summary(dataFinal.D$new.backersR)
sum(dataFinal.D$new.backersR)
sum(dataFinal.D$existing.backersR)
sum(dataFinal.D$new.backersR) + sum(dataFinal.D$existing.backersR)
sum(dataFinal.D$backersR)
summary(dataFinal.D$existing.backersR)
summary(dataFinal.D$updateR)
table(dataFinal.D$yearR)
class(dataFinal.D$yearR)

# Dummy
dataFinal.D$projects_launchedF<- ifelse(dataFinal.D$projects_launchedR >= 1,1, 0)
dataFinal.D$projects_launchedF<- ifelse(dataFinal.D$projects_launchedR ==0,0, 1)
dataFinal.D$projects_launchedF=as.factor(dataFinal.D$projects_launchedF)
table(dataFinal.D$projects_launchedF)
class(dataFinal.D$projects_launchedF)
# each Dummy
dataFinal.D$projects_launchedYes<- ifelse(dataFinal.D$projects_launchedR >= 1,1, 0)
table(dataFinal.D$projects_launchedYes)
class(dataFinal.D$projects_launchedYes)
dataFinal.D$projects_launchedNo<- ifelse(dataFinal.D$projects_launchedR ==0,1, 0)
table(dataFinal.D$projects_launchedNo)
class(dataFinal.D$projects_launchedNo)

summary(dataFinal.D$projects_backedR)
# Dummy
dataFinal.D$projects_backedF<- ifelse(dataFinal.D$projects_backedR >= 1,1, 0)
dataFinal.D$projects_backedF<- ifelse(dataFinal.D$projects_backedR ==0,0, 1)
dataFinal.D$projects_backedF=as.factor(dataFinal.D$projects_backedF)
table(dataFinal.D$projects_backedF)
class(dataFinal.D$projects_backedF)
# each Dummy
dataFinal.D$projects_backedYes<- ifelse(dataFinal.D$projects_backedR >= 1,1, 0)
table(dataFinal.D$projects_backedYes)
class(dataFinal.D$projects_backedYes)
dataFinal.D$projects_backedNo<- ifelse(dataFinal.D$projects_backedR ==0,1, 0)
table(dataFinal.D$projects_backedNo)
class(dataFinal.D$projects_backedNo)

#Save the new data set
save(list=c("dataFinal.D"),file="dataFinal.D.Cleaned.Data")
load("dataFinal.D.Cleaned.Data")

######################################### Statistics ######################################### 
library(xtable)
library(stargazer)
library(QuantPsyc)
library(car)
library(boot)
library(psych)
library(Hmisc)
library(carData)
library(MASS)
library(nlme)
library(plyr)
library(summarytools)
library(ggfortify)
#********************************************** Descriptive statitics **********************************************#
# Table 1: Descriptive statistics
dataFinal.D.DS100 <- dataFinal.D[ ,c("backersR", "new.backersR","existing.backersR",
                                     "percentage_fundedR100","durationR","start_deliveryR",
                                     "projects_launchedR","projects_backedR",
                                     "fundedR", "goalR","level_pledgesR","updateR" )]

stargazer(dataFinal.D.DS100, type = "text", out = "Descriptive statistics100.txt", summary.stat = c("n","mean", "sd", "min", "max"), digits=2)

# Appendix B: Descriptive statistics of categories
# table  bakcers
summary(dataFinal.D$backersR)
sum(dataFinal.D$backersR)
table.my1 = dataFinal.D%>%
  group_by(categoryrGroupF) %>%
  summarise_at(vars("backersR", "new.backersR","existing.backersR"), sum)
table.my1
Xtabale.my1 <-xtable(table.my1)
print(Xtabale.my1, type = "html", file = "/InternalUseOnly.html")

# test
table.myA = dataFinal.D.DS100.C%>%
  group_by(categoryrGroupF) %>%
  summarise_at(vars("backersR", "new.backersR","existing.backersR"), mean)
table.myA
Xtabale.myA <-xtable(table.myA)
print(Xtabale.myA, type = "html", file = "/InternalUseOnly.html")


table.my3 = dataFinal.D%>%
  group_by(categoryrGroupF) %>%
  summarise_at(vars( "fundedR", "goalR","level_pledgesR","updateR"), mean)
table.my3
Xtabale.my3 <-xtable(table.my3)
print(Xtabale.my3, type = "html", file = "/InternalUseOnly.html")

table.my4 = dataFinal.D%>%
  group_by(categoryrGroupF) %>%
  summarise_at(vars( "percentage_fundedR100","durationR","start_deliveryR"), mean)
table.my4
Xtabale.my4 <-xtable(table.my4)
print(Xtabale.my4, type = "html", file = "/InternalUseOnly.html")


table.my5 = dataFinal.D%>%
  group_by(categoryrGroupF) %>%
  summarise_at(vars( "projects_launchedR","projects_backedR"), mean)
table.my5
Xtabale.my5 <-xtable(table.my5)
print(Xtabale.my5, type = "html", file = "/InternalUseOnly.html")

# ok table freq.DS2
count(dataFinal.D$categoryrGroupF) 

summary(dataFinal.D$backersR)
freq.DS2 = freq(dataFinal.D$categoryrGroupF,
                order = "names",
                report.nas = FALSE,
                cumul = FALSE,
                headings = FALSE)
freq.DS2 
Xtabale.freq.DS2 <-xtable(freq.DS2)
print(Xtabale.freq.DS2, type = "html", file = "/InternalUseOnly.html")

                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
# Appendix C: Correlation matrix of independent variables
myvars2 <-  c("fundedR.log", "goalR.log", "percentage_fundedR", "durationR","start_deliveryR",
              "level_pledgesR", "updateR", "projects_launchedR", "projects_backedR")

dataFinal.D_cor <- dataFinal.D[,myvars2]
dim(dataFinal.D_cor)
head(dataFinal.D_cor)

cor(dataFinal.D$backersR,dataFinal.D$goalR.log)
cor(dataFinal.D_cor, use = "complete.obs")

res2 <- rcorr(as.matrix(dataFinal.D_cor))
res2

# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
# the results will be displayed in html or latex format
corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                    result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 

corstars(dataFinal.D_cor[,1:9], result="html")
corstars(dataFinal.D_cor[,1:9])
x<- xtable(corstars(dataFinal.D_cor[,1:9]))
print(x, type = "html", file = "/InternalUseOnly.html")


# Table 2: OLS regressions for all the projects
OLS.Model.A <- lm(backersR ~ + fundedR.log + goalR.log +level_pledgesR +  updateR,  data = dataFinal.D)
summary(OLS.Model.A) 
vif(OLS.Model.A)
dwt(OLS.Model.A)
autoplot(OLS.Model.A)
# ok 
OLS.Model.A1 <- lm(backersR ~ fundedR.log + goalR.log +level_pledgesR +  updateR
                   + Art + Comics +Crafts +Dance +Design +Fashion +FilmVideo +Food 
                   +Games +Journalism +Music +Photography +Publishing + Technology + Theater,  data = dataFinal.D)
summary(OLS.Model.A1) 
vif(OLS.Model.A1)
dwt(OLS.Model.A1)
# ok
OLS.Model.D2 <- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR
                   + percentage_fundedR100 + durationR + start_deliveryR
                   + projects_launchedR + projects_backedR
                   + Art + Comics +Crafts +Dance +Design +Fashion +FilmVideo +Food 
                   +Games +Journalism +Music +Photography +Publishing + Technology + Theater,  data = dataFinal.D)
summary(OLS.Model.D2)
vif(OLS.Model.D2)
dwt(OLS.Model.D2)
# ok

stargazer(OLS.Model.A1,OLS.Model.D2, type = "html",
          dep.var.labels=c("Number of Backers"),
          out = "OLS.A1-D2.html")

# Table 3: OLS regressions for new and returning backers for all projects
OLS.Model.BNew1<- lm(new.backersR ~ fundedR.log + goalR.log + level_pledgesR +  updateR
                     + percentage_fundedR100 + durationR + start_deliveryR
                     + projects_launchedR + projects_backedR
                     + Art + Comics +Crafts +Dance +Design +Fashion +FilmVideo +Food 
                     +Games +Journalism +Music +Photography +Publishing + Technology + Theater,  data = dataFinal.D)
summary(OLS.Model.BNew1) 
vif(OLS.Model.BNew1)
dwt(OLS.Model.BNew1)
# ok vif
OLS.Model.BExist1<- lm(existing.backersR ~ fundedR.log + goalR.log + level_pledgesR +  updateR
                       + percentage_fundedR100 + durationR + start_deliveryR
                       + projects_launchedR + projects_backedR
                       + Art + Comics +Crafts +Dance +Design +Fashion +FilmVideo +Food 
                       +Games +Journalism +Music +Photography +Publishing + Technology + Theater,  data = dataFinal.D)
summary(OLS.Model.BExist1) 
vif(OLS.Model.BExist1)
dwt(OLS.Model.BExist1)
# ok vif
stargazer(OLS.Model.BNew1,OLS.Model.BExist1, type = "html",
          dep.var.labels=c("Number of New Backers","Number of Existing Backers"),
          out = "1.OLS.NewExisting.Backers.html")

# Table 4: OLS regression for each category
#******************************************************* Art
OLS.Model.B1.Art1<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR
                       + percentage_fundedR100 + durationR + start_deliveryR
                       + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Art"))
summary(OLS.Model.B1.Art1) 
vif(OLS.Model.B1.Art1)
dwt(OLS.Model.B1.Art1)
# ok 
#******************************************************* Comics
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Comics2<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR
                          + percentage_fundedR100 + durationR + start_deliveryR
                          + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Comics"))
summary(OLS.Model.B1.Comics2) 
vif(OLS.Model.B1.Comics2)
dwt(OLS.Model.B1.Comics2)
# ok 
#******************************************************* Crafts
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Crafts3<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR
                          + percentage_fundedR100 + durationR + start_deliveryR
                          + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Crafts"))
summary(OLS.Model.B1.Crafts3) 
vif(OLS.Model.B1.Crafts3)
dwt(OLS.Model.B1.Crafts3)
# potential problem vif
#******************************************************* Dance
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Dance4<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                         + percentage_fundedR100 + durationR + start_deliveryR
                         + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Dance"))
summary(OLS.Model.B1.Dance4) 
vif(OLS.Model.B1.Dance4)
dwt(OLS.Model.B1.Dance4)
# potential problem vif
#******************************************************* Design
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Design5<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR
                          + percentage_fundedR100 + durationR + start_deliveryR
                          + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Design"))
summary(OLS.Model.B1.Design5) 
vif(OLS.Model.B1.Design5)
dwt(OLS.Model.B1.Design5)
# ok 
#******************************************************* Fashion
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Fashion6<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                           + percentage_fundedR100 + durationR + start_deliveryR
                           + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Fashion"))
summary(OLS.Model.B1.Fashion6) 
vif(OLS.Model.B1.Fashion6)
dwt(OLS.Model.B1.Fashion6)
# ok 
#******************************************************* Film & Video
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.FilmVideo7<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                             + percentage_fundedR100 + durationR + start_deliveryR
                             + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="FilmVideo"))
summary(OLS.Model.B1.FilmVideo7)
vif(OLS.Model.B1.FilmVideo7)
dwt(OLS.Model.B1.FilmVideo7)
# ok 
#******************************************************* Food
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Food8<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                        + percentage_fundedR100 + durationR + start_deliveryR
                        + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Food"))
summary(OLS.Model.B1.Food8)
vif(OLS.Model.B1.Food8)
dwt(OLS.Model.B1.Food8)
# ok 
#******************************************************* Games
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Games9<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                         + percentage_fundedR100 + durationR + start_deliveryR
                         + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Games"))
summary(OLS.Model.B1.Games9)
vif(OLS.Model.B1.Games9)
dwt(OLS.Model.B1.Games9)
# ok 
#******************************************************* Journalism
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Journalism10<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                               + percentage_fundedR100 + durationR + start_deliveryR
                               + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Journalism"))
summary(OLS.Model.B1.Journalism10)
vif(OLS.Model.B1.Journalism10)
dwt(OLS.Model.B1.Journalism10)
# potential problem vif
#******************************************************* Music
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Music11<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                          + percentage_fundedR100 + durationR + start_deliveryR
                          + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Music"))
summary(OLS.Model.B1.Music11)
vif(OLS.Model.B1.Music11)
dwt(OLS.Model.B1.Music11)
# ok 
#******************************************************* Photography
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Photography12<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                                + percentage_fundedR100 + durationR + start_deliveryR
                                + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Photography"))
summary(OLS.Model.B1.Photography12)
vif(OLS.Model.B1.Photography12)
dwt(OLS.Model.B1.Photography12)
# potential problem vif
#******************************************************* Publishing 
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Publishing13<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                               + percentage_fundedR100 + durationR + start_deliveryR
                               + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Publishing"))
summary(OLS.Model.B1.Publishing13)
vif(OLS.Model.B1.Publishing13)
dwt(OLS.Model.B1.Publishing13)
# ok
#******************************************************* Technology 
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Technology14<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                               + percentage_fundedR100 + durationR + start_deliveryR
                               + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Technology"))
summary(OLS.Model.B1.Technology14)
vif(OLS.Model.B1.Technology14)
dwt(OLS.Model.B1.Technology14)
# ok 
#******************************************************* Theater 
table(dataFinal.D$categoryrGroupF)
OLS.Model.B1.Theater15<- lm(backersR ~ + fundedR.log + goalR.log + level_pledgesR +  updateR 
                            + percentage_fundedR100 + durationR + start_deliveryR
                            + projects_launchedR + projects_backedR,  data = subset(dataFinal.D, categoryrGroupF=="Theater"))
summary(OLS.Model.B1.Theater15)
vif(OLS.Model.B1.Theater15)
dwt(OLS.Model.B1.Theater15)
# potential problem vif

stargazer(OLS.Model.B1.Art1,OLS.Model.B1.Comics2, 
          OLS.Model.B1.Design5, OLS.Model.B1.Fashion6, OLS.Model.B1.FilmVideo7, OLS.Model.B1.Food8, 
          OLS.Model.B1.Games9, OLS.Model.B1.Music11,
          OLS.Model.B1.Publishing13,OLS.Model.B1.Technology14, type = "html",
          dep.var.labels=c("Number of Backers"),
          out = "Each.Category.noVIF.html")



