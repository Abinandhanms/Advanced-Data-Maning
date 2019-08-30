
#setting the working directory path
setwd("D:/MS/Trisemester-2/ADM/GRP_Project")

#calling the addional library
library(rjson)
library(dplyr)
library(keras)
library(tidyverse)
library(tidyr)

#cleaning the raw data
data <-readLines(con = "Sarcasm_Headlines_Dataset.json",n=-1L,ok=TRUE,skipNul = TRUE)
json_data <- as.data.frame(data)
sep_file<-separate(json_data,data,c("Article","Headline","is_sarcastic"),sep = '",')
mydata<-select(sep_file,-Article)
mydata$Headline<-as.character(gsub('"headline": "','',mydata$Headline))
mydata$Headline<-as.character(gsub('"','',mydata$Headline))
mydata$is_sarcastic<-as.character(gsub('"is_sarcastic":','',mydata$is_sarcastic))
mydata$is_sarcastic<-as.character(gsub('}','',mydata$is_sarcastic))
rm(json_data)
rm(sep_file)
rm(data)



#Checking class imbalance
library(DataExplorer)
mydata$is_sarcastic<-as.factor(mydata$is_sarcastic)
plot_missing(mydata)
create_report(mydata)
sapply(mydata, function(x) {sum(is.na(x))})
tbl <- with(mydata, table(is_sarcastic))
pie(tbl)

piepercent<- round(100*tbl/sum(tbl), 1)

library(plotrix)
pie3D(tbl, labels = piepercent, main = "Data Imbalance Analysis",col = rainbow(length(tbl)))
legend("topright", c("0 = Non Sarcastic","1 = Sarcastic"), cex = 0.8,
       fill = rainbow(length(tbl)))
