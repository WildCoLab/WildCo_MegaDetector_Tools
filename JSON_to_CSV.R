
# Converts JSON output from Microsoft MegaDetector 
# Allows counting of bounding boxes by category for comparison to manual classification
# and other applications


# Created: M Fennell and L Stewart
# mitchell.fennell@gmail.com
# laura.nicole.stewart@gmail.com

rm(list=ls())

### 0. Build workspace ####
library(jsonlite)
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)

# Set your working directory
# The working directory will be a file where this script lives; within your working
# directory make a folder that contains your JSONs

# i.e.
#  C:/your_directory_here/
#           Megadetector_files/
#                  station1.json
#                  station2.json
#                  etc.

setwd("C:/your_directory_here")
getwd()

### 1. Load Data ####

#WD is the folder where the .jsons live
WD <- paste(getwd(),"/Megadetector_files",sep="")
dir(WD) #all of your jsons should show up and nothing else
jaysons<-dir(WD)
# get the full list of .json files with their folder locations
file.list <- paste(WD,jaysons,sep="/")  

# get a list of site names
# this works for me because all of my sites have names that are 15 characters long
# you could also import a new csv with your covariates and just keep the names
# just make sure it's in the same order as the file.list

site.list<-str_sub(jaysons,1,15)
site.list

### 2. Check the code ####

# This is the contents of the loop below; we shall run through with s=1
# to make sure it's working and to create a small "categories" data frame
s=1
file_dat <- fromJSON(paste0(file.list[s]), flatten = T, simplifyMatrix = T) 
#str(file_dat2)

file_dat[["detection_categories"]]
# make sure that 1=animal, 2= person, 3=vehicle; if not, change the code below
categories=data.frame(
  cat=c("Animal","Human","Vehicle"),
  category=c(1,2,3)
)
categories

file_img <- file_dat[["images"]] #choose the "images" level of the json
View(file_img)
# this is a data frame with nested data frames in the "detections" column
# so we would like to un-nest it

boxes<-unnest(file_img,cols=c(detections))%>%
  merge(categories)%>% #merge with names of categories
  select(c("file","cat","conf")) #get rid of category numbers and bounding box coordinates
head(boxes)
# this works, except it gets rid of rows with no detections
# so we'll have to add the blanks back in

blanks<-data.frame(
  file=file_dat$images$file[!file_dat$images$file %in% boxes$file],
  cat="Blank",
  conf=0)
head(blanks)

MD_output=rbind(boxes,blanks)


# make sure it looks good
MD_output$file=as.factor(MD_output$file)
MD_output$cat=as.factor(MD_output$cat)

summary(MD_output)
str(MD_output) 
# make sure # levels for $file is the number of images you have at this station
# if it all looks good try the loop

### 3. Iterate JSONS to CSVs ####

#create a new directory where you want the csvs to be saved
newDIR<-paste(getwd(),"/Megadetector_csvs",sep="")

for (s in 1:length(file.list)){
  file_dat <- fromJSON(paste0(file.list[s]), flatten = T, simplifyMatrix = T)
  file_img <- file_dat[["images"]]

  boxes<-unnest(file_img,cols=c(detections))%>%
    merge(categories)%>%
    select(c("file","cat","conf"))
  
  blanks<-data.frame(
    file=file_dat$images$file[!file_dat$images$file %in% boxes$file],
    cat="Blank",
    conf=0)
  
  MD_output=rbind(boxes,blanks)
  write.csv(MD_output, paste(newDIR,"/",site.list[s],".csv",sep=""),row.names = F)
}