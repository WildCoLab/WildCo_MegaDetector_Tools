# Step 2, move the files into separate folders
#         1. _Station_Name_Blanks
#         2. Station_Name_Animals_and_Timelapse
# put the project in the project level folder

setwd("./Motion and Timelapse")
getwd()

library(stringr)
library(dplyr)
#install.packages("exifr")
library(exifr)
#install.packages("filesstrings")
library(filesstrings)

md<-read.csv("F:/2020 Tuyeta Camera Data/MegadetectorResults/csvs/MD5_output_June2021.csv")

md$file[1]
str_locate(md$file[1],"\\\\")
str_sub(md$file[1],str_locate(md$file[1],"\\\\")[1]+1,)
md=md%>%
  mutate(station = str_sub(md$file,1,str_locate(md$file,"\\\\")[1]-1),
         orig_file = str_sub(md$file,str_locate(md$file,"\\\\")[1]+1,))
head(md)
md$station<-as.factor(md$station)
table(md$station)

new_dir<-(paste0(getwd(),"/05_Megadetector_blanks"))

ex<-readRDS("exifdata_MD5.rds")


head(ex)
ex$Sequence<-as.factor(ex$Sequence)
summary(ex)

#ex[is.na(ex$TriggerMode),]
#ex=ex[!is.na(ex$TriggerMode),]

str_locate_all(ex$SourceFile[1],"/")[[1]][5]
str_sub(ex$SourceFile[1], str_locate_all(ex$SourceFile[1],"/")[[1]][5]+1,)

ex<-ex%>%
  mutate(orig_file = str_sub(ex$SourceFile, str_locate_all(ex$SourceFile,"/")[[1]][5]+1,))

ex$orig_file<-as.factor(ex$orig_file)
head(ex)

summary(md$orig_file %in% ex$orig_file)
summary(ex$orig_file %in% md$orig_file)

mdfalse<-md[!md$orig_file %in% ex$orig_file,]
summary(mdfalse) # this is fine, these are images that already got uploaded
summary(str_detect(ex$orig_file,"BMS-PRP-025-19",)) #yes that station is there
exfalse<-ex[!ex$orig_file %in% md$orig_file,]
head(exfalse)
tail(exfalse) #this is fine



y<-merge(md,ex,by="orig_file")
y$TriggerMode<-as.factor(y$TriggerMode)
y$orig_file<-as.factor(y$orig_file)
y<-y %>% arrange(orig_file, desc(conf)) # arrange so the highest conf if on top
#need to get ride of duplicates but keep the highest conf value
copy_of_y<-y

y<-distinct(y, y$orig_file, .keep_all=T)

# create an include column 
y$include<-rep(F,nrow(y))

y[is.na(y$TriggerMode),]

#first change all time lapse include to T
for (i in 1:nrow(y)){
  if (y$TriggerMode[i] == "T"){ y$include[i]<-T }
}



summary(y) # notice that the sequence summary isn't perfect. there isn't always a full 1-2-3. I think this is fine


# I want this workflow

# if conf>0.2, and if seq == 1
# include 1
# then check if the next is 2
# if it is, include it
# if it is, check if the next is 3
# if it is, include  it

# if seq = 2 and include =F
# include 2
# if the previous is 1, include it
# if the next is 3, include it

# if seq = 3 and include = F and conf >0.2
# include 3
# if the previous is 2, include it
# if it is, and if the previous previous is 1, include it



for (i in 1:nrow(y)){
  if (y$Sequence[i] == "1 3" & y$conf[i] > 0.2){ # if conf>0.2, and if seq == 1
    y$include[i] <- T                         # include 1
    if (y$Sequence[i+1] == "2 3") {           # if we included 1, and if the next is 2
      y$include[i+1] <- T                     # include 2
      if (y$Sequence[i+2]== "3 3") {          # if we included 1 and 2 and the next is 3
        y$include[i+2] <- T                   # include 3
      }}}}



for (i in 1:nrow(y)){
  if (y$include[i] == F & y$Sequence[i] == "2 3" & y$conf[i] > 0.2){ # if conf > 0.2, seq is 2, and we haven't already included it
    y$include[i]<-T                                                  # include 2
    if (y$Sequence[i-1] == "1 3") {                                  # if we included 2 and the previous is 1
      y$include[i-1]<-T                                              # include 1
    }
    if (y$Sequence[i+1] == "3 3") {                                  # if we included 2 and the next is 3
      y$include[i+1]<-T                                              # include 3
      print(i)                                                       # and tell me where it was so I can check
    }
  }
}


for (i in 2:nrow(y)){
  if (y$include[i] == F & y$Sequence[i] == "3 3" & y$conf[i] > 0.2){ # If conf >0.2,seq = 3 and we haven't already included it,
    y$include[i]<-T                                                  # include 3
    print(i)                                                         # and tell me where it was so I can check.
    if (y$Sequence[i-1] == "2 3") {                                  # If we included 3 and the previous is 2,
      y$include[i-1]<-T                                              # include 2.
      if (y$Sequence[i-2] == "1 3") {                                # If we included 3 and 2 and the next previous is 1,
        y$include[i-2]<-T                                            # include 1.
      }
    }
  }
}
  


summary(y)



 ### Try with confidence value of 0.8 instead.


y8<-copy_of_y
y8<-distinct(y8, y8$orig_file, .keep_all=T)

# create an include column 
y8$include<-rep(F,nrow(y))

#first change all time lapse include to T
for (i in 1:nrow(y8)){
  if (y8$TriggerMode[i] == "T"){ y8$include[i]<-T }
}



for (i in 1:nrow(y8)){
  if (y8$Sequence[i] == "1 3" & y8$conf[i] > 0.8){ # if conf>0.8, and if seq == 1
    y8$include[i] <- T                         # include 1
    if (y8$Sequence[i+1] == "2 3") {           # if we included 1, and if the next is 2
      y8$include[i+1] <- T                     # include 2
      if (y8$Sequence[i+2]== "3 3") {          # if we included 1 and 2 and the next is 3
        y8$include[i+2] <- T                   # include 3
      }}}}


for (i in 1:nrow(y8)){
  if (y8$include[i] == F & y8$Sequence[i] == "2 3" & y8$conf[i] > 0.8){ # if conf > 0.8, seq is 2, and we haven't already8 included it
    y8$include[i]<-T                                                  # include 2
    if (y8$Sequence[i-1] == "1 3") {                                  # if we included 2 and the previous is 1
      y8$include[i-1]<-T                                              # include 1
    }
    if (y8$Sequence[i+1] == "3 3") {                                  # if we included 2 and the next is 3
      y8$include[i+1]<-T                                              # include 3
      print(i)                                                       # and tell me where it was so I can check
    }
  }
}


for (i in 1:nrow(y8)){
  if (y8$include[i] == F & y8$Sequence[i] == "3 3" & y8$conf[i] > 0.8){ # If conf >0.8,seq = 3 and we haven't already8 included it,
    y8$include[i]<-T                                                  # include 3
    print(i)                                                         # and tell me where it was so I can check.
    if (y8$Sequence[i-1] == "2 3") {                                  # If we included 3 and the previous is 2,
      y8$include[i-1]<-T                                              # include 2.
      if (y8$Sequence[i-2] == "1 3") {                                # If we included 3 and 2 and the next previous is 1,
        y8$include[i-2]<-T                                            # include 1.
      }
    }
  }
}


#write.csv(y8, "Megadetector_inclusion_08.csv",row.names = F)
write.csv(y, "05_Megadetector_inclusion_02.csv",row.names = F)

summary(y8)
349113/nrow(y8)
y8include<-y8[y8$include==T,]
table(y8include$station)
summary(as.data.frame(table(y8include$station)))

summary(y)
163523/nrow(y)
y2include<-y[y$include==T,]
table(y2include$station)
table(y$station)

old.dir="F:/2020 Tuyeta Camera Data/Motion and Timelapse/05_Megadetector_blanks"
new.dir="F:/2020 Tuyeta Camera Data/Motion and Timelapse/05_Megadetector_animals_and_timelapse"
create_dir(new.dir)
Stations=list.dirs(old.dir, full.names = F, recursive = F)
Stations=Stations[Stations!=""]
Stations
j=1
for (j in 1: length(Stations)){
  dir.create(paste0(new.dir,"/",Stations[j]))
  
  tmp_file_list=y2include[y2include$station==Stations[j],]$SourceFile
  tmp_file_list
  move_files(files = tmp_file_list, destinations = paste0(new.dir,"/",Stations[j]))
  print(paste("files moved at",Stations[j]))
  print(paste("Progress",j,"/",length(Stations)))
}
