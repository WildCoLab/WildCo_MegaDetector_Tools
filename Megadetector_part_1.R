### Step 1, read exif ###

# put the project in the project level folder

setwd("./Motion and Timelapse")
getwd()

library(stringr)
library(dplyr)
#install.packages("exifr")
library(exifr)
#install.packages("filesstrings")
library(filesstrings)


new_dir<-(paste0(getwd(),"/05_Megadetector_blanks"))

files<-list.files(path = new_dir, full.names = T, include.dirs = F, 
                  recursive = T)
read_exif(files[1], tags = c("DateTimeOriginal"),recursive = F, quiet = TRUE)

ex<-read_exif(files, tags = c("TriggerMode","Sequence"),recursive=F,quiet=TRUE)

saveRDS(ex, "exifdata_MD5.rds")