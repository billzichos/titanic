wd <- "~/GitHub/Titanic-Machine-Learning-From-Disaster"

setwd(wd)

# The following files are provided
#   - train.csv
#   - gendermodel.csv
#   - genderclassmodel.csv
#   - test.csv
#   - gendermodel.py
#   - genderclassmodel.py
#   - myfirstforest.py

source("~/GitHub/Get-Raw-Data/download.R")
downloadKaggle("titanic","train.csv")
downloadKaggle("titanic","gendermodel.csv")
downloadKaggle("titanic","genderclassmodel.csv")
downloadKaggle("titanic","test.csv")
downloadKaggle("titanic","gendermodel.py")
downloadKaggle("titanic","genderclassmodel.py")
downloadKaggle("titanic","myfirstforest.py")

datalist <- list(
        train=read.csv(
                paste(wd,"\\",trnFile, sep=""), header=TRUE, as.is=TRUE), 
        test=read.csv(
                paste(wd,"\\",tstFile, sep=""), header=TRUE, as.is=TRUE)
        )
