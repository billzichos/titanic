trnUrl <- "https://www.kaggle.com/c/titanic/download/train.csv"
tstUrl <- "https://www.kaggle.com/c/titanic/download/test.csv"

wd <- "C:\\Users\\Bill\\Documents\\GitHub\\Titanic-Machine-Learning-From-Disaster"

setwd <- "C:\\Users\\Bill\\Documents\\GitHub\\Titanic-Machine-Learning-From-Disaster"

trnFile <- "train.csv"
tstFile <- "test.csv"

download.file(trnUrl, paste(wd,"\\train.csv", sep=""), quiet=TRUE)
download.file(tstUrl, paste(wd,"\\test.csv", sep=""), quiet=TRUE)

datalist <- list(
        train=read.csv(
                paste(wd,"\\",trnFile, sep=""), header=TRUE, as.is=TRUE), 
        test=read.csv(
                paste(wd,"\\",tstFile, sep=""), header=TRUE, as.is=TRUE)
        )

downloadKaggle <- function(competitionName, fileName) {
        kaggleUrl <- "https://www.kaggle.com/c/"
        download.file(
                paste(
                        kaggleUrl,competitionName,'/download/',fileName,sep=""
                ),
                paste(getwd(),"\\",fileName,sep=""),
                quiet = FALSE
        )
}