wd <- "C:\\Users\\Bill\\Documents\\GitHub\\Titanic-Machine-Learning-From-Disaster"

tstFile <- "test.csv"

train=read.csv(
        paste(wd,"\\",trnFile, sep=""), header=TRUE, as.is=TRUE
        )