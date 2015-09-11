wd <- "~/GitHub/Titanic-Machine-Learning-From-Disaster"

setwd(wd)

# read in the training file and prepare for combining with test.
train <- read.csv("train.csv")
train$Source <- "Train"

# read in the test file and prepare for combining with train.
test <- read.csv("test.csv")
test$Source <- "Test"

# add the Survived column to the test dataset
test$Survived <- rep(0, nrow(test))

# combine the two datasets into single data frame
df <- rbind(train, test)

# format as factors
df$Pclass <- factor(df$Pclass, ordered=FALSE)
df$Sex <- as.factor(df$Sex, ordered = FALSE)

# impute Fare using the median fare price.
df$Fare[which(is.na(df$Fare))] <- median(df$Fare, na.rm=TRUE)

# impute Embarked by marking all NULLS as originating in Southampton
df$Embarked[which(df$Embarked=="")] <- "S"
df$Embarked <- as.factor(df$Embarked)

# Name
library(reshape)
nameSplit <- colsplit(df$Name, "[,.] | [ ]", c("lastName", "title", "otherName1", "otherName2"))
df <- cbind(df, nameSplit)
df$lastName <- as.character(df$lastName)
df$otherName1 <- as.character(df$otherName1)
df$otherName2 <- as.character(df$otherName2)
df$titleRollup1 <-
        as.factor(ifelse(df$title=="Mr", "MR",
                         ifelse(df$title=="Master", "MR",
                                ifelse(df$title=="Don", "MR",
                                       ifelse(df$title=="Dona", "MISS",
                                              ifelse(df$title=="Miss", "MISS",
                                                     ifelse(df$title=="Ms", "MISS",
                                                            ifelse(df$title=="Mlle", "MISS",
                                                                   ifelse(df$title=="Mrs", "MRS",
                                                                          ifelse(df$title=="Mme", "MRS",
                                                                                 ifelse(df$title=="Rev", "REV",
                                                                                        ifelse(df$title=="Dr", "DR",
                                                                                               ifelse(df$title=="the Countess", "COUNTESS",
                                                                                                      ifelse(df$title=="Major", "MAJ",
                                                                                                             ifelse(df$title=="Lady", "LADY",
                                                                                                                    ifelse(df$title=="Sir", "SIR",
                                                                                                                           ifelse(df$title=="Col", "COL",
                                                                                                                                  ifelse(df$title=="Capt", "CAPT",
                                                                                                                                         ifelse(df$title=="Jonkheer", "JONKHEER", "Unknown")))))))))))))))))))
df$titleRollup2 <-
        as.factor(ifelse(df$title=="Mr", "OTHER",
                         ifelse(df$title=="Master", "OTHER",
                                ifelse(df$title=="Don", "ROYALTY",
                                       ifelse(df$title=="Dona", "ROYALTY",
                                              ifelse(df$title=="Miss", "OTHER",
                                                     ifelse(df$title=="Ms", "OTHER",
                                                            ifelse(df$title=="Mlle", "OTHER",
                                                                   ifelse(df$title=="Mrs", "OTHER",
                                                                          ifelse(df$title=="Mme", "OTHER",
                                                                                 ifelse(df$title=="Rev", "CLERGY",
                                                                                        ifelse(df$title=="Dr", "DOCTOR",
                                                                                               ifelse(df$title=="the Countess", "ROYALTY",
                                                                                                      ifelse(df$title=="Major", "MILITARY",
                                                                                                             ifelse(df$title=="Lady", "ROYALTY",
                                                                                                                    ifelse(df$title=="Sir", "ROYALTY",
                                                                                                                           ifelse(df$title=="Col", "MILITARY",
                                                                                                                                  ifelse(df$title=="Capt", "MILITARY",
                                                                                                                                         ifelse(df$title=="Jonkheer", "ROYALTY", "Unknown")))))))))))))))))))