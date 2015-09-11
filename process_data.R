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
df$Fare[is.na(df$Fare)] <- median(df$Fare, na.rm=TRUE)

# impute Embarked by marking all NULLS as originating in Southampton
df$Embarked[which(df$Embarked=="")] <- "S"
df$Embarked <- as.factor(df$Embarked)

# extract cabin number from cabin
df$Cabin <- as.character(df$Cabin)
df$CabinNum <- sapply(df$Cabin, function(x) strsplit(x, '[A-Z]')[[1]][2])
df$CabinNum <- as.numeric(df$CabinNum)

# classify the cabins into 1 of 3 areas based on cabin number
df$CabinPos[df$CabinNum<50] <- "Forward"
df$CabinPos[df$CabinNum>=50 & df$CabinNum<100] <- "Middle"
df$CabinPos[df$CabinNum>=1000] <- "Aft"
df$CabinPos <- as.factor(df$CabinPos)

# derive a family size feature
df$FamilySize <- df$SibSp + df$Parch + 1

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

#impute Age
        # impute Age with -1
        #df$Age[is.na(df$Age)] <- -1

        #impute Age based on decision tree
        library("rpart")
        predicted.age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + title + FamilySize, data = df[!is.na(df$Age),], method = "anova")
        df$Age[is.na(df$Age)] <- predict(predicted.age, df[is.na(df$Age),])
