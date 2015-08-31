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
fulldatalist$Embarked[which(fulldatalist$Embarked=="")] <- "S"
fulldatalist$Embarked <- as.factor(fulldatalist$Embarked)