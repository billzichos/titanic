#******************************************************
# Table of Contents
#******************************************************

# 1. Objective
# 2. ETL - Load & Transform Data
# 3. EDA - Exploratory Data Analysis
# 4. Scoring
# 5. Appendix


#******************************************************
# Objective
#******************************************************
# Determine survival of those aboard the Titanic.  Survive or Die?


#******************************************************
# ETL
#******************************************************
# set the working directory
setwd("C:\\Users\\Bill\\Documents\\Titanic Machine Learning from Disaster\\")

# load the two datasets into a List
datalist <- list(train=read.csv("train.csv", header=TRUE, as.is=TRUE), score=read.csv("score.csv", header=TRUE, as.is=TRUE))

# add the Survived column to the scoring dataset
datalist[[2]]$Survived <- rep(0, nrow(datalist[[2]]))

# add a value that will help us differentiate the score 
# from train dataset after we combine them
datalist[[1]]$isScore <- rep(0, nrow(datalist[[1]]))
datalist[[2]]$isScore <- rep(1, nrow(datalist[[2]]))

# combine the two datasets into single data frame
fulldatalist <- rbind(datalist[[1]],datalist[[2]])

# apply formatting
     # Pclass
     fulldatalist$Pclass <- factor(fulldatalist$Pclass, ordered=FALSE)

     # Sex
     fulldatalist$Sex <- as.factor(fulldatalist$Sex)

     # SibSp

     # Parch

     # NEW - Family Size
     fulldatalist$FamilySize <- fulldatalist$SibSp + fulldatalist$Parch + 1

     # Ticket

     # Fare
     summary(fulldatalist$Fare)
     fulldatalist$Fare[which(is.na(fulldatalist$Fare))] <- median(fulldatalist$Fare, na.rm=TRUE)
     fulldatalist$Fare2 <- "30+"
     fulldatalist$Fare2[fulldatalist$Fare < 30 & fulldatalist$Fare >= 20] <- "20-30"
     fulldatalist$Fare2[fulldatalist$Fare < 20 & fulldatalist$Fare >= 10] <- "10-20"
     fulldatalist$Fare2[fulldatalist$Fare < 10] <- "<10"
     fulldatalist$Fare2 <- factor(fulldatalist$Fare2, ordered=TRUE)

     # Cabin

     # Embarked
     fulldatalist$Embarked[which(fulldatalist$Embarked=="")] <- "S"
     fulldatalist$Embarked <- as.factor(fulldatalist$Embarked)

     # Name
     library(reshape)
     nameSplit <- colsplit(fulldatalist$Name, "[,.] | [ ]", c("lastName", "title", "otherName1", "otherName2"))
     #combine the datasets
     fulldatalist <- cbind(fulldatalist, nameSplit)
     fulldatalist$lastName <- as.character(fulldatalist$lastName)
     fulldatalist$otherName1 <- as.character(fulldatalist$otherName1)
     fulldatalist$otherName2 <- as.character(fulldatalist$otherName2)
     fulldatalist$titleRollup1 <-
	as.factor(ifelse(fulldatalist$title=="Mr", "MR",
  	ifelse(fulldatalist$title=="Master", "MR",
	ifelse(fulldatalist$title=="Don", "MR",
	ifelse(fulldatalist$title=="Dona", "MISS",
      ifelse(fulldatalist$title=="Miss", "MISS",
      ifelse(fulldatalist$title=="Ms", "MISS",
      ifelse(fulldatalist$title=="Mlle", "MISS",
      ifelse(fulldatalist$title=="Mrs", "MRS",
      ifelse(fulldatalist$title=="Mme", "MRS",
      ifelse(fulldatalist$title=="Rev", "REV",
      ifelse(fulldatalist$title=="Dr", "DR",
      ifelse(fulldatalist$title=="the Countess", "COUNTESS",
      ifelse(fulldatalist$title=="Major", "MAJ",
      ifelse(fulldatalist$title=="Lady", "LADY",
      ifelse(fulldatalist$title=="Sir", "SIR",
      ifelse(fulldatalist$title=="Col", "COL",
      ifelse(fulldatalist$title=="Capt", "CAPT",
      ifelse(fulldatalist$title=="Jonkheer", "JONKHEER", "Unknown")))))))))))))))))))
     fulldatalist$titleRollup2 <-
	as.factor(ifelse(fulldatalist$title=="Mr", "OTHER",
  	ifelse(fulldatalist$title=="Master", "OTHER",
	ifelse(fulldatalist$title=="Don", "ROYALTY",
	ifelse(fulldatalist$title=="Dona", "ROYALTY",
      ifelse(fulldatalist$title=="Miss", "OTHER",
      ifelse(fulldatalist$title=="Ms", "OTHER",
      ifelse(fulldatalist$title=="Mlle", "OTHER",
      ifelse(fulldatalist$title=="Mrs", "OTHER",
      ifelse(fulldatalist$title=="Mme", "OTHER",
      ifelse(fulldatalist$title=="Rev", "CLERGY",
      ifelse(fulldatalist$title=="Dr", "DOCTOR",
      ifelse(fulldatalist$title=="the Countess", "ROYALTY",
      ifelse(fulldatalist$title=="Major", "MILITARY",
      ifelse(fulldatalist$title=="Lady", "ROYALTY",
      ifelse(fulldatalist$title=="Sir", "ROYALTY",
      ifelse(fulldatalist$title=="Col", "MILITARY",
      ifelse(fulldatalist$title=="Capt", "MILITARY",
      ifelse(fulldatalist$title=="Jonkheer", "ROYALTY", "Unknown")))))))))))))))))))

     # NEW Family ID
     fulldatalist$FamilyID <- paste(fulldatalist$lastName, as.character(fulldatalist$FamilySize), sep="")
     fulldatalist$FamilyID2 <- paste(fulldatalist$lastName, as.character(fulldatalist$FamilySize), sep="")
     fulldatalist$FamilyID[fulldatalist$FamilySize <=2] <- 'Small'
     fulldatalist$FamilyID2[fulldatalist$FamilySize <=4] <- 'Small'
     fulldatalist$FamilyID <- factor(fulldatalist$FamilyID)
     fulldatalist$FamilyID2 <- factor(fulldatalist$FamilyID2)

     # Age
     library(rpart)
     agefit <- rpart(Age ~ Pclass+Sex+SibSp+Parch+Embarked
	  +Fare2+title+FamilySize+FamilyID, data=fulldatalist[!is.na(
	  fulldatalist$Age),], method="anova")
     fulldatalist$Age[is.na(fulldatalist$Age)] <- predict(agefit, fulldatalist[is.na(fulldatalist$Age),])
     fulldatalist$Child <- 0
     fulldatalist$Child[fulldatalist$Age<18] <- 1



train.ready <- fulldatalist[fulldatalist$isScore==0,]
score.ready <- fulldatalist[fulldatalist$isScore==1,]

summary(fulldatalist)
str(fulldatalist)

source("C:\\Users\\Bill\\SkyDrive\\Documents\\R Code\\2 - Signal\\frequencyDistributions.R")

freqDistrib(fulldatalist$Age)
#******************************************************

# EDA
#******************************************************

# str(train.ready)
# summary(train.ready)
# head(train.ready)

#	PassengerID
#	  - Integer; Nominal
#	  - provides no value
#	  - it is just a sequential number assigned in the file.
#	  - the only field needed in the deliverable besides the dependant variable (survived).

#	Survived
#	  - should this be a factor or an integer since this is the dependant?
#	  - Integer; Nominal/binary
#	  - 0=died 1=lived
#	  - 61.6% of all passengers died (see Appendix 2)

#	Pclass
#	  - Integer; Ordinal
#	  - Passenger Class (1=1st 2=2nd 3=3rd)
#	  - a proxy for socio-economic status (SES); 1st~Upper 2nd~Middle 3rd~Lower
#	  - 24.2% of the passengers were upper class; 63% survived
#	  - 20.7% of the passengers were middle class; 47% survived
#	  - 55.1% of the passengers were lower class; only 24% survived
#	  - See Appendix 2 for these calcs

#	Name
#	  - Factor
#	  - Full name can be split to pull title and last name out of the name field.
#	  - 84.3% of Mr died.
#	  - 79.4% of MRS (rollup1) survived.
#	  - 75% of ROYALTY (rollup2) survived.
#	  - There was only 1 Don and they died (100%).
#	  - Conversion rules:
#		*  "Mr"		-->	"MR"		-->	OTHER
#		*  "Master" 	-->	"MR"		-->	OTHER
#		*  "Don"		-->	"MR"		-->	OTHER
#		*  "Miss"		-->	"MISS"	-->	OTHER
#		*  "Ms"		-->	"MISS"	-->	OTHER
#		*  "Mlle"		-->	"MISS"	-->	OTHER
#		*  "Mrs"		-->	"MRS"		-->	OTHER
#		*  "Mme"		-->	"MRS"		-->	OTHER
#		*  "Rev"		-->	"REV"		-->	CLERGY
#		*  "Dr"		-->	"DR"		-->	DOCTOR
#		*  "the Countess"	-->	"COUNTESS"	-->	ROYALTY
#		*  "Major"		-->	"MAJ"		-->	MILITARY
#		*  "Lady"		-->	"LADY"	-->	ROYALTY
#		*  "Sir"		-->	"SIR"		-->	ROYALTY
#		*  "Col"		-->	"COL"		-->	MILITARY
#		*  "Cap."		-->	"CAPT"	-->	MILITARY
#		*  "Jonkheer"	-->	"JONKHEER"	-->	ROYALTY

#	Sex
#	  - Factor
#	  - male or female
#	  - there was almost twice as many males on the ship than females
#	  - 81.1% of all men died; while only 25.8% of all women died

#	Age
#	  - Number
#	  - Age is in Years
#	  - Fractional if Age less than One (1)
#	  - If the Age is Estimated, it is in the form xx.5


#	SibSp
#	  - Number
#	  - Number of Siblings/Spouses Aboard
#	  - Sibling: Brother, Sister, Stepbrother, or Stepsister of Passenger Aboard Titanic
#	  - Spouse: Husband or Wife of Passenger Aboard Titanic (Mistresses and Fiances Ignored)

#	Parch
#	  - Integer; ratio
#	  - Number of Parents/Children Aboard
#	  - Parent: Mother or Father of Passenger Aboard Titanic
#	  - Child: Son, Daughter, Stepson, or Stepdaughter of Passenger Aboard Titanic

#	Ticket
#	  - Factor
#	  - Ticket Number

#	Fare
#	  - Number
#	  - Passenger Fare

#	Cabin
#	  - Factor

#	Embarked
#	  - Factor; nominal
#	  - Port of Embarkation
#	  - C=Cherbourg Q=Queenstown S=Southampton
#	  - Is there a missing Embarked value(s)?

#************************************************************
# Scoring
#************************************************************

	#******************************************************
	# 1. Everyone Dies
	#    Kaggle.com Score: 0.62679
	#******************************************************

	#create a copy of the data frame for this output
	score.test1 <- score.ready

	# set all survived values to 0
	score.test1$Survived <- 0
	# score.test1$Survived <- 0  ## would work as well ##

	#the deliverable only needs the Passenger ID and Survived
	score.test1 <- score.test1[, c(1, 2)]

	# create the csv file
	write.csv(file="bz_titanic_1.csv", x=score.test1, row.names = FALSE)

	# remove the file from memory
	rm(score.test1)

	#******************************************************
	# 2. Survival Based on Gender
	#    Kaggle.com Score: 0.76555
	#******************************************************

	# create a copy of the data frame for this output
	score.test2 <- score.ready

	# set all females to survived
	score.test2$Survived[which(score.test2$Sex=="male")] <- 0
	score.test2$Survived[which(score.test2$Sex=="female")] <- 1

	# the deliverable only needs the Passenger ID and Survived
	score.test2 <- score.test2[, c(1, 2)]

	# create the csv file
	write.csv(file="bz_titanic_2.csv", x=score.test2, row.names = FALSE)

	# remove the file from memory
	rm(score.test2)

	#******************************************************
	# 3. Survival Based on Gender, Passenger Class and Fare
	#    Kaggle.com Score: 0.76555
	#******************************************************

	# create a copy of the data frame for this output
	score.test3 <- score.ready

	score.test3$Survived <- 0
	score.test3$Survived[score.test3$Sex == "female"] <- 1
	score.test3$Survived[score.test3$Sex == "female" & score.test3$pclass.factor ==3 & score.test3$Fare > 20] <- 0
	
	# the deliverable only needs the Passenger ID and Survived
	score.test3 <- score.test3[, c(1, 2)]

	# create the csv file
	write.csv(file="bz_titanic_3.csv", x=score.test3, row.names = FALSE)

	# remove the file from memory
	rm(score.test3)

	#******************************************************
	# 4. Decision Tree (Basic)
	#    Kaggle.com Score: 0.78947
	#******************************************************

	require(rpart)

	fit <- rpart(
	  Survived ~ 
	  Pclass+title+Sex+Embarked,
	  method = "class",
	  data=train.ready
	)

	plot(fit)
	text(fit)

	Prediction <- predict(fit, score.ready, type="class")
	submit <- data.frame(PassengerId = score.ready$PassengerId, Survived = Prediction)

	# create the csv file
	write.csv(file="bz_titanic_4.csv", x=submit, row.names=FALSE)

	#******************************************************
	# 5. Decision Tree (All Variables)
	#    Kaggle.com Score: 0.78947 (no change)
	#******************************************************

	require(rpart)

	fit <- rpart(
	  Survived ~ 
	  Pclass+Sex+Child+SibSp+Parch+Embarked+Fare2+title+titleRollup1+titleRollup2,
	  method = "class",
	  data=train.ready
	)

	plot(fit)
	text(fit)

	library(rattle)
	library(rpart.plot)
	library(RColorBrewer)

	fancyRpartPlot(fit)

	Prediction <- predict(fit, score.ready, type="class")
	submit <- data.frame(PassengerId = score.ready$PassengerId, Survived = Prediction)

	# create the csv file
	write.csv(file="bz_titanic_5.csv", x=submit, row.names=FALSE)

	#******************************************************
	# 6. Decision Tree with new FamID, FamSize
	#    Kaggle.com Score: 0.79904
	#******************************************************

	require(rpart)

	fit <- rpart(
	  Survived ~ 
	  Pclass+Sex+Child+SibSp+Parch+Embarked+Fare2+title+FamilySize+FamilyID,
	  method = "class",
	  data=train.ready
	)

	plot(fit)
	text(fit)

	library(rattle)
	library(rpart.plot)
	library(RColorBrewer)

	fancyRpartPlot(fit)

	Prediction <- predict(fit, score.ready, type="class")
	submit <- data.frame(PassengerId = score.ready$PassengerId, Survived = Prediction)

	# create the csv file
	write.csv(file="bz_titanic_6.csv", x=submit, row.names=FALSE)

	#******************************************************
	# 7. Random Forests
	#    Kaggle.com Score: 0.79940 (no change)
	#******************************************************

	install.packages('randomForest')
	library(randomForest)

	set.seed(415)	

	fit <- randomForest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + title + Embarked + FamilySize,
		data = train.ready,
		importance=TRUE, ntree=2000)

	varImpPlot(fit)

#	str(train.ready)
#	str(score.ready)

	Prediction <- predict(fit, score.ready)
	submit <- data.frame(PassengerID = score.ready$PassengerId, Survived = Prediction)
	write.csv(submit, file="bz_titanic_7.csv", row.names=FALSE)
	
	#******************************************************
	# 7. Random Forests
	#    Kaggle.com Score: 0.79904 (no change)
	#******************************************************

	install.packages('party')
	library(party)

	set.seed(415)

	fit <- cforest(as.factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch + Fare + title + Embarked + FamilySize + FamilyID2,
		data = train.ready,
		controls = cforest_unbiased(ntree=2000, mtry=3))

	Prediction <- predict(fit, score.ready, OOB=TRUE, type = "response")
	submit <- data.frame(PassengerID = score.ready$PassengerId, Survived = Prediction)
	write.csv(submit, file="bz_titanic_8.csv", row.names=FALSE)

	#******************************************************
	# Select best model
	#******************************************************

	#******************************************************
	# Output the results for Kaggle.com
	#******************************************************

#************************************************************
# Appendix
#************************************************************

	#******************************************************
	# Appendix 1 - Function used to subset our data
	#******************************************************
	
	func.subset <- function(x,row,col,val) {
		which(x[row,col]==val)
	}

	#******************************************************
	# Appendix 2 - Percent survived
	#******************************************************

	table(datalist[[1]]$Survived)
	prop.table(datalist[[1]]$Survived)

	died <- func.subset(datalist[[1]],col=2,val=0)
	lived <- func.subset(datalist[[1]],col=2,val=1)

	c(length(died) / (length(died) + length(lived)), length(lived) / (length(died) + length(lived)))

	#******************************************************
	# Appendix 3 - Pclass
	#******************************************************

	table(datalist[[1]]$pclass.factor)

	first.class	<- func.subset(datalist[[1]],col=3,val=1)
		first.class.died <- func.subset(datalist[[1]],died,3,1)
			first.class.male.died <- which(paste(as.character(datalist[[1]]$pclass.factor), as.character(train.format$Sex), as.character(train.format$Survived), sep="")=="1male0")
			first.class.female.died <- which(paste(as.character(datalist[[1]]$pclass.factor), as.character(train.format$Sex), as.character(train.format$Survived), sep="")=="1female0")
		first.class.lived <- func.subset(datalist[[1]],lived,3,1)
			first.class.male.lived <- which(paste(as.character(datalist[[1]]$pclass.factor), as.character(train.format$Sex), as.character(train.format$Survived), sep="")=="1male1")
			first.class.female.lived <- which(paste(as.character(datalist[[1]]$pclass.factor), as.character(train.format$Sex), as.character(train.format$Survived), sep="")=="1female1")
	
	second.class <- func.subset(datalist[[1]],col=3,val=2)
		second.class.died <- func.subset(datalist[[1]],died,3,2)
			second.class.male.died <-
			second.class.female.died <-
		second.class.lived <- func.subset(train.format,lived,3,2)
			second.class.male.lived <-
			second.class.female.lived <-

	third.class <- func.subset(datalist[[1]],col=3,val=3)
		third.class.died <- which(datalist[[1]]$pclass.factor[died]==3)
			third.class.male.lived <-
			third.class.female.lived <-
		third.class.lived <- which(datalist[[1]]$pclass.factor[lived]==3)
			third.class.male.lived <-
			third.class.female.lived <-

	pclass.label <- c("A", "B", "C")
	pclass.total <- c(length(first.class), length(second.class), length(third.class))
	pclass.percent.of.total <- c(length(first.class) / length(train.format[,3]), length(second.class) / length(datalist[[1]]$pclass.factor), length(third.class) / length(datalist[[1]]$pclass.factor))
	pclass.percent.died <- c(length(first.class.died) / length(first.class), length(second.class.died) / length(second.class), length(third.class.died) / length(third.class))
	percentage.of.deaths.that.were.male.by.pclass <- length(first.class.male.died)/length(first.class.died)
	pclass.percent.died.female <- length(first.class.female.died)/length(first.class.died)
	pclass.percent.lived <- c(length(first.class.lived) / length(first.class), length(second.class.lived) / length(second.class), length(third.class.lived) / length(third.class))

	pclass.summary <- data.frame(pclass.label, pclass.total, pclass.percent.of.total, pclass.percent.died, pclass.percent.lived)

	#******************************************************
	# Appendix 4 - Name (Title)
	#******************************************************

	# column 1
	listOfTitles <- unique(datalist[[1]]$title)

	# column 2 - total
	listOfTotals <- c(
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[1])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[2])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[3])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[4])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[5])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[6])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[7])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[8])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[9])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[10])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[11])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[12])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[13])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[14])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[15])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[16])),
	  length(func.subset(datalist[[1]], col=13, val=listOfTitles[17])))
	

	# column 3 - percent lived
	listOfPercentLived <- c(
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[1])) / length(func.subset(train.format, col=13, val=listOfTitles[1])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[2])) / length(func.subset(train.format, col=13, val=listOfTitles[2])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[3])) / length(func.subset(train.format, col=13, val=listOfTitles[3])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[4])) / length(func.subset(train.format, col=13, val=listOfTitles[4])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[5])) / length(func.subset(train.format, col=13, val=listOfTitles[5])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[6])) / length(func.subset(train.format, col=13, val=listOfTitles[6])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[7])) / length(func.subset(train.format, col=13, val=listOfTitles[7])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[8])) / length(func.subset(train.format, col=13, val=listOfTitles[8])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[9])) / length(func.subset(train.format, col=13, val=listOfTitles[9])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[10])) / length(func.subset(train.format, col=13, val=listOfTitles[10])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[11])) / length(func.subset(train.format, col=13, val=listOfTitles[11])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[12])) / length(func.subset(train.format, col=13, val=listOfTitles[12])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[13])) / length(func.subset(train.format, col=13, val=listOfTitles[13])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[14])) / length(func.subset(train.format, col=13, val=listOfTitles[14])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[15])) / length(func.subset(train.format, col=13, val=listOfTitles[15])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[16])) / length(func.subset(train.format, col=13, val=listOfTitles[16])),
	  length(func.subset(datalist[[1]], lived, col=13, val=listOfTitles[17])) / length(func.subset(train.format, col=13, val=listOfTitles[17])))
	
	# column 4 - died
	listOfPercentDied <- c(
	  length(func.subset(train.format, died, col=13, val=listOfTitles[1])) / length(func.subset(train.format, col=13, val=listOfTitles[1])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[2])) / length(func.subset(train.format, col=13, val=listOfTitles[2])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[3])) / length(func.subset(train.format, col=13, val=listOfTitles[3])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[4])) / length(func.subset(train.format, col=13, val=listOfTitles[4])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[5])) / length(func.subset(train.format, col=13, val=listOfTitles[5])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[6])) / length(func.subset(train.format, col=13, val=listOfTitles[6])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[7])) / length(func.subset(train.format, col=13, val=listOfTitles[7])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[8])) / length(func.subset(train.format, col=13, val=listOfTitles[8])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[9])) / length(func.subset(train.format, col=13, val=listOfTitles[9])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[10])) / length(func.subset(train.format, col=13, val=listOfTitles[10])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[11])) / length(func.subset(train.format, col=13, val=listOfTitles[11])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[12])) / length(func.subset(train.format, col=13, val=listOfTitles[12])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[13])) / length(func.subset(train.format, col=13, val=listOfTitles[13])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[14])) / length(func.subset(train.format, col=13, val=listOfTitles[14])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[15])) / length(func.subset(train.format, col=13, val=listOfTitles[15])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[16])) / length(func.subset(train.format, col=13, val=listOfTitles[16])),
	  length(func.subset(train.format, died, col=13, val=listOfTitles[17])) / length(func.subset(train.format, col=13, val=listOfTitles[17])))

	title.summary <- data.frame(listOfTitles, listOfTotals, listOfPercentLived, listOfPercentDied)

	order.title.summary <- order(title.summary$listOfTotals, decreasing=TRUE)
	title.summary[order.title.summary, ]

	#******************************************************
	# Appendix 3 - Name (Title Rollup 1)
	#******************************************************

	# column 1
	listOfTitles <- as.factor(unique(train.format$titleRollup1))


	# column 2 - total
	listOfTotals <- c(
	  length(func.subset(train.format, col=17, val=listOfTitles[1])),
	  length(func.subset(train.format, col=17, val=listOfTitles[2])),
	  length(func.subset(train.format, col=17, val=listOfTitles[3])),
	  length(func.subset(train.format, col=17, val=listOfTitles[4])),
	  length(func.subset(train.format, col=17, val=listOfTitles[5])),
	  length(func.subset(train.format, col=17, val=listOfTitles[6])),
	  length(func.subset(train.format, col=17, val=listOfTitles[7])),
	  length(func.subset(train.format, col=17, val=listOfTitles[8])),
	  length(func.subset(train.format, col=17, val=listOfTitles[9])),
	  length(func.subset(train.format, col=17, val=listOfTitles[10])),
	  length(func.subset(train.format, col=17, val=listOfTitles[11])),
	  length(func.subset(train.format, col=17, val=listOfTitles[12])))
	

	# column 3 - percent lived
	listOfPercentLived <- c(
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[1])) / length(func.subset(train.format, col=17, val=listOfTitles[1])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[2])) / length(func.subset(train.format, col=17, val=listOfTitles[2])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[3])) / length(func.subset(train.format, col=17, val=listOfTitles[3])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[4])) / length(func.subset(train.format, col=17, val=listOfTitles[4])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[5])) / length(func.subset(train.format, col=17, val=listOfTitles[5])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[6])) / length(func.subset(train.format, col=17, val=listOfTitles[6])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[7])) / length(func.subset(train.format, col=17, val=listOfTitles[7])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[8])) / length(func.subset(train.format, col=17, val=listOfTitles[8])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[9])) / length(func.subset(train.format, col=17, val=listOfTitles[9])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[10])) / length(func.subset(train.format, col=17, val=listOfTitles[10])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[11])) / length(func.subset(train.format, col=17, val=listOfTitles[11])),
	  length(func.subset(train.format, lived, col=17, val=listOfTitles[12])) / length(func.subset(train.format, col=17, val=listOfTitles[12])))
	
	# column 4 - died
	listOfPercentDied <- c(
	  length(func.subset(train.format, died, col=17, val=listOfTitles[1])) / length(func.subset(train.format, col=17, val=listOfTitles[1])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[2])) / length(func.subset(train.format, col=17, val=listOfTitles[2])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[3])) / length(func.subset(train.format, col=17, val=listOfTitles[3])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[4])) / length(func.subset(train.format, col=17, val=listOfTitles[4])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[5])) / length(func.subset(train.format, col=17, val=listOfTitles[5])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[6])) / length(func.subset(train.format, col=17, val=listOfTitles[6])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[7])) / length(func.subset(train.format, col=17, val=listOfTitles[7])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[8])) / length(func.subset(train.format, col=17, val=listOfTitles[8])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[9])) / length(func.subset(train.format, col=17, val=listOfTitles[9])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[10])) / length(func.subset(train.format, col=17, val=listOfTitles[10])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[11])) / length(func.subset(train.format, col=17, val=listOfTitles[11])),
	  length(func.subset(train.format, died, col=17, val=listOfTitles[12])) / length(func.subset(train.format, col=17, val=listOfTitles[12])))

	titleRollup1.summary <- data.frame(listOfTitles, listOfTotals, listOfPercentLived, listOfPercentDied)

	order.titleRollup1.summary <- order(titleRollup1.summary$listOfTotals, decreasing=TRUE)
	titleRollup1.summary[order.titleRollup1.summary, ]


	#******************************************************
	# Appendix 3 - Name (Title Rollup 2)
	#******************************************************

	# column 1
	listOfTitles <- as.factor(unique(train.format$titleRollup2))


	# column 2 - total
	listOfTotals <- c(
	  length(func.subset(train.format, col=16, val=listOfTitles[1])),
	  length(func.subset(train.format, col=16, val=listOfTitles[2])),
	  length(func.subset(train.format, col=16, val=listOfTitles[3])),
	  length(func.subset(train.format, col=16, val=listOfTitles[4])),
	  length(func.subset(train.format, col=16, val=listOfTitles[5])))
	

	# column 3 - percent lived
	listOfPercentLived <- c(
	  length(func.subset(train.format, lived, col=16, val=listOfTitles[1])) / length(func.subset(train.format, col=16, val=listOfTitles[1])),
	  length(func.subset(train.format, lived, col=16, val=listOfTitles[2])) / length(func.subset(train.format, col=16, val=listOfTitles[2])),
	  length(func.subset(train.format, lived, col=16, val=listOfTitles[3])) / length(func.subset(train.format, col=16, val=listOfTitles[3])),
	  length(func.subset(train.format, lived, col=16, val=listOfTitles[4])) / length(func.subset(train.format, col=16, val=listOfTitles[4])),
	  length(func.subset(train.format, lived, col=16, val=listOfTitles[5])) / length(func.subset(train.format, col=16, val=listOfTitles[5])))
	
	# column 4 - died
	listOfPercentDied <- c(
	  length(func.subset(train.format, died, col=16, val=listOfTitles[1])) / length(func.subset(train.format, col=16, val=listOfTitles[1])),
	  length(func.subset(train.format, died, col=16, val=listOfTitles[2])) / length(func.subset(train.format, col=16, val=listOfTitles[2])),
	  length(func.subset(train.format, died, col=16, val=listOfTitles[3])) / length(func.subset(train.format, col=16, val=listOfTitles[3])),
	  length(func.subset(train.format, died, col=16, val=listOfTitles[4])) / length(func.subset(train.format, col=16, val=listOfTitles[4])),
	  length(func.subset(train.format, died, col=16, val=listOfTitles[5])) / length(func.subset(train.format, col=16, val=listOfTitles[5])))

	titleRollup2.summary <- data.frame(listOfTitles, listOfTotals, listOfPercentLived, listOfPercentDied)

	order.titleRollup2.summary <- order(titleRollup2.summary$listOfTotals, decreasing=TRUE)
	title.summary[order.title.summary, ]
	titleRollup1.summary[order.titleRollup1.summary, ]
	titleRollup2.summary[order.titleRollup2.summary, ]


	#******************************************************
	# Appendix 4 - Sex
	#******************************************************

	summary(train.format$Sex)
	
	prop.table(table(train.format$Sex, train.format$Survived),1)

	male <- func.subset(train.format,col=4,val="male")
	male.died <- which(train.format$Sex[died]=="male")
	male.lived <- which(train.format$Sex[lived]=="male")

	female <- func.subset(train.format,col=4,val="female")
	female.died <- which(train.format$Sex[died]=="female")
	female.lived <- which(train.format$Sex[lived]=="female")
	
	length(male.died) / length(male)
	length(female.died) / length(female)


	#******************************************************
	# Appendix 5 - Age
	#******************************************************

	summary(datalist[[1]]$Age)

	prop.table(table(datalist[[1]]$Child))

	aggregate(Survived ~ Child + Sex, data=datalist[[1]], FUN=function(x) {sum(x)/length(x)})

	#******************************************************
	# Appendix 6 - Last Name, SibSp, Parch
	#******************************************************

#	Evaluate the SibSp, Parch and Last Name fields to id the families.
#	If there is a family member in the scoring dataset that has corresponding
#	family members in the training dataset, we may be able to draw some
#	conclusions.  It would be nice if we could classify each person as 
#	Dad with 3 kids; Mom with 2 kids and Spouse; Child with 1 Brother, etc.
#	Family members would likely have nearby or the same cabins.


	#******************************************************
	# Appendix 7 - Ticket
	#******************************************************

	summary(fulldatalist$Ticket)

	fulldatalist[c(fulldatalist$Ticket, fulldatalist$Pclass),]

	

	#******************************************************
	# Appendix 8 - Fare
	#******************************************************

	summary(datalist[[1]]$Fare)
	
	table(datalist[[1]]$Fare2)

	aggregate(Survived ~ Fare2 + Child + Sex, data=datalist[[1]], FUN=function(x) {sum(x)/length(x)})


	#******************************************************
	# Appendix 9 - Cabin
	#******************************************************


	#******************************************************
	# Appendix 10 - Embarked
	#******************************************************

	cherbourg <- func.subset(datalist[[1]],col=11,val="C")
	queenstown <- func.subset(datalist[[1]],col=11,val="Q")
	southampton <- func.subset(datalist[[1]],col=11,val="S")
	unknown.embark <- func.subset(datalist[[1]],col=11,val="")

	length(cherbourg)
	length(queenstown)
	length(southampton)
	length(unknown.embark)

# We also want a way to designate Royalty (sir, lady, countess, jonkheer),
# Clergy (rev), Military (capt, col, maj) and Doctors (dr).

	#******************************************************
	# Appendix ## - Family ID
	#******************************************************

	sort(unique(fulldatalist$FamilyID))

	table(fulldatalist$FamilyID)

	str(fulldatalist)

# help(rpart)

library(Amelia)
# help(missmap)

# ID the missing values
missmap(new_train, main = "Missing Map - Train")
missmap(test, main = "Missing Map - Test")

#The two following methods are a subset of the code that can be found here https://github.com/chungkhim/titanic/blob/master/processData.r
enrichData = function(x){nameSplit = sapply(x["name"], function(y) {strsplit(y, "[,.] | [ ]")}) x["lastName"] = sapply(nameSplit, function(y) {y[1]}) x["title"] = sapply(nameSplit, function(y) {y[2]}) x["otherName"] = sapply(nameSplit, function(y) {y[3]}) return(x)}


#aggreating some rare occurence of titles into broader categories
refineData = function(x)
{
  ladies = c( "Mme" , "the Countess" , "Lady" ,  "Dona" )
  x$title[x$title %in% ladies] = "Mrs"
  
  miss = c("Mlle", "Ms")
  x$title[x$title %in% miss] = "Miss"
  
  ranks = c("Capt", "Col", "Major","Sir", "Don","Dr")
  x$title[x$title %in% ranks] = "Mr"
  
  masters = c("Jonkheer")
  x$title[x$title %in% masters] = "Master"
  
  
  return(x)
  
}  


#wrapper function to enrich and refine the train/data sets as long
#as casting some variables into factors
processData <- function(train){
  data <- enrichData(train)
  data <- refineData(train)
  
  #pclass  sex and title should be considered as a factors
  train$pclass <- as.factor(train$pclass)
  train$sex <- as.factor(train$sex)
  train$title <- as.factor(train$title)
  
  return(training);

}

fix(processData)


    library("doBy")
    siterange <- function(x){c(length(x), min(x), mean(x), max(x))}
    summaryBy(Age~agecat, data =data1, FUN=siterange)

	summaryBy(Pclass + Survived ~ PassengerID, data=train)

  ggplot(data1, aes(x=Impressions, fill=agecat))+geom_histogram(binwidth=1)
  ggplot(data1, aes(x=agecat, y=Impressions, fill=agecat))+geom_boxplot()

help(cast)
ls()
rm(training)

?complex

help(sprintf)


which.var <- function(x, val) {
	which(paste(x,'$','==pclass.factor',sep=''))
}

paste('train.format', '$', 'pclass.factor==','1', sep='')

help(paste)

	first.class	<- func.subset(train.format,col=3,val=1)
		first.class.died <- func.subset(train.format,died,3,1)
			first.class.male.died <-
			first.class.female.died <-
		first.class.lived <- func.subset(train.format,lived,3,1)
			first.class.male.lived <-
			first.class.female.lived <-


which(train.format$pclass==1)
	which(train.format$Sex=="male")
	which(train.format$Sex=="female")

help(paste)

