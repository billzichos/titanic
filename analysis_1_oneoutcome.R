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
