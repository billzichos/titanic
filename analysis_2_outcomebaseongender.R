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