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