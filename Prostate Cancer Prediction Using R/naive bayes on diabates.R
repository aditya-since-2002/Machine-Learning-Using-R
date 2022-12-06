data1 <- read.csv('diabetes.csv', stringsAsFactors = FALSE)

str(data1)

View(data1)

data1$age <- factor(data$Age)

library(caTools)
set.seed(123)
split = sample.split(data1$age, SplitRatio=0.7)
split
training_set = subset(data1, split == TRUE)
test_set = subset(data1, split == FALSE)

regressor = lm(formula = BloodPressure~ age, data=training_set)

y_predict= predict(regressor,newdata=test_set)
y_predict

plot(x=training_set$BloodPressure, y=training_set$age, main="Scatterplot", xlab="BloodPressure", ylab="age")
plot(x=test_set$BloodPressure, y=test_set$age, main="Scatterplot", xlab="BloodPressure", ylab="age")

library(ggplot2)
ggplot()+geom_point(aes(x=training_set$BloodPressure, y=training_set$age),color='red')+geom_line(aes(x=training_set$BloodPressure, y=predict(regressor, newdata=training_set)),color='blue')+
  ggtitle("age vs bloodpressure trainingset")+xlab('BloodPressure')+ylab("age")

p <- data.frame('BloodPressure'=120)
predict(regressor, p)

