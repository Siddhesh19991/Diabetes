diabetes <- read.csv("~/Downloads/diabetes.csv")
library(caret)
intrain<-createDataPartition(y=diabetes$Outcome,p=0.7,list = FALSE)
train<-diabetes[intrain,]
test<-diabetes[-intrain,]
train$Outcome<-as.factor(train$Outcome)


model1<-train(Outcome~.,method="rpart",data=train)
model2<-train(Outcome~Glucose+BloodPressure+BMI+Age+DiabetesPedigreeFunction+Insulin+Pregnancies,method="rpart",data=train)#same value as before
model3<-train(Outcome~Glucose+BloodPressure+BMI+Age,method="rpart",data=train)#same value as before
model4<-train(Outcome~Glucose+BloodPressure+BMI+Age,method="rf",data=train,prox=TRUE)#using random forest to get a better result

value<-predict(model4,test)

result<-cbind(test,value)

table(result$Outcome,result$value)
