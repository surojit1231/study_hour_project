#import the data as csv file
df<-studyhour
head(df,10)
#explantory data analysis
plot(df,main="Hours vs Percentage",xlab="Hours Studied",ylab="Percentage Score",col="dark red")
#split the data in two parts
sam_size=floor(0.75*nrow(df))
train_data<-sample((nrow(df)),size=sam_size)
train=df[train_data,]
test=df[-train_data,]
#fit a linear model on the test data
reggression1<-lm((Score)~(Hours),train)
summary(reggression1)
#take some transformation on the variables
reggression2<-lm(log(Score)~(Hours),train)
summary(reggression2)
reggression3<-lm(log(Score)~log(Hours),train)
summary(reggression3)
reggression4<-lm((Score)~(Hours)^2,train)
summary(reggression4)
reggression5<-lm((Score)^2~(Hours)^2,train)
summary(reggression5)
#take the best model from this and plotting the reggression line
plot(df,main="Hours vs Percentage",xlab="Hours Studied",ylab="Percentage Score",col="dark red")
abline(lm(Score~(Hours)^2,data=train),col="blue")
#predicting the score
y.pred1<-predict(reggression4,test)
print(y.pred1)
#Comparing Actual vs Predicted
output<-data.frame(actual<-test$Score,predicted<-y.pred1)
print(output)
#You can also test with your own data
input<-data.frame(Hours<-(5:7),Scores<-(1:3))
y.pred2<-predict(reggression4,input)
print(y.pred2)
#evaluating the model
rmse<-sqrt(mean((test$Score-y.pred1)^2))
print(rmse)
#also minimize the residual moddel
