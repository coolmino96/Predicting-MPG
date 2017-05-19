######building single variable regression model#####

#function predict_significant_values returns mean and variance of sampled test data and training data
predict_significant_values<-function(data_set,independent,dependent,independent_name,dependent_name,print=TRUE){
  
  #randomly building a model based on first 300 variables
  car_model1 <- lm( dependent ~ independent , data=data_set[1:300,])
  print(summary(car_model1))
  car_model_res1 <- car_model1$residuals
  m<-mean(car_model_res1)
  s<-sd(car_model_res1)
  
  #1  residuals vs. the predictor variable
  plot(independent,car_model_res1,ylab = "residuals",xlab = paste(independent_name,""))
  abline(a=0,b=0,col="red")
  abline(a=2*s,b=0,col="blue")
  abline(a=-2*s,b=0,col="blue")
  
  #2 absolute value of the residuals vs. the predictor variable
  plot(independent,abs(car_model_res1),ylab = paste("Absolute values of ","residuals"),xlab = paste(independent_name,""))
  abline(a=2*s,b=0,col="blue")
  
  #3 histogram of the residuals
  h<-hist(car_model_res1,breaks=20,prob=T,xlab="residuals of mpg")
  x <- seq(-10,10,length = 300)
  y <- dnorm(x,m,s)
  lines(x,y,col="red")
  
  #4 qqnormal plot of the residuals
  qqnorm(car_model_res1)
  
  #predicted_train contains training data 
  predicted_train <- data.frame()
  
  #predicted_meta contains intercept and coefficients of sampled training data
  predicted_meta <- data.frame()
  
  #predicted_test contains data that is tested against the intercept and coefficients of sampled data
  predicted_test <- data.frame()
  
  #randomly sampling the data ten times (we can change how many times you want to sample by giving different input numbers)
  for(i in 1:10){
    
    #generating 392 randomly distributed uniform numbers
    random<- runif(392)
    
    #binding randomly distributed uniform numbers to original dataset
    car_data<-cbind(data_set,random)
    
    #sorting the dataframe with respect to uniformly distributed random numbers
    car_data<-car_data[order(random),]
    
    #removing the random numbers column
    car_data<-car_data[,-6]
    
    actual_mpg <- car_data[301:392,]$mpg 
    
    #building a regression with independent variable as weight 
    if(independent_name == "weight"){
      sampled_car_model <- lm( mpg ~ weight , data=car_data[1:300,])
      predicted_mpg <- sampled_car_model$coefficients[1] + (sampled_car_model$coefficients[2]) * car_data[301:392,]$weight
      predicted_mpg_300 <- sampled_car_model$coefficients[1] + (sampled_car_model$coefficients[2]) * car_data[1:300,]$weight
    }
    
    #building a regression with independent variable as acceleration
    if(independent_name == "acceleration"){
      sampled_car_model <- lm( mpg ~ acceleration , data=car_data[1:300,])
      predicted_mpg <- sampled_car_model$coefficients[1] + (sampled_car_model$coefficients[2]) * car_data[301:392,]$acceleration
      predicted_mpg_300 <- sampled_car_model$coefficients[1] + (sampled_car_model$coefficients[2]) * car_data[1:300,]$acceleration
    }
    
    #building a regression with independent variable as displacement
    if(independent_name == "displacement"){
      sampled_car_model <- lm( mpg ~ displacement , data=car_data[1:300,])
      predicted_mpg <- sampled_car_model$coefficients[1] + (sampled_car_model$coefficients[2]) * car_data[301:392,]$displacement
      predicted_mpg_300 <- sampled_car_model$coefficients[1] + (sampled_car_model$coefficients[2]) * car_data[1:300,]$displacement
    }
    
    #building a regression with independent variable as horsepower
    if(independent_name == "horsepower"){
      sampled_car_model <- lm( mpg ~ horsepower , data=car_data[1:300,])
      predicted_mpg <- sampled_car_model$coefficients[1] + (sampled_car_model$coefficients[2]) * car_data[301:392,]$horsepower
      predicted_mpg_300 <- sampled_car_model$coefficients[1] + (sampled_car_model$coefficients[2]) * car_data[1:300,]$horsepower
    }
    
    #predicted_mpg contains predicted mpg values of all 92 rows of data that is test data
    #predicted_mpg_300 contains predicted mpg values of 300 rows of data that is training data
    
    #predicted_test contains mean and variance of test data and actual data
    predicted_test <- rbind(predicted_test,cbind(mean(actual_mpg),mean(predicted_mpg),var(actual_mpg),var(predicted_mpg)))
    
    #predicted_train contains mean and variance of train data and actual data
    predicted_train <- rbind(predicted_train,cbind(mean(car_data[1:300,]$mpg),mean(predicted_mpg_300),var(car_data[1:300,]$mpg),var(predicted_mpg_300)))
    
    #predicted_meta contains mean and variance of all the samples taken
    predicted_meta <- rbind(predicted_meta,cbind(sampled_car_model$coefficients[1],sampled_car_model$coefficients[2]))
    
    #plotting predicted vs actual for the first sample
    if (i == 1) {
      plot(predicted_mpg,actual_mpg)
      abline(lm(predicted_mpg~actual_mpg))
      abline(1,1,col="red")
    }
  }
  
   #giving column names for the dataframes
   colnames(predicted_test)<-c("mean_actual_mpg_test_92","mean_predicted_mpg_test_92","var_actual_mpg_test_92","var_predicted_mpg_test_92")
   colnames(predicted_train)<-c("mean_actual_mpg_train_300","mean_predicted_mpg_train_300","var_actual_mpg_train_300","var_predicted_mpg_train_300")
   colnames(predicted_meta)<-c("intercept","coefficients")
   
   # combining all the dataframes into a list
   predicted_list <- c(predicted_test,predicted_train,predicted_meta)
  
   return (predicted_list)
}


#read car data from given path
car_data_copy<-read.csv('C:/Users/prana/Desktop/R Project/auto-mpg.csv')

pairs(car_data_copy)

#remove cylinders since it is discrete
car_data_copy <- car_data_copy[,-2]
#remove year origin name since it is not a quantitaitve variable
car_data_copy <- car_data_copy[,-(6:8)]

pairs(car_data_copy[,1:5])

#summary of weight and mpg
summary(lm( mpg ~ weight , data=car_data_copy[1:300,]))
#summary of horsepower and mpg
summary(lm( mpg ~ horsepower , data=car_data_copy[1:300,]))
#summary of acceleration and mpg
summary(lm( mpg ~ acceleration , data=car_data_copy[1:300,]))
#summary of displacement and mpg
summary(lm( mpg ~ displacement , data=car_data_copy[1:300,]))

#summary of all the variables
summary(lm( mpg ~ ., data=car_data_copy[1:300,]))

# not using horsepower, acceleration and displacement since they are significant enough...
  #... and when regression model is built based out of the above variables residuals were not
  #distributed normally and predicted vs actual was performing poorly

# predictedlist_horsepower = predict_significant_values(car_data_copy,car_data_copy$horsepower,car_data_copy$mpg,"horsepower","mpg")
# predict_acceleration = predict_significant_values(car_data_copy,car_data_copy$horsepower,car_data_copy$mpg,"acceleration","mpg")
# predict_displacement = predict_significant_values(car_data_copy,car_data_copy$displacement,car_data_copy$mpg,"displacement","mpg")

# predicted_weight contains list of test, train and list of coefficients
predictedlist_weight <- predict_significant_values(car_data_copy,car_data_copy$weight,car_data_copy$mpg,"weight","mpg")

mean(predictedlist_weight$mean_actual_mpg_test_92)
mean(predictedlist_weight$mean_predicted_mpg_test_92)
mean(predictedlist_weight$mean_predicted_mpg_train_300)
mean(predictedlist_weight$mean_actual_mpg_train_300)

#average of all the beta1 or slopes of randomly sampled data
mc <- mean(predictedlist_weight$coefficients)

#average of all the beta0 or intercepts of randomly sampled data
mi <- mean(predictedlist_weight$intercept)

#building best model out of average of all sampled slopes and intercepts
best_model_mpg <- mi + mc * car_data_copy$weight

#residuals for best model
best_model_mpg_res <- car_data_copy$mpg - best_model_mpg 
mean(best_model_mpg)
mean(car_data_copy$mpg)
sm <- sd(best_model_mpg_res)
mm <- mean(best_model_mpg_res)

#1  residuals vs. the predictor variable
plot(car_data_copy$weight,best_model_mpg_res,ylab = "residuals",xlab = "weight")
abline(a=0,b=0,col="red")
abline(a=2*sm,b=0,col="blue")
abline(a=-2*sm,b=0,col="blue")

#2 absolute value of the residuals vs. the predictor variable
plot(car_data_copy$weight,abs(best_model_mpg_res),ylab = "Absolute values of residuals",xlab = "weight")
abline(a=2*sm,b=0,col="blue")

#3 histogram of the residuals
h<-hist(best_model_mpg_res,breaks=20,prob=T,xlab="residuals of mpg")
x <- seq(-10,10,length = 300)
y <- dnorm(x,mm,sm)
lines(x,y,col="red")

qqnorm(best_model_mpg_res)

#how our predictions compare to the car's actual reported mgp.
plot(best_model_mpg,car_data_copy$mpg)
abline(lm(best_model_mpg~car_data_copy$mpg))
abline(1,1,col="red")

#predicting mpg values based on a given weight
#we can calculate mpg by plugging in weight with the actual value you want to use to predict mpg
#in the below code
#mpg <- mi + mc * weight

##### end of building single variable regression variable#########



########## start of building multi variable regression model ############

#generating randomly distributed uniform numbers
random2<-runif(392)

#adding random2 to auto_mpg
auto_mpg<-cbind(car_data_copy,random2)

View(auto_mpg)

#sorting auto_mpg with respect to random2
auto_mpg<-auto_mpg[order(random2),]

#removing random2 from auto_mpg
auto_mpg<-auto_mpg[,-6]

#getting first 300 rows of data for training
auto_mpg_300<- auto_mpg[1:300,]

pairs(auto_mpg_300)

#creating general model with respect to all the conitnuous variables
general_model_300<- lm(mpg~.,data=auto_mpg_300)

summary(general_model_300)

#Coefficients:

#                          Estimate  Std. Error  t value   Pr(>|t|)    

#    (Intercept)  44.694362   2.891052  15.460  < 2e-16 ***

#  displacement -0.006343   0.007977  -0.795   0.4272    

#   horsepower   -0.050572   0.020083  -2.518   0.0123 *  

#       weight       -0.004821   0.000895  -5.386 1.47e-07 ***

#    acceleration -0.028377   0.141295  -0.201   0.8410

# Comparing two optional models:

a<- lm(mpg~weight + horsepower,data=auto_mpg_300)

#summary(a)

#Coefficients:

#                         Estimate   Std. Error   t value    Pr(>|t|)    

#    (Intercept) 45.0397137  0.8688146  51.840  < 2e-16 ***

#     weight      -0.0053518  0.0005547  -9.648  < 2e-16 ***

#  horsepower  -0.0548267  0.0126786  -4.324 2.09e-05 ***

a_res<- a$residuals

mean(a_res)

#7.632783e-17

hist(a_res,breaks=30,prob=T)

qqnorm(a_res)

b<- lm(mpg~weight+displacement,data=auto_mpg_300)

summary(b)

#Coefficients:

#              Estimate Std. Error t value Pr(>|t|)    

#(Intercept)  42.5506168  1.3146886  32.366  < 2e-16 ***

#weight       -0.0051018  0.0008121  -6.282 1.19e-09 ***

#displacement -0.0201736  0.0066061  -3.054  0.00246 ** 

c<- lm(mpg~weight+displacement+horsepower,data=auto_mpg_300)

#Summary(c)

#Coefficients:

#               Estimate Std. Error t value Pr(>|t|)    

#(Intercept)  44.1864427  1.3984401  31.597  < 2e-16 ***

#weight       -0.0048997  0.0008031  -6.101  3.3e-09 ***

#displacement -0.0061664  0.0079159  -0.779  0.43661    

#horsepower   -0.0479950  0.0154231  -3.112  0.00204 ** 

d<- lm(mpg~horsepower + displacement,data=auto_mpg_300)
d_res <- d$residuals
summary(d)

#Coefficients:

#                         Estimate Std. Error t value Pr(>|t|)    

#    (Intercept)  37.177263   0.844499  44.023  < 2e-16 ***

#  horsepower   -0.055606   0.016283  -3.415 0.000727 ***

#  displacement -0.041070   0.005795  -7.087 9.98e-12 ***

mean(d_res)

#-1.920744e-16

hist(d_res,breaks=30, prob=T)

qqnorm(d_res)

# We will proceed with model a based on summary values, 

plot(auto_mpg_300$weight,a_res,abline(a=0,b=0,col="red"))

abline(a=2*sd(a_res),b=0,col="blue")

abline(a=-2*sd(a_res),b=0,col="blue")

plot(auto_mpg_300$horsepower,a_res,abline(a=0,b=0,col="red"))

abline(a=2*sd(a_res),b=0,col="blue")

abline(a=-2*sd(a_res),b=0,col="blue")

a_res_abs<-abs(a_res)
auto_mpg_92 <- auto_mpg[301:392,]

plot(auto_mpg_300$weight,a_res_abs,abline(a=2*sd(a_res),b=0,col="blue"))

plot(auto_mpg_300$horsepower,a_res_abs,abline(a=2*sd(a_res),b=0,col="blue"))

mpg_estimates_92_2<- a$coefficients[1] + auto_mpg_92$weight*a$coefficients[2] + auto_mpg_92$horsepower*a$coefficients[3]

mean(mpg_estimates_92_2)

#23.0301

mean(auto_mpg_92$mpg)

#23.32609

var(mpg_estimates_92_2)

#39.39977

var(auto_mpg_92$mpg)

#65.40613
random2<-runif(392)

auto_mpg<-cbind(car_data_copy,random2)

View(auto_mpg)

auto_mpg<-auto_mpg[order(random2),]

auto_mpg<-auto_mpg[,-6]

mpg_estimates_300_2<- a$coefficients[1] + auto_mpg_300$weight*a$coefficients[2] + auto_mpg_300$horsepower*a$coefficients[3]

mpg_estimates_92_2<- a$coefficients[1] + auto_mpg_92$weight*a$coefficients[2] + auto_mpg_92$horsepower*a$coefficients[3]

means_2<-c(mean(auto_mpg_300$mpg),mean(mpg_estimates_300_2),mean(auto_mpg_92$mpg),mean(mpg_estimates_92_2))

variances_2<- c(var(auto_mpg_300$mpg),var(mpg_estimates_300_2),var(auto_mpg_92$mpg),var(mpg_estimates_92_2))

comparison_2<- matrix(c(means_2, variances_2),nrow = 4,ncol = 2)

rownames(comparison_2) <- c("300 actual","300 estimate","92 actual", "92 estimate")

colnames(comparison_2) <- c("mean","var")

comparison_2

#to predict the values of mpg from weight and horsepower as independent variables
#replace horsepower and weight with actual values in the below code

#mpg<- a$coefficients[1] + weight*a$coefficients[2] + $horsepower*a$coefficients[3]
