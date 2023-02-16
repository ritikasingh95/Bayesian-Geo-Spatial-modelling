#############################
# Find correlation between coverage with and without maternal recall in nfhs4 using various robust models
# Cross validation of results
#############################

  
  #data plot
  With_maternal_recall <- temp_sums$V1
  Without_maternal_recall <- temp_sums_2$V1
  mydata<-data.frame(With_maternal_recall,Without_maternal_recall)
  
  #R2 LM
  a1=summary(linear_model <- lm(Without_maternal_recall ~ With_maternal_recall,mydata))$r.squared
  
  #R2 RLM Calculations
  summary(robust_model_huber <- rlm(Without_maternal_recall~With_maternal_recall,mydata))
  summary(robust_model_hampel <- rlm(Without_maternal_recall~With_maternal_recall, psi= psi.hampel,mydata))
  summary(robust_model_bisquare <- rlm(Without_maternal_recall~With_maternal_recall, psi= psi.bisquare,mydata))
  r2 <- function(x){  
    SSe <- sum((x$residuals)^2);  
    observed <- x$residuals+ x$fitted.values;  
    SSt <- sum((observed-mean(observed))^2);  
    value <- 1-SSe/SSt;  
    return(value);  } 
  a2=r2(robust_model_huber)
  a3=r2(robust_model_hampel)
  a4=r2(robust_model_bisquare)
  
  #final plot
  plot(With_maternal_recall, Without_maternal_recall, type = "p") #plot
  abline(coef(linear_model), col="red")
  abline(coef(robust_model_huber), col="green")
  abline(coef(robust_model_hampel), col="blue")
  abline(coef(robust_model_bisquare), col="brown")
  
  #cooks_distance
  cooksd<-cooks.distance(linear_model)
  influential1 <- as.numeric(names(cooksd)[(cooksd > 1)])
  influential2 <- as.numeric(names(cooksd)[(cooksd > 4/nrow(With_maternal_recall))])
  
  if(length(influential1)==0){
    a5=a1
    abline(coef(linear_model), col="orange")
    } else {
    mydatanew<- mydata[-influential1,]
    a5=summary(new_linear_model <- lm(Without_maternal_recall ~ With_maternal_recall,mydatanew))$r.squared 
    abline(coef(new_linear_model), col="orange")
    }
  
  if(length(influential2)==0){
    a6=a1
    abline(coef(linear_model), col="pink")
  } else {
    mydatanew<- mydata[-influential2,]
    a6=summary(new_linear_model <- lm(Without_maternal_recall ~ With_maternal_recall,mydatanew))$r.squared 
    abline(coef(new_linear_model), col="pink")
  }
  
  #cross validation
  
  set.seed(123)
  train.control <- trainControl(method = "repeatedcv", 
                                number = 5, repeats = 3)
  a11 <- (train(With_maternal_recall ~., data = mydata, method = "lm",
                trControl = train.control))$results$Rsquared
