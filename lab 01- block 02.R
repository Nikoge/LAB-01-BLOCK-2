if (!require("pacman")) install.packages("pacman")
pacman::p_load(mboost, randomForest, dplyr, ggplot2)
options(scipen = 999)

sp <- read.csv2(file = "spambase.csv" ,header = FALSE)
sp$Spam <- as.factor(sp$Spam)
sp$Spam <- factor(spam_data$Spam, levels = c(0,1), labels = c("0", "1"))

set.seed(12345)
n =  NROW(sp)
id = sample(1:n, floor(n*(2/3)))
train = sp[id,]
test = sp[-id,]
## Trainning the Model

### Adaboost with varying depth
 
final_result <- NULL
for(i in seq(from = 10, to = 100, by = 10)){
  ada_model <- mboost::blackboost(Spam~., 
                                  data = train, 
                                  family = AdaExp(), 
                                  control=boost_control(mstop=i))
  forest_model <- randomForest(Spam~., data = train, ntree = i)
  prediction_function <- function(model, data){
    predicted <- predict(model, newdata = data, type = c("class"))
    predict_correct <- ifelse(data$Spam == predicted, 1, 0) 
    score <- sum(predict_correct)/NROW(data)
    return(score)
  }
  train_ada_model_predict <- predict(ada_model, newdata = train, type = c("class"))
  test_ada_model_predict <- predict(ada_model, newdata = test, type = c("class"))
  train_forest_model_predict <- predict(forest_model, newdata = train, type = c("class"))
  test_forest_model_predict <- predict(forest_model, newdata = test, type = c("class"))
  test_predict_correct <- ifelse(test$Spam == test_forest_model_predict, 1, 0) 
  train_predict_correct <- ifelse(train$Spam == train_forest_model_predict, 1, 0) 
  train_ada_score <-  prediction_function(ada_model, train)
  test_ada_score <-  prediction_function(ada_model, test)
  train_forest_score <-  prediction_function(forest_model, train)
  test_forest_score <-  prediction_function(forest_model, test)
  iteration_result <- data.frame(number_of_trees = i, 
                                 accuracy = c(train_ada_score, 
                                              test_ada_score, 
                                              train_forest_score, 
                                              test_forest_score), 
                                 type  = c("train", "test", "train", "test"),
                                 model = c("ADA", "ADA",  "Forest", "Forest"))
  final_result <- rbind(iteration_result, final_result)
}
final_result$error_rate_percentage <- 100*(1 - final_result$accuracy)
ggplot(data = final_result, aes(x = number_of_trees, 
                                y = error_rate_percentage, 
                                group = type, color = type)) + 
  geom_point() + 
  geom_line() + 
  ggtitle("Error Rate vs. increase in trees") + facet_grid(rows = vars(model))


 



















