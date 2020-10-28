#Neural Network Example
library(dplyr)

#read in summary statistics from epic cohort
all = read.table("./stats_sims.txt")
all_train <- all[all$set=="train",]
all_test <- all[all$set=="test",]
by_mod <-all_train %>% group_by(model)
all1 <- sample_n(by_mod, 61173)
all2 <- sample_n(by_mod, 61173)
all3 <- sample_n(by_mod, 61173)
all4 <- sample_n(by_mod, 61173)
all5 <- sample_n(by_mod, 61173)
all6 <- sample_n(by_mod, 61173)
all7 <- sample_n(by_mod, 61173)
all8 <- sample_n(by_mod, 61173)
all9 <- sample_n(by_mod, 61173)
all10 <- sample_n(by_mod, 61173)

rand <- sample(nrow(all1))
all1 <- all1[rand, ]
all1 = as.data.frame(all1)

x_train <- all1[(all1[,22]=="train"), c(6:21)]
y_train <- all1[(all1[,22]=="train"), c(5)]
y_train_2 <- all1[(all1[,22]=="train"), c(1)]
y_train_3 <- all1[(all1[,22]=="train"), c(2)]
y_train_4 <- all1[(all1[,22]=="train"), c(3)]
y_train_5 <- all1[(all1[,22]=="train"), c(4)]

x_test <- all_test[(all_test[,22]=="test"), c(6:21)]
y_test <- all_test[(all_test[,22]=="test"), c(5)]
y_test_2 <- all_test[(all_test[,22]=="test"), c(1)]
y_test_3 <- all_test[(all_test[,22]=="test"), c(2)]
y_test_4 <- all_test[(all_test[,22]=="test"), c(3)]
y_test_5 <- all_test[(all_test[,22]=="test"), c(4)]

#set all these to matrices 
x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)
y_train_2 <- as.matrix(y_train_2)
y_train_3 <- as.matrix(y_train_3)
y_train_4 <- as.matrix(y_train_4)
y_train_5 <- as.matrix(y_train_5)

x_test <- as.matrix(x_test)
y_test <- as.matrix(y_test)
y_test_2 <- as.matrix(y_test_2)
y_test_3 <- as.matrix(y_test_3)
y_test_4 <- as.matrix(y_test_4)
y_test_5 <- as.matrix(y_test_5)

y_train_2 <- log(y_train_2)
y_test_2 <- log(y_test_2)
y_train_3 <- y_train_3+1
y_train_4 <- y_train_4+1
y_test_3 <- y_test_3+1
y_test_4 <- y_test_4+1
y_train_3 <- log(y_train_3)
y_train_4 <- log(y_train_4)
y_test_3 <- log(y_test_3)
y_test_4 <- log(y_test_4)

#Example Multi-Task Neural Net Architechture (this should be replicated for each neural net in the ensemble)
library(keras)
input <- layer_input(shape = c(16))
output <- input %>%
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dropout(rate = 0.1) %>%
  layer_dense(units = 512, activation = 'relu') %>%
  layer_dropout(rate = 0.1)
pred1 <- output %>%
  layer_dense(units = 4, activation = 'softmax', name = "output1") #class 
pred2 <- output %>%
  layer_dense(units = 1, name = "output2") #mutation rate 
pred3 <- output %>%
  layer_dense(units = 1, name = "output3") #probability of mutation being beneficial 
pred4 <- output %>%
  layer_dense(units = 1, name = "output4") #positive selection
pred5 <- output %>% 
  layer_dense(units = 1,name = "output5") #negative selection

model <- keras_model(input,list(pred1, pred2, pred3, pred4, pred5)) 
model %>% compile(optimizer = "adam",loss = list(output1 = "sparse_categorical_crossentropy",output2 = "mse", output3 = "mse", output4 = "mse", output5 = "mse"),metrics = list(output1 = "accuracy", output2 = "mse", output3 = "mse", output4 = "mse", output5 = "mse"))
model %>% fit(x_train, list(output1 = y_train, output2 = y_train_2, output3 = y_train_3, output4 = y_train_4, output5 = y_train_5),epochs = 20, batch_size = 500)#, class_weight =list("0"=5.96, "1"=5.07, "2"=17.70, "3"=1))
preds <- model %>% predict(x_test, batch_size = 500) #switch to epic summary stats for predictions on real data 
preds.out <- as.data.frame(cbind(preds.1[[1]], preds.1[[2]], preds.1[[3]], preds.1[[4]], preds.1[[5]], y_test, y_test_2, y_test_3, y_test_4, y_test_5))
