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
preds <- model.1 %>% predict(x_test, batch_size = 500)
preds.out <- as.data.frame(cbind(preds.1[[1]], preds.1[[2]], preds.1[[3]], preds.1[[4]], preds.1[[5]], y_test, y_test_2, y_test_3, y_test_4, y_test_5))
