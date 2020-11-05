forestfires = read.csv(file.choose())
colnames(forestfires)
forestfires_1 =forestfires[c("FFMC","DMC","DC","ISI","temp","RH","wind","rain","area","size_category")] 
summary(forestfires_1)
colnames(forestfires_1)


normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
data_norm<-as.data.frame(lapply(forestfires_1[,-10],FUN=normalize))

forestfires_1_train<-forestfires_1[1:400,]
forestfires_1_test<-forestfires_1[401:517,]


# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 

??ksvm
# ksvm() function uses gaussian RBF kernel 
?ksvm
# Building model 
colnames(data_norm)
library(kernlab)
library(caret)
model1<-ksvm(size_category ~.,data = forestfires_1,kernel = "vanilladot")
model1

help(ksvm)
??ksvm

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rbfdot 
model_rbfdot<-ksvm(size_category ~.,data = forestfires_1_train,kernel = "rbfdot")
pred_rbfdot<-predict(model_rbfdot,newdata=forestfires_1_test)
mean(pred_rbfdot==forestfires_1_test$size_category) #64.95

# kernel = vanilladot````````````````
model_vanilla<-ksvm(size_category ~.,data = forestfires_1_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=forestfires_1_test)
mean(pred_vanilla==forestfires_1_test$size_category) #87.17
  

# kernal = besseldot
model_besseldot<-ksvm(size_category ~.,data = forestfires_1_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=forestfires_1_test)
mean(pred_bessel==forestfires_1_test$size_category) #73.50

# kernel = polydot

model_poly<-ksvm(size_category ~.,data = forestfires_1_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata =forestfires_1_test )
mean(pred_poly==forestfires_1_test$size_category) #87.17











