saraly= read.csv(file.choose())
SalaryData_Train = read.csv(file.choose())
SalaryData_Test = read.csv(file.choose())
colnames(SalaryData_Train)


Salary_train<-SalaryData_Train[1:30161,]
salary_test<-SalaryData_Test[1:15060,]

# to train model
# e1071 package from LIBSVM library
# SVMlight algorithm klar package 


# kvsm() function uses gaussian RBF kernel 

# Building model 

library(kernlab)
library(caret)
model1<-ksvm(Salary ~.,data = Salary_train,kernel = "vanilladot")
model1

help(kvsm)
??kvsm

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(Salary ~.,data = Salary_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=salary_test)
mean(pred_rfdot==salary_test$Salary) # 85.41

# kernel = vanilladot
model_vanilla<-ksvm(Salary ~.,data = Salary_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=salary_test)
mean(pred_vanilla==salary_test$Salary) #


# kernal = besseldot
model_besseldot<-ksvm(Salary ~.,data = Salary_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=salary_test)
mean(pred_bessel==salary_test$Salary)

# kernel = polydot

model_poly<-ksvm(Salary ~.,data = Salary_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = salary_test)
mean(pred_poly==Salary_train$Salary) # ``




