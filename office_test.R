#read in testing and training data
testing<-read.csv('pml-testing.csv')
training<-read.csv('pml-training.csv')
#remove index, user_name, cvtd_timestamp, new_window and num_windown
training.new<-cbind(training[,3:4],training[,8:11],training[,37:49],training[,60:68],
                    training[,84:86],training[,101:102],training[,113:124],
                    training[,139:140],training[,151:160])
training.new<-training.new[,-32]
training.new<-training.new[,-45]
#splitting data
#
training.new.A<-training.new[training.new[,55]=='A',]
training.new.B<-training.new[training.new[,55]=='B',]
training.new.C<-training.new[training.new[,55]=='C',]
training.new.D<-training.new[training.new[,55]=='D',]
training.new.E<-training.new[training.new[,55]=='E',]
dim(training.new.A)
dim(training.new.B)
dim(training.new.C)
dim(training.new.D)
dim(training.new.E)
#
training.new.trs.A.index<-sample(5580, size=3344, replace=F)
training.new.trs.B.index<-sample(3797, size=2284, replace=F)
training.new.trs.C.index<-sample(3422, size=2049, replace=F)
training.new.trs.D.index<-sample(3216, size=1931, replace=F)
training.new.trs.E.index<-sample(3607, size=2166, replace=F)

training.new.A.trs<-training.new.A[training.new.trs.A.index,]
training.new.B.trs<-training.new.B[training.new.trs.B.index,]
training.new.C.trs<-training.new.C[training.new.trs.C.index,]
training.new.D.trs<-training.new.D[training.new.trs.D.index,]
training.new.E.trs<-training.new.E[training.new.trs.E.index,]
training.new.trs<-rbind(training.new.A.trs,training.new.B.trs,training.new.C.trs,
                        training.new.D.trs,training.new.E.trs)
training.new.trs<-data.frame(training.new.trs)
#
training.new.A.inter<-training.new.A[-training.new.trs.A.index,]
training.new.B.inter<-training.new.B[-training.new.trs.B.index,]
training.new.C.inter<-training.new.C[-training.new.trs.C.index,]
training.new.D.inter<-training.new.D[-training.new.trs.D.index,]
training.new.E.inter<-training.new.E[-training.new.trs.E.index,]

testing.new.A.index<-sample(2236, size=1114, replace=F)
testing.new.B.index<-sample(1513, size=761, replace=F)
testing.new.C.index<-sample(1373, size=683, replace=F)
testing.new.D.index<-sample(1285, size=644, replace=F)
testing.new.E.index<-sample(1441, size=722, replace=F)

testing.new.A<-training.new.A.inter[testing.new.A.index,]
testing.new.B<-training.new.B.inter[testing.new.B.index,]
testing.new.C<-training.new.C.inter[testing.new.C.index,]
testing.new.D<-training.new.D.inter[testing.new.D.index,]
testing.new.E<-training.new.E.inter[testing.new.E.index,]
testing.new<-rbind(testing.new.A,testing.new.B,testing.new.C,testing.new.D,testing.new.E)
testing.new<-data.frame(testing.new)

validating.new.A<-training.new.A.inter[-testing.new.A.index,]
validating.new.B<-training.new.B.inter[-testing.new.B.index,]
validating.new.C<-training.new.C.inter[-testing.new.C.index,]
validating.new.D<-training.new.D.inter[-testing.new.D.index,]
validating.new.E<-training.new.E.inter[-testing.new.E.index,]
validating.new<-rbind(validating.new.A,validating.new.B,validating.new.C,validating.new.D,validating.new.E)

validating.new<-data.frame(validating.new)
#model training and comparison
library(caret)
set.seed(1234)

## @knitr rm
#model1

library(randomForest)
ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 3)

model<-train(training.new.trs$classe~.,
              method='rf', data=training.new.trs, preProc=c("center","scale"),
              trControl = ctrl)
#prediction
#on test set
pred<-predict(model,testing.new)
Predright_prop<-(sum(pred==testing.new[,55])/nrow(testing.new))
table(pred,testing.new[,55])
Predright_prop

#on validation set
pred.val<-predict(model,validating.new)
Predright_prop2<-(sum(pred.val==validating.new[,55])/nrow(validating.new))
table(pred.val,validating.new[,55])
Predright_prop2

#on 20 obs
testing.20<-cbind(testing[,3:4],testing[,8:11],testing[,37:49],testing[,60:68],
                  testing[,84:86],testing[,101:102],testing[,113:124],
                  testing[,139:140],testing[,151:159])
testing.20<-testing.20[,-32]
testing.20<-testing.20[,-45]
testing.20<-data.frame(testing.20)
pred.20<-predict(model,testing.20)
pred.20
