Bstn<-read.csv("C:/Users/AKHIL/Desktop/New folder/Boston.csv")
View(Bstn)

set.seed(256)
Bstn_samp<-sample(2,nrow(Bstn),replace=TRUE,prob=c(.9,.1))
Bstn_samp
Bstn_Train<-Bstn[Bstn_samp==1,]
Bstn_Test<-Bstn[Bstn_samp==2,]
Bstn_Train;Bstn_Test
                 
mdl_1<-lm(medv~.,data=Bstn_Train)
pr_1<-predict(mdl_1,Bstn_Test)                   ####adj R^2=0.7245
summary(mdl_1)                                       ## RMSE=4.14

library(dplyr)
pre1<-data.frame(pr_1,Bstn_Test$medv)
pre10<-mutate(pre1,error1=pr_1-Bstn_Test$medv,error_1sq=error1^2)
View(pre10)
mean=sum(pre10$error_1sq/nrow(pre10))
mean
RMSE=sqrt(mean(pre10$error_1sq))
RMSE






mdl_2<-lm(medv~lstat+ptratio+rad+dis+rm+nox+zn,data=Bstn_Train)
pr_2<-predict(mdl_2,Bstn_Test)                     ##adj R^2=0.7005
summary(mdl_2)


library(dplyr)                                    ##RMSE=4.60
pre3<-data.frame(pr_2,Bstn_Test$medv)
pre11<-mutate(pre2,error1=pr_2-Bstn_Test$medv,error_1sq=error1^2)
mean1=sum(pre11$error_1sq/nrow(pre11))
mean1                                           
RMSE1=sqrt(mean(pre11$error_1sq))
RMSE1










mdl_3<-lm(medv~lstat+ptratio+rad+dis+rm+nox+zn,data=Bstn_Train)
pr_3<-predict(mdl_3,Bstn_Test)                     ##adj R^2=0.700
summary(mdl_3)


library(dplyr)                                    ##RMSE=4.60
pre3<-data.frame(pr_3,Bstn_Test$medv)
pre11<-mutate(pre3,error1=pr_3-Bstn_Test$medv,error_1sq=error1^2)
mean2=sum(pre11$error_1sq/nrow(pre11))
mean2                                           
RMSE2=sqrt(mean(pre11$error_1sq))
RMSE2









mdl_4<-lm(medv~lstat+black,data=Bstn_Train)
pr_4<-predict(mdl_4,Bstn_Test)                     ##adj R^2=0.5393
summary(mdl_4)


library(dplyr)                                    ##RMSE=5.88
pre4<-data.frame(pr_4,Bstn_Test$medv)
pre12<-mutate(pre4,error1=pr_4-Bstn_Test$medv,error_1sq=error1^2)
mean3=sum(pre12$error_1sq/nrow(pre12))
mean3                                           
RMSE3=sqrt(mean(pre12$error_1sq))
RMSE3







mdl_5<-lm(medv~rm+nox+crim,data=Bstn_Train)
pr_5<-predict(mdl_5,Bstn_Test)                     ##adj R^2=0.5423
summary(mdl_5)                                                      

library(dplyr)                                    ##RMSE=5.01
pre5<-data.frame(pr_5,Bstn_Test$medv)
pre13<-mutate(pre5,error1=pr_5-Bstn_Test$medv,error_1sq=error1^2)
mean4=sum(pre13$error_1sq/nrow(pre13))
mean4                                           
RMSE4=sqrt(mean(pre13$error_1sq))
RMSE4


