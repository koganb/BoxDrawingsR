source("exactboxes.R")
library(R.matlab)



csize = 2
maxk = 1
cexpand=0.01
timeperproblem=4500


dataset <- readMat("diamond3Dsample.mat")
Apositivetraining<-dataset[["Apositivetraining"]]
Apositivetesting<-dataset[["Apositivetesting"]]
Anegativetraining<-dataset[["Anegativetraining"]]
Anegativetesting<-dataset[["Anegativetesting"]]




set.seed(1234)

Apositivetraining <- cbind(Apositivetraining, runif(nrow(Apositivetraining)))
Apositivetesting <- cbind(Apositivetesting, runif(nrow(Apositivetesting)))
Anegativetraining <- cbind(Anegativetraining, runif(nrow(Anegativetraining)))
Anegativetesting <- cbind(Anegativetesting, runif(nrow(Anegativetesting)))



Apositivetraining <- Apositivetraining[Apositivetraining[,4] <= 0.2, -4]  
Apositivetesting <- Apositivetesting[Apositivetesting[,4] <= 0.2, -4]  
Anegativetraining <- Anegativetraining[Anegativetraining[,4] <= 0.2, -4]  
Anegativetesting <- Anegativetesting[Anegativetesting[,4] <= 0.2, -4]  


res <- exactboxes(Apositivetraining, Anegativetraining, Apositivetesting, Anegativetesting, 
                  csize, maxk,cexpand,timeperproblem)

print(res)