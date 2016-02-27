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


res <- exactboxes(Apositivetraining, Anegativetraining, Apositivetesting, Anegativetesting, csize, maxk,cexpand,timeperproblem)

print(res)