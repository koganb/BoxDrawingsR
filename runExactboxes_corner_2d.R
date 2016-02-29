debugSource("exactboxes.R")
library(R.matlab)



maxk = 1
cexpand=0.01
timeperproblem=4500


dataset <- readMat("cornersample.mat")
Apositivetraining<-dataset[["Apositivetraining"]]
Apositivetesting<-dataset[["Apositivetesting"]]
Anegativetraining<-dataset[["Anegativetraining"]]
Anegativetesting<-dataset[["Anegativetesting"]]



set.seed(1234)

Apositivetraining <- cbind(Apositivetraining, runif(nrow(Apositivetraining)))
Apositivetesting <- cbind(Apositivetesting, runif(nrow(Apositivetesting)))
Anegativetraining <- cbind(Anegativetraining, runif(nrow(Anegativetraining)))
Anegativetesting <- cbind(Anegativetesting, runif(nrow(Anegativetesting)))

pos_data_fraction <- 0.5
neg_data_fraction <- 0.2


Apositivetraining <- Apositivetraining[Apositivetraining[,3] < pos_data_fraction, -3]
Apositivetesting <- Apositivetesting[Apositivetesting[,3] < pos_data_fraction, -3]
Anegativetraining <- Anegativetraining[Anegativetraining[,3] < neg_data_fraction, -3]
Anegativetesting <- Anegativetesting[Anegativetesting[,3] < neg_data_fraction, -3]



allTrainPlotData <- rbind(cbind(Apositivetraining, rep(1,nrow(Apositivetraining))),cbind(Anegativetraining, rep(-1,nrow(Anegativetraining))))
allTestPlotData <- rbind(cbind(Apositivetesting, rep(1,nrow(Apositivetesting))),cbind(Anegativetesting, rep(-1,nrow(Anegativetesting))))


plot(allTrainPlotData[,1],allTrainPlotData[,2],col=ifelse(allTrainPlotData[,3]==1,"red","black"))
plot(allTestPlotData[,1],allTestPlotData[,2],col=ifelse(allTestPlotData[,3]==1,"red","black"))


res <- exactboxes(Apositivetraining, Anegativetraining, Apositivetesting, Anegativetesting, 
                  maxk,cexpand,timeperproblem)

print(res)