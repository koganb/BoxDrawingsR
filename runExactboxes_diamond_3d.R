debugSource("exactboxes.R")
library(scatterplot3d)
library(R.matlab)
library(Matrix)



csize = 2
maxk = 2
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

pos_data_fraction <- 0.5
neg_data_fraction <- 0.2


Apositivetraining <- Apositivetraining[Apositivetraining[,4] < pos_data_fraction, -4]
Apositivetesting <- Apositivetesting[Apositivetesting[,4] < pos_data_fraction, -4]
Anegativetraining <- Anegativetraining[Anegativetraining[,4] < neg_data_fraction, -4]
Anegativetesting <- Anegativetesting[Anegativetesting[,4] < neg_data_fraction, -4]



allTrainPlotData <- rbind(cbind(Apositivetraining, rep(1,nrow(Apositivetraining))),cbind(Anegativetraining, rep(-1,nrow(Anegativetraining))))
allTestPlotData <- rbind(cbind(Apositivetesting, rep(1,nrow(Apositivetesting))),cbind(Anegativetesting, rep(-1,nrow(Anegativetesting))))

scatterplot3d(allTrainPlotData[,1],allTrainPlotData[,2],allTrainPlotData[,3],color =ifelse(allTrainPlotData[,4]==1,"red","black"))
scatterplot3d(allTestPlotData[,1],allTestPlotData[,2],allTestPlotData[,3],color =ifelse(allTestPlotData[,4]==1,"red","black"))




res <- exactboxes(Apositivetraining, Anegativetraining, Apositivetesting, Anegativetesting, 
                  maxk,cexpand,timeperproblem)

print(res)