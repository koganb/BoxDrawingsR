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

pos_data_fraction <- 0.3
neg_data_fraction <- 0.1


Apositivetraining <- Apositivetraining[Apositivetraining[,3] < pos_data_fraction, -3]
Apositivetesting <- Apositivetesting[Apositivetesting[,3] < pos_data_fraction, -3]
Anegativetraining <- Anegativetraining[Anegativetraining[,3] < neg_data_fraction, -3]
Anegativetesting <- Anegativetesting[Anegativetesting[,3] < neg_data_fraction, -3]



allTrainData <- rbind(cbind(Apositivetraining, rep(1,nrow(Apositivetraining))),cbind(Anegativetraining, rep(-1,nrow(Anegativetraining))))
allTestData <- rbind(cbind(Apositivetesting, rep(1,nrow(Apositivetesting))),cbind(Anegativetesting, rep(-1,nrow(Anegativetesting))))
write.csv(allTrainData, file = "2d_train_03_01.csv", col.names=F)
write.csv(allTestData, file = "2d_test_03_01.csv", col.names=F)
