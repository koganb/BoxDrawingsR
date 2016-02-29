debugSource("BoxDrawingsExectBoxes.R")
library(R.matlab)



maxk = 1
cexpand=0.01
timeperproblem=4500


testData <- as.matrix(read.csv("2d_test_03_01.csv", header = T))[, -1]

plot(testData[,1],testData[,2],col=ifelse(testData[,3]==1,"red","black"))

res <- exectBoxes(testData[, -3], testData[,3], 1, 0.01) 




# res <- exactboxes(Apositivetraining, Anegativetraining, Apositivetesting, Anegativetesting, 
#                   maxk,cexpand,timeperproblem)
# 
# print(res)