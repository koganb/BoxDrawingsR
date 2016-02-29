debugSource("BoxDrawingsExactBoxes.R")
library(R.matlab)



maxk = 1
cexpand=0.01
timeperproblem=4500


testData <- read.csv("2d_test_03_01.csv", header = T)[-c(1)]
trainData <- read.csv("2d_train_03_01.csv", header = T)[-c(1)]



#plot(testData[,1],testData[,2],col=ifelse(testData[,3]==1,"red","black"))

model <- exectBoxes(V3 ~ ., testData, 1, 0.01)

prediction <- predict(model, trainData[,c(1,2)])


str(res)





# res <- exactboxes(Apositivetraining, Anegativetraining, Apositivetesting, Anegativetesting, 
#                   maxk,cexpand,timeperproblem)
# 
# print(res)