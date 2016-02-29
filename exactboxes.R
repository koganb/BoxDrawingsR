#library(Rglpk)
library(pracma)
library(Rsymphony)
library(gurobi)
library(Matrix)


exactboxes  <- function ( Apositivetraining, Anegativetraining, Apositivetesting, Anegativetesting, maxk,cexpand,timeperproblem) {
  #this is the program to implement exact boxes 
  #algorithm.
  #the algorithm take in Apositivetraining, the positive training data,
  #Anegativetraining, the negative training data
  #Apositivetesting, the positive test data
  #Anegativetesting, the negative test data
  #each row is a data point.
  
  #maxk is the cluster size
  #cexpand is the parameter to control the number of boxes.
  #timeperproblem is the time that you are willing to invest for a single MIP
  #problem
  
  
  #this code requires Rglpk
  
  
  
  #also, the training and test True positive and False positive will be
  #reported.
  
  v=0.05; #this is the margin size
  M=10000; #this is a big number
  epsilon=0.00001;  #this is a small number
  
  
  # all training data
  A=rbind(Apositivetraining,Anegativetraining);
  # size of positive train
  mpositive=nrow(Apositivetraining);
  # size of negative train
  mnegative=nrow(Anegativetraining);
  # n = number of features/dimensions
  mnegativetesting=nrow(Anegativetesting);
  n=ncol(Anegativetesting);
  # m = number of train samples
  m=mpositive+mnegative;
  #tempA=ones(m,1);
  tempA=matrix(c(rep(1,mpositive),rep(-1,mnegative)),ncol=1);
  for (k in seq(2,maxk*n, length=max(0, maxk*n - 2 + 1))) {
      tempA=blkdiag(tempA,matrix(c(rep(1,mpositive),rep(-1,mnegative)),ncol=1)) ;
  }
  
  # todo: work with sparse matrix
  
  lconstraintA=cbind(
     tempA,
     matrix(rep(0,m*n*maxk*n*maxk),ncol=n*maxk),
     M*diag(m*n*maxk),
     matrix(rep(0,m*n*maxk*(m*n*maxk+m*maxk+m)),ncol=m*n*maxk+m*maxk+m))
  
  lconstraintA=rbind(-lconstraintA, lconstraintA);
  
  uconstraintA=cbind(
    matrix(rep(0,m*n*maxk*n*maxk),ncol=n*maxk), 
    tempA,
    matrix(rep(0,m*n*maxk*m*n*maxk),ncol=m*n*maxk),
    -M*diag(m*n*maxk),
    matrix(rep(0,m*n*maxk*(m*maxk+m)),ncol=m*maxk+m))
  
  rm(tempA)
  uconstraintA=rbind(uconstraintA, -uconstraintA);
  
  T=repmat(diag(m),1,n)
  D=T
  
  for (k in seq(2,maxk,length=max(0, maxk - 2 + 1))) {
      D =blkdiag(D,T)
  }
  rm(T)
  W1=blkdiag(-diag(mpositive),2*n*diag(mnegative));
  if (maxk>1) {
   for (k in seq (2,maxk,length=max(0, maxk - 2 + 1))) {
      W1 = blkdiag(W1,-diag(mpositive),2*n*diag(mnegative))
   }
  }
  W2=blkdiag(2*n*diag(mpositive),-diag(mnegative));
  if (maxk>1) {
   for (k in seq (2,maxk,length=max(0,maxk - 2 + 1))) {
      W2 = blkdiag(W2,2*n*diag(mpositive),-diag(mnegative))
   }
  }
  
  yconstraintA = cbind(matrix(0,nrow=2*m*maxk,ncol=2*n*maxk), 
                     rbind(cbind(D,D), cbind(-D,-D)),
                     rbind(W1,W2),
                     matrix(0,nrow=2*m*maxk,ncol=m)
                     )
  
  
  rm(W1)
  rm(W2)
  rm(D)
  
  positivezconstraintA <- cbind(matrix(0,nrow=2*mpositive, ncol=(2+2*m)*n*maxk),
                                rbind(repmat(cbind(diag(mpositive), matrix(0, nrow=mpositive, ncol=mnegative)),1, maxk),
                                      -repmat(cbind(diag(mpositive),matrix(0, nrow=mpositive, ncol=mnegative)),1, maxk)),
                                cbind(rbind(-maxk*diag(mpositive), diag(mpositive)), matrix(0, nrow=2*mpositive, ncol = mnegative)))
  
  
  
  negativezconstraintA <- cbind(matrix(0, nrow = 2*mnegative, ncol = (2+2*m)*n*maxk),
                                rbind(repmat(cbind(matrix(0, nrow = mnegative, ncol=mpositive), diag(mnegative)), 1, maxk), 
                                      repmat(cbind(matrix(0, nrow = mnegative, ncol=mpositive), -diag(mnegative)),1, maxk)),
                                matrix(0, nrow = 2*mnegative, ncol = mpositive),
                                rbind(maxk*diag(mnegative), -diag(mnegative)))
  
  model<-list()
  
  #The linear constraint matrix
  model$A <- rbind(lconstraintA, uconstraintA, yconstraintA,positivezconstraintA, negativezconstraintA, 
                  cbind(diag(n*maxk), -diag(n*maxk), matrix(0, nrow=n*maxk, ncol=2*m*n*maxk+m*maxk+m,0)))
  
  
  rm (lconstraintA)
  rm (uconstraintA)
  rm (yconstraintA)
  rm (positivezconstraintA)
  rm (negativezconstraintA)
  
  
  B=rbind(Apositivetraining, -Anegativetraining)
  
  B=matrix(as.vector(B), ncol = 1)  #convert matix to single column
  
  
  #The right-hand side vector for the linear constraints (one value for each row of A)
  model$rhs <- rbind(repmat(-B+v,maxk,1), 
               repmat(B+(-v+M-epsilon),maxk,1), 
               repmat(B+v,maxk,1),
               repmat(-B+(-v+M-epsilon),maxk,1),
               repmat(rbind((2*n-1)*matrix(1,mpositive,1), 2*n*matrix(1,mnegative,1)),maxk,1),
               repmat(rbind(matrix(0, mpositive,1), -matrix(1, mnegative,1)),maxk,1),
               matrix(0,2*mpositive,1),
               maxk*matrix(1, mnegative,1),
               -matrix(1,mnegative,1),
               matrix(0,n*maxk,1))
  
  #The senses of the linear constraints
  model$sense <- '<'
  
  #The variable types  'C' (continuous), 'B' (binary)
  model$vtype <- rbind(matrix('C',nrow=2*n*maxk,ncol=1), matrix('B',nrow=2*m*n*maxk+m*maxk+m,ncol=1))
  model$modelsense='max'
  
  #The lower bound vector (one value for each column of A)
  model$lb=rbind((min(min(A))-1)*matrix(1, nrow=2*n*maxk, ncol=1), matrix(0, nrow=2*m*n*maxk+m*maxk+m, ncol=1))
  
  #The upper bound vector (one value for each column of A)
  model$ub=rbind((max(max(A))+1)*matrix(1, nrow=2*n*maxk, ncol=1), matrix(1, nrow=2*m*n*maxk+m*maxk+m, ncol=1))
  
  
  bounds <- list(upper = model[['ub']], lower = model[['lb']]) 
  

  #The linear objective coefficients vector (one value for each column of A.)
  model$obj <- cbind(-cexpand*matrix(1, nrow=1,ncol=n*maxk),
                      cexpand*matrix(1, nrow=1, ncol=n*maxk),
                      matrix(0,nrow=1,ncol=2*m*n*maxk+m*maxk),
                      matrix(1,nrow=1,ncol=mpositive),
                      matrix(1, nrow=1,ncol=mnegative)
                     )
  
  params <- list()
  params$outputflag=0;
  params$timelimit=timeperproblem;  #this is the time limit, after which, we terminate the MIP solving process and move on to the next problem, the currently found solution will be reported.
  params$outputflag=1;
  params$LogToConsole=0;
  params$logfile=paste0('gurobilog.txt')
  
  #result<-gurobi(model,params)
  result <- Rsymphony_solve_LP(obj=model$obj, mat=model$A, dir=rep(c('<'), nrow(model$A)), rhs=model$rhs, bounds=bounds, types=model$vtype, max=T, verbosity=-1)
  
  if (exists("result$x")) {
    result$solution <- result$x
  }
  
  str(result)
  
  
  lowerboundary=result$solution[1:(n*maxk)]
  upperboundary=result$solution[(n*maxk+1):(2*n*maxk)]
  lowerboundary=matrix(lowerboundary, nrow=n, ncol=maxk)
  upperboundary=matrix(upperboundary, nrow=n, ncol=maxk)
  
  
  lowerideal=t(lowerboundary)
  upperideal=t(upperboundary)
  
  
  trainingpositiveclassification=matrix(0, nrow=nrow(Apositivetraining), ncol=1)
  testingpositiveclassification=matrix(0, nrow=nrow(Apositivetesting), ncol=1)
  trainingnegativeclassification=matrix(0, nrow=nrow(Anegativetraining), ncol=1)
  testingnegativeclassification=matrix(0, nrow=nrow(Anegativetesting),ncol=1)
  
  
  for (k in seq(1:maxk)) {
    trainingpositiveclassification <- trainingpositiveclassification | 
      rowSums((Apositivetraining>=repmat(lowerideal[k,],nrow(Apositivetraining),1)) & (Apositivetraining<=repmat(upperideal[k,],nrow(Apositivetraining),1)))  ==  ncol(Apositivetraining)
    testingpositiveclassification <- testingpositiveclassification | 
      rowSums((Apositivetesting>=repmat(lowerideal[k,],nrow(Apositivetesting),1)) & (Apositivetesting<=repmat(upperideal[k,],nrow(Apositivetesting),1))) == ncol(Apositivetesting)
    trainingnegativeclassification <- trainingnegativeclassification | 
      rowSums((Anegativetraining>=repmat(lowerideal[k,],nrow(Anegativetraining),1)) & (Anegativetraining<=repmat(upperideal[k,], nrow(Anegativetraining),1))) == ncol(Anegativetraining)
    testingnegativeclassification<-testingnegativeclassification | 
      rowSums((Anegativetesting>=repmat(lowerideal[k,],nrow(Anegativetesting),1)) & (Anegativetesting<=repmat(upperideal[k,], nrow(Anegativetesting),1))) == ncol(Anegativetesting)
  }
  

  TP <- sum(testingpositiveclassification)
  FP <- sum(testingnegativeclassification)
  TN <- mnegativetesting - FP;
  MIPtestingTP=TP;
  MIPtestingFP=mnegativetesting-TN;
    
  TP=sum(trainingpositiveclassification);
  FP=sum(trainingnegativeclassification);
  TN=mnegative-FP;
  MIPtrainingTP=TP;
  MIPtrainingFP=mnegative-TN;

  result <- list()
  result$MIPtrainingTP <- MIPtrainingTP
  result$MIPtrainingFP <- MIPtrainingFP
  result$MIPtestingTP <- MIPtestingTP
  result$MIPtestingFP <- MIPtestingFP
  
  return(result)
  
}


