library(pracma)
library(Rsymphony)


exactBoxes <- function (x,  ...) {
    UseMethod("exactBoxes")
}



exactBoxes.default <- function(x, y,  maxk, cexpand, positiveVal = 1, negativeVal = -1,  ...) {
    call <- match.call()
    Yname <- deparse(substitute(y))
    

    if (! all.equal(unique(y)[order(unique(y)),], sort(c(negativeVal, positiveVal)))) {
        stop(paste0("Y variable should be of type: ", c(positiveVal, negativeVal)))
    }
    
    #convert all data to numeric
    A <- data.matrix(x)
    Apositivetraining <- A[y == positiveVal,]
    Anegativetraining <- A[y == negativeVal,]
    
    
    
    
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
    n=ncol(Apositivetraining);
    
    # m = number of train samples
    m=mpositive+mnegative;
    
    tempA=matrix(c(rep(1,mpositive),rep(-1,mnegative)),ncol=1);
    for (k in seq(2,maxk*n, length=max(0, maxk*n - 2 + 1))) {
        tempA=blkdiag(tempA,matrix(c(rep(1,mpositive),rep(-1,mnegative)),ncol=1)) ;
    }
    
    
    # build lower constraint    
    lconstraintA=cbind(
        tempA,
        matrix(rep(0,m*n*maxk*n*maxk),ncol=n*maxk),
        M*diag(m*n*maxk),
        matrix(rep(0,m*n*maxk*(m*n*maxk+m*maxk+m)),ncol=m*n*maxk+m*maxk+m))
    
    lconstraintA=rbind(-lconstraintA, lconstraintA);
    

    # build upper constraint    
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
    
    result <- Rsymphony_solve_LP(obj=model$obj, mat=model$A, dir=rep(c('<'), nrow(model$A)), rhs=model$rhs, bounds=bounds, types=model$vtype, max=T, verbosity=-1)
    
    lowerboundary=result$solution[1:(n*maxk)]
    upperboundary=result$solution[(n*maxk+1):(2*n*maxk)]
    
    
    
    structure(list(lowerboundary = lowerboundary,
                   upperboundary = upperboundary,
                   maxk = maxk,
                   call   = call
    ),
    
    class = "exactBoxes"
    )
}



exactBoxes.formula <- function (formula, data,  maxk, cexpand, positiveVal = 1, negativeVal = -1, ...) {
    call <- match.call()
    if (!inherits(formula, "formula"))
        stop("method is only for formula objects")
    
    if (is.data.frame(data)) {
        ## handle formula
        
        res_column <- as.character(formula[[2]])
        X <- as.matrix(data[ ,-which(names(data) %in% c(res_column))])
        Y <- as.vector(data[ res_column])
                       
        return(exactBoxes.default(X, Y, maxk, cexpand, positiveVal = 1, negativeVal = -1, ...))
        
    }
    stop("please pass data frame as parameter")            
    
}


predict.exactBoxes <- function(model, data, .) {
    
    
    lowerideal=t(model$lowerboundary)
    upperideal=t(model$upperboundary)
        
    classification=matrix(0, nrow=nrow(data), ncol=1)
        
    for (k in seq(1:model$maxk)) {
        classification <- classification |         
            rowSums((data>=repmat(lowerideal[k,],nrow(data),1)) & (data<=repmat(upperideal[k,],nrow(data),1))) == ncol(data)
    }
    
    return (classification)
    
    
}



print.exactBoxes <- function(x, ...) {
    cat("\nExact Boxes Classifier for Imbalanced Data\n\n")
    cat("Call:\n")
    print(x$call)
}


