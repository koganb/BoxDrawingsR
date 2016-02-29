library(pracma)
library(Rsymphony)


exactBoxes <- function (x,  ...) {
    UseMethod("exactBoxes")
}



exactBoxes.default <- function(x, y,  maxk, cexpand, positiveVal = 1, negativeVal = -1,  ...) {
    call <- match.call()
    Yname <- deparse(substitute(y))
    
        
    
    #check x is numeric 
    #check y is majority or minoriry class
    
    
    A <- x
    Apositivetraining <- x[y == positiveVal,]
    Anegativetraining <- x[y == negativeVal,]
    
    
    
    
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
        classification <- trainingpositiveclassification |         
            rowSums((Apositivetesting>=repmat(lowerideal[k,],nrow(data),1)) & (data<=repmat(upperideal[k,],nrow(data),1))) == ncol(data)
    }
    
    return (classification)
    
    
}

# naiveBayes.formula <- function(formula, data, laplace = 0, ...,
#                                subset, na.action = na.pass) {
#     call <- match.call()
#     Yname <- as.character(formula[[2]])
# 
#     if (is.data.frame(data)) {
#         ## handle formula
#         m <- match.call(expand.dots = FALSE)
#         m$... <- NULL
#         m$laplace = NULL
#         m$na.action <- na.action
#         m[[1]] <- as.name("model.frame")
#         m <- eval(m, parent.frame())
#         Terms <- attr(m, "terms")
#         if (any(attr(Terms, "order") > 1))
#             stop("naiveBayes cannot handle interaction terms")
#         Y <- model.extract(m, "response")
#         X <- m[,-attr(Terms, "response"), drop = FALSE]
# 
#         return(naiveBayes(X, Y, laplace = laplace, ...))
#     } else if (is.array(data)) {
#         nam <- names(dimnames(data))
#         ## Find Class dimension
#         Yind <- which(nam == Yname)
# 
#         ## Create Variable index
#         deps <- strsplit(as.character(formula)[3], ".[+].")[[1]]
#         if (length(deps) == 1 && deps == ".")
#             deps <- nam[-Yind]
#         Vind <- which(nam %in% deps)
# 
#         ## create tables
#         apriori <- margin.table(data, Yind)
#         tables <- lapply(Vind,
#                          function(i) (margin.table(data, c(Yind, i)) + laplace) /
#                          (as.numeric(apriori) + laplace * dim(data)[i]))
#         names(tables) <- nam[Vind]
# 
#         structure(list(apriori = apriori,
#                        tables = tables,
#                        levels = names(apriori),
#                        call   = call
#                        ),
# 
#                   class = "naiveBayes"
#                   )
#     } else stop("naiveBayes formula interface handles data frames or arrays only")
# 
# }
# 
# 
# print.naiveBayes <- function(x, ...) {
#     cat("\nNaive Bayes Classifier for Discrete Predictors\n\n")
#     cat("Call:\n")
#     print(x$call)
#     cat("\nA-priori probabilities:\n")
#     print(x$apriori / sum(x$apriori))
# 
#     cat("\nConditional probabilities:\n")
#     for (i in x$tables) {print(i); cat("\n")}
# 
# }
# 
# predict.naiveBayes <- function(object,
#                                newdata,
#                                type = c("class", "raw"),
#                                threshold = 0.001,
#                                eps = 0,
#                                ...) {
#     type <- match.arg(type)
#     newdata <- as.data.frame(newdata)
#     attribs <- match(names(object$tables), names(newdata))
#     isnumeric <- sapply(newdata, is.numeric)
#     newdata <- data.matrix(newdata)
#     L <- sapply(1:nrow(newdata), function(i) {
#         ndata <- newdata[i, ]
#         L <- log(object$apriori) + apply(log(sapply(seq_along(attribs),
#             function(v) {
#                 nd <- ndata[attribs[v]]
#                 if (is.na(nd)) rep(1, length(object$apriori)) else {
#                   prob <- if (isnumeric[attribs[v]]) {
#                     msd <- object$tables[[v]]
#                     msd[, 2][msd[, 2] <= eps] <- threshold
#                     dnorm(nd, msd[, 1], msd[, 2])
#                   } else object$tables[[v]][, nd]
#                   prob[prob <= eps] <- threshold
#                   prob
#                 }
#             })), 1, sum)
#         if (type == "class")
#             L
#         else {
#             ## Numerically unstable:
#             ##            L <- exp(L)
#             ##            L / sum(L)
#             ## instead, we use:
#             sapply(L, function(lp) {
#                 1/sum(exp(L - lp))
#             })
#         }
#     })
#     if (type == "class")
#         factor(object$levels[apply(L, 2, which.max)], levels = object$levels)
#     else t(L)
# }
