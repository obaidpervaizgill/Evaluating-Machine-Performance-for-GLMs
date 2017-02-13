##Rene's Request##
#function for creating and expanding dataset
dataEXP <- function(multiply , data){
    df <- data.frame()
    i = 1
    while(i < ( multiply + 1 )){
        df <- rbind.data.frame(df,data)
        i <- i + 1
    }
    return(df)
}

#function for creating an iterating variable by 100
iterateBYHUNDRED <- function(end){
    x <- c()
    for(i in 1:end){
        x <- c(x,100*i)
    }
    return(x)
}


#function for fitting a glm and getting performance data output
extraCGLMtPERFORMACE <- function(){
    perf <- data.frame()
    for(i in 1:length(iterator <- iterateBYHUNDRED(end = 5))){
        ptm <- proc.time()
        model <- glm(mpg ~ ., family = gaussian, data = dataEXP(multiply = iterator[i], data = mtcars))
        perf <- rbind(perf, cbind(
            multiply = iterator[i]
            ,user.self = (proc.time() - ptm)[[1]]
            ,sys.self = (proc.time() - ptm)[[2]]
            ,elapsed = (proc.time() - ptm)[[3]]
        )
        )
    }
    return(perf)
}


#function for plotting GLM Performance measures
plotDATA <- function(xaxis, data){
    par(mfrow=c((length(colnames(data))-1),1))
    for(i in 1:(length(colnames(data))-1)){
        plot(data[[(colnames(data)[!colnames(data)%in%xaxis])[i]]] ~ data[[xaxis]]
             , ylab = (colnames(data)[!colnames(data)%in%xaxis])[i]
             , xlab = xaxis)
    }  
}

#function to wrap everthing up
plotDATA(xaxis = "multiply", data = extraCGLMtPERFORMACE())