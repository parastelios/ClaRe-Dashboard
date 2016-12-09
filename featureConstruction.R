# TODO: Add comment
# 
# Author: Ricardo Cachucho
###############################################################################





######################################################################################################
############Feature Construction using Search: Using corelation and Cross-Corelation##################
evoS.CCF.FC <- function(DData, IData, AIData, PParameter)
{
  AFnames <- c("Mean","SD","Min","Max","RMS")
  #AFnames <- c("Mean","Median","iqr","SD","Min","Max","RMS")
  nDVariables <- length(AIData$Variables.Names)
  nFeatures <- length(AFnames)
  output.ncol <- (nDVariables-1)*nFeatures
  anOutput <- matrix(data=NA, nrow=1, ncol=output.ncol)
  ncol.names <- vector(mode= "character", length = output.ncol)
  p <- 1
  ############just for experimental section purposes##########
  nCount <- 0
  
  for ( i in 2:nDVariables)
  {
    variableName = AIData$Variables.Names[i]
    for (k in 1:nFeatures)
    {
      AFname <- AFnames[k]
      
      #Initial Population
      oTopCor <- 0
      first.Generation <- sapply(1:PParameter$nOperations, function(i) Find_Max_CCF(y=DData,
                                                                                    x=eval(parse(text = paste(AFname,"(IData$",variableName,",AIData$Variables.nrow,PParameter$Jump,",PParameter$Size[i],")",sep=""))),
                                                                                    max=PParameter$nOperations)[1,1])
      names(first.Generation) <- sapply(1:PParameter$nOperations,
                                        function(i) c(paste(variableName, AFname, PParameter$Size[i], sep="_")))
      NAcount <- length(which(is.na(as.vector(first.Generation)) == TRUE))
      if(NAcount == length(first.Generation))
        next
      nTopCor <- max(abs(first.Generation), na.rm = TRUE)
      positive <- list(which(first.Generation == nTopCor))
      negative <- list(which(first.Generation == (-nTopCor)))
      top.Feature <- mapply(c, positive, negative, SIMPLIFY=T)
      top.Feature <- names(first.Generation[as.vector(top.Feature)])
      nCount <- nCount + PParameter$nOperations
      
      #evolutionary process
      k <- 1
      while(nTopCor > oTopCor)
      {
        if(nTopCor < 0.2)
          break
        oTopCor <- nTopCor
        seed <- as.numeric(unlist(strsplit(top.Feature,split="_"))[3])
        limits <- c(1, PParameter$Size[length(PParameter$Size)])
        #refining the search should discussed with someone!!!
        width <- round(seq(max(limits[1],(seed-PParameter$Jump/k)),min(limits[2],(seed+PParameter$Jump/k)),length.out=PParameter$nOperations))
        offspring <- sapply(1:PParameter$nOperations, function(i) Find_Max_CCF(y=DData,
                                                                               x=eval(parse(text = paste(AFname,"(IData$",variableName,",AIData$Variables.nrow,PParameter$Jump,",width[i],")",sep=""))),
                                                                               max=PParameter$nOperations)[1,1])
        names(offspring) <- sapply(1:PParameter$nOperations,
                                   function(i) c(paste(variableName, AFname, width[i], sep="_")))
        nTopCor <- max(abs(offspring), na.rm = TRUE)
        positive <- list(which(offspring == nTopCor))
        negative <- list(which(offspring == (-nTopCor)))
        top.Feature <- mapply(c, positive, negative, SIMPLIFY=T)
        top.Feature <- names(offspring[as.vector(top.Feature)])
        k <- k + 1
        nCount <- nCount + PParameter$nOperations
      }
      
      #saving the feature
      if(nTopCor > 0.2)
      {
        optimal <- as.numeric(unlist(strsplit(top.Feature,split="_"))[3])
        aCCF <- Find_Max_CCF(y=DData,x=eval(parse(text = paste(AFname,"(IData$",variableName,",AIData$Variables.nrow,PParameter$Jump,",
                                                               optimal,")",sep=""))),max=PParameter$nOperations)
        anOutput[1,p]<- aCCF[1,1]
        ncol.names[p] <- c(paste(variableName, AFname, optimal, aCCF[1,2], sep="_"))
        p <- p + 1
      }
      
    }
  }
  ncol.names <- ncol.names[1:p-1]
  anOutput <- anOutput[,1:p-1]
  names(anOutput) <- ncol.names
  alternativeOutput <- list(nCount, anOutput)
  return(alternativeOutput)
  #return(anOutput)
}
#########################Cross Correlations: Enabling Lag Regressions#################################
Find_Max_CCF<- function(x,y,max)
{
  d <- ccf(x, y, lag.max=max, plot = FALSE, na.action = na.pass)
  cor = d$acf[,,1][1:max+1]
  lag = d$lag[,,1][1:max+1]
  cor_max = which.max(abs(cor))
  res = data.frame(cor,lag)
  res_max = res[cor_max,]
  return(res_max)
}



evoS.cor.FC <- function(DData, IData, AIData, PParameter)
{
  AFnames <- c("Mean","SD","Min","Max","RMS")
  #AFnames <- c("Mean","Median","iqr","SD","Min","Max","RMS")
  nDVariables <- length(AIData$Variables.Names)
  nFeatures <- length(AFnames)
  output.ncol <- (nDVariables-1)*nFeatures
  anOutput <- matrix(data=NA, nrow=1, ncol=output.ncol)
  ncol.names <- vector(mode= "character", length = output.ncol)
  p <- 1
  
  for ( i in 2:nDVariables)
  {
    variableName = AIData$Variables.Names[i]
    for (k in 1:nFeatures)
    {
      AFname <- AFnames[k]
      
      #Initial Population
      oTopCor <- 0
      first.Generation <- sapply(1:PParameter$nOperations, function(i) cor(y=DData,
                                                                           x=eval(parse(text = paste(AFname,"(IData$",variableName,",AIData$Variables.nrow,PParameter$Jump,",PParameter$Size[i],")",sep="")))))
      names(first.Generation) <- sapply(1:PParameter$nOperations,
                                        function(i) c(paste(variableName, AFname, PParameter$Size[i], sep="_")))
      NAcount <- length(which(is.na(as.vector(first.Generation)) == TRUE))
      if(NAcount == length(first.Generation))
        next
      nTopCor <- max(abs(first.Generation), na.rm = TRUE)
      positive <- list(which(first.Generation == nTopCor))
      negative <- list(which(first.Generation == (-nTopCor)))
      top.Feature <- mapply(c, positive, negative, SIMPLIFY=T)
      top.Feature <- names(first.Generation[as.vector(top.Feature)])
      
      #evolutionary process
      k <- 1
      while(nTopCor > oTopCor)
      {
        if(nTopCor < 0.2)
          break
        oTopCor <- nTopCor
        seed <- as.numeric(unlist(strsplit(top.Feature, split="_"))[3])
        limits <- c(1, PParameter$Size[length(PParameter$Size)])
        #refining the search should discussed with someone!!!
        width <- round(seq(max(limits[1],(seed-PParameter$Jump/k)),min(limits[2],(seed+PParameter$Jump/k)),length.out=PParameter$nOperations))
        offspring <- sapply(1:PParameter$nOperations, function(i) cor(y=DData,
                                                                      x=eval(parse(text = paste(AFname,"(IData$",variableName,",AIData$Variables.nrow,PParameter$Jump,",width[i],")",sep="")))))
        names(offspring) <- sapply(1:PParameter$nOperations,
                                   function(i) c(paste(variableName, AFname, width[i], sep="_")))
        nTopCor <- max(abs(offspring), na.rm = TRUE)
        positive <- list(which(offspring == nTopCor))
        negative <- list(which(offspring == (-nTopCor)))
        top.Feature <- mapply(c, positive, negative, SIMPLIFY=T)
        top.Feature <- names(offspring[as.vector(top.Feature)])
        k <- k + 1
      }
      
      #saving the feature
      if(nTopCor > 0.2)
      {
        optimal <- as.numeric(unlist(strsplit(top.Feature, split="_"))[3])
        anOutput[1,p]<- cor(y=DData,x=eval(parse(text = paste(AFname,"(IData$",variableName,",AIData$Variables.nrow,PParameter$Jump,",
                                                              optimal,")",sep=""))))
        ncol.names[p] <- c(paste(variableName, AFname, optimal, sep="_"))
        p <- p + 1
      }
      
    }
  }
  ncol.names <- ncol.names[1:p-1]
  anOutput <- anOutput[,1:p-1]
  names(anOutput) <- ncol.names
  return(anOutput)
}


evoS.GainRatio.FC <- function(DData, IData, AIData, PParameter, dataToUse)
{
  if(missing(dataToUse)) { # 'missing()' is used to test whether a value was specified as an argument
    index <- seq(1,length(DData),1)
  } else {
    index <- dataToUse 
  }
  
  AFnames <- c("Mean","SD","Min","Max","RMS")
  #AFnames <- c("Mean","Median","iqr","SD","Min","Max","RMS")
  nDVariables <- length(AIData$Variables.Names)
  nFeatures <- length(AFnames)
  output.ncol <- (nDVariables-1)*nFeatures
  anOutput <- matrix(data=NA, nrow=1, ncol=output.ncol)
  ncol.names <- vector(mode= "character", length = output.ncol)
  p <- 1
  
  
  for ( i in 2:nDVariables)
  {
    variableName = AIData$Variables.Names[i]
    for (k in 1:nFeatures)
    {
      AFname <- AFnames[k]
      #Initial Candidates
      oTop <- 0
      FeatMatrix <- matrix(data = NA, nrow = length(DData), ncol = PParameter$nOperations)
      FeatMatrix <- sapply(1:PParameter$nOperations, function(i)
        eval(parse(text = paste(AFname,"(IData$",variableName,",AIData$Variables.nrow,PParameter$Jump,",PParameter$Size[i],")",sep=""))))
      colnames(FeatMatrix) <- sapply(1:PParameter$nOperations,
                                     function(i) c(paste(variableName, AFname, PParameter$Size[i], sep="_")))
      DataFrame <- data.frame(FeatMatrix,DData)
      DataFrame <- DataFrame[index,]
      
      colErase <- c()
      for(i in 1:PParameter$nOperations)
      {
        if(length(unique(DataFrame[,i])) == 1)
          colErase <- c(colErase,i) 
      }
      if(length(colErase) == ncol(DataFrame)-1)
        break
      if(length(colErase) >= 1)
        DataFrame <- DataFrame[,-colErase]
      
      first.Generation <- attrEval(DData ~ ., DataFrame, estimator="GainRatio")
      
      #Slecting the most promissing feature
      nTop <- max(abs(first.Generation))
      positive <- list(which(first.Generation == nTop))
      negative <- list(which(first.Generation == (-nTop)))
      top.Feature <- mapply(c, positive, negative, SIMPLIFY=T)
      
      #evolutionary process
      k <- 1
      while(nTop > oTop)
      {
        #if(nTop < 0.2)
        #	break
        oTop <- nTop
        ifelse(length(top.Feature)==1, seed <- as.numeric(unlist(strsplit(names(top.Feature),split="_"))[3]),seed <- as.numeric(unlist(strsplit(rownames(top.Feature)[1],split="_"))[3]))
        #seed <- as.numeric(unlist(strsplit(rownames(top.Feature)[1],split="_"))[3])
        limits <- c(1, PParameter$Size[length(PParameter$Size)])
        #refining the search should discussed with someone!!!
        width <- round(seq(max(limits[1],(seed-PParameter$Jump/k)),min(limits[2],(seed+PParameter$Jump/k)),length.out=PParameter$nOperations))
        FeatMatrix <- matrix(data = NA, nrow = length(DData), ncol = length(width))
        FeatMatrix <- sapply(1:PParameter$nOperations, function(i)
          eval(parse(text = paste(AFname,"(IData$",variableName,",AIData$Variables.nrow,PParameter$Jump,",width[i],")",sep=""))))
        colnames(FeatMatrix) <- sapply(1:PParameter$nOperations,
                                       function(i) c(paste(variableName, AFname, width[i], sep="_")))
        DataFrame <- data.frame(FeatMatrix,DData)
        DataFrame <- DataFrame[index,]
        
        colErase <- c()
        for(i in 1:PParameter$nOperations)
        {
          if(length(unique(DataFrame[,i])) == 1)
            colErase <- c(colErase,i) 
        }
        if(length(colErase) == ncol(DataFrame)-1)
          break
        if(length(colErase) >= 1)
          DataFrame <- DataFrame[,-colErase]
        offspring <- attrEval(DData ~ ., DataFrame, estimator="GainRatio")
        
        nTop <- max(abs(offspring))
        positive <- list(which(offspring == nTop))
        negative <- list(which(offspring == (-nTop)))
        top.Feature <- mapply(c, positive, negative, SIMPLIFY=T)
        k <- k + 1
      } 
      
      #saving the feature
      if(nTop > 0.5)
      {
        ifelse(length(top.Feature)==1, optimal <- as.numeric(unlist(strsplit(names(top.Feature),split="_"))[3]),optimal <- as.numeric(unlist(strsplit(rownames(top.Feature)[1],split="_"))[3]))
        #optimal <- as.numeric(unlist(strsplit(names(top.Feature[1]),split="_"))[3])
        x <- eval(parse(text = paste(AFname,"(IData$",variableName,",AIData$Variables.nrow,PParameter$Jump,",optimal,")",sep="")))
        DataFrame <- data.frame(x,DData)
        DataFrame <- DataFrame[index,]
        anOutput[1,p] <- attrEval(DData ~ ., DataFrame, estimator="GainRatio")
        ncol.names[p] <- c(paste(variableName, AFname, optimal, sep="_"))
        p <- p + 1
      }
      
    }
  }
  ncol.names <- ncol.names[1:p-1]
  anOutput <- anOutput[,1:p-1]
  names(anOutput) <- ncol.names
  return(anOutput)
}
