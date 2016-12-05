# TODO: Add comment
# 
# Author: Ricardo Cachucho
###############################################################################

###############################################################################
###################################PACKAGES####################################
#library(prettyR)
# library(glmnet)
#library(moments)
library(CORElearn)
library(RWeka)



###############################################################################
###############################AGGREGATE FUNCTIONS##############################
Mean <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(seq(jump,vLength,jump), function(i) mean(as.numeric(aPredictor[max(i-windowSize+1,1):i]), na.rm=TRUE)))
}

Median <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(seq(jump,vLength,jump), function(i) median(as.numeric(aPredictor[max(i-windowSize+1,1):i]), na.rm=TRUE)))
}

iqr <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(seq(jump,vLength,jump), function(i) IQR(as.numeric(aPredictor[max(i-windowSize+1,1):i]),na.rm = TRUE)))
}

SD <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(seq(jump,vLength,jump), function(i) sd(as.numeric(aPredictor[max(i-windowSize+1,1):i]),na.rm = TRUE)))
}

Min <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(seq(jump,vLength,jump), function(i) min(as.numeric(aPredictor[max(i-windowSize+1,1):i]),na.rm = TRUE)))
}

Max <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(seq(jump,vLength,jump), function(i) max(as.numeric(aPredictor[max(i-windowSize+1,1):i]),na.rm = TRUE)))
}

RMS <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(seq(jump,vLength,jump), function(i) sqrt(sum(as.numeric(aPredictor[max(i-windowSize+1,1):i])^2,na.rm = TRUE)/windowSize)))
}

#These functions analyse the sampling rate of the predictors, and set parameters used by
#feature construction and selection algorithms. Check demo.R
###############################PREPROCESSING FUNCTIONS##############################
analyseIndependentData <- function(data)
{
	n <- nrow(data)
	timeStamps <- matrix(data=data[[1]],nrow=n,ncol=1)
	localSR <- 1/(diff(timeStamps)/1000)
	#Sampling is the process of converting a signal (for example, a function of continuous time or space)
	#into a numeric sequence (a function of discrete time or space)
	medianSR = median(localSR)
	meanSR = round(mean(localSR),digits = getndp(medianSR))
	ifelse((meanSR == medianSR), stability <- "yes", stability <- "no")
	
	#ouput of the function
	output <- list(names(data),medianSR,stability,n)
	names(output) <- c('Variables.Names', 'Sampling.Rate', 'Stable.Sampling.Rate','Variables.nrow')
	return(output)
}
getndp <- function(x, tol=2*.Machine$double.eps) 
{ 
	ndp <- 0 
	while(!isTRUE(all.equal(x, round(x, ndp), tol=tol))) ndp <- ndp+1 
	if(ndp > -log10(tol)) warning("Tolerance reached, ndp possibly 
						underestimated.") 
	return(ndp) 
} 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
parameterFinder <- function(inputSR, outputSR, maxWindowSize)
{
	jump <- floor(inputSR/outputSR)
	parameter <- list(c(), jump,c())
	names(parameter) <- c("Size", "Jump","nOperations")
	for (i in 1:maxWindowSize)
	{
		parameter$Size[i] <- i*jump
	}
	parameter$nOperations <- maxWindowSize
	return(parameter)
}
