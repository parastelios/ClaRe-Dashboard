# TODO: Add comment
# 
# Author: Cachucho&Martorelli
###############################################################################


###############################################################################
###################################PACKAGES####################################
#library(prettyR)
#library(glmnet)
#library(moments)
library(CORElearn)
library(RWeka)
#library(Metrics)


###############################################################################
###############################AGGREGATE FUNCTIONS#############################
Mean <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(anIndex, function(i) mean(as.numeric(aPredictor[max(i-windowSize+1,1):i]), na.rm=TRUE)))
}

Median <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(anIndex, function(i) median(as.numeric(aPredictor[max(i-windowSize+1,1):i]), na.rm=TRUE)))
}

iqr <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(anIndex, function(i) IQR(as.numeric(aPredictor[max(i-windowSize+1,1):i]),na.rm = TRUE)))
}

SD <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(anIndex, function(i) sd(as.numeric(aPredictor[max(i-windowSize+1,1):i]),na.rm = TRUE)))
}

Min <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(anIndex, function(i) min(as.numeric(aPredictor[max(i-windowSize+1,1):i]),na.rm = TRUE)))
}

Max <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(anIndex, function(i) max(as.numeric(aPredictor[max(i-windowSize+1,1):i]),na.rm = TRUE)))
}

RMS <- function(aPredictor, vLength, jump, windowSize) {
	return(sapply(anIndex, function(i) sqrt(sum(as.numeric(aPredictor[max(i-windowSize+1,1):i])^2,na.rm = TRUE)/windowSize)))
}

###############################PREPROCESSING FUNCTIONS#############################
analyseIndependentData <- function(data)
{
	n <- nrow(data)
	timeStamps <- matrix(data=data[[1]],nrow=n,ncol=1)
	localSR <- 1/(diff(timeStamps)/1000)
	#Sampling is the process of converting a signal (for example, a function of continuous time or space)
	#into a numeric sequence (a function of discrete time or space)
	modeSR = as.numeric(Mode(localSR))
	medianSR = median(localSR)
	meanSR = round(mean(localSR),digits = getndp(medianSR))
	ifelse((meanSR == modeSR && modeSR == medianSR), stability <- "yes", stability <- "no")
	
	#ouput of the function
	output <- list(names(data),meanSR,stability,n)
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
parameterFinder <- function(inputSR, outputSR, factor)
{
	jump <- floor(inputSR/outputSR)
	parameter <- list(c(), jump,c())
	names(parameter) <- c("Size", "Jump","nOperations")
	for (i in 1:factor)
	{
		parameter$Size[i] <- i*jump
	}
	parameter$nOperations <- factor
	return(parameter)
}


##############################################################################
##################Feature Construction: Cross-Corelation######################
evoS.CCF.FC <- function(DData, IData, AIData, PParameter)
{
	AFnames <- c("Mean","Median","iqr","SD","Min","Max","RMS")
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
			
			#evolutionary process
			k <- 1
			while(nTopCor > oTopCor)
			{
				if(nTopCor < 0.2)
					break
				oTopCor <- nTopCor
				seed <- as.numeric(unlist(strsplit(top.Feature,split="_"))[3])
				limits <- c(1, PParameter$Size[length(PParameter$Size)])
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
	return(anOutput)
}
###############################Cross-Correlations###################################
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


###############################################################################
#######################FEATURE SELECTION: LAG REGRESSION#######################
embedded.CCF.FS <- function(DData, IData, AIData, PParameter)
{
	proposedDataset<- data.frame(DData)
	aStart <- 0
	indexFeatures <- 2
	aTarget <- DData
	validation <- whiteNoise(aTarget)
	if(validation >= 0.95)
		print(paste("Target attribute considered white noise: p-value = ",validation,sep=""))
	
	while(validation < 0.05)
	{
		candidatesFS <- evoS.CCF.FC(aTarget, IData, AIData, PParameter)
		if(length(candidatesFS) == 0)
		{
			print("Breaking because FC algorithm was unable to generate new candidates.")
			break
		}
		top.feature <- which(candidatesFS == max(candidatesFS))
		FSName <- names(candidatesFS[top.feature])
		FSTuple <- unlist(strsplit(FSName,split="_"))
		feature <- eval(parse(text = paste(FSTuple[2],"(IData$",FSTuple[1],",AIData$Variables.nrow,PParameter$Jump,",FSTuple[3],")",sep="")))
		
		if ((length(which(colnames(proposedDataset) == FSName)) > 0) == TRUE)
		{
			print(paste("The feature", FSName ,"was chosen again!!",sep=" "))
			break
		} else
		{
			proposedDataset[,indexFeatures] <- lag(feature,as.numeric(FSTuple[4]))
			colnames(proposedDataset)[indexFeatures] <- FSName
			indexFeatures <- indexFeatures + 1
		}
		
		evaluateModel <- lm(DData ~ 0 + ., data=proposedDataset)
		#compute the corelations for the residuals
		aTarget <- DData - predict(evaluateModel)
		
		newValidation <- whiteNoise(aTarget)
		print(newValidation)
		validation <- newValidation
	}
	return(proposedDataset)
}
whiteNoise <- function(x)
{
	n <- length(x)
	gam <- acf(x, type = "covariance",plot=F)
	gam0 <- gam$acf[1]
	ILAM <- spec.pgram(x, taper = 0, fast = FALSE,plot=F)
	ILAM <- ILAM$spec
	T <- length(ILAM)
	P2 <- (T^(-1)) * (sum(ILAM^2))
	MN <- (P2/gam0^2) - 1
	tMN <- sqrt(T) * (MN - 1)
	pval <- pnorm(tMN, mean = 0, sd = 2, lower.tail = FALSE)
	test <- pval * 2
	if (test > 1) 
		test <- 2 - test
	return(test)
}

indexForUnstableTargetSample <- function(IData, DData) {
	timeTarget <- DData$time
	NtimeTarget <- length(timeTarget)
	anIndex <- vector(mode="integer",length=NtimeTarget)
	timeACC <- IData$time
	k=1
	for(t in 1:NtimeTarget)
	{
		target <- timeTarget[t]+7200000
		aStop = FALSE
		while(aStop == FALSE)
		{
			if(timeACC[k] == target)
			{
				anIndex[t] <- k
				aStop <- TRUE
			}
			else
				k <- k + 1
		}
	}
	return(anIndex)
}


###############################################################################
###########################JAN:SPORTS DATASET##################################
WD <- "C:/Users/Cachucho&Martorelli/Dropbox/personal website/Publ"
if(getwd() != WD) setwd(WD)
# Target:
IData <- read.csv("runningPredictors1.csv")
# Predictors:
DData <- read.csv("runningTarget1.csv")
# Running Accordion:
anIndex <- indexForUnstableTargetSample(IData,DData)
AIData <- analyseIndependentData(IData)
PParameter <- parameterFinder(AIData$Sampling.Rate, 0.2, 12)
dataset <- embedded.CCF.FS(DData[[2]], IData, AIData, PParameter)
model <- lm(DData ~ 0 + ., data=dataset)
