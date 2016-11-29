# TODO: Add comment
# 
# Author: Ricardo Cachucho
###############################################################################


###############################################################################
#######################FEATURE SELECTION: DECIOSION TREES######################
embedded.IG.FS <- function(DData, IData, AIData, PParameter)
{
	if(is.numeric(DData) == TRUE)
		DData <- factor(DData)
	proposedFeatures <- data.frame(DData)
	splits <- list()
	splits <- c(splits,list(seq(1,length(DData),1)))
	aRAE <- 100
	aMinRAE <- 99
	aStart <- 0
	indexFeatures <- 2

	while(aMinRAE < aRAE)
	{
		aRAE <- aMinRAE
		start <- aStart + 1
		n <- length(splits)

		for(s in start:n)
		{
			if(length(unique(DData[splits[[s]]])) > 1)
			{
				candidatesFS <- search.IG.FC(DData,IData,AIData,PParameter,splits[[s]])
				nFS <- length(candidatesFS)
				rootFeatures <- matrix(data=NA,nrow=length(DData),ncol=nFS)
				colnames(rootFeatures) <- names(candidatesFS)
				for(i in 1:nFS)
				{
					x <- names(candidatesFS[i])
					x <- unlist(strsplit(x,split="_"))
					rootFeatures[,i] <- eval(parse(text = paste(x[2],"(IData$",x[1],",AIData$Variables.nrow,PParameter$Jump,",x[3],")",sep="")))
				}
				DataFrame <- data.frame(rootFeatures,DData)
				DataFrame <- DataFrame[splits[[s]],]

				m1 <- DecisionStump(DData ~ ., data=DataFrame)
				splits <- c(splits,list(splits[[s]][which(m1$predictions == unique(m1$predictions)[1])]),list(splits[[s]][which(m1$predictions == unique(m1$predictions)[2])]))

				FSName <- unlist(strsplit(capture.output(m1)[5],split=" "))[1]
				FSTuple <- unlist(strsplit(FSName,split="_"))
				node <- eval(parse(text = paste(FSTuple[2],"(IData$",FSTuple[1],",AIData$Variables.nrow,PParameter$Jump,",FSTuple[3],")",sep="")))
				if ((length(which(colnames(proposedFeatures) == FSName)) > 0) == TRUE)
				{
					print(paste("The feature", FSName ,"will be used in different nodes",sep=" "))
				} else
				{
					proposedFeatures[,indexFeatures] <- node
					colnames(proposedFeatures)[indexFeatures] <- FSName
					indexFeatures <- indexFeatures + 1
				}
			}
		aStart <- s
		}

		evaluateModel <- J48(DData ~ ., data=proposedFeatures)
		aMinRAE <- summary(evaluateModel)$details[7]
	}
	return(proposedFeatures)
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

	while(validation < 0.95)
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
		
		evaluateModel <- lm(DData ~ ., data=proposedDataset)
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
