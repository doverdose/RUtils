
tEq <- function(x1, x2, v1, v2, n1, n2){
	s = sqrt( (v1*(n1-1) + v2*(n2-1))/(n1+n2-2) ) * sqrt(1/n1 + 1/n2)
	t = (x1-x2) / s
	return(t)
}

tWelch <- function(x1, x2, v1, v2, n1, n2){
	s = sqrt(v1/n1 + v2/n2)
	t = (x1-x2)/s
	return(t)
}

tEqTest <- function(x1, x2, v1, v2, n1, n2){
	t = tEq(x1, x2, v1, v2, n1, n2)
	df = n1 + n2 - 2
	p = pt(t, df, lower.tail=TRUE)
	return(c(t,df,p))	
}

tWelchTest <- function(x1, x2, v1, v2, n1, n2) {
	t = tWelch(x1, x2, v1, v2, n1, n2)
	df =  round( (v1/n1+v2/n2)^2/((v1/n1)^2/(n1-1) + (v2/n2)^2/(n2-1)), 0 )
	p = pt(t, df, lower.tail=TRUE)
	return(c(t,df,p))
}

descStats <- function(f, d) {
	dataLen = aggregate(f, data=d, FUN=length)
	dataMean = aggregate(f, data=d, FUN=mean)
	dataVar = aggregate(f, data=d, FUN=var)
	stats = cbind(dataLen[ncol(dataLen)], dataMean[ncol(dataMean)], dataVar[ncol(dataVar)])
	colnames(stats) <- c("length", "mean", "var")
	return(stats)
}

tTest <- function(f, d, p) {
	analysis = descStats(f,d)
	numTests = length(p)
	numRows = 2*numTests

	testResults = data.frame(arm1 = integer(numRows), arm2 = integer(numRows), type = character(numRows), tval = numeric(numRows), df = numeric(numRows), prob = numeric(numRows))
	fac = sapply(testResults, is.factor)
	testResults[fac] = sapply(testResults[fac], as.character)

	for (i in 1:length(p)) {
		arm1 = p[[i]][1]
		arm2 = p[[i]][2]

		eq = tEqTest(analysis$mean[arm1], analysis$mean[arm2], analysis$var[arm1], analysis$var[arm2], analysis$length[arm1], analysis$length[arm2])
		uneq = tWelchTest(analysis$mean[arm1], analysis$mean[arm2], analysis$var[arm1], analysis$var[arm2], analysis$length[arm1], analysis$length[arm2])
	
		eqInd = 2*i-1
		uneqInd = 2*i
		
		testResults$arm1[eqInd] = arm1
		testResults$arm2[eqInd] = arm2
		testResults$type[eqInd] = "Equal"
		testResults$tval[eqInd] = eq[1]
		testResults$df[eqInd] = eq[2]
		testResults$prob[eqInd] = eq[3]

		testResults$arm1[uneqInd] = arm1
		testResults$arm2[uneqInd] = arm2
		testResults$type[uneqInd] = "Unequal"
		testResults$tval[uneqInd] = uneq[1]
		testResults$df[uneqInd] = uneq[2]
		testResults$prob[uneqInd] = uneq[3]

	}
	return(testResults)
}

tTest2Console <- function(f, d, p) {
	testResults = tTest(f,d,p)
	
	print(f)
	print(testResults)
}