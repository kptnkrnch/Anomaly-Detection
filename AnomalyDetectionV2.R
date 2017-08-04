library(mhsmm)
library(chron)
library(lubridate)

options(digits=2)

formatMhsmm <- function(data){

  nb.sequences = nrow(data)
  nb.observations = length(data)

  #transform list to data frame
  data_df <- data.frame(matrix(unlist(data), nrow = nb.sequences, byrow=F))


  #iterate over these in loops
  rows <- 1:nb.sequences
  observations <- 0:(nb.observations-1)

  #build vector with id values
  id = numeric(length = nb.sequences*nb.observations ) 

  for(i in rows)
  {
    for (j in observations)
    {
      id[i+j+(i-1)*(nb.observations-1)] = i
    }
  }

  #build vector with observation values
  sequences = numeric(length = nb.sequences*nb.observations) 

  for(i in rows)
  {
    for (j in observations)
    {
      sequences[i+j+(i-1)*(nb.observations-1)] = data_df[i,j+1]
    }
  }

  data.df = data.frame(id, sequences)

  #creation of hsmm.data object needed for training
  N <- as.numeric(table(data.df$id))
  train <- list(x = data.df$sequences, N = N)
  class(train) <- "hsmm.data"

  return(train)
}

# used to plot data
plotData <- function(data, startIndex=1, endIndex=0, type="line") {
	if (startIndex >= 1) {
		if (endIndex < startIndex) {
			endIndex = length(data)
		}
		if (type == "line") {
			plot(0:(endIndex - startIndex), data[startIndex:endIndex], type="n")
			lines(0:(endIndex - startIndex), data[startIndex:endIndex])
		} else if (type == "histogram") {
			hist(data[startIndex:endIndex])
		} else if (type == "dot") {
			plot(0:(endIndex - startIndex), data[startIndex:endIndex])
		} else {
			print("Error: unknown graph type")
		}
	} else {
		print("Error: start index cannot be less than 1")
	}
}

# used to plot data windows
plotWindow <- function(data, window, windowSize, type="line") {
	if (window >= 1) {
		startIndex = 1 + (window - 1) * windowSize
		endIndex = window * windowSize
		if (type == "line") {
			plot(0:(endIndex - startIndex), data[startIndex:endIndex], type="n")
			lines(0:(endIndex - startIndex), data[startIndex:endIndex])
		} else if (type == "histogram") {
			hist(data[startIndex:endIndex])
		} else {
			print("Error: unknown graph type")
		}
	} else {
		print("Error: window cannot be less than 1")
	}
}

getAverageForDay <- function(data, date, field="Global_active_power") {
	powers <- c()
	foundDay <- FALSE
	for (i in 1:length(data[["Date"]])) {
		if (grepl(date, data[["Date"]][i])) {
			foundDay = TRUE
			powers[length(powers) + 1] = data[[field]][i]
		}
		if (foundDay == TRUE && grepl(date, data[["Date"]][i]) == FALSE) {
			break
		}
	}
	return(mean(powers, na.rm = TRUE))
}

getAverageForMonth <- function(data, month, year, field="Global_active_power") {
	powers <- c()
	foundMonth <- FALSE
	date = sprintf("/%d/%d", month, year)
	for (i in 1:length(data[["Date"]])) {
		if (grepl(date, data[["Date"]][i])) {
			foundMonth = TRUE
			powers[length(powers) + 1] = data[[field]][i]
		}
		if (foundMonth == TRUE && grepl(date, data[["Date"]][i]) == FALSE) {
			break
		}
	}
	return(mean(powers, na.rm = TRUE))
}
################################################################################################################
# Data loading and HMM training
################################################################################################################

dataset <- read.csv('D:\\SFU\\CMPT 318\\Project\\courseproject_data_V2\\train.txt',header=TRUE, stringsAsFactors=FALSE)
testset1 <- read.csv('D:\\SFU\\CMPT 318\\Project\\courseproject_data_V2\\test1.txt',header=TRUE, stringsAsFactors=FALSE)
testset2 <- read.csv('D:\\SFU\\CMPT 318\\Project\\courseproject_data_V2\\test2.txt',header=TRUE, stringsAsFactors=FALSE)

traindayform <- formatMhsmm(data.frame(dataset[["Global_active_power"]]))
testdayform1 <- formatMhsmm(data.frame(testset1[["Global_active_power"]]))
testdayform2 <- formatMhsmm(data.frame(testset2[["Global_active_power"]]))

# 6 states and 6 emissions (Morning, DayTime, Evening) X (Summer, Winter)
winterPowersMorning = c()
winterPowersDaytime = c()
winterPowersEvening = c()
summerPowersMorning = c()
summerPowersDaytime = c()
summerPowersEvening = c()
winterIndexMorning = 1
winterIndexDaytime = 1
winterIndexEvening = 1
summerIndexMorning = 1
summerIndexDaytime = 1
summerIndexEvening = 1
for (i in 1:length(dataset[["Date"]])) {
	curMonth <- month(as.POSIXlt(dataset[["Date"]][i], format="%d/%m/%Y"))
	
	# Winter/Fall-ish months
	if (curMonth == 10 || curMonth == 11 || curMonth == 12 || curMonth == 1 || curMonth == 2 || curMonth == 3) {
		if (dataset[["Time"]][i] >= "00:00:00" && dataset[["Time"]][i] < "06:00:00") {
			winterPowersMorning[winterIndexMorning] = dataset[["Global_active_power"]][i]
			winterIndexMorning = winterIndexMorning + 1
		} else if (dataset[["Time"]][i] >= "06:00:00" && dataset[["Time"]][i] < "17:00:00") {
			winterPowersDaytime[winterIndexDaytime] = dataset[["Global_active_power"]][i]
			winterIndexDaytime = winterIndexDaytime + 1
		} else {
			winterPowersEvening[winterIndexEvening] = dataset[["Global_active_power"]][i]
			winterIndexEvening = winterIndexEvening + 1
		}
	# Summer/Spring-ish months
	} else {
		if (dataset[["Time"]][i] >= "00:00:00" && dataset[["Time"]][i] < "06:00:00") {
			summerPowersMorning[summerIndexMorning] = dataset[["Global_active_power"]][i]
			summerIndexMorning = summerIndexMorning + 1
		} else if (dataset[["Time"]][i] >= "06:00:00" && dataset[["Time"]][i] < "17:00:00") {
			summerPowersDaytime[summerIndexDaytime] = dataset[["Global_active_power"]][i]
			summerIndexDaytime = summerIndexDaytime + 1
		} else {
			summerPowersEvening[summerIndexEvening] = dataset[["Global_active_power"]][i]
			summerIndexEvening = summerIndexEvening + 1
		}
	} 
}
meanPowers = c(mean(summerPowersMorning, na.rm = TRUE), mean(summerPowersDaytime, na.rm = TRUE), mean(summerPowersEvening, na.rm = TRUE), mean(winterPowersMorning, na.rm = TRUE), mean(winterPowersDaytime, na.rm = TRUE), mean(winterPowersEvening, na.rm = TRUE))
sdPowers = c(sd(summerPowersMorning, na.rm = TRUE), sd(summerPowersDaytime, na.rm = TRUE), sd(summerPowersEvening, na.rm = TRUE), sd(winterPowersMorning, na.rm = TRUE), sd(winterPowersDaytime, na.rm = TRUE), sd(winterPowersEvening, na.rm = TRUE))
k=6
#init probabilities
init <- rep(1/k, k)

#transition matrix
P <- matrix(rep(1/k, k*k), nrow = k)

#emission matrix:  here I used a Gaussian distribution, replace muEst and sigmaEst by your initial estimates of mean and variance
b <- list(mu = meanPowers, sigma = sdPowers)

startmodel <- hmmspec(init = init, trans = P, parms.emis = b, dens.emis = dnorm.hsmm)

hmm <- hmmfit(traindayform$x, startmodel, mstep = mstep.norm, maxit = 100)

hmm$model$init = init <- rep(1/k, k)


############################################################################################################
# Point anomaly detection using thresholds TEST SET 1
############################################################################################################

test1form <- formatMhsmm(data.frame(testset1[["Global_active_power"]]))
prediction <- predict (hmm,test1form$x)

thresholds <- c(0.5, 1, 1.25, 1.5, 1.75, 2)
anomaliesThreshold1 <- c()
anomaliesThreshold2 <- c()
anomaliesThreshold3 <- c()
anomaliesThreshold4 <- c()
anomaliesThreshold5 <- c()
anomaliesThreshold6 <- c()
for (n in 1:length(thresholds)) {
	print(sprintf("Evaluating Threshold %f:", thresholds[n]))
	numAnomalies = 0
	threshold = thresholds[n]
	anomalyFile <- file(sprintf("D:\\SFU\\CMPT 318\\Project\\Output\\PointAnomalies-%f.txt", thresholds[n]))
	output <- c()
	for (i in 1:length(prediction$s)) {
		state = prediction$s[i]
		expectedValue = hmm$model$parms$mu[state]
		if (is.na(prediction$x[i]) == FALSE) {
			difference = abs(prediction$x[i] - expectedValue)
			anomalyStrength = difference / (2 * threshold)
			if (anomalyStrength > 1) {
				anomalyStrength = 1.0
			}
			if (is.na(prediction$x[i]) == FALSE && (difference > threshold)) {
				numAnomalies = numAnomalies + 1
				if (n == 1) {
					anomaliesThreshold1[length(anomaliesThreshold1) + 1] = i
				} else if (n == 2) {
					anomaliesThreshold2[length(anomaliesThreshold2) + 1] = i
				} else if (n == 3) {
					anomaliesThreshold3[length(anomaliesThreshold3) + 1] = i
				} else if (n == 4) {
					anomaliesThreshold4[length(anomaliesThreshold4) + 1] = i
				} else if (n == 5) {
					anomaliesThreshold5[length(anomaliesThreshold5) + 1] = i
				} else if (n == 6) {
					anomaliesThreshold6[length(anomaliesThreshold6) + 1] = i
				}
				output[length(output) + 1] = sprintf("1,%.2f", anomalyStrength)
			} else {
				output[length(output) + 1] = sprintf("0,%.2f", anomalyStrength)
			}
		} else {
			output[length(output) + 1] = sprintf("1,%.2f", 1.0)
		}
	}
	writeLines(output, anomalyFile)
	close(anomalyFile)
}

############################################################################################################
# Collective anomaly detection TEST SET 1
############################################################################################################

testform <- formatMhsmm(data.frame(testset1[["Global_active_power"]]))

windows <- c(5, 10, 15, 20)
anomaliesWindow1 <- c()
anomaliesWindow2 <- c()
anomaliesWindow3 <- c()
anomaliesWindow4 <- c()
for (n in 1:length(windows)) {
	print(sprintf("Evaluating Window %d:", windows[n]))
	numAnomalies = 0
	threshold = 1.0
	windowSize = windows[n]
	anomalyFile <- file(sprintf("D:\\SFU\\CMPT 318\\Project\\Output\\CollectiveAnomalies-%d.txt", windows[n]))
	output <- c()
	
	test_results <- c()
	train_results <- c()
	for (i in 0:(floor(length(testset1[["Global_active_power"]]) / windowSize))) {
		startIndex = (1 + (i*windowSize))
		endIndex = ((i+1)*windowSize)
		if (endIndex > length(testset1[["Global_active_power"]]) || startIndex > length(testset1[["Global_active_power"]])) {
			break
		}
		
		testform <- formatMhsmm(data.frame(testset1[startIndex:endIndex, "Global_active_power"]))
		testLikelihood <- predict (hmm,testform$x)

		test_results[length(test_results) + 1] = testLikelihood$loglik
	}
	for (i in 0:(floor(length(dataset[["Global_active_power"]]) / windowSize))) {
		startIndex = (1 + (i*windowSize))
		endIndex = ((i+1)*windowSize)
		if (endIndex > length(dataset[["Global_active_power"]]) || startIndex > length(dataset[["Global_active_power"]])) {
			break
		}

		trainform <- formatMhsmm(data.frame(dataset[startIndex:endIndex, "Global_active_power"]))
		trainLikelihood <- predict (hmm,trainform$x)

		train_results[length(train_results) + 1] = trainLikelihood$loglik
	}
	sdTrain = sd(train_results, na.rm = TRUE)
	meanTrain = mean(train_results, na.rm = TRUE)

	# Eliminitating outliers/noise using 1.5*IQR (Boxplot outliers)
	quartiles = as.vector(quantile(train_results))
	lowerQuartile = quartiles[2]
	upperQuartile = quartiles[4]
	interQuartileRange = abs(lowerQuartile) + abs(upperQuartile)

	upperBound = upperQuartile + 1.5 * interQuartileRange
	lowerBound = lowerQuartile - 1.5 * interQuartileRange

	# Determining the number of anomalies found (test data) and the amount of noise found (training data)
	numAnomalies <- 0
	anomalies <- c()
	for (i in 1:length(test_results)) {
		expectedValue = (abs(upperBound - lowerBound) / 2) + lowerBound
		difference = abs(test_results[i] - expectedValue)
		anomalyStrength = difference / (abs(upperBound - lowerBound))
		if (anomalyStrength > 1) {
			anomalyStrength = 1.0
		}
		#print(sprintf("AnomalyStrength: %f", difference))
		
		if (test_results[i] > upperBound + threshold) {
			numAnomalies = numAnomalies + 1
			anomalies[numAnomalies] = i

			if (anomalyStrength > 1) {
				anomalyStrength = 1.0
			}

			if (n == 1) {
				anomaliesWindow1[length(anomaliesWindow1) + 1] = i
			} else if (n == 2) {
				anomaliesWindow2[length(anomaliesWindow2) + 1] = i
			} else if (n == 3) {
				anomaliesWindow3[length(anomaliesWindow3) + 1] = i
			} else {
				anomaliesWindow4[length(anomaliesWindow4) + 1] = i
			}

			output[length(output) + 1] = sprintf("1,%.2f", anomalyStrength)
		} else if (test_results[i] < lowerBound - threshold) {
			numAnomalies = numAnomalies + 1
			anomalies[numAnomalies] = i

			if (anomalyStrength > 1) {
				anomalyStrength = 1.0
			}

			if (n == 1) {
				anomaliesWindow1[length(anomaliesWindow1) + 1] = i
			} else if (n == 2) {
				anomaliesWindow2[length(anomaliesWindow2) + 1] = i
			} else if (n == 3) {
				anomaliesWindow3[length(anomaliesWindow3) + 1] = i
			} else {
				anomaliesWindow4[length(anomaliesWindow4) + 1] = i
			}

			output[length(output) + 1] = sprintf("1,%.2f", anomalyStrength)
		} else {
			output[length(output) + 1] = sprintf("0,%.2f", anomalyStrength)
		}
	}
	#print(sprintf("number of anomalies: %d", numAnomalies))

	writeLines(output, anomalyFile)
	close(anomalyFile)
}

############################################################################################################
# Contextual anomaly detection TEST SET 1
############################################################################################################

test1form <- formatMhsmm(data.frame(testset1[["Global_active_power"]]))
prediction <- predict (hmm,test1form$x)

thresholds <- c(1.0)
anomaliesThreshold1 <- c()
anomaliesThreshold2 <- c()
anomaliesThreshold3 <- c()
anomaliesThreshold4 <- c()
anomaliesThreshold5 <- c()
anomaliesThreshold6 <- c()
for (n in 1:length(thresholds)) {
	print(sprintf("Evaluating Threshold %f:", thresholds[n]))
	numAnomalies = 0
	nonContextualThreshold = thresholds[n]
	anomalyFile <- file(sprintf("D:\\SFU\\CMPT 318\\Project\\Output\\ContextualAnomalies.txt"))
	output <- c()
	for (i in 1:length(prediction$s)) {
		state = prediction$s[i]
		expectedValue = hmm$model$parms$mu[state]
		standardDeviation = hmm$model$parms$sigma[state]
		threshold = nonContextualThreshold + 3 * standardDeviation
		if (is.na(prediction$x[i]) == FALSE) {
			difference = abs(prediction$x[i] - expectedValue)
			anomalyStrength = difference / (2 * threshold)
			if (anomalyStrength > 1) {
				anomalyStrength = 1.0
			}
			if (is.na(prediction$x[i]) == FALSE && (difference > threshold)) {
				numAnomalies = numAnomalies + 1
				if (n == 1) {
					anomaliesThreshold1[length(anomaliesThreshold1) + 1] = i
				} else if (n == 2) {
					anomaliesThreshold2[length(anomaliesThreshold2) + 1] = i
				} else if (n == 3) {
					anomaliesThreshold3[length(anomaliesThreshold3) + 1] = i
				} else if (n == 4) {
					anomaliesThreshold4[length(anomaliesThreshold4) + 1] = i
				} else if (n == 5) {
					anomaliesThreshold5[length(anomaliesThreshold5) + 1] = i
				} else if (n == 6) {
					anomaliesThreshold6[length(anomaliesThreshold6) + 1] = i
				}
				output[length(output) + 1] = sprintf("1,%.2f", anomalyStrength)
			} else {
				output[length(output) + 1] = sprintf("0,%.2f", anomalyStrength)
			}
		} else {
			output[length(output) + 1] = sprintf("1,%.2f", 1.0)
		}
	}
	writeLines(output, anomalyFile)
	close(anomalyFile)
}

############################################################################################################
# Point anomaly detection using thresholds TEST SET 2
############################################################################################################

test1form <- formatMhsmm(data.frame(testset2[["Global_active_power"]]))
prediction <- predict (hmm,test1form$x)

thresholds <- c(0.5, 1, 1.25, 1.5, 1.75, 2)
anomaliesThreshold1 <- c()
anomaliesThreshold2 <- c()
anomaliesThreshold3 <- c()
anomaliesThreshold4 <- c()
anomaliesThreshold5 <- c()
anomaliesThreshold6 <- c()
for (n in 1:length(thresholds)) {
	print(sprintf("Evaluating Threshold %f:", thresholds[n]))
	numAnomalies = 0
	threshold = thresholds[n]
	anomalyFile <- file(sprintf("D:\\SFU\\CMPT 318\\Project\\Output\\PointAnomalies-%f.txt", thresholds[n]))
	output <- c()
	for (i in 1:length(prediction$s)) {
		state = prediction$s[i]
		expectedValue = hmm$model$parms$mu[state]
		if (is.na(prediction$x[i]) == FALSE) {
			difference = abs(prediction$x[i] - expectedValue)
			anomalyStrength = difference / (2 * threshold)
			if (anomalyStrength > 1) {
				anomalyStrength = 1.0
			}
			if (is.na(prediction$x[i]) == FALSE && (difference > threshold)) {
				numAnomalies = numAnomalies + 1
				if (n == 1) {
					anomaliesThreshold1[length(anomaliesThreshold1) + 1] = i
				} else if (n == 2) {
					anomaliesThreshold2[length(anomaliesThreshold2) + 1] = i
				} else if (n == 3) {
					anomaliesThreshold3[length(anomaliesThreshold3) + 1] = i
				} else if (n == 4) {
					anomaliesThreshold4[length(anomaliesThreshold4) + 1] = i
				} else if (n == 5) {
					anomaliesThreshold5[length(anomaliesThreshold5) + 1] = i
				} else if (n == 6) {
					anomaliesThreshold6[length(anomaliesThreshold6) + 1] = i
				}
				output[length(output) + 1] = sprintf("1,%.2f", anomalyStrength)
			} else {
				output[length(output) + 1] = sprintf("0,%.2f", anomalyStrength)
			}
		} else {
			output[length(output) + 1] = sprintf("1,%.2f", 1.0)
		}
	}
	writeLines(output, anomalyFile)
	close(anomalyFile)
}

############################################################################################################
# Collective anomaly detection TEST SET 2
############################################################################################################

testform <- formatMhsmm(data.frame(testset2[["Global_active_power"]]))

windows <- c(5, 10, 15, 20)
anomaliesWindow1 <- c()
anomaliesWindow2 <- c()
anomaliesWindow3 <- c()
anomaliesWindow4 <- c()
for (n in 1:length(windows)) {
	print(sprintf("Evaluating Window %d:", windows[n]))
	numAnomalies = 0
	threshold = 1.0
	windowSize = windows[n]
	anomalyFile <- file(sprintf("D:\\SFU\\CMPT 318\\Project\\Output\\CollectiveAnomalies-%d.txt", windows[n]))
	output <- c()
	
	test_results <- c()
	train_results <- c()
	for (i in 0:(floor(length(testset2[["Global_active_power"]]) / windowSize))) {
		startIndex = (1 + (i*windowSize))
		endIndex = ((i+1)*windowSize)
		if (endIndex > length(testset2[["Global_active_power"]]) || startIndex > length(testset2[["Global_active_power"]])) {
			break
		}
		
		testform <- formatMhsmm(data.frame(testset2[startIndex:endIndex, "Global_active_power"]))
		testLikelihood <- predict (hmm,testform$x)

		test_results[length(test_results) + 1] = testLikelihood$loglik
	}
	for (i in 0:(floor(length(dataset[["Global_active_power"]]) / windowSize))) {
		startIndex = (1 + (i*windowSize))
		endIndex = ((i+1)*windowSize)
		if (endIndex > length(dataset[["Global_active_power"]]) || startIndex > length(dataset[["Global_active_power"]])) {
			break
		}

		trainform <- formatMhsmm(data.frame(dataset[startIndex:endIndex, "Global_active_power"]))
		trainLikelihood <- predict (hmm,trainform$x)

		train_results[length(train_results) + 1] = trainLikelihood$loglik
	}
	sdTrain = sd(train_results, na.rm = TRUE)
	meanTrain = mean(train_results, na.rm = TRUE)

	# Eliminitating outliers/noise using 1.5*IQR (Boxplot outliers)
	quartiles = as.vector(quantile(train_results))
	lowerQuartile = quartiles[2]
	upperQuartile = quartiles[4]
	interQuartileRange = abs(lowerQuartile) + abs(upperQuartile)

	upperBound = upperQuartile + 1.5 * interQuartileRange
	lowerBound = lowerQuartile - 1.5 * interQuartileRange

	# Determining the number of anomalies found (test data) and the amount of noise found (training data)
	numAnomalies <- 0
	anomalies <- c()
	for (i in 1:length(test_results)) {
		expectedValue = (abs(upperBound - lowerBound) / 2) + lowerBound
		difference = abs(test_results[i] - expectedValue)
		anomalyStrength = difference / (abs(upperBound - lowerBound))
		if (anomalyStrength > 1) {
			anomalyStrength = 1.0
		}
		#print(sprintf("AnomalyStrength: %f", difference))
		
		if (test_results[i] > upperBound + threshold) {
			numAnomalies = numAnomalies + 1
			anomalies[numAnomalies] = i

			if (anomalyStrength > 1) {
				anomalyStrength = 1.0
			}

			if (n == 1) {
				anomaliesWindow1[length(anomaliesWindow1) + 1] = i
			} else if (n == 2) {
				anomaliesWindow2[length(anomaliesWindow2) + 1] = i
			} else if (n == 3) {
				anomaliesWindow3[length(anomaliesWindow3) + 1] = i
			} else {
				anomaliesWindow4[length(anomaliesWindow4) + 1] = i
			}

			output[length(output) + 1] = sprintf("1,%.2f", anomalyStrength)
		} else if (test_results[i] < lowerBound - threshold) {
			numAnomalies = numAnomalies + 1
			anomalies[numAnomalies] = i

			if (anomalyStrength > 1) {
				anomalyStrength = 1.0
			}

			if (n == 1) {
				anomaliesWindow1[length(anomaliesWindow1) + 1] = i
			} else if (n == 2) {
				anomaliesWindow2[length(anomaliesWindow2) + 1] = i
			} else if (n == 3) {
				anomaliesWindow3[length(anomaliesWindow3) + 1] = i
			} else {
				anomaliesWindow4[length(anomaliesWindow4) + 1] = i
			}

			output[length(output) + 1] = sprintf("1,%.2f", anomalyStrength)
		} else {
			output[length(output) + 1] = sprintf("0,%.2f", anomalyStrength)
		}
	}

	writeLines(output, anomalyFile)
	close(anomalyFile)
}

############################################################################################################
# Contextual anomaly detection TEST SET 2
############################################################################################################

test1form <- formatMhsmm(data.frame(testset2[["Global_active_power"]]))
prediction <- predict (hmm,test1form$x)

thresholds <- c(1.0)
anomaliesThreshold1 <- c()
anomaliesThreshold2 <- c()
anomaliesThreshold3 <- c()
anomaliesThreshold4 <- c()
anomaliesThreshold5 <- c()
anomaliesThreshold6 <- c()
for (n in 1:length(thresholds)) {
	print(sprintf("Evaluating Threshold %f:", thresholds[n]))
	numAnomalies = 0
	nonContextualThreshold = thresholds[n]
	anomalyFile <- file(sprintf("D:\\SFU\\CMPT 318\\Project\\Output\\ContextualAnomalies.txt"))
	output <- c()
	for (i in 1:length(prediction$s)) {
		state = prediction$s[i]
		expectedValue = hmm$model$parms$mu[state]
		standardDeviation = hmm$model$parms$sigma[state]
		threshold = nonContextualThreshold + 3 * standardDeviation
		if (is.na(prediction$x[i]) == FALSE) {
			difference = abs(prediction$x[i] - expectedValue)
			anomalyStrength = difference / (2 * threshold)
			if (anomalyStrength > 1) {
				anomalyStrength = 1.0
			}
			if (is.na(prediction$x[i]) == FALSE && (difference > threshold)) {
				numAnomalies = numAnomalies + 1
				if (n == 1) {
					anomaliesThreshold1[length(anomaliesThreshold1) + 1] = i
				} else if (n == 2) {
					anomaliesThreshold2[length(anomaliesThreshold2) + 1] = i
				} else if (n == 3) {
					anomaliesThreshold3[length(anomaliesThreshold3) + 1] = i
				} else if (n == 4) {
					anomaliesThreshold4[length(anomaliesThreshold4) + 1] = i
				} else if (n == 5) {
					anomaliesThreshold5[length(anomaliesThreshold5) + 1] = i
				} else if (n == 6) {
					anomaliesThreshold6[length(anomaliesThreshold6) + 1] = i
				}
				output[length(output) + 1] = sprintf("1,%.2f", anomalyStrength)
			} else {
				output[length(output) + 1] = sprintf("0,%.2f", anomalyStrength)
			}
		} else {
			output[length(output) + 1] = sprintf("1,%.2f", 1.0)
		}
	}
	writeLines(output, anomalyFile)
	close(anomalyFile)
}


######################################################################################################
# Graphing Scripts
######################################################################################################

# Graphing the means from a years worth of data by day
year = 2007
dayMeans <- c()
for (day in 1:31) {
	for (month in 1:12) {
		dayPower = getAverageForDay(dataset, sprintf("%d/%d/%d", day, month, year))
		if (is.na(dayPower) == FALSE) {
			dayMeans[length(dayMeans) + 1] = dayPower
			print(sprintf("Completed day: %d", length(dayMeans)))
		}
	}
}
plotData(dayMeans, 1, length(dayMeans))

weekMeans <- c()
week <- c()
day = 1
for (i in 1:length(dayMeans)) {
	week[length(week) + 1] = dayMeans[i]
	day = day + 1
	if (day > 7) {
		day = 1
		weekMeans[length(weekMeans) + 1] = mean(week)
		week <- c()
	}
}
plotData(weekMeans , 1, length(weekMeans ))

# Graphing the means from a years worth of data by day
year = 2009
monthMeans <- c()
for (month in 1:12) {
	monthlyPower = getAverageForMonth(dataset, month, year)
	if (is.na(monthlyPower) == FALSE) {
		monthMeans[length(monthMeans) + 1] = monthlyPower
	}
}
plotData(monthMeans, 1, length(monthMeans))