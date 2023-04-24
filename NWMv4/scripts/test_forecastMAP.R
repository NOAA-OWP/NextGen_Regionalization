forecastMAP <- function(QPF, OBS, FCST, config, parameters, randomGen, eventIndex, nMember) {
  
  precipThreshold = config$precipThreshold
  parametersDayOfYear = config$JulianDay
  
  # Find the observed values for time steps for which both OBS and FCST
  # are positive (s)
  posOBS <- which(!is.nan(OBS) & OBS>=precipThreshold)
  posFCST <- which(!is.nan(FCST) & FCST>=precipThreshold)
  zeroOBS <- which(!is.nan(OBS) & OBS<precipThreshold)
  zeroFCST <- which(!is.nan(FCST) & FCST<precipThreshold)
  bothPositiveIndex = intersect(posOBS, posFCST)
  positiveOBS <- OBS[bothPositiveIndex]
  positiveFCST <- FCST[bothPositiveIndex]
  sortedPositiveOBS = sort(positiveOBS)
  sortedPositiveFCST = sort(positiveFCST)
  
  # FCST is positve but OBS is zero
  O0_F1 <- FCST[intersect(posFCST, zeroOBS)]
  sortedO0_F1 <- sort(O0_F1)
  
  # OBS is positive but FCST is zero
  O1_F0 <- OBS[intersect(posOBS, zeroFCST)]
  sortedO1_F0 <- sort(O1_F0)
  
  # Find the parameter day closest to the forecast day
  dayCalculation = abs(parameters$availableParameterDays - parametersDayOfYear)
  dayIndex <- which.min(dayCalculation)
  #[~, dayIndex] = min(dayCalculation);
  
  # Extract the parameters that match the forecastday and the canonical
  # event
  p00 <- parameters$EPT$BivariateProbOfNoPrecip[eventIndex,dayIndex]
  p10 <- parameters$EPT$POPFcst[eventIndex,dayIndex]
  p01 <- parameters$EPT$POPObs[eventIndex,dayIndex]
  
  # Calculate the probability of both (OBS + FCST) positive
  p11 = 1 - p00 - p01 - p10
  
  if (QPF <  precipThreshold) {
    condProbOfNoRain <- p00 / (p00 + p01)
    numPosMembers <- round(nMember * (1 - condProbOfNoRain))
    if (numPosMembers > 0) {
      ensembleValues <- rep(0,numPosMembers)
      for (i in 1:numPosMembers) {
        # TODO: This assumes stratified sampling is set to false, which is 
        # only the case for hindcasting)
        probability = runif(1)
        indexToUse = floor(probability*length(O1_F0)) + 1;
        # TODO: The O1_F1 list is not sorted. In JAVA the list is in a
        # different order, causing different results. Not sure how to
        # mimic the JAVA order in MATLAB
        ensembleValues[i] = O1_F0[indexToUse];
      }
    } else {
      ensembleValues <- NULL
      #ensembleValues = []
    }
    
    # If there are only a limited number of cases where both
    # precipitation was forecasted and observed use an approach that
    # doesn't rely on the GEFS forecast.  
  } else if (length(sortedPositiveFCST) < config$MIN_SAMPLE_SIZE_1) {
    
    # Calculate the conditional probability of rain; avoid division by zero
    condProbOfRain <- ifelse ((p10 + p11) == 0, 0, p11 / (p10 + p11)) 
    
    # Use the probablity of rain to set the number of traces that will
    # show precipitation
    numPosMembers <- round(nMember * condProbOfRain)
    
    # Use Stratified Random Sampling to select values from the positive
    # observations to populate the forecast ensemble
    ensembleValues <- rep(0,numPosMembers)
    for (i in 1:numPosMembers) {
      probability <- (i-1)/numPosMembers + runif(1)/numPosMembers
      indexToUse = floor(probability*length(sortedPositiveOBS)) + 1
      tmp <- sortedPositiveOBS(indexToUse)
      if (length(tmp)==0) { # Seeing where and why the statement below doesn't always work
        message("Unable to generate forecast ensemble for GEFS (event will be skipped)")
      } else { 
        ensembleValues[i] = tmp
      }
    }
  } else {
    #if length(sortedO1_F0) >= config.MIN_SAMPLE_SIZE_2
    if (length(sortedO0_F1) >= config$MIN_SAMPLE_SIZE_2) {
      numPosMembers <- nMember
      isStrictEPT <- TRUE
      # Calculate the probability of observed precip without forecasted precip
      O1_F0_scale <- parameters$EPT$ObsZeroFcstDistribution$scale[eventIndex,dayIndex]
      O1_F0_shape <- parameters$EPT$ObsZeroFcstDistribution$shape[eventIndex,dayIndex]
      O1_F0_shift <- parameters$EPT$ObsZeroFcstDistribution$shift[eventIndex,dayIndex]
      O1_F0_prob <- pgamma(QPF-O1_F0_shift, shape=O1_F0_shape, scale=O1_F0_scale)
      #O1_F0_prob = gampdf(QPF-O1_F0_shift, O1_F0_shape, O1_F0_scale);
      
      # Calculate the probability of observed precip with forecasted precip
      O1_F1_scale <- parameters$EPT$FcstIntermittencyDistribution$scale[eventIndex,dayIndex]
      O1_F1_shape <- parameters$EPT$FcstIntermittencyDistribution$shape[eventIndex,dayIndex]
      O1_F1_shift <- parameters$EPT$FcstIntermittencyDistribution$shift[eventIndex,dayIndex]
      O1_F1_prob <- pgamma(QPF-O1_F1_shift, O1_F1_shape, O1_F1_scale)
      
      # Check the values are reasonable
      if (O1_F0_prob > 1000 | is.na(O1_F0_prob)) {
        isStrictEPT <- FALSE
        bx1 <- p00 + p01
        bx2 <- 1 - bx1
        by1 <- p00 + p10
        by2 <- 1 - by1
      } else {
        # Use probabilities to calculate the number of positive members
        if (O1_F1_prob > 0) {
          probOfPrecip <- p11 * O1_F1_prob / (p10 * O1_F0_prob + p11 * O1_F1_prob)
        } else {
          probOfPrecip <- 0
        }
        numPosMembers <- probOfPrecip * nMember
        if (ceiling(numPosMembers) == floor(numPosMembers)) {
          # This check is not required for the calculation, but makes
          # sure the random number generator tracks with JAVA.
          numPosMembers <- floor(numPosMembers)
        } else {
          probFloor <- ceiling(numPosMembers) - numPosMembers
          if (runif(1) < probFloor) {
            numPosMembers <- floor(numPosMembers)
          } else {
            numPosMembers <- ceiling(numPosMembers)
          }
        }
      }  # Check for reasonable probability values
    } else { # O1_F0 is small
      numPosMembers <- nMember
      isStrictEPT <-  FALSE
      bx1 <- p00 + p01
      bx2 <- 1 - bx1
      by1 <- p00 + p10
      by2 <- 1 - by1
    }
    FCST_scale <- parameters$EPT$FcstBivarMarginalDistribution$scale[eventIndex,dayIndex]
    FCST_shape <- parameters$EPT$FcstBivarMarginalDistribution$shape[eventIndex,dayIndex]
    FCST_shift <- parameters$EPT$FcstBivarMarginalDistribution$shift[eventIndex,dayIndex]
    
    OBS_scale <- parameters$EPT$ObsBivarMarginalDistribution$scale[eventIndex,dayIndex]
    OBS_shape <- parameters$EPT$ObsBivarMarginalDistribution$shape[eventIndex,dayIndex]
    OBS_shift <- parameters$EPT$ObsBivarMarginalDistribution$shift[eventIndex,dayIndex]
    
    rho <- parameters$EPT$Rho[eventIndex,dayIndex]
    
    # Transform the forecasted value to standard-normal space (z-space)
    if (isStrictEPT) {
      prob <- pgamma(QPF-FCST_shift,FCST_shape,FCST_scale)
    } else {
      prob <- bx1 + bx2 * pgamma(QPF-FCST_shift,FCST_shape,FCST_scale)
    }
    zQPF <- qnorm(prob)
    
    # Assign values one at a time
    ensembleValues <- rep(0,numPosMembers)
    for (i in 1:numPosMembers) {
      if (config$reforecasting)  { # Reforcasting doesn't use stratified sampling
        zValue <- rnorm(1)  #TODO: check if rnorm function is equivalent to nextGaussian
        #zValue = randomGen.nextGaussian();
      } else {
        randomValue <- runif(1) #TODO: check if runif is equivalent to Java nextDouble
        #randomValue = randomGen.nextDouble();
        probability <- (i-1)/numPosMembers + randomValue/numPosMembers
        zValue <- qnorm(probability)
      }
      w <- rho * zQPF + sqrt(1 - rho^2) * zValue
      
      # The maximum limit is set here in order to mimic JAVA results
      cdfValue <- min(pnorm(w), 0.99999)
      if (isStrictEPT) {
        ensembleValues[i] <- qgamma(cdfValue, OBS_shape, OBS_scale) + OBS_shift
      } else {
        ensembleValues[i] <- qgamma(cdfValue - by1, OBS_shape, OBS_scale) / by2 + OBS_shift
      }
      if (ensembleValues[1] < precipThreshold) ensembleValues[i] <- precipThreshold
    }  # assign values
    # sort the ensemble values
    ensembleValues <- sort(ensembleValues)
  }  # determining how to fill the forecast ensemble
  
  # Add the traces to match the probability of no rain
  forecastEventEnsemble <- rep(0,nMember - numPosMembers)
  forecastEventEnsemble <- c(forecastEventEnsemble, ensembleValues) 
} # function forecastMAP
