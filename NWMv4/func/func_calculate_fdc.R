#Calculate flow duration curve.
#Adapted from rwrfhydro/R/calculate_fdc.R
#(https://github.com/NCAR/rwrfhydro/blob/master/R/calculate_fdc.R)
#
#
#----- FUNCTION SUMMARY -------
# 1. CalcFdc
#      This function calculates the flow exceedance probabilities and
#      adds them to the dataframe.
#      REQUIRED INPUTS:
#           1. Dataframe with streamflow (dataframe)
#      OPTIONAL INPUTS:
#           1. Column name of the column containing streamflow (string)
#              The column name 'vals' is used as default since that is
#              what the USGS data uses.
# 2. CalcFdcSpline
#      This function returns a spline function that can be used to
#      estimated flow for a specified exceedance threshold.
#      REQUIRED INPUTS:
#           1. Dataframe with streamflow (dataframe)
#      OPTIONAL INPUTS:
#           1. Column name of the column containing streamflow (string)
#              The column name 'vals' is used as default since that is
#              what the USGS data uses.
#------------------------------


#--Calculate the flow exceedance probabilities for a single location--

CalcFdc <- function(flowDF, colName) {
    flowData <- rank(-flowDF[, colName], na.last='keep')
    flowDF[, paste0(colName, '.fdc')] <- NA
    flowDF[, paste0(colName, '.fdc')] <- flowData/(sum(!is.na(flowDF[, colName]))+1)

    flowDF
}

#-----------------------------------------------------------------

#---Generate a spline-fit funciton for a flow duration curve.-----

# This can be used to estimate the flow value that is exceeded x% of time.

# Example, return flow for exceedance value of 20% (entered as 0.2):
# flowSpline <- CalcFdcSpline(flowDataFrame, 'flowColumnName')
# flow20PcntExceedance <- flowSpline(0.2)

CalcFdcSpline <- function(flowDF, colName) {
    flowSpline <- splinefun(flowDF[, paste0(colName, '.fdc')], flowDF[, colName],
                                method = 'natural')
    flowSpline
}

#------------------------------------------------------------------
