# Calculate the slope of the center of the FDC (Boscarello paper)

#--------------FUNCTION SUMMARY --------------
# 1. Calc_FDC_Slope
#    REQUIRED INPUTS
#        1. Spline function of the flow
#    OPTIONAL INPUTS
#        1. low_exceedance_probability (the exceedance probability for the high flow point of interest)
#        2. high_exceedance_probability (the exceedance probability for the low flow point of interest)
#     DEPENDENCIES
#         None
#
#---------------------------------------------

Calc_FDC_Slope <- function(splineFlow, low_exceedance_probability = 0.33,
                                       high_exceedance_probability = 0.66) {
    slopeFDC <- (log(splineFlow(low_exceedance_probability)) - 
                  log(splineFlow(high_exceedance_probability))) / 
                  (high_exceedance_probability - low_exceedance_probability)
    return(slopeFDC)
}
