#' Calculate goodness of fit statistics
#' @details This function calculates various goodness of fit statistics. 
#' All statistics are imported from the \code{hydroGOF} package. See \link[hydroGOF]{gof} documentation for further details.
#' Statistics are calculated by feature and model initiallation time.
#' \code{pairedData} dataframe must contain the following columns: 'feature_id','initTime','validTime','leadTime','streamflow_mod','streamflow_obs','modelVersion'. 
#' @param pairedData A dataframe of paired modeled and observed values generated using \code{join_modobs}. See details
#' @param obsColName A character indicating the name of hte column to be used as observed values
#' @param modColName A character indicating the name of hte column to be used as the modeled values
#' @param groupVars Optional, character vector of grouping variables for statistics. Default is by tag, feature_id, and initTime.
#' @param returnStats Optional character vector of statistics to return, returns all if NULL. For a complete list of statistics see \link[hydroGOF]{gof}.  
#' @return A data.frame containing goodness of fit statistics.
#' @import data.table
#' @seealso stash_timeslice \link[hydroGOF]{gof}
#' @examples
#' \dontrun{
#' exampleData <- data.frame(streamflow_obs = 2:11,
#' streamflow_mod = 1:10,
#' tag = 'test1',
#' feature_id = rep(1,10),
#' inittime = '2010-10-01')
#' pairedData <- exampleData
#' }
#' @export
calc_gof <- function(pairedData,
                     obsColName='streamflow_obs',
                     modColName='streamflow_mod',
                     groupVars = c('tag','feature_id','inittime'),
                     returnStats=c('ME', 'RMSE', 'PBIAS', 'NSE', 'r')) {
  
  
  #Check inputs
  if(!any(groupVars %in% names(pairedData))) {
    stop('invalid groupVars argument')
  }
  
  ######################
  #Use data.table to calculate stats by group
  pairedData <- data.table::as.data.table(pairedData)
  pairedData <-   data.table::setkeyv(pairedData,cols = groupVars)
  gofStats <- pairedData[, nwm_gof(obs=get(obsColName),sim=get(modColName),na.rm=TRUE), by=groupVars]
  
  #Clean out NaNs
  gofStats[is.nan(value),value := NA]
  
  #Convert back to data.frame
  gofStats <- as.data.frame(gofStats)
  
  return(gofStats)
}

# File gof.R
# Part of the hydroGOF R package, http://www.rforge.net/hydroGOF/ ; 
#                                 http://cran.r-project.org/web/packages/hydroGOF/
# Copyright 2011-2013 Mauricio Zambrano-Bigiarini
# Distributed under GPL 2 or later

################################################################################
# Author: Mauricio Zambrano-Bigiarini                                          #
################################################################################
# Started: 15-Dic-2008 -> 03 Feb 2009;                                         #
# Updates: 06-Sep-09                                                           #
#          2010                                                                #
#          21-Jan-2011                                                         #
#          08-May-2012                                                         #
################################################################################

######################
#Modified 2017-09-06 by Joe Mills (jmills@ucar.edu)
#Added exception handling to individual statistics
######################

# It computes:
# 'me'        : Mean Error
# 'mae'       : Mean Absolute Error
# 'rms'       : Root Mean Square Error
# 'nrms'      : Normalized Root Mean Square Error
# 'r'         : Pearson Correlation coefficient ( -1 <= r <= 1 )
# 'r.Spearman': Spearman Correlation coefficient ( -1 <= r <= 1 ) 
# 'R2'        : Coefficient of Determination ( 0 <= r2 <= 1 )
#               Gives the proportion of the variance of one variable that
#               that is predictable from the other variable
# 'rSD'       : Ratio of Standard Deviations, rSD = SD(sim) / SD(obs)
# 'RSR'       : Ratio of the RMSE to the standard deviation of the observations
# 'NSE'       : Nash-Sutcliffe Efficiency ( -Inf <= NSE <= 1 )
# 'mNSE'      : Modified Nash-Sutcliffe Efficiency
# 'rNSE'      : Relative Nash-Sutcliffe Efficiency
# 'd'         : Index of Agreement( 0 <= d <= 1 )
# 'md'        : Modified Index of Agreement( 0 <= md <= 1 )
# 'rd'        : Relative Index of Agreement( 0 <= md <= 1 )
# 'PI'        : Persistence Index ( 0 <= PI <= 1 ) 
# 'PBIAS'     : Percent Bias ( -1 <= PBIAS <= 1 )
# 'bR2'       : weighted coefficient of determination
# 'KGE'       : Kling-Gupta efficiency
# 'VE'        : Volumetric efficiency
# 'logNSE'    : Log-transformed NSE
# 'wtNSE'     : Weighted NSE
#' @import hydroGOF
#gof <-function(sim, obs, ...) UseMethod("gof")
nwm_gof <- function (sim, obs, stats = c('QMEAN','ME', 'RMSE', 'PBIAS', 'NSE', 'r', 'logNSE', 'wtNSE', 'KGE'),
                     na.rm=TRUE, do.spearman=FALSE, do.pbfdc=FALSE, 
                     j=1, norm="sd", s=c(1,1,1), method="2012",out.type="single", 
                     lQ.thr=0.7, hQ.thr=0.2, digits=2, ...){
  
  method   <- match.arg(method)
  
  statsFuns <- list(
    QMEAN = function(sim, obs, na.rm,...) {
       mean(sim, na.rm=na.rm)
    },
    ME = function(sim, obs, na.rm,...) {
      tryCatch({hydroGOF::me(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('ME calculation failed with ',e))
                 return(NA)
               })
    },
    MAE    = function(sim, obs, na.rm,...) {
      tryCatch({hydroGOF::mae(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('MAE calculation failed with ',e))
                 return(NA)
               })
    },
    MSE    = function(sim, obs, na.rm,...) {
      tryCatch({hydroGOF::mse(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('MSE calculation failed with ',e))
                 return(NA)
               })
    },
    RMSE   = function(sim, obs, na.rm,...) {
      tryCatch({hydroGOF::rmse(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('RMSE calculation failed with ',e))
                 return(NA)
               })
    },
    NRMSE  = function(sim, obs, na.rm, norm, ...) {
      tryCatch({hydroGOF::nrmse(sim, obs, na.rm=na.rm, norm=norm)},
               error=function(e){
                 warning(paste0('NRMSE calculation failed with ',e))
                 return(NA)
               })
    },
    RSR    = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::rsr(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('RSR calculation failed with ',e))
                 return(NA)
               })
    },
    rSD    = function(sim, obs, na.rm,...) {
      tryCatch({hydroGOF::rSD(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('rSD calculation failed with ',e))
                 return(NA)
               })
    },
    PBIAS  = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::pbias(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('PBIAS calculation failed with ',e))
                 return(NA)
               })
    },
    NSE    = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::NSE(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('NSE calculation failed with ',e))
                 return(NA)
               })
    },
    mNSE   = function(sim, obs, na.rm, j, ...) {
      tryCatch({hydroGOF::mNSE(sim, obs, na.rm=na.rm, j=j)},
               error=function(e){
                 warning(paste0('mNSE calculation failed with ',e))
                 return(NA)
               })
    },
    rNSE   = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::rNSE(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('rNSE calculation failed with ',e))
                 return(NA)
               })
    },
    d      = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::d(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('d calculation failed with ',e))
                 return(NA)
               })
    },
    md     = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::md(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('md calculation failed with ',e))
                 return(NA)
               })
    },
    rd     = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::rd(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('rd calculation failed with ',e))
                 return(NA)
               })
    },
    cp     = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::cp(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('cp calculation failed with ',e))
                 return(NA)
               })
    },
    r      = function(sim, obs, ...) {
      tryCatch({hydroGOF::rPearson(sim, obs)},
               error=function(e){
                 warning(paste0('r calculation failed with ',e))
                 return(NA)
               })
    },
    bR2    = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::br2(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('bR2 calculation failed with ',e))
                 return(NA)
               })
    },
    KGE    = function(sim, obs, na.rm, s, method, out.type="single", ...) {
      tryCatch({hydroGOF::KGE(sim, obs, na.rm=na.rm, s=s, method=method, out.type="single")},
               error=function(e){
                 warning(paste0('KGE calculation failed with ',e))
                 return(NA)
               })
    },
    VE     = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::VE(sim, obs, na.rm=na.rm)},
               error=function(e){
                 warning(paste0('VE calculation failed with ',e))
                 return(NA)
               })
    },
    logNSE    = function(sim, obs, na.rm, ...) {
      tryCatch({hydroGOF::NSE(sim, obs, na.rm=na.rm, FUN=log, epsilon="Pushpalatha2012")},
               error=function(e){
                 warning(paste0('logNSE calculation failed with ',e))
                 return(NA)
               })
    },
    wtNSE    = function(sim, obs, na.rm, ...) {
      tryCatch({0.5*(hydroGOF::NSE(sim, obs, na.rm=na.rm) + hydroGOF::NSE(sim, obs, na.rm=na.rm, FUN=log, epsilon="Pushpalatha2012"))},
               error=function(e){
                 warning(paste0('logNSE calculation failed with ',e))
                 return(NA)
               })
    }
  )
  
  # 'R2' is the Coefficient of Determination
  # The coefficient of determination, R2, is useful because it gives the proportion of
  # the variance (fluctuation) of one variable that is predictable from the other variable.
  # It is a measure that allows us to determine how certain one can be in making
  # predictions from a certain model/graph.
  # The coefficient of determination is the ratio of the explained variation to the total
  # variation.
  # The coefficient of determination is such that 0 <  R2 < 1,  and denotes the strength
  # of the linear association between x and y.
  # R2 <- tryCatch({r^2},
  #                #warning=function(w){},
  #                error=function(e){
  #                  warning(paste0('R2 calculation failed with ',e))
  #                  return(NA)
  #                })
  # 
  # if (do.spearman) {
  #   tryCatch({
  #     r.Spearman <- cor(sim, obs, method="spearman", use="pairwise.complete.obs")
  # 
  #     # if 'sim' and 'obs' were matrixs or data.frame, then the correlation
  #     # between observed and simulated values for each variable is given by the diagonal of 'r.Pearson'
  #     if ( is.matrix(r.Spearman) | is.data.frame(r.Spearman) ) {
  #       r.Spearman        <- diag(r.Spearman)
  #     } # IF end
  #   },
  #   #warning=function(w){},
  #   error=function(e){
  #     warning(paste0('r.Spearman calculation failed with ',e))
  #     return(NA)
  #   })
  # 
  # } # IF end
  
  # if (do.pbfdc) {
  #   tryCatch({
  #     pbfdc  <- hydroGOF::pbiasfdc(sim, obs, na.rm=na.rm, lQ.thr=lQ.thr, hQ.thr=hQ.thr, plot=FALSE, ...)
  #   },
  #   #warning=function(w){},
  #   error=function(e){
  #     warning(paste0('pbfdc calculation failed with ',e))
  #     return(NA)
  #   })
  # }
  
  # gof <- rbind(ME, MAE, MSE, RMSE, NRMSE, PBIAS, RSR, rSD, NSE, mNSE, rNSE, d, md, rd, cp, r, R2, bR2, KGE, VE)
  #
  # rownames(gof)[5] <- "NRMSE %"
  # rownames(gof)[6] <- "PBIAS %"
  
  # if (do.spearman) { gof <- rbind(gof, r.Spearman) }
  
  # if (do.pbfdc) {
  #   gof <- rbind(gof, pbfdc)
  #   rownames(gof)[length(rownames(gof))] <- "pbiasFDC %"
  # } # IF end
  
  # Rounding the final results, ofr avoiding scientific notation
  # gof <- round(gof, digits)
  
  #Use do.call to evaluate selected statistics
  statsOut <- lapply(statsFuns[stats], do.call, list(sim=sim,
                                                     obs=obs,
                                                     na.rm=na.rm,
                                                     norm=norm,
                                                     j=j,
                                                     s=s, 
                                                     method=method, 
                                                     out.type="single"))
  #Convert to data.frame
  statsOut <- as.data.table(statsOut)
  
  #Reshape to long format
  statsOut <- data.table::melt(statsOut,id.vars=NULL)
  names(statsOut) <- c('statistic','value')
                       
  return(statsOut)
  
}

