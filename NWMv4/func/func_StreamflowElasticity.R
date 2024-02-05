StreamflowElasticity <- function(dfRR){

  # dfRR: 1st column - streamflow; 2nd column - precip;
  # 3rd column: Date in POSIXct format

  tmpdf<-dfRR
  #tmpdf$flow <-(tmpdf$flow*3600/WS_area)*1000 # to mm
  gbdf <- dplyr::group_by(tmpdf,WY)
  df1  <- dplyr::summarise(gbdf,sum_PCP  = sum(p_mean,na.rm=T),
                           sum_q = sum(flow,na.rm=T),.groups = 'drop')
  df1 <- dplyr::ungroup(df1)
  dP <- diff(df1$sum_PCP)
  mP <- mean(df1$sum_PCP)
  dQ <- diff(df1$sum_q)
  mQ <- mean(df1$sum_q)

  se <- quantile(dQ/dP * (mP/mQ),probs=0.5) 
  return(se)
}   

