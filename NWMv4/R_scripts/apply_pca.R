# apply principal component analysis to basin attributes and return the PCA results

apply_pca <- function(recs0, recs1,dtAttr0, attrs) {
  
  require(psych)
  
  # get the valid attributes for the first receiver to be processed this round
  dt1 <- subset(dtAttr0, (id %in% recs0) & (!id %in% recs1) & (tag=="receiver"))[1,]
  vars <- names(dt1)[!is.na(dt1)]
  dtAttr <- dtAttr0[,vars,with=F]
  vars <- vars[vars %in% attrs]     # attrs included for current round
  vars0 <- attrs[!attrs %in% vars]  # attrs excluded for current round
  
  if (length(vars)==0) return(list())
  
  if (length(vars0)>0) {
    message(paste0("Excluding ", length(vars0)," attributes: ", paste(vars0,collapse=",")))
  } else {
    message("Using all attributes")
  }
  
  # ignore donors & receivers with NA attribute values
  dtAttr <- na.omit(dtAttr)
  
  # reduce table to only attributes that are to be used
  dtAttr1 <- dtAttr[,vars, with=FALSE]
  
  # standardize the data 
  dtAttr2 <- scale(dtAttr1)
  
  # Conduct PCA to remove correlation between attributes
  # first conduct a preliminary PCA with princomp to help decide
  # how many principle components to use
  pca <- princomp(dtAttr2,cor=TRUE)
  pvar <- (pca$sdev)^2/sum((pca$sdev)^2)
  pvar <- rbind(pvar,cumsum(pvar))
  rownames(pvar) <- c('var','cum_var')
  colnames(pvar) <- 1:ncol(dtAttr1)
  
  # decide which PCs to use base on variance explained by each individual PC
  # keep all PCs that explain >10% variance; total variance explained should be larger than 80%
  nc1 <- max(which(pvar[1,]>=0.1)) #number of PCs to use
  if (cumsum(pvar[2,nc1]<0.8)) nc1 <- min(which(pvar[2,]>=0.8))
  message(paste0("Number of PCs selected: ", nc1))
  message(paste0("PCA total portion of variance explained ... ", cumsum(pvar[2,nc1])))
  
  # conduct refined PCA with "principal" (psych package) given the chosen number of PCs
  pca1 <- principal(dtAttr2,nfactors=nc1,rotate="varimax")
  
  # Get the PCA scores for calculating Gower's distance
  mydata <- pca1$scores
  
  # weights for chosen PCs are proporitonal to the variances they explained
  w1 <- pca1$Vaccounted["Proportion Explained",]
  
  # check the loadings corresponding to each PC (if needed)
  if (FALSE) {
    source("func_plot_loadings.R")
    plot_loadings(pca1)
  }
  
  return(list(dtAttr=dtAttr,scores=mydata, weights=w1))
}
