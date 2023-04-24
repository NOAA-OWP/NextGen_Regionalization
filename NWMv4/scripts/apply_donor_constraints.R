# apply a few constraints to donors 
apply_donor_constraints <- function(rec, donors, dists, pars, dtAttr) {
  
  # 1. narrow down to donors with the same snowiness category
  snowy <- subset(dtAttr,id==rec & tag=="receiver")$snowy
  snowy1 <- subset(dtAttr,id%in%donors & tag=="donor")$snowy
  ix1 <- which(snowy1==snowy)
  if (length(ix1)>0) {dists <- dists[ix1]; donors <- donors[ix1]}
  
  # 2. further narrow down to those donors within maximum spatial distance defined
  ix1 <- which(dists <= pars$maxSpaDist)
  if (length(ix1)>0) {dists <- dists[ix1]; donors <- donors[ix1]}
  
  # 3. further narrow down based on screening attributes
  for (att1 in names(pars$maxAttrDiff)) {
    ix1 <- which(abs(subset(dtAttr,id==rec & tag=="receiver")[[att1]] - 
                       subset(dtAttr,id%in%donors & tag=="donor")[[att1]])<= pars$maxAttrDiff[[att1]])
    if (length(ix1)>0) {dists <- dists[ix1]; donors <- donors[ix1]}
  }
  
  # 4. further narrow down to donors in the same HSG
  hsg <- subset(dtAttr,id==rec & tag=="receiver")$hsg
  hsg1 <- subset(dtAttr,id%in%donors & tag=="donor")$hsg
  ix1 <- which(hsg1==hsg)
  if (length(ix1)>0) {dists <- dists[ix1]; donors <- donors[ix1]}
  
  return(list(donor=donors, dist=dists))
  
} #function apply_donor_constraints