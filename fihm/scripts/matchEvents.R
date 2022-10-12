# Match observed events with model events
#
# Output is a list with 4 variables: 
# 1) events_obs_match (data.table)
#    observed events that are matched with model events
# 2) events_mod_match (data.table) 
#    model events that are matched with obs events
#
# 3) matched_mod (vector) 
#    values indicating whether the orginal model events are matched
#    1: one or more obs events found (combine obs events if more than one found)
#    0: no obs event found to match
#    NA: obs data is missing at model event peak 
#
# 4) matched_obs (vector)
#    values indicating whether the orginal obs events are matched
#    1: one of more model events found (combine model events if more than one found)
#    0: no model event found to match
#

matchEvents <- function(data_obs,data_mod,events_obs, events_mod) {

library(data.table)

no1 <- nrow(events_obs)
nm1 <- nrow(events_mod)
match_mod <- rep(0,nm1)
match_obs <- rep(0,no1)
events_mod1 <- events_obs1 <- data.table()

if (no1>=1 & nm1>=1) {
events_obs <- events_obs[order(start),]
events_mod <- events_mod[order(start),]

# loop through observed events to find matches in the model events
# match is identified if model peak is within the observed event period
for (i1 in 1:no1) {

dates1 <- seq(events_obs$start[i1],events_obs$end[i1],by="hour")
ix0 <- which(events_mod$peak %in% dates1)
if (length(ix0)>0) {
  events_obs1 <- rbind(events_obs1,events_obs[rep(i1,length(ix0)),])
  events_mod1 <- rbind(events_mod1,events_mod[ix0,])
  match_obs[i1] <- 1
  match_mod[ix0] <- 1
}}

# loop through model events to find additional matches of obs events
# match is identified if obs peak is within the model event period
for (i1 in 1:nm1) {
dates1 <- seq(events_mod$start[i1],events_mod$end[i1],by="hour")
ix0 <- which((events_obs$peak %in% dates1) & (match_obs==0))
if (length(ix0)>0) {
  events_mod1 <- rbind(events_mod1,events_mod[rep(i1,length(ix0)),])
  events_obs1 <- rbind(events_obs1,events_obs[ix0,])
  match_mod[i1] <- 1
  match_obs[ix0] <- 1
}}

if (nrow(events_obs1)>0) {

# sort events in order
events_obs1 <- events_obs1[order(peak),]
events_mod1 <- events_mod1[order(peak),]

# combine events if duplicated starts, peaks, ends
while(1) {
ix1 <- which(duplicated(events_mod1$start))
ix1 <- c(ix1,which(duplicated(events_mod1$peak)))
ix1 <- c(ix1,which(duplicated(events_mod1$end)))
ix1 <- c(ix1,which(duplicated(events_obs1$start)))
ix1 <- c(ix1,which(duplicated(events_obs1$peak)))
ix1 <- c(ix1,which(duplicated(events_obs1$end)))
ix1 <- sort(unique(ix1))
if (length(ix1)==0) break

# combine obs events
events_obs1$start[ix1[1]-1] <- min(events_obs1$start[ix1[1]-1],events_obs1$start[ix1[1]])
events_obs1$end[ix1[1]-1] <- max(events_obs1$end[ix1[1]-1],events_obs1$end[ix1[1]])
peak0 <- data_obs$value[match(events_obs1$peak[ix1[1]-1],data_obs$time)]
peak1 <- data_obs$value[match(events_obs1$peak[ix1[1]],data_obs$time)]
if (peak0<peak1) events_obs1$peak[ix1[1]-1] <- events_obs1$peak[ix1[1]]
events_obs1[,nhour:=as.integer(difftime(end,start,units="hour"))+1]
events_obs1[,nrise:=as.integer(difftime(peak,start,units="hour"))+1]
events_obs1[,nrece:=as.integer(difftime(end,peak,units="hour"))]

# combine model events
events_mod1$start[ix1[1]-1] <- min(events_mod1$start[ix1[1]-1],events_mod1$start[ix1[1]])
events_mod1$end[ix1[1]-1] <- max(events_mod1$end[ix1[1]-1],events_mod1$end[ix1[1]])
peak0 <- data_mod$value[match(events_mod1$peak[ix1[1]-1],data_mod$time)]
peak1 <- data_mod$value[match(events_mod1$peak[ix1[1]],data_mod$time)]
if (peak0<peak1) events_mod1$peak[ix1[1]-1] <- events_mod1$peak[ix1[1]]
events_mod1[,nhour:=as.integer(difftime(end,start,units="hour"))+1]
events_mod1[,nrise:=as.integer(difftime(peak,start,units="hour"))+1]
events_mod1[,nrece:=as.integer(difftime(end,peak,units="hour"))]

events_obs1 <- events_obs1[-ix1[1],]
events_mod1 <- events_mod1[-ix1[1],]
}}

# for model events where observation is missing, mark with NA 
for (i1 in 1:nm1) {
  if (match_mod[i1]==1) next
  if (is.na(match(events_mod$peak[i1],data_obs$time))) match_mod[i1] <- NA 
}
}

return(list(events_obs_match=events_obs1,
            events_mod_match=events_mod1,
            matched_mod=match_mod,
            matched_obs=match_obs))
}
