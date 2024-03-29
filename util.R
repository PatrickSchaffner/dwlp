library(rjson)

# Generate linear demand-functions with exponential time-sensitivity
# Q := demand if p = 0
# P := price where demand becomes 0
# A := sensitivity to delivery-time
demand.function <- function(Q,P,A) {
  e <- Q/P
  f <- function (p,t) {
    return(max(0,(Q-e*p)*exp(-A*t)))
  }
  return(f)
}

# Calls the gmaps distancematrix API and returns two SxM matrices with distances and durations
# origins := warhouse-locations
# destinations := markets
gmaps.distancematrix <- function (origins,destinations) {
  json <- gmaps.distancematrix.fetchjson(origins,destinations)
  if (json$status!='OK') return(list(error=1, message=json$status))
  origins <- json$origin_addresses
  destinations <- json$destination_addresses
  distance <- matrix(nrow=length(origins),ncol=length(destinations))
  duration <- matrix(nrow=length(origins),ncol=length(destinations))
  for (o in 1:length(origins)) {
    for (d in 1:length(destinations)) {
      e <- json$rows[[o]]$elements[[d]]
      if (e$status!='OK') return(list(error=1, message=paste(
	e$status, ':', origins[o], '-', destinations[d])))
      distance[o,d] <- e$distance$value/1000
      duration[o,d] <- e$duration$value/3600
    }
  }
  list(error=0, message='OK', origins = origins, destinations = destinations, distance = distance, duration = duration)
}

# Reads JSON model from GMaps
gmaps.distancematrix.fetchjson <- function (o,d) {
  o <- URLencode(paste(o, collapse='|'))
  d <- URLencode(paste(d, collapse='|'))
  url <- paste('http://maps.googleapis.com/maps/api/distancematrix/json?origins=',
    o, '&destinations=', d, '&sensor=false', collapse='|',sep='')
  fromJSON(file=url)
}

# Returns the probability that lower < X < upper where X ~ N(mean,sd)
normal.interval <- function(lower,upper,mean,sd) {
  return(pnorm(upper,mean=mean,sd=sd)-pnorm(lower,mean=mean,sd=sd))
}

# Generates plots for a given warehouse. It shows profits generated by markets.
generate.demandseries <- function(demand,cost,duration,price) {
  P <- length(price)
  M <- length(demand)
  s <- matrix(0,nrow=P,ncol=M)
  for (p in 1:P) for (m in 1:M) {
    s[p,m] <- max(demand[[m]](price[p],duration[m])*(price[p]-cost[m]),0)
  }
  return(s)
}

# Plot demandseries for up to 12 warehouses
plot.demandseries <- function(demand,cost,duration,price,labels) {
  W <- dim(cost)[1]
  if (W>12) return()
  M <- length(demand)
  color <- rainbow(M)
  if (W>9) layout(matrix(1:12,nrow=3,ncol=4,byrow=TRUE))
  else if (W>6) layout(matrix(1:9,3,3,byrow=TRUE))
  else if (W>4) layout(matrix(1:6,nrow=2,ncol=3,byrow=TRUE))
  else if (W>2) layout(matrix(1:4,2,2,byrow=TRUE))
  else if (W==2) layout(matrix(1:2,nrow=1,ncol=2,byrow=TRUE))
  else layout(matrix(1,1,1))
  for (w in 1:W) {
    demandseries <- generate.demandseries(demand,cost[w, ],duration[w, ],price)
    matplot(price,demandseries,type='l',main=labels$warehouse[w],xlab='Price',ylab='Profit',col=color)
    legend('topleft', legend=labels$market, col=color, pch=0)
  }
}