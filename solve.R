
# Generates a matrix E[i,j] of conditional probabilities P[p'=j|p=i] where p is the i-th entry
# and p' is the j-th entry in the grid.
# Note: sum(E[i, ]) = 1 for all i
generate.transitionmatrix <- function(grid,drift,sd) {
  N <- length(grid)
  pmin <- grid[1]
  pmax <- grid[N]
  d <- (pmax-pmin)/(2*(N-1))
  E <- matrix(nrow = N, ncol = N)
  for (i in 1:N) {
    p1 <- grid[i]
    PG <- normal.interval(pmin,pmax,p1,sd) # P[pmin<p'<pmax|p=p1]
    for (j in 1:N) {
      p2 <- grid[j]
      lower <- max(pmin,p2-d)
      upper <- min(pmax,p2+d)
      PI <- normal.interval(lower,upper,p1,sd) # P[p2-d<p'<p2+d|p=p1] = P[p'=p2|p=p1]
      E[i,j] = PI/PG # Bayes with P[pmin<p'<pmax|p=p1,p'=p2] = 1 (per definition of pmin,pmax)
    }
  }
  return(E)
}

# Calculates instanteanous payoffs in one period.
# Here no relocations are considered, the warehouse is fixed.
# price := vector of prices in grid
# demand := vector of demand functions
# cost & duration := matrices of transportation costs & durations
generate.payoffs <- function(price,demand,cost,duration) {
  N <- dim(cost)[1] # Number of warehouses in grid
  M <- length(price) # Number of prices in grid
  O <- length(demand) # Number of markets
  p <- matrix(0,nrow=N,ncol=M)
  for (w in 1:N) {
    for (i in 1:M) {
      for (m in 1:O) {
	p[w,i] <- p[w,i] + max(demand[[m]](price[i],duration[w,m])*(price[i]-cost[w,m]) ,0)
      }
    }
  }
  return(p)
}

# Value-function iteration
solve <- function(prob) {
  price <- seq(prob$price_min, prob$price_max, length.out=prob$grid_length)
  payoff <- generate.payoffs(price, prob$demand, prob$cost, prob$duration)
  E <- generate.transitionmatrix(price,problem$drift,problem$sd)
  V <- matrix(1,nrow=prob$warehouses,ncol=prob$grid_length)
  VP <- matrix(0,nrow=prob$warehouses,ncol=prob$grid_length)
  while (max(abs(V-VP))>prob$threshold) {
    VP <- V
    V <- payoff+prob$discount*(VP%*%t(E)) # New Value-function without relocations
    opt <- max.col(t(V)) # Get warehouse with highest payoff (relocation target)
    # Payoff you would get by relocation to optimal location
    relocation_payoff <- V[cbind(opt,1:prob$grid_length)]-prob$relocation_cost
    # Indexes of warehouse-price combinations that have a relocation as optimal policy
    relocations <- which(t(t(V)<relocation_payoff),arr.ind=TRUE)
    r <- dim(relocations)[1]
    if (r>0) for (i in 1:r) {
      w <- relocations[i, 1]
      p <- relocations[i, 2]
      # Apply relocations to V
      V[w,p] <- relocation_payoff[p]
    }
  }
  policy <- matrix(1,nrow=prob$warehouses,ncol=prob$grid_length)*(1:prob$warehouses)
  if (r>0) for (i in 1:r) {
    w <- relocations[i, 1]
    p <- relocations[i, 2]
    policy[w,p] <- opt[p]
  }
  result <- list(
    price = price,
    payoff = payoff,
    valuefunc = V,
    policyfunc = policy 
  )
  return(result)
}