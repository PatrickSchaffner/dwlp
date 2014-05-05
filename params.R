
###################### CONFIGURATION [EDIT] ######################

# Locations
markets <- c('St. Gallen', 'Gossau', 'Abtwil SG', 'Teufen', 'Wittenbach', 'Waldstatt')
warehouses <- c('Teufen', 'Abtwil SG', 'Wittenbach', 'Waldstatt')

# Demand
demand <- list(
  demand.function(50,30,2), # St. Gallen
  demand.function(40,35,1.5), # Gossau
  demand.function(30,20,0.6), # Abtwil
  demand.function(40,20,0.1), # Teufen
  demand.function(25,25,0.4), # Wittenbach
  demand.function(25,15,0.3) # Waldstatt
)

# Cost parameter
R <- 100 # Relocation cost
L <- 15 # Hourly wage of drivers
T <- 0.7 # Transportation cost per km
F <- 5 # Production cost per unit

# Discount factor
B <- 0.9

# Price process: p' = p + e , e ~ N(0,sd)
price.sd <- 3

# Grid size
price.min <- 0
price.max <- 40
grid.size <- 200

# Iteration tolerance
threshold = 0.0001


###################### LOADING GMAPS DATA ######################

# Read distance & duration from gmaps
gmaps <- gmaps.distancematrix(warehouses,markets)
if (gmaps$error==1) stop(paste('Error: Cannot fetch data from gmaps:',gmaps$message))
distance <- gmaps$distance
duration <- gmaps$duration


###################### VALIDATION ######################

# Read problem dimensions
M <- length(markets)
N <- length(warehouses)

# Validate params
if (M<1) stop('Error: No markets specified.')
if (N<2) stop('Error: Need at least two warehouse locations.')
if (B>=1||B<=0) stop('Error: Discount factor must be between zero and one.')
if (length(demand)!=M) stop('Error: Too few/many demand functions specified.')
if (R<0) stop('Error: Relocation cost must be positive.')
if (L<0) stop('Error: Hourly wage must be positive.')
if (T<0) stop('Error: Transportation cost must be positive.')
if (F<0) stop('Error: Production cost must be positive.')
if (price.min<0) stop('Error: Prices must be positive.')
if (price.max<=price.min) stop('Error: Invalid grid dimension specified.')
if (grid.size<2) stop('Error: Grid size is invalid.')
if (price.sd<0) stop('Error: Invalid standard deviation for prices specified.')
if (price.sd>price.max-price.min) stop('Error: Standard deviation is too large.')


###################### PREPROCESSING ######################

# Calculate transporation cost
cost <- 2*(distance*T+duration*L)+F

# Problem setup
problem <- list(
  markets = M,
  warehouses = N,
  demand = demand,
  discount = B,
  cost = cost,
  relocation_cost = R,
  duration = duration,
  price_min = price.min,
  price_max = price.max,
  grid_length = grid.size,
  sd = price.sd,
  threshold = threshold
)
