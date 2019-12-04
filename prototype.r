# =======
# Model
# =======

#Define individuals
human    <- create_individual('human', 10000000)
mosquito <- create_individual('mosquito', 1000000000)

#Define states
S       <- create_state(human, "S", 10000000)
I       <- create_state(human, "I", 0)
Treated <- create_state(human, "T", 0)
D       <- create_state(human, "D", 0)
A       <- create_state(human, "A", 0)
U       <- create_state(human, "U", 0)

E       <- create_state(mosquito, "E", 1000000000)
L       <- create_state(mosquito, "L", 0)
P       <- create_state(mosquito, "P", 0)


states <- c(
  S,
  I,
  Treated,
  D,
  A,
  U,
  E,
  L,
  P
)

#Define variables
pre_erythrocytic_immunity <- create_variable(human, "Ib", 0, function() {}) #TODO, how do I write this?
initial_age               <- create_constant(human, "initial_age", runif(length(human), 0, 30))

variables = c(pre_erytrocytic_immunity)
constants = c(initial_age)

#Define parameters
b0  <- 0.590076
b1  <- 0.5
Ibo <- 43.8787
kb  <- 2.15506

# Process functions return a vector of individuals to transfer given the model state
# The first argument is an object exposing the individuals of the source state
# The accepted methods are:
#   get_state (will all be of the source state)
#   get_constant
#   get_variable
# The second argument is the timestep of the simulation
#TODO: not finished
infection_function <- function(susceptable, timestep) {
  age <- susceptable$get_constant(initial_age) + timestep / 365
  unique_biting_rate <- 1 - rho * exp(-age/a0)
  EIR <- 1 #some kind of function of time?
  prob_bitten <- unique_biting_rate * EIR
  prob_develop <- b0 * 
    (
      b1 +
      (1 - b1)/
      1 + (susceptable$get_variable(pre_erythocytic_immunity)/Ibo)**kb
    )
  p <- prob_bitten * prob_develop
  random <- runif(length(susceptable), 0, 1)
  return random > p
}

immunity <- function(last_bitten, age) {
  #Calculate acquired immunity from last_bitten
  #Calculate and maternal immunity from age
  #Then calculate immunity using parameters
  #TODO: implement
  return rep(1, length(last_bitten))
}

#Define processes
processes <- c(
  create_process("Infection", S, I, infection_function)
)

#TODO: try a mosquito

simulation <- simulate(
  individuals,
  states,
  constants,
  variables,
  processes,
  365*10 # 10 years
)
