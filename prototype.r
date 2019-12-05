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
last_bitten <- create_variable(human, "last_bitten", 0, function() {}) #TODO, how do I write this?
initial_age <- create_constant(human, "initial_age", runif(length(human), 0, 30))

variables = c(last_bitten)
constants = c(initial_age)

#Define parameters
b0  <- 0.590076
b1  <- 0.5
Ibo <- 43.8787
kb  <- 2.15506
rd  <- 1/5
ra  <- 1/195
ru  <- 1/110
rt  <- 1/5
ft  <- 1/2 #What to set??

# Process functions return a vector of individuals to transfer given the model state
# The first argument is an object exposing the individuals of the source state
# The accepted methods are:
#   get_state (will all be of the source state)
#   get_constant
#   get_variable
# The second argument is the timestep of the simulation
infection_function <- function(susceptable, timestep) {
  age <- susceptable$get_constant(initial_age) + timestep / 365
  labmda = force_of_infection(age, timestep)
  phi = immunity(age, susceptable$get_variable(last_bitten))
  random <- runif(length(susceptable), 0, 1)
  return random > (lambda * phi)
}

untreated_progression_function <- function(infected, timestep) {
  random <- runif(length(infected), 0, 1)
  return random > (1 - ft)
}

treatment_function <- function(infected, timestep) {
  random <- runif(length(infected), 0, 1)
  # are these always uniform distribution? If so, it could become part of the simulation
  return random > ft
}

asymptomatic_progression_function <- function(diseased, timestep) {
  random <- runif(length(diseased), 0, 1)
  # are these always uniform distribution? If so, it could become part of the simulation
  return random > rd
}

#Other functions...

#Define processes
processes <- c(
  create_process("Infection", S, I, infection_function),
  create_process("Untreated Progression", I, D, untreated_progression_function),
  create_process("Treatment", I, Treated, treatment_function)
  create_process("Asymptomatic Progression", D, A, asymptomatic_progression_function)
  #Other processes... 
)

#TODO: add mosquito processes

simulation <- simulate(
  individuals,
  states,
  constants,
  variables,
  processes,
  365*10 # 10 years
)


#=================
#Utility functions
#=================
force_of_infection <- function(age, timestep) {
  #Calculate the unique biting rate (psi) from age
  #Calculate the mean EIR (epsilon0) from time (how??)
  #Sample the relative biting rate (xi) from a normal distribution
  #Calculate immunity level (b) (why?? isn't this part of phi?)
  #TODO: implement
  return rep(1, length(timestep_last_bitten))
}

immunity <- function(age, timestep_last_bitten) {
  #Calculate acquired immunity from last_bitten
  #Calculate and maternal immunity from age
  #Then calculate immunity using parameters
  #TODO: implement
  return rep(1, length(timestep_last_bitten))
}
