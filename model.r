# =====
# Model
# =====

# Define individuals
human_population = 100 * 1000
mosquito_population = 100 * human_population
human    <- create_individual('human', human_population)
mosquito <- create_individual('mosquito', mosquito_population)

# Define states
S       <- create_state(human, "S", human_population)
I       <- create_state(human, "I", 0)
Treated <- create_state(human, "T", 0)
D       <- create_state(human, "D", 0)
A       <- create_state(human, "A", 0)
U       <- create_state(human, "U", 0)

E       <- create_state(mosquito, "E", mosquito_population)
L       <- create_state(mosquito, "L", 0)
P       <- create_state(mosquito, "P", 0)
Sm      <- create_state(mosquito, "Sm", 0)
Im      <- create_state(mosquito, "Im", 0)

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

# Define variables
# TODO

#variables = c(last_bitten)
blood_meal_rate <- create_constant(mosquito, function() {return 1}) # Not correct
constants = c(initial_age, blood_meal_rate)

# Define parameters
b0  <- 0.590076
b1  <- 0.5
Ibo <- 43.8787
kb  <- 2.15506
rd  <- 1/5
ra  <- 1/195
ru  <- 1/110
rt  <- 1/5
de  <- #??
ft  <- 1/2 #What to set??

gamma <- #carrying capacity parameter
delta <- #mean time between feeds

# How do we do distributions?

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

#TODO: repeated code
infection_w_immunity_function <- function(source_humans, timestep) {
  age <- source_humans$get_constant(initial_age) + timestep / 365
  labmda = force_of_infection(age, timestep)
  phi = immunity(age, source_humans$get_variable(last_bitten))
  random <- runif(length(susceptable), 0, 1)
  return random > (lambda * (1 - phi))
}

fixed_probability <- function(p) {
  return function(source_humans, timestep) {
    random <- runif(length(source_humans), 0, 1)
    return random > p
  }
}

mosquito_infection_function <- function(susceptable_mosquitos, timestep) {
  lambda <- mosquito_force_of_infection(
    susceptable_mosquitos$get_constant(blood_meal_rate),
    timestep
  )
  random <- runif(length(susceptable), 0, 1)
  return random > lambda
}

# Define processes
processes <- c(
  create_process("Infection", S, I, infection_function),
  create_process("Untreated Progression", I, D, fixed_probability(1 - ft)),
  create_process("Treatment", I, Treated, fixed_probability(ft)),
  create_process("Asymptomatic Progression", D, A, fixed_probability(rd)),
  create_process("Subpatient Progression", A, U, fixed_probability(ra)),
  create_process("Subpatient Recovery", U, S, fixed_probability(ru)),
  create_process("Treatment Recovery", Treated, S, fixed_probability(rt)),
  create_process("Asymptomatic Infection", S, A, infection_w_immunity_function),
  create_process("Subpatient Infection", U, I, infection_function),
  create_process("Subpatient Asymptomatic Infection", U, A, infection_w_immunity_function),
  create_process("Infection from Asymptomatic", A, I, infection_function),
  create_process("Asymptomatic reinfection", A, A, infection_w_immunity_function),

  create_process("Larval growth", E, L, fixed_probability(rel)),
  create_process("Pupal stage", L, P, fixed_probability(rl)),
  create_process("Susceptable Female Development", P, Sm, fixed_probability(rpl)),
  create_process("Mosquito Infection", Sm, Im, mosquito_infection_function)
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
