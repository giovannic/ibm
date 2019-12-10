# =====
# Model
# =====

# Define individuals
human_population <- 100 * 1000
mosquito_population <- 100 * human_population
human    <- create_individual('human', human_population)
mosquito <- create_individual('mosquito', mosquito_population)
timestep_to_day <- 1

# Define states
S       <- create_state(human, "S", human_population)
I       <- create_state(human, "I", 0)
Treated <- create_state(human, "T", 0)
D       <- create_state(human, "D", 0)
A       <- create_state(human, "A", 0)
U       <- create_state(human, "U", 0)
# Mosquito states
E       <- create_state(mosquito, "E", mosquito_population)
L       <- create_state(mosquito, "L", 0)
P       <- create_state(mosquito, "P", 0)
Sm      <- create_state(mosquito, "Sm", 0)
Im      <- create_state(mosquito, "Im", 0)

states <- c(
  # Human
  S,
  I,
  Treated,
  D,
  A,
  U,
  # Mosquito
  E,
  L,
  P,
  Sm,
  Im
)


# Define parameters
parameters = list(
  b0    = 0.590076
  b1    = 0.5
  ib0   = 43.8787
  kb    = 2.15506
  rd    = 1/5
  ra    = 1/195
  ru    = 1/110
  rt    = 1/5
  ft    = 1/2 #What to set?? TODO: is this related to theta?
  av1   = .92
  av2   = .74
  av3   = .94
  cd    = 0.068
  ct    = 0.021896
  ca    = 1.82425
  cu    = 0.00062
)

# Define variables
age <- create_variable(
  human,
  "age",
  function() { rexp(human_population, rate=1/10) },
  create_interval_updater(function(a) {return a+1}, 365*timestep_to_day)
)

maternal_immunity <- create_variable(
  human,
  "ICM",
  function() {return 1}
) # Not correct

ib  <- create_variable(
  human,
  "IB",
  function() {return 1},
) # Pre-erythoctic immunity

acquired_immunity <- create_variable(
  human,
  "ICA",
  function() {return 1}
) # Not correct

variables = c(age, ib, acquired_immunity, maternal_immunity)

xi <- create_constant(
  human,
  "xi",
  function(n, parameters) {
    return rlnorm(n, -parameters$sigma**2/2,parameters$sigma**2)
  }
)

mosquito_variety <- create_constant(
  mosquito,
  "variety",
  function() {
    p <- runif(mosquito_population)
    v <- rep(0, mosquito_population)
    v[which(p > .5)] <- 1
    v[which(p > .2 & p < .5)] <- 2
    v[which(p < .2)] <- 3
    return v
  }
)

constants = c(xi, mosquito_variety)

# Process functions return a vector of individuals to transfer given the model state
# The first argument is an object exposing the individuals of the source state
# The accepted methods are:
#   get_state (will all be of the source state)
#   get_constant
#   get_variable
infection_function <- function(susceptable) {
  variables <- infection_variables(source_humans)
  random <- runif(length(susceptable), 0, 1)
  return random > (variables$lambda * variables$phi)
}

infection_w_immunity_function <- function(source_humans) {
  variables <- infection_variables(source_humans)
  random <- runif(length(susceptable), 0, 1)
  return random > (variables$lambda * (1 - variables$phi))
}

fixed_probability <- function(p) {
  return function(source_humans, timestep) {
    random <- runif(length(source_humans), 0, 1)
    return random > p
  }
}

mosquito_infection_function <- function(susceptable_mosquitos, timestep) {
  lambda <- mosquito_force_of_infection(
    susceptable_mosquitos$get_constant(mosquito_variety),
    age$get_value(),
    human$get_state(),
    xi$get_value(),
    parameters
  )
  random <- runif(length(susceptable), 0, 1)
  return random > lambda
}

# Define processes
processes <- c(
  # Human
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
  # Mosquito
  create_process("Larval growth", E, L, fixed_probability(rel)),
  create_process("Pupal stage", L, P, fixed_probability(rl)),
  create_process("Susceptable Female Development", P, Sm, fixed_probability(rpl)),
  create_process("Mosquito Infection", Sm, Im, mosquito_infection_function)
)

simulation <- simulate(
  individuals,
  states,
  constants,
  variables,
  processes,
  365*10 # 10 years
)

# =================
# Utility functions
# =================
infection_variables <- function(source_humans) {
  labmda <- force_of_infection(
    source_humans$get_variable(age),
    mosquito_variety$get_value(),
    source_humans$get_constant(xi),
    mosquito$get_state(),
    source_humans$get_variable(ib),
    parameters
  )
  phi <- immunity(
    source_humans$get_variable(acquired_immunity),
    source_humans$get_variable(maternal_immunity),
    parameters
  )
  return list(lambda=lambda, phi=phi)
}
