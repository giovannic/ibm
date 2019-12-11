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
  b0    = 0.590076,
  b1    = 0.5,
  ib0   = 43.8787,
  kb    = 2.15506,
  rd    = 1/5,
  ra    = 1/195,
  ru    = 1/110,
  rt    = 1/5,
  ft    = 1/2, #What to set?? NOTE: is this related to theta?
  av1   = .92,
  av2   = .74,
  av3   = .94,
  cd    = 0.068,
  ct    = 0.021896,
  ca    = 1.82425,
  cu    = 0.00062,
  rm    = 1 / (67.6952 * timestep_to_day),
  rb    = 1 / (10 * 365 * timestep_to_day),
  rc    = 1 / (30 * 365 * timestep_to_day),
  ub    = 1 / 7.19919,
  uc    = 1 / 67.6952,
)

# Define variables
age <- create_variable(
  human,
  "age",
  function() { rexp(human_population, rate=1/10) %>% trunc },
  create_interval_updater(
    function(a, timestep) { a+1 },
    365*timestep_to_day
  )
)

last_bitten <- create_variable(
  human,
  "last_bitten",
  function() { rep(-1, human_population) },
  create_dummy_updater() #Updated by mosquito biting process
)

last_infected <- create_variable(
  human,
  "last_infected"
  function() { rep(-1, human_population) },
  create_dummy_updater() #Updated by mosquito biting process
)

# Maternal immunity
icm <- create_variable(
  human,
  "ICM",
  function() {
    first_immunity <- 1 # NOTE: how do we initialise this?
    t <- age$get_value() * 365 * timestep_to_day
    first_immunity * exp(-(t * parameters$rm))
  },
  create_updater(
    function(i, timestep) {
      i - parameters$rm * i
    }
  )
)

# Pre-erythoctic immunity
ib  <- create_variable(
  human,
  "IB",
  function() { rep(0, human_population) },
  create_updater(
    function(i, timestep) {
      immunity_decay(
        i,
        last_bitten$get_value(),
        timestep,
        parameters$rb,
        parameters$ub
      )
    }
  )
)

 # Acquired immunity to severe disease
ica <- create_variable(
  human,
  "ICA",
  function() { rep(0, human_population) },
  create_updater(
    function(i, timestep) {
      immunity_decay(
        i,
        last_infected$get_value(),
        timestep,
        parameters$rc,
        parameters$uc
      )
    }
  )
)

variables = c(age, last_bitten, last_infected, ib, ica, icm)

xi <- create_constant(
  human,
  "xi",
  function(n, parameters) {
    rlnorm(n, -parameters$sigma**2/2,parameters$sigma**2)
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
    v
  }
)

constants = c(xi, mosquito_variety)

infection_function <- function(timestep) {
  source_humans <- which(human$get_state() %in% c('S', 'U', 'A'))

  labmda <- force_of_infection(
    age$get_value()[source_humans],
    mosquito_variety$get_value(),
    xi$get_value(xi)[source_humans],
    mosquito$get_state(),
    ib$get_value(ib)[source_humans],
    parameters
  )

  infected_humans <- source_humans[runif(length(source_humans), 0, 1) > lambda]

  phi <- immunity(
    ica$get_value()[infected_humans],
    icm$get_value()[infected_humans],
    parameters
  )

  symptomatic <- runif(length(infected_humans), 0, 1) > phi

  # return updates
  # NOTE: clean this up
  list(
    states=list(
      I=infected_humans[symptomatic],
      A=infected_humans[!symptomatic]
    ),
    variables=list(
      last_bitten=list(
        index=infected_humans,
        value=timestep
      )
      last_infected=list(
        index=infected_humans[symptomatic],
        value=timestep
      )
    )
  )
}

mosquito_infection_function <- function(timestep) {
  source_mosquitos <- which(mosquito$get_state() == 'Sm')
  lambda <- mosquito_force_of_infection(
    mosquito_variety$get_value()[source_mosquitos],
    age$get_value(),
    human$get_state(),
    xi$get_value(),
    parameters
  )
  infected = source_mosquitos[
    runif(length(source_mosquitos), 0, 1) > lambda
  ]
  list(
    states=list(
      Im=infected,
    )
  )
}

# Define processes
processes <- c(
  # ===============
  # Human Processes
  # ===============

  # Untreated Progression
  create_fixed_probability_state_change_process(I, D, 1 - ft),
  # Treatment
  create_fixed_probability_state_change_process(I, Treated, ft),
  # Asymptomatic Progression
  create_fixed_probability_state_change_process(D, A, rd),
  # Subpatient Progression
  create_fixed_probability_state_change_process(A, U, ra),
  # Subpatient Recovery
  create_fixed_probability_state_change_process(U, S, ru),
  # Treatment Recovery
  create_fixed_probability_state_change_process(Treated, S, rt),

  # ==================
  # Mosquito Processes
  # ==================

  # Larval growth
  create_fixed_probability_state_change_process(E, L, rel),
  # Pupal stage
  create_fixed_probability_state_change_process(L, P, rl),
  # Susceptable Female Development
  create_fixed_probability_state_change_process(P, Sm, rpl),
  # Mosquito Infection
  # Mosquitos move from Sm -> Im
  # NOTE: Is this not meant to happen in the infection function?
  create_process(mosquito_infection_function)

  # =============
  # Mosquito bite
  # =============
  # Mosquito bites move humans from S, U, A -> I; S, U, A -> A
  # and has a side effect of boosting immunity
  create_process(infection_function),
)

simulation <- simulate(
  individuals,
  states,
  constants,
  variables,
  processes,
  365*10 # 10 years
)

# =======
# Utility
# =======

immunity_decay <- function(level, last_timestep, timestep, rate, delay) {
  boost <- (timestep - last_timestep) == delay
  level[boost] <- 1 # NOTE: what is the level to boost to?
  level[!boost] <- i - parameters$rm * i
  level
}
