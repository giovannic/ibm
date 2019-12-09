package(dployr)

# ===============
# Human functions
# ===============

# Calculate the unique biting rate (psi) from age
# Calculate the mean EIR (epsilon0) from time (how??)
# Sample the relative biting rate (xi) from a normal distribution
# Calculate immunity level (b)
force_of_infection <- function(
  age,
  v,
  xi,
  mosquito_state,
  ib,
  timestep,
  parameters
  ) {

  psi <- unique_biting_rate(age)
  _pi <- (xi * psi) / sum(xi * psi)

  # Prepare mosquito frame
  mosquito_frame <- data.frame(variant=v, state=mosquito_state)
  infectious_count <- mosquito_frame[which(mosquito_frame$state == 'Im')] %>%
    groupby("variant") %>% tally()
  infectious_count$blood_meal_rate <- blood_meal_rate(
    infectious_count$variant,
    parameters
  )

  epsilon0 <- _pi * sum(infectious_count$blood_meal_rate * infectious_count$n)
  b <- infection_probability(ib, parameters)
  return epsilon0 * xi * b * psi
}

immunity <- function(age, timestep_last_bitten) {
  # Calculate acquired immunity from last_bitten
  # Calculate and maternal immunity from age
  # Then calculate immunity using parameters
  #TODO: implement
  return rep(1, length(timestep_last_bitten))
}

# Unique biting rate (psi) for a human of a given age
unique_biting_rate <- function(age, parameters) {
  return 1 - parameters$rho * exp(- age / a0)
}

# Relative biting rate (xi) drawn from log normal
relative_biting_rate <- function(n, parameters) {
  return rlnorm(n, -parameters$sigma**2/2,parameters$sigma**2)
}

infection_probability <- function(ib, parameters) {
  return parameters$bmin + (parameters$bmax - parameters$bmin) /
    (1 + (ib / parameters$ib0)**parameters$kb)
}

# ==================
# Mosquito functions
# ==================

# Implementd from Griffin et al 2010 S1 page 6 (should it be page 7?)
# TODO: change to page 7 formulation
mosquito_force_of_infection <- function(v, age, state, xi, timestep, parameters) {

  # Prepare data frame
  human_frame <- data.frame(
    "age" = age,
    "state" = state
    "xi" = xi
  )

  summary <- human_frame %>% group_by(.dots=c("state", "age", "xi")) %>% tally()

  summary$infectivity = recode(
    human_frame$state,
    D=parameters$cd,
    Treated=parameters$ct,
    A=parameters$ca,
    U=parameters$cu
  )

  summary$psi <- unique_biting_rate(summary$age) * summary$xi

  #Calculate integral
  integral = sum(summary$xi * summary$psi * summary$infectivity * summary$n)

  # Calculate normaliser
  age_distribution <- human_frame %>% groupby("age") %>% tally()
  age_distribution$p <- age_distribution$n / length(human_frame)
  omega <- sum(age_distribution$p * unique_biting_rate(age_distribution$age))

  return (blood_meal_rate(v, parameters) / omega) * integral
}

blood_meal_rate <- function(v, parameters) {
  return recode(
    v,
    `1`=parameters$av1,
    `2`=parameters$av2,
    `3`=parameters$av3
  )
}
