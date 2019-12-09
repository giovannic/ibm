package(dployr)

# ===============
# Human functions
# ===============

# Implemented from Winskill 2017 - Supplementary Information page 3
# Calculate the unique biting rate (psi) from age
# Calculate the mean EIR (epsilon0) from time
# Sample the relative biting rate (xi) from a normal distribution
# Calculate immunity level (b)
force_of_infection <- function(
  age,
  v,
  xi,
  mosquito_state,
  ib,
  parameters
  ) {

  psi <- unique_biting_rate(age)
  _pi <- human_pi(xi, psi)

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

# Implemented from Winskill 2017 - Supplementary Information page 4
# Calculate acquired immunity from last_bitten
# Calculate and maternal immunity from age
# Then calculate immunity using parameters
immunity <- function(acquired_immunity, maternal_immunity, parameters) {
  return parameters$phi0 * (
    parameters$phi1 +
      (1 - parameters$phi1) /
      1 + ((acquired_immunity + maternal_immunity) / parameters$ic0)
      ** parameters$kc
    )
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

human_pi <- function(xi, psi) {
 (xi * psi) / sum(xi * psi)
}

# ==================
# Mosquito functions
# ==================

# Implemented from Griffin et al 2010 S1 page 7
mosquito_force_of_infection <- function(v, age, state, xi, parameters) {

  # Prepare data frame
  human_frame <- data.frame(
    "age" = age,
    "state" = state
    "xi" = xi
  )
  human_frame$psi <- unique_biting_rate(human_frame$age)
  human_frame$pi <- human_pi(human_frame$xi, human_frame$psi)
  human_frame$infectivity = recode(
    human_frame$state,
    D=parameters$cd,
    Treated=parameters$ct,
    A=parameters$ca,
    U=parameters$cu
  )

  mean_infectivity <- sum(human_frame$pi * human_frame$infectivity)
  return blood_meal_rate(v, parameters) * mean_infectivity
}

blood_meal_rate <- function(v, parameters) {
  return recode(
    v,
    `1`=parameters$av1,
    `2`=parameters$av2,
    `3`=parameters$av3
  )
}
