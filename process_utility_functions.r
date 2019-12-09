package(dployr)

# ===============
# Human functions
# ===============

force_of_infection <- function(age, timestep) { #Not correct
  # Calculate the unique biting rate (psi) from age
  # Calculate the mean EIR (epsilon0) from time (how??)
  # Sample the relative biting rate (xi) from a normal distribution
  # Calculate immunity level (b) (why?? isn't this part of phi?)
  #TODO: implement
  return rep(1, length(timestep))
}

immunity <- function(age, timestep_last_bitten) { #Not correct
  # Calculate acquired immunity from last_bitten
  # Calculate and maternal immunity from age
  # Then calculate immunity using parameters
  #TODO: implement
  return rep(1, length(timestep_last_bitten))
}


# Unique biting rate (psi) for a human of a given age
unique_biting_rate <- function(age) {
  return 1 - rho * exp(- age / a0)
}

# Relative biting rate (xi) drawn from log normal
relative_biting_rate <- function(n) {
  return rlnorm(n, -sigma**2/2,sigma**2)
}

# ==================
# Mosquito functions
# ==================

mosquito_force_of_infection <- function(v, timestep) {

  # Prepare data frame
  age <- time_delayed_human_age$get_value() #TODO: move variable extraction into the model code
  state <- time_delayed_human_state$get_value()
  xi <- time_delayed_human_xi$get_value()

  human_frame <- data.frame(
    "age" = age,
    "state" = state
    "xi" = xi
  )

  summary <- human_frame %>% group_by(.dots=c("state", "age", "xi")) %>% tally()

  summary$constant = recode(
    human_frame$state,
    D=cd, #TODO: move global parameters to function parameters
    Treated=ct,
    A=ca,
    U=cu
  )

  summary$psi <- unique_biting_rate(summary$age) * summary$xi

  #Calculate integral
  integral = sum(summary$xi * summary$psi * summary$constant * summary$n)

  # Calculate normaliser
  age_distribution <- human_frame %>% groupby("age") %>% tally()
  age_distribution$p <- age_distribution$n / length(human_frame)
  omega <- sum(age_distribution$p * unique_biting_rate(age_distribution$age))

  forces = recode(
    v,
    `1`=(av1/omega)*integral,
    `2`=(av2/omega)*integral,
    `3`=(av3/omega)*integral
  )
  return forces
}
