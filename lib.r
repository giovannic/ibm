# =======
# Library
# =======

# Factories
create_individual <- function(name, size) {
  list(name=name, size=size)
}

create_variable <- function(individual, name, initial, update) {
  list(individual=individual, name=name, initial, update)
}

create_updater <- function(updater, interval) {
  list(updater=updater, interval=interval)
}

create_interval_updater <- function(updater, interval) {
  list(updater=updater, interval=interval)
}

create_dummy_updater <- function() {
  list(updater=function() {}, interval=1000000)
}

create_constant <- function(individual, name, initial) {
  list(individual=individual, name=name, initial)
}

create_state <- function(individual, name, initial) {
  list(individual=individual, name=name, initial=initial)
}

create_state_change_process <- function(from, to, rate) {
  list(from=from, to=to, rate=rate)
}

create_fixed_probability_state_change_process <- function(from, to, rate) {
  create_state_change_process(from, to, fixed_probability(rate))
}

create_process <- function(f) {
  list(f=f)
}

simulate <- function(
  individuals,
  states,
  constants,
  variables,
  processes,
  end_time
  ) {
  #Initialise simulation frames
  for (i in individuals) {
  }
}

# =======
# Utility
# =======
fixed_probability <- function(p) {
  function(source_humans, timestep) {
    random <- runif(length(source_humans), 0, 1)
    random > p
  }
}
