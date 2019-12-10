# =======
# Library
# =======

# Factories
create_individual <- function(name, size) {
  return list(name=name, size=size)
}

create_variable <- function(individual, name, initial, update) {
  return list(individual=individual, name=name, initial, update)
}

create_constant <- function(individual, name, initial) {
  return list(individual=individual, name=name, initial)
}

create_state <- function(individual, name, initial) {
  return list(individual=individual, name=name, initial=initial)
}

create_process <- function(name, from, to, rate) {
  return list(name=name, from=from, to=to, rate=rate)
}

create_interval_updater <- function(updater, interval) {
  return list(updater=updater, interval=interval)
}

simulate <- function(individuals, states, constants, variables, processes, end_time) {
  #...
}
