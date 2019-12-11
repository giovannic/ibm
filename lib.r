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
  list(updater=function() {}, interval=1000000) #TODO: must be a better solution
}

create_constant <- function(individual, name, initial) {
  list(individual=individual, name=name, initial)
}

create_state <- function(individual, name, initial) {
  list(individual=individual, name=name, initial=initial)
}

create_fixed_probability_state_change_process <- function(i, from, to, rate) {
  source_individuals <- which(individual$get_state() == from$get_name())
  target_individuals <- source_individuals[
    runif(length(source_individuals), 0, 1) > rate
  ]
  updates <- list(states=list(to, target_individuals))
}

create_process <- function(f) {
  list(f=f)
}

simulate <- function(
  individuals,
  processes,
  end_time
  ) {
  # Initialise individuals
  for (i in individuals) {
    i$initialise()
  }

  # Main loop
  simulation_frames <- list()
  timesteps <- 1:end_time
  for (timestep in timesteps) {
    for (i in individuals) {
      i$step_variables(timestep)
    }
    for (p in processes) {
      p$step(timestep)
    }
    frames <- list()
    for (i in individuals) {
      append(frames, i$render_simulation_frame())
    }
    append(simulation_frames, frames)
  }

  return simulation_frames
}
