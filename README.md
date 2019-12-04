# Individual-Based Transmission Model

An IBM transmission model is initialised with:

*Individuals*

This is the type of organism that is being modelled. Each individual has a state and variable space (like age, pre-erythocytic immunity...). This is initialised with the number to model.

*States*

These are the states of the transmission model. Each individual is in one state at any one time. The most basic states used in transmission models are S, I, R. They can be accessed from the State object. The initial states can be set as values.

*Parameters*

These are constant values in the transmission model. They can be accessed from the global model object.

*Variables*

These are continuous values assigned to each individual that are updated on every timestep. They can be accessed from the individual object. The initial variables can be set as values.

*Constants*

These are continuous values assigned to each individual that stay constant throughout the simulation. They can be accessed from the individual object. The initial variables can be set as values.

*Processes*

These are descriptions for how individuals change state. Every timestep a process moves individuals in a source state to a target state. The individuals that are moved are determined by a _process function_.

*End time*

The number of timesteps to run the simulation for.

## Computation

In a simulation, we:

 1. initialise the first timestep _simulation frame_ for each type of individual (individual x (state+variables))
 2. We then loop through each timestep, computing process functions and updating variables creating a new simulation frame.
 3. We return a complete simulation (simulation frame dimensions x time)

## Questions

*How do we deal with functions that depend on the past?*

e.g.

    * time lag for infection
    * immunity level since bitten (pre-erythocytic immunity)

We could create a `variable` "timestep bitten" which is -1 when not and 1 when an individual was last bitten. This will help calculate the time lag and the immunity level for a susceptable individual only if the "timestep bitten" variable has been set > -1.

*Where does EIR come from?*

Possibly the data. This would mean I need vector based parameters

*What are the limits of creating process functions in this way*
*What are the redundant computations?*
  * Low density areas
  * Variables calculated for every individual at every time (why not lazy?)

*Are parameter structures necessary? Do you want to explore ranges?*

Not for an R version. They overcomplicate things. Let's remove them.

*How many mosquitoes/humans do we expect to model?*

10Ms humans
??? mosquitos

*What will individual modelling allow us to do?*


*How does the mosquito simulation affect the human simulation?*


*Can we compress variables and states?*

Do we expect large groups of individuals (human or mosquitoes) to have the same state/variable combinations?

*Can we compress time?*

Is there a way to know when the next transition will be?

*How do we model the circle (infection)?*

Is it ok for it to be another state?

*Where are the performance improvements?*

Perhaps variable updates?
Perhaps the simulation loop? (doubtful)
Perhaps the variable/process functions?
Parallelisation
Distribution for memory usage
