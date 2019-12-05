# Individual-Based Transmission Model

An IBM transmission model is initialised with:

*Individuals*

This is the type of organism that is being modelled. Each individual has a state and variable space (like age, pre-erythocytic immunity...). This is initialised with the number to model.

*States*

These are the states of the transmission model. Each individual is in one state at any one time. The most basic states used in transmission models are S, I, R. They can be accessed from the State object. The initial states can be set as values.

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
