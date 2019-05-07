# Actor model in Elm

## Concepts

Some concepts / vocabulary.

### Component

- Like an Elm program with out-messages.
- Can NOT know anything about other components.
- Is self contained and can be run as a standalone app. This makes development/testing easy.
- When a component is started (instantiated) it is called a process.
- Each process is identified by a unique PID (process id).
- Knowing a PID, you can send messages to that process.

There are three types of components:

- UI Component
- Service Component
- Layout Component

See Webbhuset.Component for more info.


### Actor

- An *actor* is a *component* that has been "wrapped" to be part of a system.
- The actor defines how a component's out-messages should be handled.
- Maps received messages to the component's in-message type.
- An actor implements the connections between components.
- The actor uses the `ActorSystem` module to perform actions, such as sending messages, spawning processes.

### System

- Defines which components are avaiable and bootstraps them.
- The "runtime", routes messages, spawns actors etc.


