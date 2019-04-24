# Actor model in Elm

## Concepts

Some concepts / vocabulary.

### Component

- Is the same as an actor.
- Like an Elm program with out-messages instead of Cmd's.
- Can NOT know anything about other components.
- Is self contained and can be run as a standalone app. This makes development/testing easy.
- When a component is started (instantiated) it is called a process.
- Each process is identified by a unique PID (process id).
- Knowing a PID, you can send messages to that process.

There are three types of components:

#### UI Component

- Similar to **Browser.element**.

```elm
type alias UI model msgIn msgOut =
    { init : PID -> (model, List msgOut, Cmd msgIn)
    , recv : msgIn -> model -> (model, List msgOut, Cmd msgIn)
    , view : model -> Html msgIn
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }
```

#### Service Component

- Similar to **Platform.worker**, ie. has no view function.

```elm
type alias Service model msgIn msgOut =
    { init : PID -> (model, List msgOut, Cmd msgIn)
    , recv : msgIn -> model -> (model, List msgOut, Cmd msgIn)
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }
```

#### Layout Component

- Like a UI Component but can render other components.
- The `view` function has a different signature. This is needed to be able to render children.

```elm
type alias Layout model msgIn msgOut msg =
    { init : PID -> (model, List msgOut, Cmd msgIn)
    , recv : msgIn -> model -> (model, List msgOut, Cmd msgIn)
    , view : (msgIn -> msg) -> model -> (PID -> Html msg) -> Html msg
    , kill : model -> List msgOut
    , subs : model -> Sub msgIn
    }
```

### Actor

- An *actor* is a *component* that has been "wrapped" to be part of a system.
- The actor defines how a component's out-messages should be handled.
- Maps received messages to the component's in-message type.
- An actor implements the connections between components.
- The actor uses the `ActorSystem` module to perform actions, such as sending messages, spawning processes.

### System

- Defines which components are avaiable and bootstraps them.
- The "runtime", routes messages, spawns actors etc.


## Modules


### Webbhuset.Component

- Helper for creating components.

### Webbhuset.Component.Dev

- Helper for testing and developing components.

### Webbhuset.Actor

- Helper for wrapping a component into an actor

### Webbhuset.ActorSystem

- Bootstrap code for all components
- Initialize the whole application.

