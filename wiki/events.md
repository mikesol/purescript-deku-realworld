# Events

Events are the bread and butter of Deku communication. If you've used Rx or any reactive framework, events in Deku shoul feel familiar. An emitter emits events, a subscriber listens to them, and various functions are used to transform and combine events.

In the example below, we use an `interval` emitter, which emits events every _n_ milliseconds, to control various parts of our DOM.

[Try Me]()
```purescript
```

`Event` is defined in [`purescript-hyrule`]() and it has many useful instances. Above, we see `Event`'s [`Functor`]() instance in action, which is where we get [`<#>`]() and [`$>`]() from. `Event` also has instances for `Apply` and `Applicative`, allowing you to use `<*>` and `pure`, as in the example below.

[Try Me]()
```purescript
```


Finally, `Event` has an instance of a special typeclass called `IsEvent`. `IsEvent` deals mostly with operations over time. For example, `fold` allows you to fold over iterations of an event, which is how state is handled in Deku.

[Try Me]()
```purescript
```

There are many more useful functions and combinators in the [`purescript-hyrule`]() library to work with events.

In web applications, events are mainly used to capture interactions with UI elements. Deku has a [hooks-inspired syntax](./hooks.md) for creating these interactions.