# Hooks

Deku uses React-like hooks for creating local events in components. The `useState` hook, like React's `useState`, updates the UI whenever a value is changed. There is also a `useState'` function that does not require an initial value, and the UI changes only when the event is emitted.

[Try me]()
```purescript
```

Sometimes, you need to modify an event that is subscribed to multiple times. This is often the case, for example, when working with local state. In these cases, `useMemoized` is a useful tool.

[Try me]()
```purescript
```

Dynamic elements, like lists, can be created with `dyn` and can be addressed with the `useMailboxed` hook.

[Try me]()
```purescript
```

A special case of dynamic elements is when one element replaces another. This is how routing is done, and for this case, there is a special function called `switcher`.

[Try me]()
```purescript
```

Outside of these patterns, Deku and this RealWorld application rely heavily on [standard functional programming conventions and standard PureScript libraries](./functional-programming).