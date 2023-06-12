# ![RealWorld Example App](logo.png)

> ### purescript-deku codebase containing real world examples (CRUD, auth, advanced patterns, etc) that adheres to the [RealWorld](https://github.com/gothinkster/realworld) spec and API.


### [Demo](https://deku-realworld.netlify.app/)&nbsp;&nbsp;&nbsp;&nbsp;[RealWorld](https://github.com/gothinkster/realworld)


This codebase was created to demonstrate a fully fledged fullstack application built with **purescript-deku** including CRUD operations, authentication, routing, pagination, and more.

We've gone to great lengths to adhere to the ****purescript-deku** community styleguides & best practices.

For more information on how to this works with other frontends/backends, head over to the [RealWorld](https://github.com/gothinkster/realworld) repo.


# How it works

Deku is a framework for building reactive apps using the PureScript language. It's very fast, boasting a [benchmark suite](https://github.com/mikesol/purescript-deku/blob/main/test/Performance/Snapshot/TodoTest.js) that's competitive with the fastest UI frameworks. Its goal is to combine together the speed of industry-leading frameworks with the power of a purely functional language. It is also fully interoperable with several other PureScript libraries including [`ocarina`](https://github.com/mikesol/purescript-ocarina) and [`rito`](https://github.com/mikesol/purescript-rito), which are used for WebAudio and WebGL respectively.

Deku's speed is shown off in the following open-source applications:

- [Verity Scheel's post on parsing](https://cofree.coffee/~verity/parser.html) implements a full-featured tweening engine to animate rule-based parsing steps.
- [joyride.fm](https://joyride.fm) is a social rhythm game.

## Deku RealWorld

This app uses idiomatic Deku patterns, including:

- [HTML templating](./wiki/templating.md)
- [Reactive events](./wiki/events.md)
- [Hooks](./wiki/hooks.md)
- [Classic programming patterns](./wiki/functional-programming.md)

For more information on how to use Deku, check out the [deku documentation](https://purescript-deku.netlify.app/) (see also [old documentation](https://mikesol.github.io/purescript-deku)). There's also an article called [Horizontal and Vertical Events](https://dev.to/mikesol/horizontal-and-vertical-events-1pm1) that explores some of the framework's design decisions.

# Getting started

```bash
npm install && npm start
```
