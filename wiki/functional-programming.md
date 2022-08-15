# Functional programming

Deku is a micro-framework. It relies heavily on existing PureScript libraries such as `hyrule` and `effect`, both of which are over seven years old and exploit battle-tested functional patterns.

We've already gone over how Deku uses [`Event`-s](./events.md) from [`hyrule`](https://github.com/mikesol/purescript-hyrule). This document will explore some other common libraries used in Deku apps.

## Effect

Deku callbacks, like `OnClick` and `OnFocus`, are executed in the `Effect` monad. `Effect` is similar to `IO` in Haskell and represents a computation where each computation has a synchronous side-effect, like printing to the console or retrieving information from a DOM element, that exists outside of a pure functional context.

In the following example, the `Effect` monad is used to echo the contents of a `textarea` to a `div` whenever a `button` is pressed.

## Applicative validators

Forms are ubiqutous in web programming, and Deku apps rely on standard PureScript validation libraries to validate and parse data. In this RealWorld app, the following libraries are used:

- [`purescript-validation`](https://github.com/purescript/purescript-validation)
- [`purescript-simple-json`](https://github.com/justinwoo/purescript-simple-json)

In the example below, the `validation` library is used to validate a form on submit.

## Generic codecs

PureScript has a mature routing ecosystem that most Deku apps use for their routing. This RealWorld app uses [`routing-duplex`](https://github.com/natefaubion/purescript-routing-duplex), which makes use of generic codecs to simplify routing.