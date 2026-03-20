# `zippy`

Simple ZIP archive manipulation.

[Why?](#why)

## Features

* Creating archives
* Reading archives
* Compression methods:
  * No compression
  * DEFLATE
* In-memory manipulation via `zippy-memory`
* [`streaming`](https://hackage.haskell.org/package/streaming) support via `zippy-streaming`

## Unsupported (at the moment)

* Timestamps
* Updating archives
* Directories
* ZIP64
* Compression methods other than DEFLATE

If you like this library and would like to request a feature, then create an [issue](https://github.com/LightAndLight/zippy/issues).

## See also

* [`zip`](https://hackage.haskell.org/package/zip) - the most complete ZIP library on Hackage.

* [`zip-archive`](https://hackage.haskell.org/package/zip-archive) - supports fewer features than `zip`, but has a pure interface.

## Why?

I created `zippy` for [aesthetic](https://en.wikipedia.org/wiki/Aesthetics) reasons.
Neither [`zip`](https://hackage.haskell.org/package/zip) nor [`zip-archive`](https://hackage.haskell.org/package/zip-archive) satisfy my sense of how the original problem should be solved,
so I wanted to build something that reflected my values.

The main thing I disagree with `zip` on is its choice of dependencies.
Specifically:

* [`transformers-base`](https://hackage.haskell.org/package/transformers-base) and [`monad-control`](https://hackage.haskell.org/package/monad-control)

  To me, [`MonadBaseControl`](https://hackage.haskell.org/package/monad-control/docs/Control-Monad-Trans-Control.html#t:MonadBaseControl) is a legacy concept from when the Haskell community was
  experimenting with ways to adapt
  [`bracket`](https://hackage.haskell.org/package/base/docs/Control-Exception.html#v:bracket)
  to monad transformers.
  In my opinion,
  [`MonadMask`](https://hackage.haskell.org/package/exceptions/docs/Control-Monad-Catch.html#t:MonadMask)
  from
  [`exceptions`](https://hackage.haskell.org/package/exceptions)
  is the definitive solution.

* [`resourcet`](https://hackage.haskell.org/package/resourcet)

  Based on the [`unliftio` ecosystem](https://hackage.haskell.org/package/unliftio)
  (via [`unliftio-core`](https://hackage.haskell.org/package/unliftio-core)),
  which it uses for
  [`mask`](https://hackage.haskell.org/package/base/docs/Control-Exception.html#v:mask)ing
  exceptions.
  Another place where `MonadMask` should be used.

* [`conduit`](https://hackage.haskell.org/package/conduit) ecosystem

  Haskell has an abundance of streaming libraries, and a ZIP archive library shouldn't mandate a particular choice.

`zip-archive` has a better dependency story, but it doesn't support streaming.

My goal with `zippy` was to keep a minimal dependency footprint while supporting opt-in streaming.
I did this by writing `zippy` such that it can be *extended* with new libraries that provide streaming support (e.g. `zippy-streaming`).
This meant exposing a low-level ZIP archive interface for such libraries, which is another place my taste departs from the norm.

`zip` provides
[`Codec.Archive.Zip.Internal`](https://hackage.haskell.org/package/zip/docs/Codec-Archive-Zip-Internal.html)
which contains "Low-level, non-public types and operations.".
"Internal" modules are conventional in Haskell[^stackoverflow-internal][^discourse-internal],
and in my experience tend to lack a coherent design and interface stability when compared to their non-internal counterparts.
Even though this kind of "internal" module allows some kind of extension, it's usually not a good experience.

In contrast, `zippy` provides a low-level ZIP interface (`Zip.Archive`) that is designed for external consumption.
The high-level `Zip` module hides datatype internals and implementation details,
and `Zip.Archive` exposes these in a structured, curated way.
`Zip` becomes the first client of the `Zip.Archive` interface, as if they were separate packages.
Adding another client of `Zip.Archive` (`zippy-streaming`) gives an even better feel for the module's usability.
There's now an organising principle for deciding where a feature goes:
Clients of `Zip.Archive` understand the logical structure of ZIP archives but doesn't care about bit-bashing,
and `Zip.Archive` provides an interface into that logical structure and takes care of the bit manipulation.

A solid low-level module with two usage examples makes it much easier to write extensions to `zippy`,
such as a package that supports *your* favourite streaming library.
This design also results in fewer dependencies for the core library as more dependencies are made *opt-in* via extension packages
(i.e. `zippy-streaming` depends on `streaming` so that `zippy` doesn't have to).

[^stackoverflow-internal]: <https://stackoverflow.com/questions/9190638/how-why-and-when-to-use-the-internal-modules-pattern>
[^discourse-internal]: <https://discourse.haskell.org/t/internal-modules/6972>
