# `zippy`

Simple ZIP archive manipulation.

## Features

* Creating archives
* Reading archives
* Compression methods:
  * No compression
  * DEFLATE
* In-memory manipulation via `zippy-memory`
* [`streaming`](https://hackage.haskell.org/package/streaming) support via `zippy-streaming`

## Unsupported (at the moment)

* Updating archives
* Directories
* ZIP64
* Compression methods other than DEFLATE

If you like this library and would like to request a feature, then create an [issue](https://github.com/LightAndLight/zippy/issues).

## See also

* [`zip`](https://hackage.haskell.org/package/zip)

  The most complete ZIP library on Hackage.

* [`zip-archive`](https://hackage.haskell.org/package/zip-archive)

  Supports fewer features than `zip`, but has a pure interface.
