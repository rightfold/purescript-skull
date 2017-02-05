# Skull

Skull is a library for batching requests. Requests invoked in rapid succession
are batched and performed as a single request batch.

```bash
bower install --save purescript-skull
```

- [GitHub](https://github.com/rightfold/purescript-skull)
- [Pursuit](https://pursuit.purescript.org/packages/purescript-skull)
- [Travis CI](https://travis-ci.org/rightfold/purescript-skull)

## Batching versus other approaches

Consider two requests that return numbers that you want to add. The requests
are independent of each other; one does not depend on the result of the other.
There are three approaches to performing these requests:

### Sequential

Perform the requests sequentially. That is, wait for the first request to
complete before performing the second request. This is very straightforward:

```purescript
do
  a <- request 1
  b <- request 2
  pure $ a + b
```

This approach suffers from the need to wait for the first request to finish
before the second request begins, even though the requests are otherwise
independent.

### Parallel

Perform the requests in parallel. The computation will take as long as the
longest request takes. This is also very straightforward, by using the `ParAff`
applicative instance:

```purescript
sequential $ (+) <$> parallel (request 1) <*> parallel (request 2)
```

However, the server will still receive two requests, and any *overhead* of
sending a single request is being duplicated.

### Batched

Combine the two requests into a single request, perform that, and split the
response into two responses before returning. This approach does not suffer
from either of the problems described above, but does require some more work.
In particular, requests need to be combined, responses need to be split, and
requests and responses must be matched up. Skull helps you with the latter,
while leaving the former two up to you.

## Usage

To use Skull, you must tell it how to combine requests and split responses. To
do this, you create a _batcher_. See the documentation of the `Batcher` type to
see how to do this.

Next, you must create a _state_, which keeps track of pending requests. Given a
batcher named `batcher`, all you have to do is the following:

```purescript
state <- newState batcher
```

You should use the same state value for all requests you want to batch
together. Once you have a state value, you can start scheduling requests. The
interface looks much like the parallel example above, but will in fact
automatically use the provided batcher.

```purescript
let action = (+) <$> requestA 1 <*> requestA 2
in runSkullA action state
```

Here, `runSkullA` will return the sum of the two responses.

## Concurrent applications

If your application is inherently concurrent, you may want to batch requests
that happen to be performed quickly after each other, but may not necessarily
always do so. This may happen if the requests come from different concurrent
parts of your application. You can use the non-applicative interface for this,
namely the `request` function.

```purescript
response <- request state 1
```
