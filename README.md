# purescript-skull

Skull is a library for batching requests. When you perform a request using
Skull, it will wait for a small amount of time for other requests to be
performed as well, and batch them into a single batch request. When the
response has arrived, it will be dissected and all the requests will return.

Here is an example of how this would work:

```purescript
(+) <$> getA <*> getB
```

Here, `getA` and `getB` are actions that perform requests. Skull will
automatically combine these into a single request, reducing network traffic and
server load, at the cost of a (small but configurable) delay.
