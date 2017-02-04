# purescript-skull

Skull is a library for batching requests. Requests are scheduled for some
amount of time, after which they are executed as a single request batch.

Consider two requests that return numbers that you want to add. The requests
are independent of each other; one does not depend on the result of the other.
There are three approaches to performing these requests:

1. Perform the requests sequentially. That is, wait for the first request to
   complete before performing the second request.
2. Perform the requests in parallel. The computation will take as long as the
   longest request takes.
3. Combine the two requests into a single request, perform that, and split the
   response into two responses before returning.

The first approach is suboptimal because the requests are independent and it is
not necessary to wait for one before performing the other one.

The second approach is better. However, the overhead of sending a request and
receiving a response is duplicated.

The third approach is the approach that the applicative interface helps you
with. You can find this interface in the `Control.Applicative.Skull` module.

Additionally, Skull supports batching requests that come from independent
concurrent components of your application. This interface is in the
`Control.Skull` module and requires to you pass around a state value.
