lambdatools
===========

Elixir lambda calculi / computer science tools. Somehow inspired by [this](http://scidok.sulb.uni-saarland.de/volltexte/2004/289/pdf/Schwinghammer-Diplom.pdf).

provides
===

### Lambda

``Lambda.HigherOrder``: currently has only fixed-point combinator implementation;

TODO: some more neat things from lambda calculi

### Future

``Future``: currently imeplements fragile (one-time-access) futures.

Usage (in a nutshell): 

``` Elixir
iex(1)> {:future, f} = Future.new(fn() ->
  :timer.sleep(10000)
  :morning
end)

iex(2)> Future.get(f) #=> :timeout

iex(3)> Future.get(f, 10000) #=> hangs for a while, then returns :morning

iex(4)> Future.get(f) #=> :noproc (futures are fragile at the moment)
```

Please note that for one new/1 you MUST make one and only one call to get/1
because at the moment, the only way to remove unneeded futures from the system
is to count "references" incremented when a future gets constructed with new/1
and decremented when a future successfully returns its value due to get/1.
Thus, iex(1) and iex(2) were legit use case (we have timeouted from the get/1,
thus we can retry), while iex(3) attempt could have broken expectations of 
another consumer of the same function executed in a future.

Please also note that for one function reference only one future gets constructed.
Thus please, pass pure functions in the future for your own goodness.

Future implementation in lambdatools are completely OTP-based and should match
your favorite fault-tolerance model with smallest tweaks. I currently am working 
on a flexible fault tolerance system.

Ideas are welcome.

TODO:
 - wrappers to control lifespan of the Futures, 
 - wrappers to trap exits and catch exceptions,
 - onsuccess and onfail callbacks
 - tests
 - benchmarking
 - configuration for sofo
 - several sofos with one monitor
