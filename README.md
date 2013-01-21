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
fpid = Future.new(fn() ->
  :timer.sleep(10000)
  :morning
end)

Future.get(fpid) #=> :timeout

Future.get(fpid, 10000) #=> hangs for a while, then returns :morning

Future.get(fpid) #=> error: noproc (futures are fragile at the moment)
```

TODO:
 - wrappers to control lifespan of the Futures, 
 - wrappers to trap exits and catch exceptions,
 - onsuccess and onfail callbacks
 - tests
 - benchmarking
