Ikaria is an attempt at implementing an Actor model in Delphi.

Actors may

* compute something
* create other Actors
* send messages (in the form of tuples) to other Actors
* react to received messages.

You implement Actors by subclassing TActor. Then, for each interesting message, you implement a finder method and a reactor method. The former takes a tuple and returns a Boolean indicating a match, and the latter takes a tuple and does something with it. Every Actor by default Steps through its computation, usually waiting to receive an interesting message.

Note that you, as implementor of an Actor, need to exercise caution. You can easily write a malicious Actor that sits in a tight loop, starving the processor (and preventing a clean shutdown of the Environment). The framework doesn't protect you from rabbit-bombing yourself.

Currently, the framework runs each Actor in its own, transient, thread.

(Why "Ikaria"? It's the town of one Thespis, regarded as being the very first actor. "Delphi" is the name of an ancient Greek town, so it amused me to name this framework after another Greek town.

TODO:
-----

* Trapping of exits (when a flag is set)
* Should receiving exits be pushed into the primitive layer? Right now it's possible to have a badly- or maliciously-written Actor refuse to terminate because it never processes an exit message.
* Parallel map: take an array and a function to be applied to each element in the array, and fork/join the processing, a la mapreduce.
* "Futures"?