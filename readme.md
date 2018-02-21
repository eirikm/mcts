## Hi there!

Welcome to the Scala solution for JavaBin's Language Shootout 2018.

This code solves two popular board games using the Monte Carlo Tree Search algorithm.

It tries to balance between idiomatic scala, simple code (algorithm allowing), and somewhat good performance: 

The code is 99% Scala standard library, with the addition of a [very simple library](https://github.com/lihaoyi/fansi) 
for doing fancy terminal output.

The code is written in the functional programming paradigm. For performance reasons you won't see many functions, 
but it's completely referentially transparent. 
There are mutable optimizations within methods, but as long as the interface is kept immutable, that's always a nice thing to do!
 
### Building/Running

You will need sbt. 
```bash

$ wget https://raw.githubusercontent.com/paulp/sbt-extras/master/sbt -O ~/bin/sbt
$ chmod +x ~/bin/sbt

//in ~/.profile if you want
export PATH=~/bin:$PATH

$ sbt

# Wait while sbt downloads the internet, then type

sbt> coreJVM/run
```

### Scala.js
If you have node.js installed, you can try:
```
sbt> coreJS/run
```

### Scala Native
It needs a few system dependencies, consult [Scala Native environment setup](http://www.scala-native.org/en/latest/user/setup.html) if it fails.
```
sbt> coreNative/run
```
