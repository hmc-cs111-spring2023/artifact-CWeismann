# artifact-CWeismann

## About Ludus
Ludus is an internal DSL implemented in Scala. The purpose of Ludus is to aid in 
the development of board games, by providing a set of classes, functions, and 
methods that may be useful in a wide variety of games. Likewise, Ludus is 
designed to be more readable than a general-purpose language, especially to 
people who may have a background in board games but are not experienced 
programmers. Since board game rules can be extremely varied and complex, Ludus 
is Turing-complete, and allows programmers to use the full capabilities of Scala 
in addition to specific Ludus features.

## Writing Ludus Programs
Ludus programs are constructed in three main stages.
- First, subclasses of the main Ludus classes are constructed as needed. For 
example, the game Hearts requires all cards to have a suit (like Diamonds), and 
a value (like 5). These are not member variables of the base ```Card``` class, 
so they must be declared in a subclass, which we'll call ```PlayingCard```. 
After declaring any relevant member variables, then declare any helpful methods; 
for example, the ```PlayingCard``` class overrides the built-in ```toString``` 
function to allow face cards and aces to be displayed with J/Q/K/A instead of 
their numerical value.
- Next, the ```play()``` function in your Game subclass must be implemented to 
run the full game.
- Finally, a main function must be interested to initialize the ```Game``` subclass 
and ```play()``` it.

## Running Ludus Programs
1. While in a terminal window, navigate to a folder with a copy of the 
```boardGame.scala``` main file and your specific game file, which we'll call 
```<gameName>.scala``` here.
2. Enter the Scala REPL by running the command ```scala``` in the terminal.
3. Load the main Ludus file by typing ```load boardGame.scala``` in the REPL.
4. Load the game file by typing ```load <gameName>.scala``` in the REPL.
5. Run the game by typing ```main``` in the REPL.

## Notes
The Scala implementation of Ludus is very difficult to both read and write.
Ludus will likely be reimplemented in Python, Groovy, or another dynamic language
at some point in the near future.
