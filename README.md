# DBIM-Language

DBIM is less of a language and more of a programming environment. 

That is to say that it really is two languages.

There is the Lambda-Script (Declarative Backing) and the Pi-Script (Imperative Main).

The reasoning behind these two languages and the core idea behind DBIM is the observation that even how deep into a paradigm a program  might be, the *main* section of the code is always procedural in nature.

DBIM takes this observation seriously and decides to organize its program structure around this. Hence, the separation of computation and IO tasks completely. 

In DBIM all computation and declarations are written in a purely functional/declarative language called the Lambda-Script. The other language in DBIM, Pi-Script, is a procedural language that calls these functions and objects for effectful interaction.

### Here is an example module in the Lambda-Script:

```
export let  foo : Int   be 0 in

export let fac : ( Int -> Int )   be 

    \  n : Int  -> 

        | n is 0  -> 1
        | n is 1 -> 1
        | elsewhere -> n * ( fac $ ( (- 1) + n ) ) 

in

module Prelude
```

### Now here is the Pi-Script calling fac from said module

``` 
Main: 

    open prelude;

    print (fac  1);
    print (fac  2);
    print (fac  3);
    print (fac  4);

End
```

### Currently in Developement
DBIM was started recently and still needs much more facilites and testing
#### Things that it still needs
* Abrstract data types (thus there are yet no lists)
* looping constructs and general more IO for Pi-Script

### Future/current goals
* Complete Dependent typing
* Declarativly defined classes for objects
* Functors over modules
* pi-calculus based conccurency model for the pi-calculus


