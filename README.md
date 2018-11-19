# DBIM-Language

DBIM is less of a language and more of a programming environment. 

That is to say that it really is two languages.

There is the Lambda-Script (Declarative Backing) and the Pi-Script (Imperative Main).

The reasoning behind these two languages and the core idea behind DBIM is the observation that even how deep into a paradigm a program  might be, the *main* section of the code is always procedural in nature.

DBIM takes this observation seriously and decides to organize its program structure around this. Hence, the separation of computation and IO tasks completely. 

In DBIM all computation and declarations are written in a purely functional/declarative language called the Lambda-Script. The other language in DBIM, Pi-Script, is a procedural language that calls these functions and objects for effectful interaction.

### Here is an example module in the Lambda-Script:

```
export let  foo be 0 in

export let fac be 

    \  n : Int  -> 

        if n is 1 then 1
        else 
                n * ( fac $  (n - 1)  ) 

in

module facMod
```


### Now here is the Pi-Script calling fac from said module

``` 
Procedure:

    print (fac  1);
    print (fac  2);
    print (fac  3);
    print (fac  4);

End

Main: 

    open facMod;
    ! Procedure;

End
```

### Currently in Developement
DBIM was started recently and still needs much more facilites and testing

### Future/current goals
* Complete Dependent typing (Currently in Developement )
#### here is some example of currently working dependent types
```
export let vector be

    \ a : TYPE -> 
    \ n : Int -> 

                | n is 0 ->     @ END
                | elsewhere ->  (a , (vector $ a $ (n - 1) ) )

in

export let goodVector be

    ( 1 , ( 2 , ( @ END ) ) ) : ( vector $! Int $! 2 )

in

export let badVector be

    ( 1 , ( 2 ,  ( @ END ) ) ) : ( vector $! Int $! 3 )

in

```
#### This code brings out an error message from the compiler
#### The 'bad' vector is wrongly typed with the number 3

* Built in concurrecy and parrallelism for the lambda-script (Currently in Developement)
* Declarativly defined classes for objects
* Functors over modules
* pi-calculus based conccurency model for the pi-script


