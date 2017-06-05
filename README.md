# Game
An OTP game application
## Environment
* MacOS 10.12.5

## Prepare
1. Install postgresql
2. Install erlang/OPT
3. Install rebar3

##Build
~~~bash
    $ rebar3 compile
~~~

## DB configure
~~~
 db.sql
~~~
##Player

hp, mp, x, y.

##Action
|Method			|Data|
|:--------------	|:-----|
|auth				|{"user":"","password":""}|
|show				|{}|
|list_player		|{}|
|enter\_in_game	|{"playerId":0}|
|enter\_out_game	|{"playerId":0}|
|move				|{"x":0,"y":0}|
|worlds			|{}|
|friends			|{}|
|talk				|{"userId":0,"msg":""}|
|logout			|{}|