# Game
An OTP game application
## Environment
* MacOS 10.12.5

## Prepare
1. Install postgresql
2. Install erlang/OPT
3. Install rebar3

## Build
~~~bash
    $ rebar3 compile
~~~

## DB configure
~~~
 db.sql
~~~
## Player

hp, mp, x, y.

## Action
|Request Method	|Request Data|Response Header|Response|Feature|
|:--------------	|:-----|:-----|:-----|:-----|
|auth				|{"user":"","password":""}|AuthResp|ok \| error |auth user|
|show				|{}|ShowResp|[{"username":"","id":0}]|list user|
|create_player	|{"name":""}|CreatePlayerResp|{"playerId":0}|create user's player|
|delete_player	|{"playerId":""}|DeletePlayerResp|ok \| error|delete user's player|
|list_player		|{}|ListPlayerResp|[{"id":0,"name":""}]|list user's player|
|enter\_in_game	|{"playerId":0}|EnterInGameResp|ok \| error|enter in game|
|enter\_out_game	|{"playerId":0}|EnterOutGameResp|ok \| error|enter out game|
|move				|{"x":0,"y":0}|MoveResp|{"x":0,"y":0}|move player|
|worlds			|{}|WorldsResp|[world list]|list worlds|
|friends			|{}|FriendsResp|[friends list]|list friends|
|talk				|{"userId":0,"msg":""}|TalkResp|ok \| error|talk|
|logout			|{}|LogoutResp|ok \| error|logout|

## Question