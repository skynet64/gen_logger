# gen_logger

Erlang gen_logger behavior.

## Why

When writing an erlang library, the logger is usally application-dependent. For exmaple, [emqttc] (https://github.com/emqtt/emqttc) library used in application with 'lager' logger: 

```
{ok, C} = emqttc:start_link([{host, "localhost"}, {logger, {lager, info}}]).
```

## Usage

```

Logger = gen_logger:new(debug).

Logger:info("hello...").
Logger:info("format: ~p", ["hello"]).

Logger:error("error msg").
Logger:error("format error msg", []).


```

# API 

```
-type level() :: all | debug | info | warning | error | critical | none.

gen_logger:new(debug).

%lager with debug
gen_logger:new(lager, debug).

%otp with info
gen_logger:new(otp, info).

%io:format error
gen_logger:new(stdout, error).
```

## Behavior

```
-behavior(gen_logger).
```

## Callbacks

```

-callback debug(Msg :: string()) -> ok. 
-callback debug(Msg :: string(), Args :: list()) -> ok. 

-callback info(Msg :: string()) -> ok. 
-callback info(Msg :: string(), Args :: list()) -> ok. 

-callback warning(Msg :: string()) -> ok. 
-callback warning(Msg :: string(), Args :: list()) -> ok. 

-callback error(Msg :: string()) -> ok. 
-callback error(Msg :: string(), Args :: list()) -> ok. 

-callback critical(Msg :: string()) -> ok. 
-callback critical(Msg :: string(), Args :: list()) -> ok. 

```

## Define a logger

```
-module(my_logger).

-behaviour(gen_logger).

-export([debug/1, debug/2,
         info/1, info/2,
         warning/1, warning/2,
         error/1, error/2,
         critical/1, critical/2]).

......

```

Use the logger:

```
gen_logger:new(my, info).
```

## License

The MIT License (MIT)

# Author

feng at emqtt.io

