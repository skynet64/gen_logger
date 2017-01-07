# Erlang gen_logger behaviour

A general logger behaviour for erlang applications.

## Usage

```
Logger = gen_logger:new(debug).

Logger:info("hello...").
Logger:info("format: ~p", ["hello"]).

Logger:error("error msg").
Logger:error("format error msg", []).
```

[emqttc](https://github.com/emqtt/emqttc) for example, start a client with different logger:

```
%% start with lager
emqttc:start_link([{logger, {lager, info}}]).

%% start with error_logger
emqttc:start_link([{logger, {error_logger, error}}]).
```

# API 

```
-type level() :: all | debug | info | warning | error | critical | none.

gen_logger:new(debug).

%lager with info level
gen_logger:new({lager, info}).

%lager with debug level
gen_logger:new(lager, debug).

%error_logger with info level
gen_logger:new(error_logger, info).

%io:format with error level
gen_logger:new(console, error).
```

## Behaviour

```
-behaviour(gen_logger).
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

## Define my logger

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

New my logger:

```
gen_logger:new(my, info).
```

## License

The MIT License (MIT)

# Author

feng at emqtt.io

