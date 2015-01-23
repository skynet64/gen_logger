# gen_logger

Erlang gen_logger behaviour.

# Why

The logger is usally application-dependent when writing an erlang library. For exmaple, [emqttc](https://github.com/emqtt/emqttc) library is used in application with 'lager' logger: 

```
{ok, C} = emqttc:start_link([{host, "localhost"}, {logger, lager}]).
```

# Callbacks

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

# Define a logger

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

## License

The MIT License (MIT)

# Author

feng at emqtt.io

