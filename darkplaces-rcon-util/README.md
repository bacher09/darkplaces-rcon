# darkplaces-rcon-util

This is implementation of rcon shell for darkplaces engine.
View this [demo] if you want to get more info.

## Features

* Simple and easy CLI interface
* Supports both ipv4 and ipv6 protocols
* Bookmarks and default options
* Saves command history to file
* Internal commands and smart autocomplete for them
* Suspend support with <kbd>Ctrl</kbd>+<kbd>z</kdb>

## Installation

The most easy way to install this tool is to download
binary from GitHub releases page. Also you may build latest
release by performing this command:

    $ cabal install darkplaces-rcon-util

Note, you should have cabal in your system to install in that way.
Also you can build it from git sources, for detailed instructions
follow [this guide][git build].

## Usage

    Usage: drcon ([-4] | [-6]) [-p|--password PASSWORD] [-m|--mode MODE]
                 [-d|--time-diff TIMEDIFF] [-t|--timeout TIMEOUT]
                 [-e|--encoding ENCODING] [--color COLOR_MODE] SERVER [COMMAND]
      Darkplaces rcon client utility

    Available options:
      -h,--help                Show this help text
      -4                       Forces to use IPv4 addresses only
      -6                       Forces to use IPv6 addresses only
      -p,--password PASSWORD   Server's password
      -m,--mode MODE           Use secure rcon, same as `rcon_secure' cvar, 1 is
                               default
      -d,--time-diff TIMEDIFF  Integer difference between client and server time,
                               can be negative
      -t,--timeout TIMEOUT     How many time wait for response after send or previous
                               response
      -e,--encoding ENCODING   Server encoding. Major options is `utf8' and `nexuiz'
      --color COLOR_MODE       Possible values are: `auto', `always' and `never'
      SERVER                   Server to connect or config section
      COMMAND                  Command that will be sent to server


## Configuration

By default config file is located in `~/.drcon/drcon.ini`.
Here's an example of config file

```ini
[DEFAULT]
# prompt can be set only in default section
# it is optional and by default "%P %N> "
prompt="%{[0;32m%}%P %{[1;34m%}%N> %{[0m%}"

# optional, by default 1.5
timeout=0.6

# mode can be 0, 1 or 2, 1 is default value.
# it has the same meaning as rcon_secure cvar in darkplaces
# 0 means non secure rcon protocol
# 1 means srcon protocol with timestramp
# 2 means srcon protocol with challenge
mode=1

# you can set default password for all servers
# it can be overridden in server section
# password=defaultpassword

# encoding can be utf8 or nexuiz, utf8 is default
# encoding=utf8

# addrfamily can be any, inet or inet6,
# values have the same meaning as in ssh_config
# any means booths ipv4 and ipv6
# inet means force only ipv4
# inet6 means force only ipv6


[xonotic]
server=somehost:26000
password=somepassword

[nexuiz]
server=1.2.3.4:26000
password=somepassword
encoding=nexuiz

[xonotic-ipv6]
server=[FE80::0202:B3FF:FE1E:8329]:26000
password=somepassword
mode=2
```
Input behavior can be changed via `~/.haskeline` config file. For more info
read [this][haskeline config] and [this][haskeline bind].

## Prompt

Prompt value is uses syntax of Haskell's string literal, however you also
can use special substitutions:

| Sequence | Printed                 |
| -------- | ----------------------- |
| %N       | Server Name             |
| %h       | Server Host             |
| %p       | Server port             |
| %T       | System time (HH:MM)     |
| %*       | System time (HH:MM:SS)  |
| %D       | System date (YY-MM-DD)  |
| %m       | Connect mode: 0, 1 or 2 |
| %P       | Program name (drcon)    |
| %v       | Program version         |
| %%       | Symbol '%'              |
| %{       | Same as \ESC            |
| %}       | End of escape sequence  |

It is possible to embed escape sequences in prompt value, this will allow you
to use colored prompt.

Here's one tip for those who are familiar with bash prompt settings:

* %{  &mdash; same as \[\e in bash
* %}  &mdash; same as \] in bash

## Environment variables

* `DRCON_HOME` &mdash; path to `.drcon` home dir.
* `DRCON_HISTFILE` &mdash; path where history will be stored

If `DRCON_HOME` is empty then `~/.drcon/` will be used as default value.
Some people prefer store configs in "~/.config/" folder, for this you can
change this var like this

    $ export DRCON_HOME="~/.config/drcon/"

Default value for `DRCON_HISTFILE` is `$DRCON_HOME/drcon_history`.


[demo]: https://asciinema.org/a/20146
[git build]:  ../README.md#building-from-source
[haskeline config]: http://trac.haskell.org/haskeline/wiki/UserPrefs
[haskeline bind]: http://trac.haskell.org/haskeline/wiki/CustomKeyBindings
[string literal]: http://book.realworldhaskell.org/read/characters-strings-and-escaping-rules.html#escapes.escape
