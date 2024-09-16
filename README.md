rebar3_grisp_io
=====
![GitHub Release](https://img.shields.io/github/v/release/grisp/rebar3_grisp_io)

Rebar plug-in for grisp.io. To obtain information about the plugin and its tasks, use the followinf command:

```shell
rebar3 help grisp-io [<task>]
```

📖 **Table of content**
- [rebar3\_grisp\_io](#rebar3_grisp_io)
    - [Installation](#installation)
    - [Usage](#usage)
    - [Tasks](#tasks)

## Installation

To install the plugin globally, add it to your "plugins" list in `~/.config/rebar3/rebar.config`

For example:
```erlang
{plugins, [
    rebar3_hex,
    rebar3_grisp,
    rebar3_grisp_io
]}.
```

Afterwards to update it to the latest version, you need to update the Hex index and then the plugin:

```shell
rebar3 udpdate
rebar3 plugins upgrade rebar3_grisp_io
```

## Usage

to complete

## Tasks
