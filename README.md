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
        - [Authentication](#authentication)
        - [Deploy](#deploy)
        - [Upload](#upload)
        - [Validate](#validate)
        - [Version](#version)

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

To verify that everything works correctly you can check the version of the plugin by calling:

```shell
rebar3 grisp-io version
```

## Usage

to complete

## Tasks

### Authentication

This command allows you to authenticate on `grisp.io` and receive an API token.

The API token is encrypted and saved locally on your computer using the provided local password.

```shell
rebar3 grisp-io auth
===> Analyzing applications...
===> Compiling rebar3_grisp_io

Username > <Username>
Password > <Password>
Authentication successful - Please provide new local password
Local password > <LocalPassword>
Confirm your local password > <LocalPassword> % Must be the same
Token successfully requested
```
---
### Deploy

> [!IMPORTANT]
> You need an authentication token to run this command see: [authenticate](#authenticate)
> You need an uploaded package on `grisp.io` as well see: [upload](#upload)

This command allows you to update a grisp board using a package uploaded previously on `grisp.io`

This command has 2 options (mandatory options are marked with :exclamation:
- `--device` or `-d`: This option specifies the serial number of the target device :exclamation:
- `--package` or `-p`: This option specifies the full package name that needs to be deployed

> [!NOTE]
> :pushpin: If no package name is specified, it will use the informations contained in your `rebar.config` to deduce the package name
---
### Upload

> [!IMPORTANT]
> You need to authenticate first and request a token using [authenticate](#authenticate)

This command allows you to upload a package on `grisp.io`. Internally, this command will call `rebar3 grisp pack` and create
a new release package. Release packages are identified by 3 elements:
- The platform name (by default `grisp2`)
- The application name
- The release version in SemVer fashion (by default `0.1.0`)

This command has 2 options:
- `--force` or `-f`: This option will force an overwritting of the local and remote files of a given project and release
- `--no-pack` or `-p`: With this option enabled, the command won't run internally the command `rebar3 grisp pack`
---
### Validate

> [!IMPORTANT]
> You need to authenticate first and request a token using [authenticate](#authenticate)

This command allows you to validate an update that has been deployed on a device

You must specify the serial number of the device in the command:

```shell
rebar3 grisp-io validate <serial-number>
```

---
### Version

```shell
rebar3 grisp-io version
===> Analyzing applications...
===> Compiling rebar3_grisp_io
rebar3_grisp_io: 0.1.0
```
