# Test boundaries

The Common Test suites mix provider-level unit tests with live GRiSP.io
integration tests. `meck` is used at provider boundaries where interactive
input, local configuration, expensive package generation, or remote side
effects would make a test unsafe or nondeterministic.

## Mocked commands

| Command | Mocked modules | Reason |
|---|---|---|
| `validate` | `rebar3_grisp_io_io`, `rebar3_grisp_io_config`, `rebar3_grisp_io_api` | Validation requires a device in a particular update state. Mocking prevents state-dependent failures and changes to a real device. |
| `reboot` | `rebar3_grisp_io_io`, `rebar3_grisp_io_config`, `rebar3_grisp_io_api` | Rebooting is disruptive and must never happen as a side effect of the command test. |
| `version` | `rebar3_grisp_io_io` | Capture and assert console output. |

These suites test command parsing, token/configuration flow, API arguments,
success output, and local error handling. They do not test the remote REST
implementation.

## Live integration commands

| Command | Live behavior | Remaining mocks |
|---|---|---|
| `auth` / `deauth` | Requests a real Eresu token, revokes it through the command, and verifies that it can no longer authenticate. | Interactive prompts and output are mocked so the suite can run unattended. |
| `upload` | Uploads and removes the fixture package using the configured account. | Interactive I/O is mocked, and `grisp_pack` is mocked so the checked-in fixture is used instead of building a package. |
| `deploy` | Uploads the fixture and starts deployment for the configured device. Teardown calls `cancel_update/3` before deleting the package so no update process remains in the backend. | Interactive prompts and output are mocked. |
| `list` | Lists packages in the configured account, including an uploaded fixture and the empty state after cleanup. | Interactive prompts and output are mocked. |
| `delete` | Uploads the fixture, deletes it through the command, and verifies its absence through the API. | Interactive prompts and output are mocked. |
| `cancel` | Uploads the fixture, starts a real deployment, and cancels its pending backend update process. This works while the configured device is disconnected because the process is still waiting for the device. | Interactive prompts and output are mocked. |

Live suites require `GRISP_CI_USERNAME`, `GRISP_CI_PASSWORD`, and
`GRISP_CI_DEVICE`. The configured device must be linked to that account for the
project platform. Each live suite revokes its authentication token during
teardown so repeated test runs do not consume the account's token quota. Never
store those credentials in a tracked script or fixture.

## Running mocked suites

The connected-device command suites can be run without CI credentials:

```shell
rebar3 as test ct --suite=test/rebar3_grisp_io_validate_SUITE.erl
rebar3 as test ct --suite=test/rebar3_grisp_io_reboot_SUITE.erl
```
