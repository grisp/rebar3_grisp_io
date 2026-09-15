# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to
[Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [1.0.0] - 2026-09-15

### What's Changed

#### Added

- A `deauth` command that revokes the current authentication token and removes
  its encrypted local credentials, including credentials for an already
  invalid token.
- Commands to list stored update packages, cancel an update, and reboot a
  linked device.
- Upload progress feedback.

#### Changed

- Upload package bodies using streaming requests.
- Allow `delete` to remove any explicitly named package while retaining the
  project-derived package name as the default.
- Run package and deployment lifecycle tests against the live GRiSP.io API,
  while keeping connected-device operations mocked.

#### Fixed

- Improve REST API error reporting for authentication, authorization, missing
  packages and devices, and rejected device operations.
- Correct deployment, upload, and package deletion behavior with current
  Hackney versions.

## [0.1.0] - 2024-10-12

### What's Changed

#### Added

- API to manage and deploy grisp device updates through GRiSP.io
  - upload and delete update packages
  - start deployments and validate updates

[Unreleased]: https://github.com/grisp/rebar3_grisp_io/compare/1.0.0...HEAD
[1.0.0]: https://github.com/grisp/rebar3_grisp_io/compare/0.1.0...1.0.0
[0.1.0]: https://github.com/grisp/rebar3_grisp_io/compare/023e51e181ee80491299d3c5fb9f604f4729d35a...0.1.0
