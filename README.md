[![Version](https://img.shields.io/hackage/v/twitchapi)][badges:0-hackage]
[![Build](https://img.shields.io/github/workflow/status/wuest/haskell-twitchapi/CI)][badges:1-CI]
[![License](https://img.shields.io/github/license/wuest/haskell-twitchapi)][badges:2-license]

# Twitch API

This library provides a client for Twitch's APIs, providing (currently
incomplete) support for [PubSub][main:0-pubsub] and [Helix][main:1-helix]
interfaces.

## API Support

Currently partial support is provided for PubSub and Helix (Bits and Channel
Points support are currently implemented) interfaces.

## License

This library is released under the [MIT License][license-1:MIT]

## Contributions

Whether the contribution concerns code, documentation, bug reports, or something
else entirely, contributions are welcome and appreciated!

If the contribution is relating to a security concern, please see
[SECURITY.md][SECURITY].

For all other contributions, please see
[CONTRIBUTING.md][CONTRIBUTING].  In short:

  * Fork the project.
  * Add tests for any new functionality/to verify that bugs have been fixed.
    - In particular, generating data for parsers is valuable
  * Send a pull request on GitHub.

## Code of Conduct

The this project is governed by a [Code of Conduct][code-of-conduct].

[badges:0-hackage]: https://hackage.haskell.org/package/twitchapi
[badges:1-CI]: https://github.com/wuest/haskell-twitchapi/actions/workflows/ci.yaml
[badges:2-license]: https://github.com/wuest/haskell-twitchapi/blob/main/LICENSE
[main:0-pubsub]: https://dev.twitch.tv/docs/pubsub
[main:1-helix]: https://dev.twitch.tv/docs/api
[license-1:MIT]: https://github.com/wuest/haskell-twitchapi/blob/main/LICENSE
[SECURITY]: https://github.com/wuest/haskell-twitchapi/blob/main/SECURITY.md
[CONTRIBUTING]: https://github.com/wuest/haskell-twitchapi/blob/main/CONTRIBUTING.md
[code-of-conduct]: https://github.com/wuest/haskell-twitchapi/blob/main/CODE_OF_CONDUCT.md
