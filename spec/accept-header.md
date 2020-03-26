# BinAST `Accept` header

## Motivation

BinAST has seen multiple iterations of its encoder, but clients might only support a specific version.

## Syntax

The HTTP header `Accept` with the value `application/javascript-binast; version=VERSION`.

Where the parameter `VERSION` is one of the following:
- [context02](https://github.com/binast/binjs-ref/blob/master/spec/old-context.md)
- [context01](https://github.com/binast/binjs-ref/blob/master/spec/context.md)
- multipart

This list should be updated when a new encoder is implemented.

## Backward compatibility

Clients are currently sending an `Accept` header without any parameter. We will continue to support them by assuming the version is `multipart`.

## Unsupported version

If the server receives a request with an unsupported version, we should make sure to fail gracefully.
The client is recommanded to also accept `application/javascript` so that the server can send the original JavaScript.
