
For the time being, we use the [ESTree](https://github.com/estree/estree)
specification as a target for decoding JavaScript.

**This section is very much in progress.**

**FIXME** Specify amendments to ESTree:
- specify order of fields;
- introduce interfaces `String`, `Boolean`, ... and use them instead of sum types `string | boolean | ...`;
- introduce interfaces `Get`, `Set`, ... and use them instead of enums `"get" | "set" | ...`;
- introduce interface `Null` and use it instead of all the `| null`;
- extend functions with BinJS metadata;
- we don't care about locations;
- packing booleans?;
- implicit interface for `null`;
- ... ?