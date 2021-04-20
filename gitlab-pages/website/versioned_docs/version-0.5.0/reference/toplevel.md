---
id: toplevel
title: Top-Level
description: Available functions at the top level
hide_table_of_contents: true
---

import Syntax from '@theme/Syntax';
import SyntaxTitle from '@theme/SyntaxTitle';

These types and functions are available without any needed prefix.

<SyntaxTitle syntax="pascaligo">
type address
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type address
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type address
</SyntaxTitle>

An untyped address which can refer to a smart contract or account.

<SyntaxTitle syntax="pascaligo">
type big_map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type ('key, 'value) big_map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type big_map ('key, 'value)
</SyntaxTitle>

<Syntax syntax="pascaligo">

The type of a big map from values of type `key` to
values of type `value` is `big_map (key, value)`.

```pascaligo group=big_map
type move is int * int
type register is big_map (address, move)
```

</Syntax>
<Syntax syntax="cameligo">

The type of a big map from values of type `key` to values
of type `value` is `(key, value) big_map`.

```cameligo group=big_map
type move = int * int
type register = (address, move) big_map
```

</Syntax>
<Syntax syntax="reasonligo">

The type of a big map from values of type `key` to
values of type `value` is `big_map(key, value)`.

```reasonligo group=big_map
type move = (int, int);
type register = big_map(address, move);
```

</Syntax>

Be aware that a `big_map` cannot appear inside another `big_map`.

<SyntaxTitle syntax="pascaligo">
type bool
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type bool
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type bool
</SyntaxTitle>

<SyntaxTitle syntax="pascaligo">
type bytes
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type bytes
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type bytes
</SyntaxTitle>


<SyntaxTitle syntax="pascaligo">
type contract('parameter)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type 'parameter contract
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type contract('parameter)
</SyntaxTitle>

A typed contract. 

Use `unit` as `parameter` to indicate an implicit account. 

<SyntaxTitle syntax="pascaligo">
type chain_id
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type chain_id
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type chain_id
</SyntaxTitle>

The identifier of a chain, used to indicate test or main chains.

<SyntaxTitle syntax="pascaligo">
type int
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type int
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type int
</SyntaxTitle>

An integer. 

The only size limit to integers is gas.

<SyntaxTitle syntax="pascaligo">
type key
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type key
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type key
</SyntaxTitle>

A public cryptographic key.

<SyntaxTitle syntax="pascaligo">
type key_hash
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type key_hash
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type key_hash
</SyntaxTitle>

The hash of a public cryptographic key.

<SyntaxTitle syntax="pascaligo">
type list ('t)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type 't list
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type list('t)
</SyntaxTitle>

A sequence of elements of the same type.

<SyntaxTitle syntax="pascaligo">
type map ('key, 'value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type ('key, 'value) map
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type map ('key, 'value)
</SyntaxTitle>

<Syntax syntax="pascaligo">

The type of a map from values of type `key` to
values of type `value` is `map (key, value)`.

```pascaligo group=maps
type move is int * int
type register is map (address, move)
```

</Syntax>
<Syntax syntax="cameligo">

The type of a map from values of type `key` to values
of type `value` is `(key, value) map`.

```cameligo group=maps
type move = int * int
type register = (address, move) map
```

</Syntax>
<Syntax syntax="reasonligo">

The type of a map from values of type `key` to
values of type `value` is `map (key, value)`.

```reasonligo group=maps
type move = (int, int);
type register = map (address, move);
```

</Syntax>

<SyntaxTitle syntax="pascaligo">
type nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type nat
</SyntaxTitle>

A natural number.

The only size limit to natural numbers is gas.

<SyntaxTitle syntax="pascaligo">
type operation
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type operation
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type operation
</SyntaxTitle>

An operation emitted by the contract


<SyntaxTitle syntax="pascaligo">
type set ('value)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type 'value set
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type set('value)
</SyntaxTitle>

<SyntaxTitle syntax="pascaligo">
type signature
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type signature
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type signature
</SyntaxTitle>

A cryptographic signature.


<SyntaxTitle syntax="pascaligo">
type string
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type string
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type string
</SyntaxTitle>

A sequence of characters.

<SyntaxTitle syntax="pascaligo">
type tez
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type tez
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type tez
</SyntaxTitle>

A specific type for tokens.

<SyntaxTitle syntax="pascaligo">
type timestamp
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type timestamp
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type timestamp
</SyntaxTitle>

A date in the real world.

<SyntaxTitle syntax="pascaligo">
type unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
type unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
type unit
</SyntaxTitle>


<SyntaxTitle syntax="pascaligo">
function is_nat: int -> option(nat)
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val is_nat: int -> nat option
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let is_nat: int => option(nat)
</SyntaxTitle>

Convert an `int` to a `nat` if possible.

Note that `Michelson.is_nat` is deprecated. Please use `is_nat` instead.

<SyntaxTitle syntax="pascaligo">
function abs: int -> nat
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val abs: int -> nat
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let abs: int => nat
</SyntaxTitle>

Cast an `int` to `nat`.

<SyntaxTitle syntax="pascaligo">
function int: nat -> int
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val int: nat -> int
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let int: nat => int
</SyntaxTitle>

Cast an `nat` to `int`.

<SyntaxTitle syntax="pascaligo">
const unit: unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val unit: unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let (): unit
</SyntaxTitle>

A helper to create a unit.

<a name="failwith"></a>
<SyntaxTitle syntax="pascaligo">
function failwith : 'a -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val failwith : 'a -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let failwith: 'a => unit
</SyntaxTitle>

Cause the contract to fail with an error message or integer. Other types are 
not supported at the moment.

Using this currently requires in general a type annotation on the
`failwith` call.

<Syntax syntax="pascaligo">

```pascaligo
function main (const p : int; const s : unit) : list (operation) * unit is
  block {
    if p > 10 then failwith ("Failure.") else skip
  }
  with ((nil : list (operation)), s)
```

</Syntax>
<Syntax syntax="cameligo">

```cameligo
let main (p,s : int * unit) = if p > 10 then failwith "Failure."
```

</Syntax>
<Syntax syntax="reasonligo">

```reasonligo
let main = ((p,s) : (int, unit)) =>
  if (p > 10) { failwith ("Failure."); };
```

</Syntax>
<Syntax syntax="cameligo">
`Current.failwith` is deprecated. Use `Tezos.failwith` or `failwith` instead.
</Syntax>
<Syntax syntax="reasonligo">
`Current.failwith` is deprecated. Use `Tezos.failwith` or `failwith` instead.
</Syntax>


<SyntaxTitle syntax="pascaligo">
function assert : bool -> unit
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val assert : bool -> unit
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let assert: bool => unit
</SyntaxTitle>

Check if a certain condition has been met. If not the contract will fail.

<SyntaxTitle syntax="pascaligo">
function ediv : int -> int -> option (int * nat)
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
function ediv : mutez -> nat -> option (mutez * mutez)
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
function ediv : mutez -> mutez -> option (nat * mutez)
</SyntaxTitle>
<SyntaxTitle syntax="pascaligo">
function ediv : nat -> nat -> option (nat * nat)
</SyntaxTitle>

<SyntaxTitle syntax="cameligo">
val ediv : int -> int -> (int * nat) option
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val ediv : mutez -> nat -> (mutez * mutez) option 
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val ediv : mutez -> mutez -> (nat * mutez) option
</SyntaxTitle>
<SyntaxTitle syntax="cameligo">
val ediv : nat -> nat -> (nat * nat) option
</SyntaxTitle>

<SyntaxTitle syntax="reasonligo">
let ediv: (int, int) => option((int, nat))
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let ediv: (mutez, nat) => option((mutez, mutez))
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let ediv: (mutez, mutez) => option((nat, mutez))
</SyntaxTitle>
<SyntaxTitle syntax="reasonligo">
let ediv: (nat, nat) => option((nat, nat))
</SyntaxTitle>

Compiles to Michelson `EDIV`, one operation to get both the quotient and remainder of a division. `ediv x y` returns None if `y` is zero, otherwise returns `Some (quotient, remainder)` such that `x = (quotient * y) + remainder` and `0 <= remainder < abs(y)`.