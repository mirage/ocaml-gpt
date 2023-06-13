# ocaml-gpt

## Introduction

This library provides functionality for working with the GUID Partition Table (GPT). It allows you to create, manipulate, and marshal/unmarshal GPT headers and partitions.

## Dependencies
- `OCaml` (version 4.02 or later)
- `dune` (build system)
- `uuidm` (library for UUID manipulation)
- `checkseum` (library for checksum calculations)
- `ocaml-cstruct` (library for working with C-like structures)

## Installation
1) Install OCaml and dune by following the instructions for your platform. [Up and Running with Ocaml](https://ocaml.org/docs/up-and-running)
2) Install the required dependencies using OPAM (OCaml package manager)
```sh
opam install .
```
3) Build the project using dune
```sh
dune build
```

4) Run Test
```sh
dune build @runtest
```

## Usage

### Module: Partition
This module provides functions for working with GPT partitions.
```ocaml
Partition.make
val make :
  ?name:string ->
  type_guid:string ->
  attributes:int64 ->
  starting_lba:int64 ->
  ending_lba:int64 ->
  (Partition.t, string) result
```
This function creates a new GPT partition with the specified parameters. It returns a `Partition.t` value wrapped in the Result type, indicating success or failure.

#### Partition.unmarshal
```ocaml
val unmarshal : Cstruct.t -> (Partition.t, string) result
```
This function takes a `Cstruct.t` buffer and unmarshals the data into a `Partition.t` value. It returns the unmarshalled partition wrapped in the Result type, indicating success or failure.

#### Partition.marshal
```ocaml
val marshal : Cstruct.t -> Partition.t -> unit
```
This function marshals a `Partition.t` value into a `Cstruct.t` buffer.


### Module: Gpt
This module provides functions for working with GPT headers.

#### Gpt.make
```ocaml
val make : Partition.t list -> (t, string) result
```
This function creates a new GPT header with the specified list of partitions. It returns a `Gpt.t` value wrapped in the Result type, indicating success or failure.

#### Gpt.unmarshal
```ocaml
val unmarshal : Cstruct.t -> (t, string) result
```
This function takes a `Cstruct.t` buffer and unmarshals the data into a `Gpt.t` value. It returns the unmarshalled GPT header wrapped in the Result type, indicating success or failure.

#### Gpt.marshal
```ocaml
val marshal : Cstruct.t -> t -> unit
```
This function marshals a `Gpt.t` value into a `Cstruct.t` buffer.

## Example Usage
Here's an example of how you can use this library to create and manipulate GPT headers and partitions:

```ocaml
let create_gpt_disk () =
  let partition1 = Match Partition.make ~name:"Partition 1" ~type_guid:"12345678-1234-1234-1234-123456789abc" ~attributes:0L 1L 100L
  with 
  | Ok p -> p
  | Error error -> Printf.eprintf "Error %s" error
  in
  match make [partition1] with
  | Ok gpt -> gpt
  | Error error -> Printf.eprintf "Error %s" error

```

## License
This libray is licensed with the Ocaml standard ISC license. See here [License](LICENSE)

## Contributions
Please report bugs using the issue tracker at [https://github.com/PizieDust/ocaml-gpt/issues](https://github.com/PizieDust/ocaml-gpt/issues)