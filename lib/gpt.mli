(*
 * Copyright (C) 2013 Citrix Inc
 *
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

module Partition : sig
  (* extracted from https://en.m.wikipedia.org/wiki/GUID_Partition_Table *)
  type t = private {
    type_guid : Uuidm.t;
    partition_guid : Uuidm.t;
    starting_lba : int64;
    ending_lba : int64;
    attributes : int64;
    name : string; (*name should be encoded as a utf-16 string of 72 bytes*)
  }
  (* private field types for the Partition record. can be accessed using
     the smart constructors `make` *)

  val make :
    ?name:string ->
    type_guid:Uuidm.t ->
    attributes:int64 ->
    int64 ->
    int64 ->
    (t, string) result
  (* [make ?name type_guid attributes starting_lba ending_lba] constructs a Partition.t
      An [Error _] is returned if:
        - the length of name not 72 bytes
  *)

  val unmarshal : Cstruct.t -> (t, string) result
  (* [unmarshal buf] is the partition entry encoded in the beginning of buf*)
  val marshal : Cstruct.t -> t -> unit
  (* [marshal buf] decodes the partition entry at the beginning of buf *)
end

type t = private {
  signature : string;
  revision : int32;
  header_size : int32;
  header_crc32 : int32;
  reserved : int32;
  current_lba : int64;
  backup_lba : int64;
  first_usable_lba : int64;
  last_usable_lba : int64;
  disk_guid : Uuidm.t;
  partition_entry_lba : int64;
  num_partition_entries : int32;
  partitions : Partition.t list;
  partition_size : int32;
  partitions_crc32 : int32;
}

val make :  ?disk_guid:Uuidm.t -> disk_size:int64 -> sector_size:int ->  Partition.t list -> (t, string) result
(* [make ?disk_guid disk_size sector_size partitions] constructs a GPT given a desired list of
     partitions. An [Error _] is returned if:

    - The number of partitions exceeds 128,
    - Any of the partitions overlap with each other or the first sector,

    The optional argument [disk_guid] specifies the disk guid to be
    written in the GPT. If [disk_guid] is not provided, a default value
    is created using the Uuidm library. *)
val unmarshal : Cstruct.t -> sector_size:int -> (t, string) result 
(* [unmarshal buf] buf should be  all of sector 2 to sector 34. Sector 1 is the protective MBR*)
val marshal : Cstruct.t -> t -> unit
