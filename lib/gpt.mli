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
  type t = {
    type_guid : Uuidm.t;
    partition_guid : Uuidm.t;
    starting_lba : int64;
    ending_lba : int64;
    attributes : int64;
    name : string;
  }

  val make :
    ?name:string ->
    type_guid:string ->
    attributes:int64 ->
    int64 ->
    int64 ->
    (t, string) result

  val unmarshal : Cstruct.t -> (t, string) result
  val marshal : Cstruct.t -> t -> unit
end

type t = {
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

val make : Partition.t list -> (t, string) result
val unmarshal : Cstruct.t -> (t, string) result
val marshal : Cstruct.t -> t -> unit
