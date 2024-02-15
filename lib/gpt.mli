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
  (** private field types for the Partition record. Can be created using
      the smart constructors [make] *)

  val make :
    ?name:string ->
    type_guid:Uuidm.t ->
    attributes:int64 ->
    int64 ->
    int64 ->
    (t, string) result
  (** [make ?name ~type_guid ~attributes starting_lba ending_lba] constructs a
      Partition.t. [name] is assumed to be a zero-padded utf-16le encoded
      string.
      @raise Invalid_argument if [name] is not exactly 72 bytes *)

  val is_zero_partition : t -> bool
  (** [is_zero_partition partition] is [true] if [partition] is the all-zero
      partition (i.e. the unused partition entry) *)

  val unmarshal : Cstruct.t -> t
  (** [unmarshal buf] is the partition entry encoded in the beginning of [buf].
      @raise Invalid_argument if [buf] is too small to contain a partition entry. *)

  val marshal : Cstruct.t -> t -> unit
  (** [marshal buf partition] serializes the partition entry at the beginning of [buf] *)
end

type t = private {
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
  partition_size : int32;
  partitions_crc32 : int32;
  partitions : Partition.t list;
}

val make :  ?disk_guid:Uuidm.t -> disk_sectors:int64 -> sector_size:int ->  Partition.t list -> (t, string) result
(** [make ?disk_guid ~disk_sectors ~sector_size partitions] constructs a GPT given
    a desired list of partitions. The header is assumed to be written in LBA 1,
    and the partition table is written from LBA 2 with 128 partition entries
    allocated. The backup header is assumed written in the last LBA.
    An [Error _] is returned if the number of partitions exceeds 128, or any of
    the partitions overlap with each other or the first sector,

    The optional argument [disk_guid] specifies the disk guid to be
    written in the GPT. If [disk_guid] is not provided, a random value is used. *)

val unmarshal : Cstruct.t -> sector_size:int ->
  ([ `Read_partition_table of int64 * int ] * (Cstruct.t -> (t, string) result) , string) result
(** [unmarshal ~sector_size buf] on success is a pair [(`Read_partition (lba, num_sectors), k)]
   where [lba] is the logical block address (sector) to read [num_sectors]
   sectors of the partition table. The bytes read should then be passed to [k]
   which then on success returns the GPT header and the partition table.
   An [Error _] is returned when any field has an unexpected value or the checksums are wrong.
   @raise Invalid_argument when [buf] is too small to contain a GPT header
*)

val marshal_header : sector_size:int -> primary:bool -> Cstruct.t -> t -> unit
(** [marshal_header ~sector_size buf t] serializes the GPT header to [buf].
    The caller is expected to write the contents of [buf] to [t.current_lba]
    and [t.backup_lba]. *)

val marshal_partition_table : sector_size:int -> Cstruct.t -> t -> unit
(** [marshal_partition_table ~sector_size buf t] serializes the GPT partition table to [buf].
    The caller is expected to write the contents of [buf] to [t.partition_entry_lba]. *)
