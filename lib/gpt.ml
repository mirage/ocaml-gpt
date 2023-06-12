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
 let ( >>= ) = Result.bind

 let sizeof = 128

  module Partition = struct
    type t = {
      type_guid : string;
      partition_guid : string;
      starting_lba : int64;
      ending_lba : int64;
      attributes : int64;
      name : string;
    }
  
    let make ?(name="") ~type_guid ~attributes starting_lba ending_lba =
      match Uuidm.of_string type_guid with
    | None -> Error (Printf.sprintf "Invalid type_guid: not a valid UUID\n%!")
    | Some _ ->
        let partition_guid = Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ()) in
        Ok {
          type_guid;
          partition_guid;
          starting_lba;
          ending_lba;
          attributes;
          name;
        }    
  
    (** extracted from https://en.m.wikipedia.org/wiki/GUID_Partition_Table **)
    let sizeof = 128
    let type_guid_offset = 0 (* mixed endian *)
    let partition_guid_offset = 16 (* mixed endian *)
    let starting_lba_offset = 32 (* little endian *)
    let ending_lba_offset = 40 (* inclusive, usually odd *)
    let attributes_offset = 48
    let name_offset = 56
  
    let unmarshal buf =
      (if Cstruct.length buf < sizeof then
         Error (Printf.sprintf "Partition entry too small: %d < %d" (Cstruct.length buf) sizeof)
       else Ok ())
      >>= fun () ->
      let buf = Cstruct.sub buf 0 sizeof in
      let type_guid_bytes = Bytes.sub (Cstruct.to_bytes buf) type_guid_offset 16 in
      let type_guid =Bytes.to_string type_guid_bytes in
      let partition_guid_bytes = Bytes.sub ((Bytes.sub (Cstruct.to_bytes buf) partition_guid_offset 16)) 0 16 in
      let partition_guid = Bytes.to_string partition_guid_bytes in
      let starting_lba = Cstruct.LE.get_uint64 buf starting_lba_offset in
      let ending_lba = Cstruct.LE.get_uint64 buf ending_lba_offset in
      let attributes = Cstruct.LE.get_uint64 buf attributes_offset in
      let name_bytes = Bytes.sub (Cstruct.to_bytes buf) name_offset 72 in
      let name = Bytes.to_string name_bytes in
      Ok {
        type_guid;
        partition_guid;
        starting_lba;
        ending_lba;
        attributes;
        name;
      }
  
      let marshal (buf : Cstruct.t) t =
        let type_guid_bytes =  Bytes.of_string t.type_guid in
        let partition_guid_bytes =Bytes.of_string t.partition_guid in
        let name_bytes = Bytes.of_string t.name in
        let name_length = min (String.length t.name) 72 in
        Cstruct.blit (Cstruct.of_bytes type_guid_bytes) 0 buf (type_guid_offset + 1) 16;
        Cstruct.blit (Cstruct.of_bytes partition_guid_bytes) 0 buf (partition_guid_offset + 1) 16;
        Cstruct.LE.set_uint64 buf starting_lba_offset t.starting_lba;
        Cstruct.LE.set_uint64 buf ending_lba_offset t.ending_lba;
        Cstruct.LE.set_uint64 buf attributes_offset t.attributes;
        Cstruct.blit (Cstruct.of_bytes name_bytes) 0 buf name_offset name_length
  end
  

(* GPT header from wikipedia https://en.m.wikipedia.org/wiki/GUID_Partition_Table *)
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
  disk_guid : string;
  partition_entry_lba : int64;
  num_partition_entries : int;
  partitions : Partition.t list;
  partition_size : int32;
  partitions_crc32 : int32;
}

let calculate_header_crc32 header =
  let header_bytes =
    Bytes.concat Bytes.empty [
      Bytes.of_string header.signature;
      Bytes.of_string (Int32.to_string header.revision);
      Bytes.of_string (Int32.to_string header.header_size);
      Bytes.of_string (Int32.to_string header.header_crc32);
      Bytes.of_string (Int32.to_string header.reserved);
      Bytes.of_string (Int64.to_string header.current_lba);
      Bytes.of_string (Int64.to_string header.backup_lba);
      Bytes.of_string (Int64.to_string header.first_usable_lba);
      Bytes.of_string (Int64.to_string header.last_usable_lba);
      Bytes.of_string header.disk_guid;
      Bytes.of_string (Int64.to_string header.partition_entry_lba);
      Bytes.of_string (Int.to_string header.num_partition_entries);
      Bytes.of_string (Int32.to_string header.partitions_crc32);
    ]
  in
  let len = Bytes.length header_bytes in
  let crc32 = Checkseum.Crc32.digest_bytes header_bytes 0 len Checkseum.Crc32.default in
  crc32

  let calculate_partition_crc32 partitions =
    List.fold_left (fun crc32 partition ->
      let partition_data =
        Bytes.concat Bytes.empty [
          Bytes.of_string partition.Partition.type_guid;
          Bytes.of_string partition.Partition.partition_guid;
          Bytes.of_string (Int64.to_string partition.Partition.starting_lba);
          Bytes.of_string (Int64.to_string partition.Partition.ending_lba);
          Bytes.of_string (Int64.to_string partition.Partition.attributes);
          Bytes.of_string partition.Partition.name;
        ]
      in
      let len = Bytes.length partition_data in
      let result = Checkseum.Crc32.digest_bytes partition_data 0 len crc32 in
      result
    ) Checkseum.Crc32.default partitions

let make partitions = 
  let current_lba = 1L in
  let backup_lba = 0L in
  let first_usable_lba = 0L in
  let last_usable_lba = 0L in
  let partition_entry_lba = 2L in
  let num_partition_entries = List.length partitions in
  let disk_guid =  Uuidm.to_string (Uuidm.v4_gen (Random.State.make_self_init ()) ()) in
  let partition_size = Int32.of_int Partition.sizeof in
  let header_size = Int32.of_int sizeof in
  let revision = 0x010000l in
  let signature = "EFI PART" in
  let reserved = 0l in
  let partitions_crc32 =
    Optint.to_int32 (calculate_partition_crc32 partitions)
  in
  let header_crc32 =
    let header = {
      signature;
      revision;
      header_size;
      header_crc32 = 0l; (* Temporary value before caculating *)
      reserved;
      current_lba;
      backup_lba;
      first_usable_lba;
      last_usable_lba;
      disk_guid;
      partition_entry_lba;
      num_partition_entries;
      partitions;
      partition_size;
      partitions_crc32;
    } in
    Optint.to_int32 (calculate_header_crc32 header)
  in
  Ok { 
    signature;
    revision;
    header_size;
    header_crc32;
    reserved;
    current_lba; 
    backup_lba; 
    first_usable_lba; 
    last_usable_lba;
    disk_guid;
    partition_entry_lba;
    num_partition_entries; 
    partitions;
    partition_size;
    partitions_crc32;
  }

  let signature_offset = 0
  let revision_offset = 8
  let header_size_offset = 12
  let header_crc32_offset = 16
  let reserved_offset = 20
  let current_lba_offset = 24
  let backup_lba_offset = 32
  let first_usable_lba_offset = 40
  let last_usable_lba_offset = 48
  let disk_guid_offset = 56
  let partition_entry_lba_offset = 72
  let num_partition_entries_offset = 80
  let partition_size_offset = 84
  let partitions_crc32_offset = 88

let unmarshal buf =
  if Cstruct.length buf < sizeof then
    Error (Printf.sprintf "GPT too small: %d < %d" (Cstruct.length buf) sizeof)
  else
    let signature = Cstruct.sub buf signature_offset revision_offset |> Cstruct.to_string in
    match signature with
    | "EFI PART" ->
      let revision = Cstruct.LE.get_uint32 buf revision_offset in
      if revision = 0x010000l then
        let header_size = Cstruct.LE.get_uint32 buf header_size_offset in
        let header_crc32 = Cstruct.LE.get_uint32 buf header_crc32_offset in
        let reserved = Cstruct.LE.get_uint32 buf reserved_offset in
        let current_lba = Cstruct.LE.get_uint64 buf current_lba_offset in
        let backup_lba = Cstruct.LE.get_uint64 buf backup_lba_offset in
        let first_usable_lba = Cstruct.LE.get_uint64 buf first_usable_lba_offset in
        let last_usable_lba = Cstruct.LE.get_uint64 buf last_usable_lba_offset in
        let disk_guid_bytes = Cstruct.sub buf disk_guid_offset partition_entry_lba_offset |> Cstruct.to_string in
        let disk_guid = match Uuidm.of_bytes disk_guid_bytes with
          | Some guid -> Ok (Uuidm.to_bytes guid)
          | None -> Error (Printf.sprintf "Failed to parse disk_guid; got '%s'" disk_guid_bytes)
        in
        disk_guid >>= fun disk_guid ->
        let partition_entry_lba = Cstruct.LE.get_uint64 buf partition_entry_lba_offset in
        let num_partition_entries = Cstruct.LE.get_uint32 buf num_partition_entries_offset |> Int32.to_int in
        let partitions_crc32 = Cstruct.LE.get_uint32 buf partitions_crc32_offset in
        let partition_size = Cstruct.LE.get_uint32 buf partition_size_offset in
        let partitions = [] in
        Ok {
          signature;
          revision;
          header_size;
          header_crc32;
          reserved;
          current_lba;
          backup_lba;
          first_usable_lba;
          last_usable_lba;
          disk_guid;
          partition_entry_lba;
          num_partition_entries;
          partitions;
          partition_size;
          partitions_crc32;
        }
      else
        Error (Printf.sprintf "Unknown revision; expected 0x10000, got 0x%08lx" revision)
    | x ->
      Error (Printf.sprintf "Signature not found; expected 'EFI PART', got '%s'" x)
  
    let marshal (buf : Cstruct.t) t =
      Cstruct.blit_from_string t.signature 0 buf signature_offset revision_offset;
      Cstruct.LE.set_uint32 buf revision_offset t.revision;
      Cstruct.LE.set_uint32 buf header_size_offset t.header_size;
      Cstruct.LE.set_uint32 buf header_crc32_offset t.header_crc32;
      Cstruct.LE.set_uint32 buf reserved_offset t.reserved;
      Cstruct.LE.set_uint64 buf current_lba_offset t.current_lba;
      Cstruct.LE.set_uint64 buf backup_lba_offset t.backup_lba;
      Cstruct.LE.set_uint64 buf first_usable_lba_offset t.first_usable_lba;
      Cstruct.LE.set_uint64 buf last_usable_lba_offset t.last_usable_lba;
      Cstruct.blit_from_string t.disk_guid 56 buf disk_guid_offset partition_entry_lba_offset;
      Cstruct.LE.set_uint64 buf partition_entry_lba_offset t.partition_entry_lba;
      Cstruct.LE.set_uint32 buf num_partition_entries_offset (Int32.of_int t.num_partition_entries);
      Cstruct.LE.set_uint32 buf partition_size_offset t.partition_size;
      Cstruct.LE.set_uint32 buf partitions_crc32_offset t.partitions_crc32;