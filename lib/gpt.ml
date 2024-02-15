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
let ( let* ) = Result.bind

let guid_len = 16

(* The size of a header not counting the reserved space *)
let sizeof = 92

module Partition = struct
  type t = {
    type_guid : Uuidm.t;
    partition_guid : Uuidm.t;
    starting_lba : int64;
    ending_lba : int64;
    attributes : int64;
    name : string; (*name should be encoded as a utf-16 string of 72 bytes*)
  }

  let make ?(name =  String.make 72 '\000') ~type_guid ~attributes starting_lba ending_lba =
    let partition_guid = Uuidm.v4_gen (Random.State.make_self_init ()) () in
    if String.length name <> 72 then
      Printf.ksprintf invalid_arg "Name length %d should be exactly 72\n" (String.length name);
    Ok
      {
        type_guid;
        partition_guid;
        starting_lba;
        ending_lba;
        attributes;
        name;
      }

  let is_zero_partition p =
    let zero_uuid = Option.get (Uuidm.of_string "00000000-0000-0000-0000-000000000000") in
    Uuidm.equal p.type_guid zero_uuid &&
    Uuidm.equal p.partition_guid zero_uuid &&
    p.starting_lba = 0L && p.ending_lba = 0L && p.attributes = 0L &&
    String.for_all (Char.equal '\000') p.name

  (** extracted from https://en.m.wikipedia.org/wiki/GUID_Partition_Table **)

  let sizeof = 128
  let type_guid_offset = 0 (* mixed endian *)
  let partition_guid_offset = 16 (* mixed endian *)
  let starting_lba_offset = 32 (* little endian *)
  let ending_lba_offset = 40 (* inclusive, usually odd *)
  let attributes_offset = 48
  let name_offset = 56

  let unmarshal buf =
    if Cstruct.length buf < sizeof then
      Printf.ksprintf invalid_arg "Partition entry too small: %d < %d"
        (Cstruct.length buf) sizeof;
    let type_guid_bytes =
      Cstruct.sub buf type_guid_offset guid_len
      |> Cstruct.to_string
    in
    (* We can use [Option.get] as we ensure the string is sufficiently long *)
    let type_guid = Option.get (Uuidm.of_mixed_endian_bytes type_guid_bytes) in
    let partition_guid_bytes =
      Cstruct.sub buf partition_guid_offset guid_len
      |> Cstruct.to_string
    in
    let partition_guid = Option.get (Uuidm.of_mixed_endian_bytes partition_guid_bytes) in
    let starting_lba = Cstruct.LE.get_uint64 buf starting_lba_offset in
    let ending_lba = Cstruct.LE.get_uint64 buf ending_lba_offset in
    let attributes = Cstruct.LE.get_uint64 buf attributes_offset in
    let name_bytes = Cstruct.sub buf name_offset 72 in
    let name = Cstruct.to_string name_bytes in
    { type_guid; partition_guid; starting_lba; ending_lba; attributes; name }

  let marshal (buf : Cstruct.t) t =
    let name_buf = Cstruct.create 72 in
    let name_struct = Cstruct.of_string t.name in
    let name_length = min (Cstruct.length name_buf) (Cstruct.length name_struct) in
    Cstruct.blit_from_string t.name 0 name_buf 0 name_length;
    Cstruct.blit_from_string
      (Uuidm.to_mixed_endian_bytes t.type_guid) 0
      buf type_guid_offset guid_len;
    Cstruct.blit_from_string
      (Uuidm.to_mixed_endian_bytes t.partition_guid) 0
      buf partition_guid_offset guid_len;
    Cstruct.LE.set_uint64 buf starting_lba_offset t.starting_lba;
    Cstruct.LE.set_uint64 buf ending_lba_offset t.ending_lba;
    Cstruct.LE.set_uint64 buf attributes_offset t.attributes;
    Cstruct.blit name_buf 0 buf name_offset 72
end

(* GPT header from wikipedia https://en.m.wikipedia.org/wiki/GUID_Partition_Table *)
type t = {
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

let signature = "EFI PART"

let signature_offset = 0
let signature_len = 8
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

let calculate_header_crc32 header =
  (* this is basically [marshal_header] :( *)
  let buf = Cstruct.create sizeof in
  Cstruct.blit_from_string signature 0 buf 0 (String.length signature);
  Cstruct.LE.set_uint32 buf revision_offset header.revision;
  Cstruct.LE.set_uint32 buf header_size_offset header.header_size;
  (* zero header_crc32 *)
  Cstruct.LE.set_uint32 buf reserved_offset header.reserved;
  Cstruct.LE.set_uint64 buf current_lba_offset header.current_lba;
  Cstruct.LE.set_uint64 buf backup_lba_offset header.backup_lba;
  Cstruct.LE.set_uint64 buf first_usable_lba_offset header.first_usable_lba;
  Cstruct.LE.set_uint64 buf last_usable_lba_offset header.last_usable_lba;
  Cstruct.blit_from_string (Uuidm.to_mixed_endian_bytes header.disk_guid)
    0 buf disk_guid_offset guid_len;
  Cstruct.LE.set_uint64 buf partition_entry_lba_offset header.partition_entry_lba;
  Cstruct.LE.set_uint32 buf num_partition_entries_offset header.num_partition_entries;
  Cstruct.LE.set_uint32 buf partition_size_offset header.partition_size;
  Cstruct.LE.set_uint32 buf partitions_crc32_offset header.partitions_crc32;
  Checkseum.Crc32.digest_bigstring buf.buffer buf.off buf.len Checkseum.Crc32.default

let calculate_partition_crc32 num_partitions partitions =
  let num_partitions = Int32.to_int num_partitions in
  let buf = Cstruct.create Partition.sizeof in
  let crc =
    List.fold_left
      (fun crc32 partition ->
         Partition.marshal buf partition;
         Checkseum.Crc32.digest_bigstring buf.buffer buf.off buf.len crc32)
      Checkseum.Crc32.default partitions
  in
  let zero_partition = String.make Partition.sizeof '\000' in
  let rec loop crc n =
    if n = 0 then
      crc
    else
      let crc = Checkseum.Crc32.digest_string zero_partition 0 Partition.sizeof crc in
      loop crc (pred n)
  in
  loop crc (num_partitions - List.length partitions)

let table_sectors_required num_partition_entries sector_size =
  (((num_partition_entries * Partition.sizeof) + sector_size - 1) /sector_size)

let make ?(disk_guid) ~disk_sectors ~sector_size partitions =
  let num_partition_entries = 128 in
  let num_actual_partition_entries = List.length partitions in
  let* () =
    if num_actual_partition_entries > num_partition_entries then
      Error
        ((Printf.sprintf "Number of partitions %d exceeds required number %d\n%!")
           num_actual_partition_entries num_partition_entries)
    else
      Ok ()
  in
  let partitions =
    List.sort
      (fun p1 p2 ->
         Int64.unsigned_compare p1.Partition.starting_lba
           p2.Partition.starting_lba)
      partitions
  in
  (* Check for overlapping partions *)
  let* _last_partition_lba =
    List.fold_left
      (fun r p ->
         let* offset = r in
         if Int64.unsigned_compare offset p.Partition.starting_lba < 0 then
           Ok p.Partition.ending_lba
         else Error (Printf.sprintf "Partitions overlap"))
      (Ok 1L) partitions
  in
  let current_lba = 1L in
  let backup_lba = Int64.sub disk_sectors 1L in
  let last_usable_lba = Int64.sub backup_lba 1L in
  let partition_entry_lba = 2L in
  let first_usable_lba =
    let partition_table_sectors = table_sectors_required num_partition_entries sector_size in
    Int64.(add partition_entry_lba (of_int partition_table_sectors))
  in
  let disk_guid = Option.value disk_guid ~default:(Uuidm.v4_gen (Random.State.make_self_init ()) ()) in
  let partition_size = Int32.of_int Partition.sizeof in
  let header_size = Int32.of_int sizeof in
  let revision = 0x010000l in
  let reserved = 0l in
  let num_partition_entries = Int32.of_int num_partition_entries in
  let partitions_crc32 =
    Optint.to_int32 (calculate_partition_crc32 num_partition_entries partitions)
  in
  let header =
    {
      revision;
      header_size;
      header_crc32 = 0l;
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
  in
  let header_crc32 = Optint.to_int32 (calculate_header_crc32 header) in
  Ok { header with header_crc32 }

let unmarshal buf ~sector_size =
  if Cstruct.length buf < sizeof then
    Printf.ksprintf invalid_arg "GPT too small: %d < %d" (Cstruct.length buf) sizeof;
  let signature =
    Cstruct.sub buf signature_offset signature_len |> Cstruct.to_string
  in
  let* () =
    match signature with
    | "EFI PART" -> Ok ()
    | x ->
      Error
        (Printf.sprintf "Signature not found; expected 'EFI PART', got '%s'" x)
  in
  let revision = Cstruct.LE.get_uint32 buf revision_offset in
  let* () =
    if revision = 0x010000l then Ok ()
    else
      Error
        (Printf.sprintf "Unknown revision; expected 0x10000, got 0x%08lx"
           revision)
  in
  let header_size = Cstruct.LE.get_uint32 buf header_size_offset in
  let header_crc32 = Cstruct.LE.get_uint32 buf header_crc32_offset in
  let* () =
    let crc32 =
      Checkseum.Crc32.digest_bigstring buf.buffer buf.off header_crc32_offset
        Checkseum.Crc32.default
    in
    let crc32 = Checkseum.Crc32.digest_string "\000\000\000\000" 0 4 crc32 in
    let crc32 =
      Checkseum.Crc32.digest_bigstring buf.buffer (buf.off + header_crc32_offset + 4)
        (Int32.to_int header_size - header_crc32_offset - 4)
        crc32
    in
    let header_crc32' = Checkseum.Crc32.to_int32 crc32 in
    if header_crc32' = header_crc32 then
      Ok ()
    else
      Error "Bad GPT header checksum"
  in
  let reserved = Cstruct.LE.get_uint32 buf reserved_offset in
  let current_lba = Cstruct.LE.get_uint64 buf current_lba_offset in
  let backup_lba = Cstruct.LE.get_uint64 buf backup_lba_offset in
  let first_usable_lba =
    Cstruct.LE.get_uint64 buf first_usable_lba_offset
  in
  let last_usable_lba =
    Cstruct.LE.get_uint64 buf last_usable_lba_offset
  in
  let disk_guid_bytes =
    Cstruct.sub buf disk_guid_offset guid_len
    |> Cstruct.to_string
  in
  let* disk_guid =
    match Uuidm.of_mixed_endian_bytes disk_guid_bytes with
    | Some guid -> Ok guid
    | None ->
      Error
        (Printf.sprintf "Failed to parse disk_guid; got '%s'"
           disk_guid_bytes)
  in
  let partition_entry_lba =
    Cstruct.LE.get_uint64 buf partition_entry_lba_offset
  in
  let num_partition_entries =
    Cstruct.LE.get_uint32 buf num_partition_entries_offset
  in
  let partitions_crc32 =
    Cstruct.LE.get_uint32 buf partitions_crc32_offset
  in
  let partition_size =
    Cstruct.LE.get_uint32 buf partition_size_offset
  in
  (* let's not try to parse partition entries with a size we don't understand *)
  let* () =
    if partition_size <> Int32.of_int Partition.sizeof then
      Error (Printf.sprintf "Unexpected partition size: %lu" partition_size)
    else Ok ()
  in
  let partition_entry_sectors =
    (Int32.to_int num_partition_entries * Partition.sizeof + sector_size - 1) / sector_size
  in
  Ok (`Read_partition_table (partition_entry_lba, partition_entry_sectors),
      fun buf ->
        if Cstruct.length buf < Int32.to_int num_partition_entries * Partition.sizeof then
          Printf.ksprintf invalid_arg "partition table buffer too small";
        let buf = Cstruct.sub buf 0 (Int32.to_int num_partition_entries * Partition.sizeof) in
        let partitions_crc32' =
          Checkseum.Crc32.digest_bigstring buf.buffer buf.off buf.len Checkseum.Crc32.default
          |> Checkseum.Crc32.to_int32
        in
        let* () =
          if Int32.equal partitions_crc32' partitions_crc32 then Ok ()
          else
            Error "Bad partition table checksum"
        in
        let rev_partitions =
          List.fold_left
            (fun acc buf ->
               let entry = Partition.unmarshal buf in
               if Partition.is_zero_partition entry then
                 acc
               else
                 entry :: acc)
            []
            (List.init
               (Int32.to_int num_partition_entries)
               (fun i -> Cstruct.sub buf (i * Partition.sizeof) Partition.sizeof))
        in
        let partitions = List.rev rev_partitions in
        Ok
          {
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
          })

let marshal_header ~sector_size ~primary (buf : Cstruct.t) t =
  if Cstruct.length buf < sector_size || Cstruct.length buf < sizeof then
    invalid_arg "Gpt.marshal_header";
  let t =
    if primary then
      t
    else
      (* The backup header has the current and backup LBAs swapped *)
      let t =
        { t with current_lba = t.backup_lba; backup_lba = t.current_lba }
      in
      { t with header_crc32 = Optint.to_int32 (calculate_header_crc32 t) }
  in
  Cstruct.blit_from_string signature 0 buf signature_offset revision_offset;
  Cstruct.LE.set_uint32 buf revision_offset t.revision;
  Cstruct.LE.set_uint32 buf header_size_offset t.header_size;
  Cstruct.LE.set_uint32 buf header_crc32_offset t.header_crc32;
  Cstruct.LE.set_uint32 buf reserved_offset t.reserved;
  Cstruct.LE.set_uint64 buf current_lba_offset t.current_lba;
  Cstruct.LE.set_uint64 buf backup_lba_offset t.backup_lba;
  Cstruct.LE.set_uint64 buf first_usable_lba_offset t.first_usable_lba;
  Cstruct.LE.set_uint64 buf last_usable_lba_offset t.last_usable_lba;
  Cstruct.blit_from_string
    (Uuidm.to_mixed_endian_bytes t.disk_guid) 0
    buf disk_guid_offset guid_len;
  Cstruct.LE.set_uint64 buf partition_entry_lba_offset t.partition_entry_lba;
  Cstruct.LE.set_uint32 buf num_partition_entries_offset t.num_partition_entries;
  Cstruct.LE.set_uint32 buf partition_size_offset t.partition_size;
  Cstruct.LE.set_uint32 buf partitions_crc32_offset t.partitions_crc32;
  Cstruct.memset (Cstruct.sub buf sizeof (sector_size - sizeof)) 0

let marshal_partition_table ~sector_size (buf : Cstruct.t) t =
  if Cstruct.length buf < Int32.to_int t.num_partition_entries * Partition.sizeof then
    invalid_arg "Gpt.marshal_partition_table";
  if Cstruct.length buf mod sector_size <> 0 then
    invalid_arg "Gpt.marshal_partition_table";
  List.iteri
    (fun i p ->
      Partition.marshal
        (Cstruct.sub buf (i * Int32.to_int t.partition_size) Partition.sizeof)
        p)
    t.partitions;
  Cstruct.memset
    (Cstruct.shift buf (Int32.to_int t.num_partition_entries * Partition.sizeof))
    0
