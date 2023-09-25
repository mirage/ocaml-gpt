(*let ( let* ) = Result.bind *)

let get_ok = function
  | Ok x -> x
  | Error s -> Alcotest.failf "expected Ok, got Error \"%S\"" s

module Testable_partition = struct
  let pp ppf
      {
        Gpt.Partition.type_guid;
        partition_guid;
        starting_lba;
        ending_lba;
        attributes;
        name;
      } =
    Fmt.pf ppf
      "{ type_guid = %s; partition_guid = %s; starting_lba = %Ld; ending_lba = \
       %Ld; attributes = %Ld; name = %S }"
      (Uuidm.to_string type_guid)
      (Uuidm.to_string partition_guid)
      starting_lba ending_lba attributes name

  type t = Gpt.Partition.t

  let equal = ( = ) (* :/ *)
end

module Testable_gpt = struct
  type t = Gpt.t

  let pp ppf { Gpt.revision; header_size; header_crc32; reserved; current_lba;
               backup_lba; first_usable_lba; last_usable_lba; disk_guid;
               partition_entry_lba; num_partition_entries; partitions;
               partition_size; partitions_crc32 } =
    Fmt.pf ppf
      "{ revision = %lu; header_size = %lu; header_crc32 = %lu; reserved = %lu; \
         current_lba = %Lu; backup_lba = %Lu; first_usable_lba = %Lu; \
         last_usable_lba = %Lu; disk_guid = %a; partition_entry_lba = %Lu; \
         num_partition_entries = %lu; partition_size = %lu; partitions_crc32 = %lu; \
         partitions = %a; }"
      revision header_size header_crc32 reserved current_lba backup_lba first_usable_lba
      last_usable_lba Uuidm.pp disk_guid partition_entry_lba num_partition_entries
      partition_size partitions_crc32
      Fmt.(list Testable_partition.pp) partitions

  let equal t { Gpt.revision; header_size; header_crc32; reserved; current_lba;
                backup_lba; first_usable_lba; last_usable_lba; disk_guid;
                partition_entry_lba; num_partition_entries; partitions;
                partition_size; partitions_crc32 } =
    t.Gpt.revision = revision && t.header_size = header_size &&
    t.header_crc32 = header_crc32 && t.reserved = reserved &&
    t.current_lba = current_lba && t.backup_lba = backup_lba &&
    t.first_usable_lba = first_usable_lba && t.last_usable_lba = last_usable_lba &&
    Uuidm.equal t.disk_guid disk_guid &&
    t.partition_entry_lba = partition_entry_lba &&
    t.num_partition_entries = num_partition_entries &&
    t.partition_size = partition_size && t.partitions_crc32 = partitions_crc32 &&
    Testable_partition.equal t.partitions partitions
end

let partition =
  (module Testable_partition : Alcotest.TESTABLE with type t = Gpt.Partition.t)

let gpt =
  (module Testable_gpt : Alcotest.TESTABLE with type t = Gpt.t)

let utf16le_of_ascii s =
  String.init (2 * String.length s)
    (fun i ->
       if i land 1 = 0 then
         s.[i / 2]
       else
         '\000')

let name_of_ascii s =
  let s = utf16le_of_ascii s in
  String.init 72
    (fun i ->
       if i < String.length s then s.[i] else '\000')

let test_make_partition () =
  let type_guid = Option.get (Uuidm.of_string "12345678-1234-1234-1234-123456789abc") in
  match
    Gpt.Partition.make ~type_guid
      ~name:(name_of_ascii "Test Partition")
      ~attributes:255L 2048L 4096L
  with
  | Ok partition -> ignore partition
  | Error error -> Alcotest.failf "Error creating partition: %s" error

let test_make_gpt_no_partitions () =
  match Gpt.make ~disk_sectors:1024L ~sector_size:512 [] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "Expected Ok, got %s" e

let test_make_gpt_too_many_partitions () =
  let type_guid = Option.get (Uuidm.of_string "12345678-1234-1234-1234-123456789abc") in
  let num_partitions = 129 in
  let partitions =
    Array.init num_partitions (fun i ->
        let partition =
          Gpt.Partition.make ~type_guid
            ~name:(Printf.ksprintf name_of_ascii "Partition %d" (i + 1))
            ~attributes:255L
            (Int64.of_int (i * 2048))
            (Int64.of_int ((i + 1) * 2048))
        in
        match partition with
        | Ok p -> p
        | Error _ -> Alcotest.fail "Expected Ok")
  in
  match Gpt.make ~disk_sectors:1024L ~sector_size:512 (Array.to_list partitions) with
  | Ok _ -> Alcotest.fail "Expected too many partitons error"
  | Error _ -> ()

let test_make_gpt_overlapping_partitions () =
  let type_guid = Option.get (Uuidm.of_string "12345678-1234-1234-1234-123456789abc") in
  let p1 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:(name_of_ascii "Partition 1") ~attributes:255L 2048L 4096L)
  in
  let p2 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:(name_of_ascii "Partition 1") ~attributes:255L 3048L 4096L)
  in
  match (Gpt.make ~disk_sectors:1024L ~sector_size:512 [ p1; p2 ], Gpt.make ~disk_sectors:1024L ~sector_size:512 [ p2; p1 ]) with
  | Ok _, _ | _, Ok _ -> Alcotest.fail "Expected overlapping error"
  | Error _, Error _ -> ()

let test_make_gpt_sorted_partitions () =
  let type_guid = Option.get (Uuidm.of_string "12345678-1234-1234-1234-123456789abc") in
  let p1 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:(name_of_ascii "Partition 1") ~attributes:255L 2048L 1L)
  in
  let p2 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:(name_of_ascii "Partition 2") ~attributes:255L 4096L 1L)
  in
  let m1 = get_ok (Gpt.make ~disk_sectors:1024L ~sector_size:512 [ p1; p2 ]) in
  let m2 = get_ok (Gpt.make ~disk_sectors:1024L ~sector_size:512 [ p2; p1 ]) in
  (* polymorphic compare :`( *)
  Alcotest.(
    check (list partition) "partitons equal" m1.partitions m2.partitions)

let test_marshal_unmarshal () =
  let type_guid = Option.get (Uuidm.of_string "12345678-1234-1234-1234-123456789abc") in
  let p1 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:(name_of_ascii "Partition 1") ~attributes:255L 2048L 1L)
  in
  let p2 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:(name_of_ascii "Partition 2") ~attributes:255L 4096L 1L)
  in
  let morig = get_ok (Gpt.make ~disk_sectors:1024L ~sector_size:512 [ p1; p2 ]) in
  let buf_header = Cstruct.create 512 in
  let partition_table_len =
    let len = Int32.to_int morig.num_partition_entries * Int32.to_int morig.partition_size in
    512 * ((len + 511) / 512)
  in
  let buf_partition_table = Cstruct.create partition_table_len in
  Gpt.marshal_header ~sector_size:512 buf_header morig;
  Gpt.marshal_partition_table ~sector_size:512 buf_partition_table morig;
  match Gpt.unmarshal ~sector_size:512 buf_header with
  | Error e -> Alcotest.failf "Failed to parse marshalled gpt header: %s" e
  | Ok (`Read_partition_table (_lba, sectors), k) ->
    Printf.printf "expected %d, got %d\n%!" (partition_table_len / 512) sectors;
    Alcotest.(check int) "partition table length" (partition_table_len / 512) sectors;
    match k buf_partition_table with
    | Error e -> Alcotest.failf "Failed to parse marshalled partition table: %s" e
    | Ok unmarshalled ->
      Alcotest.check gpt "unmarshalled equal to original" morig unmarshalled

let partition_test_collection =
  [
    ("correct-partition", `Quick, test_make_partition);
  ]

let gpt_header_test_collection =
  [
    ("gpt-empty-partitions", `Quick, test_make_gpt_no_partitions);
    ("gpt-too-many-partitions", `Quick, test_make_gpt_too_many_partitions);
    ("gpt-overlapping-partitions", `Quick, test_make_gpt_overlapping_partitions);
    ("gpt-sorted-partitions", `Quick, test_make_gpt_sorted_partitions);
  ]

let gpt_test_collection =
  [
    ("gpt marshal then unmarshal", `Quick, test_marshal_unmarshal);
  ]

let () =
  Alcotest.run "Ocaml Gpt"
    [
      ("Test GPT Partitions", partition_test_collection);
      ("Test GPT Header", gpt_header_test_collection);
      ("Test GPT", gpt_test_collection);
    ]
