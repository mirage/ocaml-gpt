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
       %Ld; attributes = %Ld; name = %s }"
      (Uuidm.to_string type_guid)
      (Uuidm.to_string partition_guid)
      starting_lba ending_lba attributes name

  type t = Gpt.Partition.t

  let equal = ( = ) (* :/ *)
end

let partition =
  (module Testable_partition : Alcotest.TESTABLE with type t = Gpt.Partition.t)

let test_make_partition () =
  let type_guid = Option.get (Uuidm.of_string "12345678-1234-1234-1234-123456789abc") in 
  match
    Gpt.Partition.make ~type_guid
      ~name:"Test Partition" ~attributes:255L 2048L 4096L
  with
  | Ok partition -> ignore partition
  | Error error -> Alcotest.failf "Error creating partition: %s" error

let test_make_gpt_no_partitions () =
  match Gpt.make ~disk_size:1024L ~sector_size:512 [] with
  | Ok _ -> ()
  | Error e -> Alcotest.failf "Expected Ok, got %s" e

let test_make_gpt_too_many_partitions () =
  let type_guid = Option.get (Uuidm.of_string "12345678-1234-1234-1234-123456789abc") in
  let num_partitions = 129 in
  let partitions =
    Array.init num_partitions (fun i ->
        let partition =
          Gpt.Partition.make ~type_guid
            ~name:(Printf.sprintf "Partition %d" (i + 1))
            ~attributes:255L
            (Int64.of_int (i * 2048))
            (Int64.of_int ((i + 1) * 2048))
        in
        match partition with
        | Ok p -> p
        | Error _ -> Alcotest.fail "Expected Ok")
  in
  match Gpt.make ~disk_size:1024L ~sector_size:512 (Array.to_list partitions) with
  | Ok _ -> Alcotest.fail "Expected too many partitons error"
  | Error _ -> ()

let test_make_gpt_overlapping_partitions () =
  let type_guid = Option.get (Uuidm.of_string "12345678-1234-1234-1234-123456789abc") in
  let p1 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:"Partition 1" ~attributes:255L 2048L 4096L)
  in
  let p2 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:"Partition 1" ~attributes:255L 3048L 4096L)
  in
  match (Gpt.make ~disk_size:1024L ~sector_size:512 [ p1; p2 ], Gpt.make ~disk_size:1024L ~sector_size:512 [ p2; p1 ]) with
  | Ok _, _ | _, Ok _ -> Alcotest.fail "Expected overlapping error"
  | Error _, Error _ -> ()

let test_make_gpt_sorted_partitions () =
  let type_guid = Option.get (Uuidm.of_string "12345678-1234-1234-1234-123456789abc") in
  let p1 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:"Partition 1" ~attributes:255L 2048L 1L)
  in
  let p2 =
    get_ok
      (Gpt.Partition.make ~type_guid
         ~name:"Partition 2" ~attributes:255L 4096L 1L)
  in
  let m1 = get_ok (Gpt.make ~disk_size:1024L ~sector_size:512 [ p1; p2 ]) in
  let m2 = get_ok (Gpt.make ~disk_size:1024L ~sector_size:512 [ p2; p1 ]) in
  (* polymorphic compare :`( *)
  Alcotest.(
    check (list partition) "partitons equal" m1.partitions m2.partitions)

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

let () =
  Alcotest.run "Ocaml Gpt"
    [
      ("Test GPT Partitions", partition_test_collection);
      ("Test GPT Header", gpt_header_test_collection);
    ]
