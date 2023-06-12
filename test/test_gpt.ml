(*let ( let* ) = Result.bind *)

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
      "{ type_guid = %s; partition_guid = %s; starting_lba = %Ld; \
        ending_lba = %Ld; attributes = %Ld; name \
        = %s }"
      type_guid partition_guid starting_lba ending_lba
      attributes name

  type t = Gpt.Partition.t

  let equal = ( = ) (* :/ *)
end

let _partition =
  (module Testable_partition : Alcotest.TESTABLE with type t = Gpt.Partition.t)

let test_make_partition () =
  match
    Gpt.Partition.make
      ~type_guid:"12345678-1234-1234-1234-123456789abc"
      ~name:"Test Partition"
      ~attributes:255L
      2048L
      4096L
  with
  | Ok partition -> ignore partition
  | Error error -> Alcotest.failf "Error creating partition: %s" error

let test_make_partition_wrong_type_guid () =
  match Gpt.Partition.make ~type_guid:"012345678" ~name:"Test Partition" ~attributes:255L 2048L 4096L
  with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail "Expected an invalid type_uuid error"

let partition_test_collection =
  [
    ("correct-partition", `Quick, test_make_partition);
    ("wrong-type_guid", `Quick, test_make_partition_wrong_type_guid);
  ]

let () =
Alcotest.run "Ocaml Gpt"
  [ ("Test GPT Partitions", partition_test_collection)]