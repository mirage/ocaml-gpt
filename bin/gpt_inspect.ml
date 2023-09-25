open Cmdliner

let _sizeof = 128

let print_gpt_partitions partitions =
  List.iteri
    (fun i part ->
      if
        Uuidm.to_string part.Gpt.Partition.type_guid
        <> "00000000-0000-0000-0000-000000000000"
      then (
        Printf.printf "    Partition %d:\n" (i + 1);
        Printf.printf "      name: %s\n" part.Gpt.Partition.name;
        Printf.printf "      type-guid: %s\n"
          (Uuidm.to_string ~upper:true part.Gpt.Partition.type_guid);
        Printf.printf "      partition-guid: %s\n"
          (Uuidm.to_string ~upper:true part.Gpt.Partition.partition_guid);
        Printf.printf "      attributes: %Ld\n" part.Gpt.Partition.attributes;
        Printf.printf "      starting-lba: %Ld\n"
          part.Gpt.Partition.starting_lba;
        Printf.printf "      ending-lba: %Ld\n" part.Gpt.Partition.ending_lba))
    partitions

let print_gpt_header gpt =
  Printf.printf "GPT header:\n";
  Printf.printf "  revision: 0x%08lx\n" gpt.Gpt.revision;
  Printf.printf "  gpt-size: %lu\n" gpt.Gpt.header_size;
  Printf.printf "  gpt-checksum: %lu\n" gpt.Gpt.header_crc32;
  Printf.printf "  reserved: %lu\n" gpt.Gpt.reserved;
  Printf.printf "  current-lba: %Lu\n" gpt.Gpt.current_lba;
  Printf.printf "  backup-lba: %Lu\n" gpt.Gpt.backup_lba;
  Printf.printf "  first-usable-lba: %Lu\n" gpt.Gpt.first_usable_lba;
  Printf.printf "  last-usable-lba: %Lu\n" gpt.Gpt.last_usable_lba;
  Printf.printf "  disk-guid: %s\n"
    (Uuidm.to_string ~upper:true gpt.Gpt.disk_guid);
  Printf.printf "  first-partition-lba: %Lu\n" gpt.Gpt.partition_entry_lba;
  Printf.printf "  total-partitions: %lu\n" gpt.Gpt.num_partition_entries;
  Printf.printf "  partition-size: %lu\n" gpt.Gpt.partition_size;
  Printf.printf "  partitions-checksum: %lu\n" gpt.Gpt.partitions_crc32;
  Printf.printf "  Partitions: \n"; 
  Printf.printf "  *********** \n"; 
  print_gpt_partitions gpt.Gpt.partitions

let really_input fd buf pos len =
  let rec loop pos remaining =
    if remaining > 0 then
      let len = Unix.read fd buf pos remaining in
      if len = 0 then raise End_of_file;
      loop (pos + len) (remaining - len)
  in
  loop pos len

let read_gpts sector_size gpts =
  List.iter
    (fun gpt ->
       let fd = Unix.openfile gpt Unix.[O_RDONLY; O_CLOEXEC] 0 in
       let buf = Bytes.create sector_size in
       let _ = Unix.lseek fd sector_size Unix.SEEK_SET in
       let () = really_input fd buf 0 sector_size in
       match Gpt.unmarshal (Cstruct.of_bytes buf) ~sector_size with
       | Error msg ->
         Printf.printf "Failed to read GPT header from %s: %s\n" gpt msg;
         exit 1
       | Ok (`Read_partition_table (lba, num_sectors), k) ->
         let buf = Bytes.create (sector_size * num_sectors) in
         let _ = Unix.lseek fd (sector_size * Int64.to_int lba) Unix.SEEK_SET in
         let () = really_input fd buf 0 (sector_size * num_sectors) in
         let () = Unix.close fd in
         match k (Cstruct.of_bytes buf) with
         | Error msg ->
           Printf.printf "Failed to read GPT partition table from %s: %s\n" gpt msg;
           exit 1
         | Ok gpt ->
           print_gpt_header gpt
    )
    gpts

let gpts = Arg.(non_empty & pos_all file [] & info [] ~docv:"disk_images")

let sector_size =
  let doc = Arg.info ["sector-size"] ~docv:"SECTOR_SIZE" in
  Arg.(value & opt int 512 & doc)

let cmd =
  let doc =
    "Inspect the GUID Partition Table (GPT) headers of one or more disk images."
  in
  let info = Cmd.info "gpt_inspect" ~version:"1.0.0" ~doc in
  Cmd.v info Term.(const read_gpts $ sector_size $ gpts)

let main () = exit (Cmd.eval cmd)
let () = main ()
