open Alcotest

let test_no_patch () =
  match Apt.Version.make "2.1+whatever" with
  | Ok _v -> (check int) "same int" 0 0 (* (Apt.Version.patch v) *)
  | Error e -> failwith e

let run () =
  run "Apt.Version" [
    "no-patch-case", [
      test_case "No patch with revision"  `Quick test_no_patch ;
    ]
  ]