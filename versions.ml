open Batteries

module Version = struct
  type t =
    { epoch: int ;
      major: int ;
      minor: int ;
      patch: int ;
      revision: string option ; }

  let regexp =
    Str.regexp ("^" ^ "\\([0-9]*:\\)?" ^ "\\([0-9]+\\)\\.?" ^ "\\([0-9]*\\)\\.?" ^ "\\([0-9]*\\)" ^ "\\([a-zA-Z0-9\\.\\+~-]*\\)" ^ "$")

  let make s =
    match Str.string_match regexp s 0 with
    | true -> (
      let epoch_s = try Str.matched_group 1 s with Not_found -> "" in
      let major_s = try Str.matched_group 2 s with Not_found -> "" in
      let minor_s = try Str.matched_group 3 s with Not_found -> "" in
      let patch_s = try Str.matched_group 4 s with Not_found -> "" in
      let revision = try Some (Str.matched_group 5 s) with Not_found -> None in
      Ok { epoch = (try Int.of_string epoch_s with Failure _f -> 0) ;
           major = (try Int.of_string major_s with Failure _f -> 0) ;
           minor = (try Int.of_string minor_s with Failure _f -> 0) ;
           patch = (try Int.of_string patch_s with Failure _f -> 0) ;
           revision = revision ; })
    | false -> Error "not matched"

  let revision_as_int t =
    match t.revision with
    | Some r ->
      (let just_numbers = Str.global_replace (Str.regexp "[^0-9]+") "" r in
        Int.of_string just_numbers)
    | None -> 0

  let v_index t =
    t.epoch * 1_000_000_000 +
    t.major * 1_000_000 +
    t.minor * 1_000 +
    t.patch +
    (revision_as_int t) / 1_000_000_000

  let compare t1 t2 =
    Int.compare (v_index t1) (v_index t2)

  let to_string (v: t) =
    Printf.sprintf "Major: %i, Minor: %i, Patch: %i, Revision: %s" v.major v.minor v.patch (match v.revision with | Some revision -> revision | None -> "")

end

module Date = struct
  type t = Unix.tm
  let make y m d =
    Unix.({
      tm_sec = 0 ; tm_min = 0 ; tm_hour = 0 ;
      tm_mday = d ;
      tm_mon = m ;
      tm_year = y ;
      tm_wday = 0 ; tm_yday = 0 ;
      tm_isdst = false })

  (* Sat, 28 Jan 2017 00:06:50 +0100 *)
  let regexp = Str.regexp "^[a-zA-Z]+, \\([0-9][0-9]\\) \\([a-zA-Z]+\\) \\(20[0-9][0-9]\\)"

  let make_from_unix_date_string s =
    match Str.string_match regexp s 0 with
    | true ->
      (let d_s = Str.matched_group 1 s in
       let m_s = Str.matched_group 2 s in
       let y_s = Str.matched_group 3 s in
       let d = Int.of_string d_s in
       let m = match m_s with
       | "Jan" -> 1
       | "Feb" -> 2
       | "Mar" -> 3
       | "Apr" -> 4
       | "May" -> 5
       | "Jun" -> 6
       | "Jul" -> 7
       | "Aug" -> 8
       | "Sep" -> 9
       | "Oct" -> 10
       | "Nov" -> 11
       | "Dec" -> 12
       | _ -> 0 in
       let y = Int.of_string y_s in
       Some (make y m d))
    | false -> None

  let to_string (d: t) =
    Printf.sprintf "%d-%d-%d" d.tm_year d.tm_mon d.tm_mday
end

type package_version =
  { name: string ;
    version: Version.t ;
    release_date: Date.t ; }

let pkgs_dir = "/usr/share/doc/"

type pkg_string =
  | VersionString of string
  | DateString of string

let version_regexp = Str.regexp "^[a-zA-Z0-9-]+ (\\([^\\(\\)]+\\))"
let date_regexp = Str.regexp "^ -- .+  \\(.+\\)$"

let pkg_string_of_line line =
  match Str.string_match version_regexp line 0 with
  | true -> Some (VersionString (Str.matched_group 1 line))
  | false ->
    (match Str.string_match date_regexp line 0 with
     | true -> Some (DateString (Str.matched_group 1 line))
     | false -> None)

let package_versions_from_changelog pkg =
  let changelog_pattern = pkgs_dir ^ pkg ^ "/changelog*.gz" in
  let input = Unix.open_process_in ("/usr/bin/zcat " ^ changelog_pattern) in
  let input_enum = BatIO.lines_of input in
  let pkg_strings = Enum.filter_map pkg_string_of_line input_enum in
  let (version_strings, date_strings) = Enum.partition
    (fun s-> match s with
              | VersionString _vs -> true
              | DateString _ds -> false)
    pkg_strings in
  Enum.fold2
    (fun vs ds l ->
      match (vs, ds) with
      | (VersionString vs, DateString ds) -> (
        match (Version.make vs, Date.make_from_unix_date_string ds) with
        | (Ok v, Some d) -> l @ [{ name = pkg ; version = v ; release_date = d ; }]
        | (_, _) -> l
      )
      | (_, _) -> l)
    []
    version_strings
    date_strings

let all_versions () =
  let pkg_names = Sys.readdir pkgs_dir in
  Array.iter
    (fun pkg -> Printf.printf "%s\n" pkg)
    pkg_names

let _ =
  all_versions ()


