open Batteries
open Date

module Version: sig
  type t
  val make : string -> (t, string) result
  val compare : t -> t -> int
  val to_string : t -> string
end = struct
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

let installed_packages () =
  let input = Unix.open_process_in "apt list --installed" in
  let pkg_lines = BatEnum.skip 1 (BatIO.lines_of input) in
  let pkg_names = BatEnum.map (fun l -> List.hd (String.split_on_char '/' l)) pkg_lines in
  List.of_enum pkg_names

let held_packages () =
  ()