open Batteries

module Date : sig
  type t
  val make_from_unix_date_string : string -> t option
  val to_string : t -> string
end = struct
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
