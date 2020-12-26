let all_versions () =
  let pkg_names = Apt.installed_packages() in
  List.iter
    (fun pkg ->
      let versions = Apt.package_versions_from_changelog pkg in
      List.iter
        (fun (pv : Apt.package_version) -> Printf.printf "%s %s\n" pkg (Apt.Version.to_string pv.version))
        versions
    )
    pkg_names

let () =
  all_versions ()
