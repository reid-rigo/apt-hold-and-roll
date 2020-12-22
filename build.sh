case $1 in

  build)
    dune build
    echo
    ;;

  *)
    dune build
    echo
    ./_build/default/versions.exe
    ;;

esac