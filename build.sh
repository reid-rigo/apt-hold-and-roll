case $1 in

  build)
    dune build
    echo
    ;;

  test)
    dune runtest
    ;;

  compress)
    upx _build/default/cli/Cli.exe -o _build/default/cli/Cli.min.exe
    ;;

  *)
    dune build
    echo
    ./_build/default/cli/Cli.exe
    ;;

esac