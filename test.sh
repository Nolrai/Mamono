
main () {
  ./format.sh
  find . -name "*-test.exe" -type f -perm -111 -exec echo rm -v {} \;
  find . -name "*-test.exe" -type f -perm -111 -exec rm -v {} \;
  cabal clean && ( cabal build Mamono-test || exit 1 )

  find . -name "*-test.exe" -type f -perm -111 -exec {} \;
}

main "$@"
