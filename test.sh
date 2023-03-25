
main () {
  find . -name "*-test.exe" -type f -perm -111 -exec echo rm -v {} \;
  find . -name "*-test.exe" -type f -perm -111 -exec rm -v {} \;
  cabal build || exit 1

  find . -name "*-test.exe" -type f -perm -111 -exec {} \;
}

main "$@"
