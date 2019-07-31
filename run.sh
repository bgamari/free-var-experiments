build() {
  mod=$1
  ghc -O -ddump-stg -ddump-to-file -ddump-ds -ddump-simpl-trace -ddump-simpl -dverbose-core2core -ddump-inlinings \
    $mod.hs 2>&1 | tee $mod.log > /dev/null
}

touch Test2.hs; build Test2
touch Step3.hs; build Step3
touch Step4.hs; build Step4

