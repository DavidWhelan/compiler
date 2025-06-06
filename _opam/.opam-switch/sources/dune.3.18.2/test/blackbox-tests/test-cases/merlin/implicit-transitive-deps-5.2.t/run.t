  $ stdlib="$(ocamlc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/STDLIB=$stdlib:$BUILD_PATH_PREFIX_MAP"

  $ dune build @check

Merlin must be able to locate the definitions of values coming from an
implicit transitive dependency, even when `implicit-transitive-dependency` is
set to `false`. They should be part of the source path returned by the
configuration.

In this test the dependencies are as follow:
main -> lib1 -> lib2 -> stdlib

When using OCaml >= 5.2 these hidden dependencies are passed to merlin using the
new `*H` directives.
  $ FILE=$PWD/bin/main.ml
  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin |
  > sed -E "s/[[:digit:]]+:/\?:/g" | tr '(' '\n' | grep -E ":[BS]H?\?"
  ?:B?:$TESTCASE_ROOT/_build/default/bin/.main.eobjs/byte)
  ?:B?:$TESTCASE_ROOT/_build/default/src/lib1/.lib1.objs/byte)
  ?:S?:$TESTCASE_ROOT/bin)
  ?:S?:$TESTCASE_ROOT/src/lib1)
  ?:BH?:/STDLIB/unix)
  ?:BH?:$TESTCASE_ROOT/_build/default/src/lib2/.lib2.objs/byte)
  ?:SH?:/STDLIB/unix)
  ?:SH?:$TESTCASE_ROOT/src/lib2)

  $ FILE=$PWD/src/lib1/lib1.ml
  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin |
  > sed -E "s/[[:digit:]]+:/\?:/g" | tr '(' '\n' | grep -E ":[BS]H?\?"
  ?:B?:/STDLIB/unix)
  ?:B?:$TESTCASE_ROOT/_build/default/src/lib1/.lib1.objs/byte)
  ?:B?:$TESTCASE_ROOT/_build/default/src/lib2/.lib2.objs/byte)
  ?:S?:/STDLIB/unix)
  ?:S?:$TESTCASE_ROOT/src/lib1)
  ?:S?:$TESTCASE_ROOT/src/lib2)

  $ FILE=$PWD/src/lib2/lib2.ml
  $ printf "(4:File%d:%s)" ${#FILE} $FILE | dune ocaml-merlin |
  > sed -E "s/[[:digit:]]+:/\?:/g" | tr '(' '\n' | grep -E ":[BS]H?\?"
  ?:B?:/STDLIB/unix)
  ?:B?:$TESTCASE_ROOT/_build/default/src/lib2/.lib2.objs/byte)
  ?:S?:/STDLIB/unix)
  ?:S?:$TESTCASE_ROOT/src/lib2)
