namespace java com.jpgneves.miser.test

include "foo"
include "Foo"
cpp_include "Bar"

# Some typedefs
typedef bool mybool
typedef i64 long
typedef list<string> text

/*
  My amazing Struct
*/
struct Foo {
  1: bool a
  2: mybool b
  3: optional string c
  4: long d
  5: required string e
  6: map<long, text> f
}

union Bar {
  1: bool a // And even a union!
  2: i32 b
}