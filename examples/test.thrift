namespace java com.jpgneves.miser.test

include "foo"
include "Foo"
cpp_include "Bar"

# Some typedefs
typedef bool mybool
typedef i64 long
typedef list<string> text

// And some consts
const i64 MyInt = -32
const double MyDouble = 1.0e-2
const list<i8> MyList = [0, 1, 2, 3, 4, 5]
const map<i8, string> MyMap = { 0:1, 1:[2,3] }

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
  7: list<i8> g = [0, 1, 2, 3]
}

union Bar {
  1: bool a // And even a union! (And inline comments!)
  2: i32 b
}

exception Deaded {
  1: string msg
}

enum Baz {
     ONE
     TWO = 2
}

service TestService {
  string sayHello(1: string name)
  oneway void kill() throws (1: Deaded heDed)
}

service InheritingService extends TestService {
  map<long, text> doSomething()
}