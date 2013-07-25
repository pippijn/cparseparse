(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  { empty with
    tool = "cpapa.native";
    suffixes = [".cc"; ".c"; ".ii"; ".i"];
    options = Some "-trivial";
    dirs = [
      "ccparse";
      "in";
    ];
  };
])
