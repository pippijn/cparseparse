(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  {
    tool = "cpapa";
    suffixes = [".cc"; ".c"; ".ii"; ".i"];
    options = Some "-trivial";
    dirs = [
      "testsuite/ccparse";
      "testsuite/in";
    ];
  };
])
