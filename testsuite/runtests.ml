(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  {
    tool = "cpapa";
    suffixes = [".cc"; ".c"; ".ii"; ".i"];
    options = Some "-trivial";
    dirs = [
      "ccparse";
      "in";
    ];
  };
])
