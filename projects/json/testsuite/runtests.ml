(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  {
    tool = "jsontest";
    suffixes = [".json"];
    options = None;
    dirs = [
      "testsuite/json";
    ];
  };
])
