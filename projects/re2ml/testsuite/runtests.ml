(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  {
    tool = "re2ml";
    suffixes = [".mll"];
    options = None;
    dirs = [
      "src";
      "testsuite/re2ml";
    ];
  };
])
