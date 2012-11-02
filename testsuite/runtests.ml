(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

let testsuite = "testsuite", Framework.([
  {
    tool = "arith";
    suffixes = [".c"];
    options = None;
    dirs = [
      "testsuite/elkhound/arith/tests";
    ];
  };
  {
    tool = "sless";
    suffixes = [".c"];
    options = None;
    dirs = [
      "testsuite/elkhound/scannerless/tests";
    ];
  };
  {
    tool = "re2ml";
    suffixes = [".mll"];
    options = None;
    dirs = [
      "src";
      "testsuite/re2ml";
    ];
  };
  {
    tool = "treematch";
    suffixes = [".tm"];
    options = Some "-special";
    dirs = [
      "testsuite/treematch";
    ];
  };
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


let () =
  Cmdline.run (Framework.run_testsuite testsuite)
