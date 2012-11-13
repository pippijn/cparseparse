(* OASIS_START *)
(* DO NOT EDIT (digest: e4ae53f5977ab44bc9b53b3f2583d838) *)
module OASISGettext = struct
(* # 21 "/tmp/oasis/src/oasis/OASISGettext.ml" *)

  let ns_ str =
    str

  let s_ str =
    str

  let f_ (str : ('a, 'b, 'c, 'd) format4) =
    str

  let fn_ fmt1 fmt2 n =
    if n = 1 then
      fmt1^^""
    else
      fmt2^^""

  let init =
    []

end

module OASISExpr = struct
(* # 21 "/tmp/oasis/src/oasis/OASISExpr.ml" *)



  open OASISGettext

  type test = string 

  type flag = string 

  type t =
    | EBool of bool
    | ENot of t
    | EAnd of t * t
    | EOr of t * t
    | EFlag of flag
    | ETest of test * string
    

  type 'a choices = (t * 'a) list 

  let eval var_get t =
    let rec eval' =
      function
        | EBool b ->
            b

        | ENot e ->
            not (eval' e)

        | EAnd (e1, e2) ->
            (eval' e1) && (eval' e2)

        | EOr (e1, e2) ->
            (eval' e1) || (eval' e2)

        | EFlag nm ->
            let v =
              var_get nm
            in
              assert(v = "true" || v = "false");
              (v = "true")

        | ETest (nm, vl) ->
            let v =
              var_get nm
            in
              (v = vl)
    in
      eval' t

  let choose ?printer ?name var_get lst =
    let rec choose_aux =
      function
        | (cond, vl) :: tl ->
            if eval var_get cond then
              vl
            else
              choose_aux tl
        | [] ->
            let str_lst =
              if lst = [] then
                s_ "<empty>"
              else
                String.concat
                  (s_ ", ")
                  (List.map
                     (fun (cond, vl) ->
                        match printer with
                          | Some p -> p vl
                          | None -> s_ "<no printer>")
                     lst)
            in
              match name with
                | Some nm ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for the choice list '%s': %s")
                         nm str_lst)
                | None ->
                    failwith
                      (Printf.sprintf
                         (f_ "No result for a choice list: %s")
                         str_lst)
    in
      choose_aux (List.rev lst)

end


# 117 "myocamlbuild.ml"
module BaseEnvLight = struct
(* # 21 "/tmp/oasis/src/base/BaseEnvLight.ml" *)

  module MapString = Map.Make(String)

  type t = string MapString.t

  let default_filename =
    Filename.concat
      (Sys.getcwd ())
      "setup.data"

  let load ?(allow_empty=false) ?(filename=default_filename) () =
    if Sys.file_exists filename then
      begin
        let chn =
          open_in_bin filename
        in
        let st =
          Stream.of_channel chn
        in
        let line =
          ref 1
        in
        let st_line =
          Stream.from
            (fun _ ->
               try
                 match Stream.next st with
                   | '\n' -> incr line; Some '\n'
                   | c -> Some c
               with Stream.Failure -> None)
        in
        let lexer =
          Genlex.make_lexer ["="] st_line
        in
        let rec read_file mp =
          match Stream.npeek 3 lexer with
            | [Genlex.Ident nm; Genlex.Kwd "="; Genlex.String value] ->
                Stream.junk lexer;
                Stream.junk lexer;
                Stream.junk lexer;
                read_file (MapString.add nm value mp)
            | [] ->
                mp
            | _ ->
                failwith
                  (Printf.sprintf
                     "Malformed data file '%s' line %d"
                     filename !line)
        in
        let mp =
          read_file MapString.empty
        in
          close_in chn;
          mp
      end
    else if allow_empty then
      begin
        MapString.empty
      end
    else
      begin
        failwith
          (Printf.sprintf
             "Unable to load environment, the file '%s' doesn't exist."
             filename)
      end

  let var_get name env =
    let rec var_expand str =
      let buff =
        Buffer.create ((String.length str) * 2)
      in
        Buffer.add_substitute
          buff
          (fun var ->
             try
               var_expand (MapString.find var env)
             with Not_found ->
               failwith
                 (Printf.sprintf
                    "No variable %s defined when trying to expand %S."
                    var
                    str))
          str;
        Buffer.contents buff
    in
      var_expand (MapString.find name env)

  let var_choose lst env =
    OASISExpr.choose
      (fun nm -> var_get nm env)
      lst
end


# 215 "myocamlbuild.ml"
module MyOCamlbuildFindlib = struct
(* # 21 "/tmp/oasis/src/plugins/ocamlbuild/MyOCamlbuildFindlib.ml" *)

  (** OCamlbuild extension, copied from 
    * http://brion.inria.fr/gallium/index.php/Using_ocamlfind_with_ocamlbuild
    * by N. Pouillard and others
    *
    * Updated on 2009/02/28
    *
    * Modified by Sylvain Le Gall 
    *)
  open Ocamlbuild_plugin

  (* these functions are not really officially exported *)
  let run_and_read = 
    Ocamlbuild_pack.My_unix.run_and_read

  let blank_sep_strings = 
    Ocamlbuild_pack.Lexers.blank_sep_strings

  let split s ch =
    let buf = Buffer.create 13 in
    let x = ref [] in
    let flush () = 
      x := (Buffer.contents buf) :: !x;
      Buffer.clear buf
    in
      String.iter 
        (fun c ->
           if c = ch then 
             flush ()
           else
             Buffer.add_char buf c)
        s;
      flush ();
      List.rev !x

  let split_nl s = split s '\n'

  let before_space s =
    try
      String.before s (String.index s ' ')
    with Not_found -> s

  (* this lists all supported packages *)
  let find_packages () =
    List.map before_space (split_nl & run_and_read "ocamlfind list")

  (* this is supposed to list available syntaxes, but I don't know how to do it. *)
  let find_syntaxes () = ["camlp4o"; "camlp4r"]

  (* ocamlfind command *)
  let ocamlfind x = S[A"ocamlfind"; x]

  let dispatch =
    function
      | Before_options ->
          (* by using Before_options one let command line options have an higher priority *)
          (* on the contrary using After_options will guarantee to have the higher priority *)
          (* override default commands by ocamlfind ones *)
          Options.ocamlc     := ocamlfind & A"ocamlc";
          Options.ocamlopt   := ocamlfind & A"ocamlopt";
          Options.ocamldep   := ocamlfind & A"ocamldep";
          Options.ocamldoc   := ocamlfind & A"ocamldoc";
          Options.ocamlmktop := ocamlfind & A"ocamlmktop"
                                  
      | After_rules ->
          
          (* When one link an OCaml library/binary/package, one should use -linkpkg *)
          flag ["ocaml"; "link"; "program"] & A"-linkpkg";
          
          (* For each ocamlfind package one inject the -package option when
           * compiling, computing dependencies, generating documentation and
           * linking. *)
          List.iter 
            begin fun pkg ->
              flag ["ocaml"; "compile";  "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "ocamldep"; "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "doc";      "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "link";     "pkg_"^pkg] & S[A"-package"; A pkg];
              flag ["ocaml"; "infer_interface"; "pkg_"^pkg] & S[A"-package"; A pkg];
            end 
            (find_packages ());

          (* Like -package but for extensions syntax. Morover -syntax is useless
           * when linking. *)
          List.iter begin fun syntax ->
          flag ["ocaml"; "compile";  "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "ocamldep"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "doc";      "syntax_"^syntax] & S[A"-syntax"; A syntax];
          flag ["ocaml"; "infer_interface"; "syntax_"^syntax] & S[A"-syntax"; A syntax];
          end (find_syntaxes ());

          (* The default "thread" tag is not compatible with ocamlfind.
           * Indeed, the default rules add the "threads.cma" or "threads.cmxa"
           * options when using this tag. When using the "-linkpkg" option with
           * ocamlfind, this module will then be added twice on the command line.
           *                        
           * To solve this, one approach is to add the "-thread" option when using
           * the "threads" package using the previous plugin.
           *)
          flag ["ocaml"; "pkg_threads"; "compile"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "doc"] (S[A "-I"; A "+threads"]);
          flag ["ocaml"; "pkg_threads"; "link"] (S[A "-thread"]);
          flag ["ocaml"; "pkg_threads"; "infer_interface"] (S[A "-thread"])

      | _ -> 
          ()

end

module MyOCamlbuildBase = struct
(* # 21 "/tmp/oasis/src/plugins/ocamlbuild/MyOCamlbuildBase.ml" *)

  (** Base functions for writing myocamlbuild.ml
      @author Sylvain Le Gall
    *)



  open Ocamlbuild_plugin
  module OC = Ocamlbuild_pack.Ocaml_compiler

  type dir = string 
  type file = string 
  type name = string 
  type tag = string 

(* # 56 "/tmp/oasis/src/plugins/ocamlbuild/MyOCamlbuildBase.ml" *)

  type t =
      {
        lib_ocaml: (name * dir list) list;
        lib_c:     (name * dir * file list) list; 
        flags:     (tag list * (spec OASISExpr.choices)) list;
        (* Replace the 'dir: include' from _tags by a precise interdepends in
         * directory.
         *)
        includes:  (dir * dir list) list; 
      } 

  let env_filename =
    Pathname.basename 
      BaseEnvLight.default_filename

  let dispatch_combine lst =
    fun e ->
      List.iter 
        (fun dispatch -> dispatch e)
        lst 

  let tag_libstubs nm =
    "use_lib"^nm^"_stubs"

  let nm_libstubs nm =
    nm^"_stubs"

  let dispatch t e = 
    let env = 
      BaseEnvLight.load 
        ~filename:env_filename 
        ~allow_empty:true
        ()
    in
      match e with 
        | Before_options ->
            let no_trailing_dot s =
              if String.length s >= 1 && s.[0] = '.' then
                String.sub s 1 ((String.length s) - 1)
              else
                s
            in
              List.iter
                (fun (opt, var) ->
                   try 
                     opt := no_trailing_dot (BaseEnvLight.var_get var env)
                   with Not_found ->
                     Printf.eprintf "W: Cannot get variable %s" var)
                [
                  Options.ext_obj, "ext_obj";
                  Options.ext_lib, "ext_lib";
                  Options.ext_dll, "ext_dll";
                ]

        | After_rules -> 
            (* Declare OCaml libraries *)
            List.iter 
              (function
                 | nm, [] ->
                     ocaml_lib nm
                 | nm, dir :: tl ->
                     ocaml_lib ~dir:dir (dir^"/"^nm);
                     List.iter 
                       (fun dir -> 
                          List.iter
                            (fun str ->
                               flag ["ocaml"; "use_"^nm; str] (S[A"-I"; P dir]))
                            ["compile"; "infer_interface"; "doc"])
                       tl)
              t.lib_ocaml;

            (* Declare directories dependencies, replace "include" in _tags. *)
            List.iter 
              (fun (dir, include_dirs) ->
                 Pathname.define_context dir include_dirs)
              t.includes;

            (* Declare C libraries *)
            List.iter
              (fun (lib, dir, headers) ->
                   (* Handle C part of library *)
                   flag ["link"; "library"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("-l"^(nm_libstubs lib)); A"-cclib";
                        A("-l"^(nm_libstubs lib))]);

                   flag ["link"; "library"; "ocaml"; "native"; tag_libstubs lib]
                     (S[A"-cclib"; A("-l"^(nm_libstubs lib))]);
                        
                   flag ["link"; "program"; "ocaml"; "byte"; tag_libstubs lib]
                     (S[A"-dllib"; A("dll"^(nm_libstubs lib))]);

                   (* When ocaml link something that use the C library, then one
                      need that file to be up to date.
                    *)
                   dep ["link"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                   dep  ["compile"; "ocaml"; "program"; tag_libstubs lib]
                     [dir/"lib"^(nm_libstubs lib)^"."^(!Options.ext_lib)];

                   (* TODO: be more specific about what depends on headers *)
                   (* Depends on .h files *)
                   dep ["compile"; "c"] 
                     headers;

                   (* Setup search path for lib *)
                   flag ["link"; "ocaml"; "use_"^lib] 
                     (S[A"-I"; P(dir)]);
              )
              t.lib_c;

              (* Add flags *)
              List.iter
              (fun (tags, cond_specs) ->
                 let spec = 
                   BaseEnvLight.var_choose cond_specs env
                 in
                   flag tags & spec)
              t.flags
        | _ -> 
            ()

  let dispatch_default t =
    dispatch_combine 
      [
        dispatch t;
        MyOCamlbuildFindlib.dispatch;
      ]

end


# 478 "myocamlbuild.ml"
open Ocamlbuild_plugin;;
let package_default =
  {
     MyOCamlbuildBase.lib_ocaml =
       [("pa_unpack", ["src/pa_unpack"]); ("unpack", ["src/unpack"])];
     lib_c = [];
     flags = [];
     includes = [];
     }
  ;;

let dispatch_default = MyOCamlbuildBase.dispatch_default package_default;;

# 493 "myocamlbuild.ml"
(* OASIS_STOP *)
(* PBUILD_START *)
module Pbuild = struct
# 1 "/usr/local/lib/ocaml/3.12.1/pbuild/pbuild.ml"
  open Ocamlbuild_plugin
  
  let atomize = Command.atomize
  
  
  let tool = function
    | [] -> (fun x -> x)
    | [tool] -> (fun _ -> tool)
    | _ -> failwith "invalid: multiple tool names"
  
  
  (****************************************************************************
   * :: Noweb/LaTeX rules
   ***************************************************************************)
  
  let rec extract_latex_deps dir fh deps =
    try
      let line = input_line fh in
      let linelen = String.length line in
  
      let deps =
        if linelen > 8 && String.sub line 0 6 = "\\input" then
          let dep =
            dir ^
            String.sub line 7 (linelen - 8) ^ ".tex"
          in
          [dep] :: deps
        else
          deps
      in
  
      extract_latex_deps dir fh deps
    with End_of_file ->
      deps
  
  
  let latex_deps dir tex =
    let fh = open_in tex in
    let deps = extract_latex_deps dir fh [] in
    close_in fh;
    deps
  
  
  let noweb = function
    | After_rules ->
        rule "Extract code from literate file"
          ~prod:"%.ml"
          ~dep:"%.nw"
          begin fun env build ->
            Cmd(S[A"notangle"; A"-L# %L \"%F\"%N"; A"-Rml"; A(env "%.nw"); Sh">"; A(env "%.ml")])
          end;
  
  
        rule "Extract definitions from literate file"
          ~prod:"%.defs"
          ~dep:"%.nw"
          begin fun env build ->
            Cmd(S[A"nodefs"; A(env "%.nw"); Sh">"; A(env "%.defs")])
          end;
  
  
        rule "Extract documentation from literate file"
          ~prod:"%.tex"
          ~dep:"%.nw"
          begin fun env build ->
            Cmd(S[A"noweave"; A"-delay"; A(env "%.nw"); Sh">"; A(env "%.tex")])
          end;
  
  
        rule "Generate literate documentation"
          ~prod:"%.pdf"
          ~dep:"%.tex"
          begin fun env build ->
            let tex = env "%.tex" in
            (* get the directory including trailing slash *)
            let dir = String.sub tex 0 (String.rindex tex '/' + 1) in
            let deps = latex_deps dir tex in
            List.iter (function
              | Outcome.Good _  -> ()
              | Outcome.Bad exn -> raise exn
            ) (build deps);
            Cmd(S[A"texi2pdf"; A"-I"; A dir; A"-o"; A(env "%.pdf"); A"-q"; A"-b"; A tex])
          end;
  
    | _ -> ()
  ;;
  
  
  (****************************************************************************
   * :: Re2ml rules
   ***************************************************************************)
  
  let re2ml re2ml = function
    | After_rules ->
        rule "Compile re2ml specification to ML"
          ~prod:"%.ml"
          ~deps:("%.mlr" :: re2ml)
          begin fun env build ->
            Cmd(S[A(tool re2ml "re2ml"); A"-auto-loc"; A(env "%.mlr")])
          end;
  
    | _ -> ()
  ;;
  
  
  (****************************************************************************
   * :: Treematch rules
   ***************************************************************************)
  
  let treematch treematch = function
    | After_rules ->
        rule "Compile Treematch specification to ML"
          ~prod:"%.ml"
          ~deps:("%.tm" :: treematch)
          begin fun env build ->
            let sed o n = [Sh"|"; A"sed"; A"-e"; A ("s$" ^ o ^ "$" ^ n ^ "$g")] in
  
            Cmd(S([A(tool treematch "treematch"); A"-special"; A(env "%.tm")]
              @ sed "type t = \\([^;|]*\\);;"
                    "type t = \\1 with sexp;;"
              @ sed " | SEXP;;"
                    " with sexp;;"
              @ sed "\\.true" ".True"
              @ sed "\\.false" ".False"
              @ [Sh">"; A(env "%.ml")]
            ))
          end;
  
    | _ -> ()
  ;;
  
  
  (****************************************************************************
   * :: Elkhound rules
   ***************************************************************************)
  
  let elkhound elkhound = function
    | After_rules ->
        rule "Compile grammar to ML"
          ~prods:[
            "%Ptree.ml";
            "%PtreeActions.mli";
            "%PtreeActions.ml";
            "%Treematch.tm";
            "%TreematchActions.mli";
            "%TreematchActions.ml";
  
            "%Actions.mli";
            "%Actions.ml";
            "%Names.mli";
            "%Names.ml";
            "%Tables.dat";
            "%Tables.mli";
            "%Tables.ml";
            "%Tokens.mli";
            "%Tokens.ml";
          ]
          ~deps:("%.gr" :: elkhound)
          begin fun env build ->
            Cmd(S[A(tool elkhound "elkhound"); A"-timing"; A(env "%.gr")])
          end;
  
    | _ -> ()
  ;;
  
  (****************************************************************************
   * :: C++ rules
   ***************************************************************************)
  
  let cxx args = function
    | After_rules ->
        rule "Compile C++ source file"
          ~prod:"%.o"
          ~dep:"%.cpp"
          begin fun env build ->
            Cmd(atomize (["g++"; env "%.cpp"; "-c"; "-o"; env "%.o";
              "-fPIC";
              "-O3";
              "-ggdb3";
              "-std=c++0x";
            ] @ args))
          end;
  
    | _ -> ()
  ;;
end
# 684 "myocamlbuild.ml"
(* PBUILD_STOP *)

let _ = Ocamlbuild_plugin.dispatch (MyOCamlbuildBase.dispatch_combine [
  Pbuild.re2ml [];
  dispatch_default;
])
