let min_fi_float = -. (2.0 ** 52.0)
let max_fi_float = (2.0 ** 52.0) -. 1.0

module Make(SP : sig
  val succ : float -> float
  val pred : float -> float
end) = struct
  let cases_for_unparse_jcsnafi case case_exn =
    let case = case % Json_JCSnafi.unparse_jcsnafi in
    let case_exn jv = case_exn (fun () -> Json_JCSnafi.unparse_jcsnafi jv) in
    [ (* literal case *)
      case (`null) {|null|};
      case (`bool true) {|true|};
      case (`bool false) {|false|};

      (* string case *)
      case (`str "\u{20ac}") {|"€"|};
      case (`str "$")	{|"$"|};	
      case (`str "\u{000F}") {|"\u000f"|};
      case (`str "\u{000a}") {|"\n"|}; (* "\x0A" *)
      case (`str "A") {|"A"|};	
      case (`str "'") {|"'"|};
      case (`str "\u{0042}") {|"B"|};
      case (`str "\u{0022}") {|"\""|}; (* "\x22" *)
      case (`str "\u{005c}") {|"\\"|}; (* "\x5C" *)
      case (`str "\\") {|"\\"|};
      case (`str "\"") {|"\""|};
      case (`str "/") {|"/"|};
      case (`str "\x08") {|"\b"|};
      case (`str "\x09") {|"\t"|};
      case (`str "\x0C") {|"\f"|};
      case (`str "\x0D") {|"\r"|};
      (* NUL and U+10FC10 *)
      case (`str "\000\000\xF4\x8F\xB0\x90") "\"\u0000\u0000\244\143\176\144\"";

      (* Boundary of 1-byte characters |00..7F| *)
      case (`str "\x00") {|"\u0000"|};
      case (`str "\x7F") "\"\x7F\"";
      case_exn (`str "\x80") (Invalid_argument "Invalid Unicode: \x80");

      (* Boundary of 2-byte characters |C2..DF|80..BF| *)
      (*   1st byte check |C2..DF|<valid>| *)
      case_exn (`str "\xC1\x80") (Invalid_argument "Invalid Unicode: \xC1\x80");
      case (`str "\xC2\x80") "\"\xC2\x80\"";
      case (`str "\xDF\x80") "\"\xDF\x80\"";
      case_exn (`str "\xE0\x80") (Invalid_argument "Invalid Unicode: \xE0\x80");
      (*   2st byte check |C2 and DF|80..BF| *)
      case_exn (`str "\xC2\x7F") (Invalid_argument "Invalid Unicode: \xC2\x7F");
      case (`str "\xC2\xBF") "\"\xC2\xBF\"";
      case_exn (`str "\xC2\xC0") (Invalid_argument "Invalid Unicode: \xC2\xC0");
      case_exn (`str "\xDF\x7F") (Invalid_argument "Invalid Unicode: \xDF\x7F");
      case (`str "\xDF\xBF") "\"\xDF\xbf\"";
      case_exn (`str "\xDF\xC0") (Invalid_argument "Invalid Unicode: \xDF\xc0");

      (* Boundary of 3-byte characters |E0..EF|80..BF^|80..BF| *)
      (*   1st byte check |E0..EF|<valid>|<valid>| *)
      case_exn (`str "\xDF\x80\x80") (Invalid_argument "Invalid Unicode: \xDF\x80\x80");
      case (`str "\xE0\xA0\x80") "\"\xE0\xA0\x80\""; (* ^if 1st byte is E0, 2nd byte must be outside the range E0..9F. *)
      case (`str "\xEF\xBF\xBF") "\"\xEF\xBF\xBF\"";
      case_exn (`str "\xF0\x80\x80") (Invalid_argument "Invalid Unicode: \xF0\x80\x80");
      (*   2nd byte check |<valid>|80..BF|<valid>|*)
      case_exn (`str "\xE1\x7F\x80") (Invalid_argument "Invalid Unicode: \xE1\x7F\x80");
      case (`str "\xE1\x80\x80") "\"\xE1\x80\x80\""; (* include checking 3rd byte *)
      case (`str "\xE1\xBF\x80") "\"\xE1\xBF\x80\"";
      case_exn (`str "\xE1\xC0\x80") (Invalid_argument "Invalid Unicode: \xE1\xC0\x80");
      (*   3rd byte check |<valid>|<valid>|80..BF|*)
      case_exn (`str "\xE1\x80\x7F") (Invalid_argument "Invalid Unicode: \xE1\x80\x7F");
      case (`str "\xE1\x80\xBF") "\"\xE1\x80\xBF\"";
      case_exn (`str "\xE1\x80\xC0") (Invalid_argument "Invalid Unicode: \xE1\x80\xC0");
      (*   3-byte characters special check *)
      case_exn (`str "\xE0\x9F\xBF") (Invalid_argument "Invalid Unicode: \xE0\x9F\xBF");
      case (`str "\xED\x9F\xBF") "\"\xED\x9F\xBF\"";
      case_exn (`str "\xED\xA0\x80") (Invalid_argument "Invalid Unicode: \xED\xA0\x80");
      case_exn (`str "\xED\xBF\xBF") (Invalid_argument "Invalid Unicode: \xED\xBF\xBF");
      case (`str "\xEE\x80\x80") "\"\xEE\x80\x80\"";

      (* Boundary of 4-byte characters |F0..F4|80..BF^|80..BF|80..BF| *)
      (*   1st byte check |F0..F4|<valid>|<valid>|<valid>| *)
      case_exn (`str "\xEF\xBF\xBF\xBF") (Invalid_argument "Invalid Unicode: \xEF\xBF\xBF\xBF");
      case (`str "\xF0\x90\x80\x80") "\"\xF0\x90\x80\x80\""; (* ^if 1st byte is F0, 2nd byte is invalid if if it falls within the range 80..8F. *)
      case (`str "\xF4\x8F\xBF\xBF") "\"\xF4\x8F\xBF\xBF\""; (* ^if 1st byte is F4, 2nd byte is invalid if it is 90 or greater. *)
      case_exn (`str "\xF5\x80\x80\x80") (Invalid_argument "Invalid Unicode: \xF5\x80\x80\x80");
      (*   2nd byte check |<valid>|80..BF|<valid>|<valid>| *)
      case_exn (`str "\xF1\x7F\x80\x80") (Invalid_argument "Invalid Unicode: \xF1\x7F\x80\x80");
      case (`str "\xF1\x80\x80\x80") "\"\xF1\x80\x80\x80\""; (* include checking 3rd and 4th byte *)
      case (`str "\xF1\xBF\xBF\xBF") "\"\xF1\xBF\xBF\xBF\""; (* include checking 3rd and 4th byte *)
      case_exn (`str "\xF1\xC0\x80\x80") (Invalid_argument "Invalid Unicode: \xF1\xC0\x80\x80");
      (*   3rd byte check |<valid>|<valid>|80..BF|<valid>| *)
      case_exn (`str "\xF1\x80\x7F\x80") (Invalid_argument "Invalid Unicode: \xF1\x80\x7F\x80");
      case_exn (`str "\xF1\x80\xC0\x80") (Invalid_argument "Invalid Unicode: \xF1\x80\xC0\x80");
      (*   4th byte check |<valid>|<valid>|<valid>|80..BF| *)
      case_exn (`str "\xF1\x80\x80\x7F") (Invalid_argument "Invalid Unicode: \xF1\x80\x80\x7F");
      case_exn (`str "\xF1\x80\x80\xC0") (Invalid_argument "Invalid Unicode: \xF1\x80\x80\xC0");
      (*   4-byte characters special check *)
      case_exn (`str "\xF0\x8F\xBF\xBF") (Invalid_argument "Invalid Unicode: \xF0\x8F\xBF\xBF");
      case_exn (`str "\xF4\x90\x80\x80") (Invalid_argument "Invalid Unicode: \xF4\x90\x80\x80");

      (* number case *)
      case (`num min_fi_float) {|-4503599627370496|};
      case (`num max_fi_float) {|4503599627370495|};
      case (`num (-0.)) {|0|};
      case (`num 0.) {|0|};
      case (`num (+0.)) {|0|};
      case_exn (`num (SP.pred min_fi_float)) (Invalid_argument "Number cannot be safely encoded with Json_JCSnafi (encountering: -4503599627370497.000000)");
      case_exn (`num (SP.succ max_fi_float)) (Invalid_argument "Number cannot be safely encoded with Json_JCSnafi (encountering: 4503599627370495.500000)");
      case_exn (`num (-1.5)) (Invalid_argument "Number cannot be safely encoded with Json_JCSnafi (encountering: -1.500000)");
      case_exn (`num 4.8) (Invalid_argument "Number cannot be safely encoded with Json_JCSnafi (encountering: 4.800000)");

      (* object case *)
      case (`obj []) {|{}|};
      case (`obj [("null", `null)]) {|{"null":null}|};
      case (`obj [("boolean", `bool true)]) {|{"boolean":true}|};
      case (`obj [("boolean", `bool false)]) {|{"boolean":false}|};
      case (`obj [("string", `str "foo")]) {|{"string":"foo"}|};
      case (`obj [("string", `str "あ")]) {|{"string":"あ"}|};
      case (`obj [("string", `str "\u{20ac}")]) {|{"string":"€"}|};
      case (`obj [("string", `str "$")])	{|{"string":"$"}|};	
      case (`obj [("string", `str "\u{000F}")]) {|{"string":"\u000f"}|};
      case (`obj [("string", `str "\u{000a}")]) {|{"string":"\n"}|};
      case (`obj [("string", `str "A")]) {|{"string":"A"}|};	
      case (`obj [("string", `str "'")]) {|{"string":"'"}|};
      case (`obj [("string", `str "\u{0042}")]) {|{"string":"B"}|};
      case (`obj [("string", `str "\u{0022}")]) {|{"string":"\""}|};
      case (`obj [("string", `str "\u{005c}")]) {|{"string":"\\"}|};
      case (`obj [("string", `str "\\")]) {|{"string":"\\"}|};
      case (`obj [("string", `str "\"")]) {|{"string":"\""}|};
      case (`obj [("string", `str "/")]) {|{"string":"/"}|};
      case (`obj [("あ", `null)]) {|{"あ":null}|};
      case (`obj [("\u{20ac}", `null)]) {|{"€":null}|};
      case (`obj [("$", `null)]) {|{"$":null}|};
      case (`obj [("\u{000F}", `null)]) {|{"\u000f":null}|};
      case (`obj [("\u{000a}", `null)]) {|{"\n":null}|};
      case (`obj [("A", `null)]) {|{"A":null}|};
      case (`obj [("'", `null)]) {|{"'":null}|};
      case (`obj [("\u{0042}", `null)]) {|{"B":null}|};
      case (`obj [("\u{0022}", `null)]) {|{"\"":null}|};
      case (`obj [("\u{005c}", `null)]) {|{"\\":null}|};
      case (`obj [("\\", `null)]) {|{"\\":null}|};
      case (`obj [("\"", `null)]) {|{"\"":null}|};
      case (`obj [("/", `null)]) {|{"/":null}|};
      case (`obj [("number", `num 1.0)]) {|{"number":1}|};
      case (`obj [("null", `null); ("boolean", `bool true); ("string", `str "foo"); ("number", `num 1.0)])
            {|{"boolean":true,"null":null,"number":1,"string":"foo"}|};
      case (`obj [("obj", `obj [("name", `str "foo"); ("age", `num 30.0)])]) {|{"obj":{"age":30,"name":"foo"}}|};
      case (`obj [("array", `arr [])]) {|{"array":[]}|};
      case (`obj [("array", `arr [`null])]) {|{"array":[null]}|};
      case (`obj [("array", `arr [`bool true; `bool false])]) {|{"array":[true,false]}|};
      case (`obj [("array", `arr [`null; `bool true; `bool false; `str "foo"; `num 1.0])])
            {|{"array":[null,true,false,"foo",1]}|};
      case_exn (`obj [("foo", `bool true); ("foo", `bool false)]) (Invalid_argument "Duplicate property names: foo");
      case_exn (`obj [("あ", `bool true); ("あ", `bool false)])  (Invalid_argument "Duplicate property names: あ");
      case_exn (`obj [("あいう", `bool true); ("あいう", `bool false)])  (Invalid_argument "Duplicate property names: あいう");
      case_exn (`obj [("\u{20ac}", `bool true); ({|€|}, `bool false)]) (Invalid_argument "Duplicate property names: €");
      case_exn (`obj [ ("\128", `arr [ ]); ]) (Invalid_argument "Invalid Unicode: \128");

      (* array case *)
      case (`arr []) {|[]|};
      case (`arr [`null]) {|[null]|};
      case (`arr [`bool true]) {|[true]|};
      case (`arr [`bool false]) {|[false]|};
      case (`arr [`str "foo"]) {|["foo"]|};
      case (`arr [`str "あ"]) {|["あ"]|};
      case (`arr [`str "foo"; `str "あ"; `str "\u{20ac}"; `str "$"; `str "\u{000F}"; `str "\u{000a}"; `str "A"; `str "'"; `str "\u{0042}"; `str "\u{0022}"; `str "\u{005c}"; `str "\\"; `str "\""; `str "/"])
          {|["foo","あ","€","$","\u000f","\n","A","'","B","\"","\\","\\","\"","/"]|};
      case (`arr [`str "fooあ\u{20ac}$\u{000F}\u{000a}A'\u{0042}\u{0022}\u{005c}\\\"/"])
          {|["fooあ€$\u000f\nA'B\"\\\\\"/"]|};
      case (`arr [`num 1.0]) {|[1]|};
      case (`arr [`num (-1.0)]) {|[-1]|};
      case_exn (`arr [`num 2.3]) (Invalid_argument "Number cannot be safely encoded with Json_JCSnafi (encountering: 2.300000)");
      case_exn (`arr [`num (-5.0); `num 2.3]) (Invalid_argument "Number cannot be safely encoded with Json_JCSnafi (encountering: 2.300000)");
      case_exn (`arr [`num 2.3; `num (-5.0)]) (Invalid_argument "Number cannot be safely encoded with Json_JCSnafi (encountering: 2.300000)");
      case (`arr [`obj []]) {|[{}]|};
      case (`arr [`obj [("null", `null)]]) {|[{"null":null}]|};
      case (`arr [`obj [("boolean", `bool true)]]) {|[{"boolean":true}]|};
      case (`arr [`obj [("boolean", `bool false)]]) {|[{"boolean":false}]|};
      case (`arr [`obj [("string", `str "foo")]]) {|[{"string":"foo"}]|};
      case (`arr [`obj [("string", `str "あ")]]) {|[{"string":"あ"}]|};
      case (`arr [`obj [("string", `str "\u{20ac}")]]) {|[{"string":"€"}]|};
      case (`arr [`obj [("string", `str "$")]])	{|[{"string":"$"}]|};	
      case (`arr [`obj [("string", `str "\u{000F}")]]) {|[{"string":"\u000f"}]|};
      case (`arr [`obj [("string", `str "\u{000a}")]]) {|[{"string":"\n"}]|};
      case (`arr [`obj [("string", `str "A")]]) {|[{"string":"A"}]|};	
      case (`arr [`obj [("string", `str "'")]]) {|[{"string":"'"}]|};
      case (`arr [`obj [("string", `str "\u{0042}")]]) {|[{"string":"B"}]|};
      case (`arr [`obj [("string", `str "\u{0022}")]]) {|[{"string":"\""}]|};
      case (`arr [`obj [("string", `str "\u{005c}")]]) {|[{"string":"\\"}]|};
      case (`arr [`obj [("string", `str "\\")]]) {|[{"string":"\\"}]|};
      case (`arr [`obj [("string", `str "\"")]]) {|[{"string":"\""}]|};
      case (`arr [`obj [("string", `str "/")]]) {|[{"string":"/"}]|};
      case (`arr [`obj [("あ", `null)]]) {|[{"あ":null}]|};
      case (`arr [`obj [("\u{20ac}", `null)]]) {|[{"€":null}]|};
      case (`arr [`obj [("$", `null)]])	{|[{"$":null}]|};	
      case (`arr [`obj [("\u{000F}", `null)]]) {|[{"\u000f":null}]|};
      case (`arr [`obj [("\u{000a}", `null)]]) {|[{"\n":null}]|};
      case (`arr [`obj [("A", `null)]]) {|[{"A":null}]|};	
      case (`arr [`obj [("'", `null)]]) {|[{"'":null}]|};
      case (`arr [`obj [("\u{0042}", `null)]]) {|[{"B":null}]|};
      case (`arr [`obj [("\u{0022}", `null)]]) {|[{"\"":null}]|};
      case (`arr [`obj [("\u{005c}", `null)]]) {|[{"\\":null}]|};
      case (`arr [`obj [("\\", `null)]]) {|[{"\\":null}]|};
      case (`arr [`obj [("\"", `null)]]) {|[{"\"":null}]|};
      case (`arr [`obj [("/", `null)]]) {|[{"/":null}]|};
      case (`arr [`obj [("number", `num 1.0)]]) {|[{"number":1}]|};
      case (`arr [`obj [("null", `null); ("boolean", `bool true); ("string", `str "foo"); ("number", `num 1.0)]])
            {|[{"boolean":true,"null":null,"number":1,"string":"foo"}]|};
      case (`arr [`obj [("obj", `obj [("name", `str "foo"); ("age", `num 30.0)])]]) {|[{"obj":{"age":30,"name":"foo"}}]|};
      case (`arr [`null; `bool true; `bool false; `num 1.0; `num (-1.0); `str "foo"; `str "あ"; `obj [("obj", `obj [("name", `str "foo"); ("age", `num 30.0)])]])
            {|[null,true,false,1,-1,"foo","あ",{"obj":{"age":30,"name":"foo"}}]|};
      case (`arr [`arr []]) "[[]]";
      case (`arr [`arr [`bool true; `bool false]]) {|[[true,false]]|};
      case (`arr [`arr [`bool true; `bool false]; `arr [`num 2.0; `num (-5.0)]]) {|[[true,false],[2,-5]]|};
      case (`arr [`arr [`bool true; `bool false]; `arr [`num 2.0; `num (-5.0)]; `arr [`obj [("name", `str "foo"); ("age", `num 30.0)]; `obj [("name", `str "bar"); ("age", `num 23.0)]]])
            {|[[true,false],[2,-5],[{"age":30,"name":"foo"},{"age":23,"name":"bar"}]]|};

      (* RFC 8785, sec3.2.2 for jcsnafi*)
      case (`obj [ ("numbers", `arr [`num 333333333.0; `num 4.0; `num 2e+3; `num 0.0]);
                    ("string", `str "\u{20ac}$\u{000F}\u{000a}A'\u{0042}\u{0022}\u{005c}\\\"/");
                    ("literals", `arr [`null; `bool true; `bool false])])
            {|{"literals":[null,true,false],"numbers":[333333333,4,2000,0],"string":"€$\u000f\nA'B\"\\\\\"/"}|};

      (* RFC 8785, sec3.2.2 original *)
      case_exn (`obj [ ("numbers", `arr [`num 333333333.33333329; `num 1E30; `num 4.50; `num 2e-3; `num 0.000000000000000000000000001]);
                    ("string", `str "\u{20ac}$\u{000F}\u{000a}A'\u{0042}\u{0022}\u{005c}\\\"/");
                    ("literals", `arr [`null; `bool true; `bool false])])
                (Invalid_argument "Number cannot be safely encoded with Json_JCSnafi (encountering: 333333333.333333)");
                (* {|{"literals":[null,true,false],"numbers":[333333333.3333333,1e+30,4.5,0.002,1e-27],"string":"€$\u000f\nA'B\"\\\\\"/"}|}; *)

      (* regression; 

         found with QCHECK_SEED=350655625 node _build/default/jsoo/unit_test/baselib_json_jcsnafi_rfc_8785_test.bc.js
         at commit a626f41c6e17dac2a768c3be153852dd0881896b (part of PR https://github.com/kxcinc/kxclib-ocaml/pull/79) *)
      case 
        (`obj ["", `null; "\000\000\000\244\143\176\128", `null])
        ({|{"":null,"\u0000\u0000\u0000|}^"\xF4\x8F\xB0\x80"^{|":null}|});
    ]

  let cases_for_is_encodable case =
    let case = case % Json_JCSnafi.is_encodable in
    [ (* literal case *)
      case (`null) true;
      case (`bool true) true;
      case (`bool false) true;

      (* string case *)
      case (`str "\u{20ac}") true;
      case (`str "$")	true;	
      case (`str "\u{000F}") true;
      case (`str "\u{000a}") true; (* "\x0A" *)
      case (`str "A") true;	
      case (`str "'") true;
      case (`str "\u{0042}") true;
      case (`str "\u{0022}") true; (* "\x22" *)
      case (`str "\u{005c}") true; (* "\x5C" *)
      case (`str "\\") true;
      case (`str "\"") true;
      case (`str "/") true;
      case (`str "\x08") true;
      case (`str "\x09") true;
      case (`str "\x0C") true;
      case (`str "\x0D") true;

      (* Boundary of 1-byte characters |00..7F| *)
      case (`str "\x00") true;
      case (`str "\x7F") true;
      case (`str "\x80") false;

      (* Boundary of 2-byte characters |C2..DF|80..BF| *)
      (*   1st byte check |C2..DF|<valid>| *)
      case (`str "\xC1\x80") false;
      case (`str "\xC2\x80") true;
      case (`str "\xDF\x80") true;
      case (`str "\xE0\x80") false;
      (*   2st byte check |C2 and DF|80..BF| *)
      case (`str "\xC2\x7F") false;
      case (`str "\xC2\xBF") true;
      case (`str "\xC2\xC0") false;
      case (`str "\xDF\x7F") false;
      case (`str "\xDF\xBF") true;
      case (`str "\xDF\xC0") false;

      (* Boundary of 3-byte characters |E0..EF|80..BF^|80..BF| *)
      (*   1st byte check |E0..EF|<valid>|<valid>| *)
      case (`str "\xDF\x80\x80") false;
      case (`str "\xE0\xA0\x80") true; (* ^if 1st byte is E0, 2nd byte must be outside the range E0..9F. *)
      case (`str "\xEF\xBF\xBF") true;
      case (`str "\xF0\x80\x80") false;
      (*   2nd byte check |<valid>|80..BF|<valid>|*)
      case (`str "\xE1\x7F\x80") false;
      case (`str "\xE1\x80\x80") true; (* include checking 3rd byte *)
      case (`str "\xE1\xBF\x80") true;
      case (`str "\xE1\xC0\x80") false;
      (*   3rd byte check |<valid>|<valid>|80..BF|*)
      case (`str "\xE1\x80\x7F") false;
      case (`str "\xE1\x80\xBF") true;
      case (`str "\xE1\x80\xC0") false;
      (*   3-byte characters special check *)
      case (`str "\xE0\x9F\xBF") false;
      case (`str "\xED\x9F\xBF") true;
      case (`str "\xED\xA0\x80") false;
      case (`str "\xED\xBF\xBF") false;
      case (`str "\xEE\x80\x80") true;

      (* Boundary of 4-byte characters |F0..F4|80..BF^|80..BF|80..BF| *)
      (*   1st byte check |F0..F4|<valid>|<valid>|<valid>| *)
      case (`str "\xEF\xBF\xBF\xBF") false;
      case (`str "\xF0\x90\x80\x80") true; (* ^if 1st byte is F0, 2nd byte is invalid if if it falls within the range 80..8F. *)
      case (`str "\xF4\x8F\xBF\xBF") true; (* ^if 1st byte is F4, 2nd byte is invalid if it is 90 or greater. *)
      case (`str "\xF5\x80\x80\x80") false;
      (*   2nd byte check |<valid>|80..BF|<valid>|<valid>| *)
      case (`str "\xF1\x7F\x80\x80") false;
      case (`str "\xF1\x80\x80\x80") true; (* include checking 3rd and 4th byte *)
      case (`str "\xF1\xBF\xBF\xBF") true; (* include checking 3rd and 4th byte *)
      case (`str "\xF1\xC0\x80\x80") false;
      (*   3rd byte check |<valid>|<valid>|80..BF|<valid>| *)
      case (`str "\xF1\x80\x7F\x80") false;
      case (`str "\xF1\x80\xC0\x80") false;
      (*   4th byte check |<valid>|<valid>|<valid>|80..BF| *)
      case (`str "\xF1\x80\x80\x7F") false;
      case (`str "\xF1\x80\x80\xC0") false;
      (*   4-byte characters special check *)
      case (`str "\xF0\x8F\xBF\xBF") false;
      case (`str "\xF4\x90\x80\x80") false;

      (* number case *)
      case (`num min_fi_float) true;
      case (`num max_fi_float) true;
      case (`num (-0.)) true;
      case (`num 0.) true;
      case (`num (+0.)) true;
      case (`num (SP.pred min_fi_float)) false;
      case (`num (SP.succ max_fi_float)) false;
      case (`num (-1.5)) false;
      case (`num 4.8) false;

      (* object case *)
      case (`obj []) true;
      case (`obj [("null", `null)]) true;
      case (`obj [("boolean", `bool true)]) true;
      case (`obj [("boolean", `bool false)]) true;
      case (`obj [("string", `str "foo")]) true;
      case (`obj [("string", `str "あ")]) true;
      case (`obj [("string", `str "\u{20ac}")]) true;
      case (`obj [("string", `str "$")])	true;
      case (`obj [("string", `str "\u{000F}")]) true;
      case (`obj [("string", `str "\u{000a}")]) true;
      case (`obj [("string", `str "A")]) true;
      case (`obj [("string", `str "'")]) true;
      case (`obj [("string", `str "\u{0042}")]) true;
      case (`obj [("string", `str "\u{0022}")]) true;
      case (`obj [("string", `str "\u{005c}")]) true;
      case (`obj [("string", `str "\\")]) true;
      case (`obj [("string", `str "\"")]) true;
      case (`obj [("string", `str "/")]) true;
      case (`obj [("あ", `null)]) true;
      case (`obj [("\u{20ac}", `null)]) true;
      case (`obj [("$", `null)]) true;
      case (`obj [("\u{000F}", `null)]) true;
      case (`obj [("\u{000a}", `null)]) true;
      case (`obj [("A", `null)]) true;
      case (`obj [("'", `null)]) true;
      case (`obj [("\u{0042}", `null)]) true;
      case (`obj [("\u{0022}", `null)]) true;
      case (`obj [("\u{005c}", `null)]) true;
      case (`obj [("\\", `null)]) true;
      case (`obj [("\"", `null)]) true;
      case (`obj [("/", `null)]) true;
      case (`obj [("number", `num 1.0)]) true;
      case (`obj [("null", `null); ("boolean", `bool true); ("string", `str "foo"); ("number", `num 1.0)])
            true;
      case (`obj [("obj", `obj [("name", `str "foo"); ("age", `num 30.0)])]) true;
      case (`obj [("array", `arr [])]) true;
      case (`obj [("array", `arr [`null])]) true;
      case (`obj [("array", `arr [`bool true; `bool false])]) true;
      case (`obj [("array", `arr [`null; `bool true; `bool false; `str "foo"; `num 1.0])])
            true;
      case (`obj [("foo", `bool true); ("foo", `bool false)]) false;
      case (`obj [("あ", `bool true); ("あ", `bool false)]) false;
      case (`obj [("あいう", `bool true); ("あいう", `bool false)]) false;
      case (`obj [("\u{20ac}", `bool true); ({|€|}, `bool false)]) false;
      case (`obj [ ("\128", `arr [ ]); ]) false;

      (* array case *)
      case (`arr []) true;
      case (`arr [`null]) true;
      case (`arr [`bool true]) true;
      case (`arr [`bool false]) true;
      case (`arr [`str "foo"]) true;
      case (`arr [`str "あ"]) true;
      case (`arr [`str "foo"; `str "あ"; `str "\u{20ac}"; `str "$"; `str "\u{000F}"; `str "\u{000a}"; `str "A"; `str "'"; `str "\u{0042}"; `str "\u{0022}"; `str "\u{005c}"; `str "\\"; `str "\""; `str "/"])
            true;
      case (`arr [`str "fooあ\u{20ac}$\u{000F}\u{000a}A'\u{0042}\u{0022}\u{005c}\\\"/"])
            true;
      case (`arr [`num 1.0]) true;
      case (`arr [`num (-1.0)]) true;
      case (`arr [`num 2.3]) false;
      case (`arr [`num (-5.0); `num 2.3]) false;
      case (`arr [`num 2.3; `num (-5.0)]) false;
      case (`arr [`obj []]) true;
      case (`arr [`obj [("null", `null)]]) true;
      case (`arr [`obj [("boolean", `bool true)]]) true;
      case (`arr [`obj [("boolean", `bool false)]]) true;
      case (`arr [`obj [("string", `str "foo")]]) true;
      case (`arr [`obj [("string", `str "あ")]]) true;
      case (`arr [`obj [("string", `str "\u{20ac}")]]) true;
      case (`arr [`obj [("string", `str "$")]])	true;
      case (`arr [`obj [("string", `str "\u{000F}")]]) true;
      case (`arr [`obj [("string", `str "\u{000a}")]]) true;
      case (`arr [`obj [("string", `str "A")]]) true;
      case (`arr [`obj [("string", `str "'")]]) true;
      case (`arr [`obj [("string", `str "\u{0042}")]]) true;
      case (`arr [`obj [("string", `str "\u{0022}")]]) true;
      case (`arr [`obj [("string", `str "\u{005c}")]]) true;
      case (`arr [`obj [("string", `str "\\")]]) true;
      case (`arr [`obj [("string", `str "\"")]]) true;
      case (`arr [`obj [("string", `str "/")]]) true;
      case (`arr [`obj [("あ", `null)]]) true;
      case (`arr [`obj [("\u{20ac}", `null)]]) true;
      case (`arr [`obj [("$", `null)]])	true;
      case (`arr [`obj [("\u{000F}", `null)]]) true;
      case (`arr [`obj [("\u{000a}", `null)]]) true;
      case (`arr [`obj [("A", `null)]]) true;
      case (`arr [`obj [("'", `null)]]) true;
      case (`arr [`obj [("\u{0042}", `null)]]) true;
      case (`arr [`obj [("\u{0022}", `null)]]) true;
      case (`arr [`obj [("\u{005c}", `null)]]) true;
      case (`arr [`obj [("\\", `null)]]) true;
      case (`arr [`obj [("\"", `null)]]) true;
      case (`arr [`obj [("/", `null)]]) true;
      case (`arr [`obj [("number", `num 1.0)]]) true;
      case (`arr [`obj [("null", `null); ("boolean", `bool true); ("string", `str "foo"); ("number", `num 1.0)]])
            true;
      case (`arr [`obj [("obj", `obj [("name", `str "foo"); ("age", `num 30.0)])]]) true;
      case (`arr [`null; `bool true; `bool false; `num 1.0; `num (-1.0); `str "foo"; `str "あ"; `obj [("obj", `obj [("name", `str "foo"); ("age", `num 30.0)])]])
            true;
      case (`arr [`arr []]) true;
      case (`arr [`arr [`bool true; `bool false]]) true;
      case (`arr [`arr [`bool true; `bool false]; `arr [`num 2.0; `num (-5.0)]]) true;
      case (`arr [`arr [`bool true; `bool false]; `arr [`num 2.0; `num (-5.0)]; `arr [`obj [("name", `str "foo"); ("age", `num 30.0)]; `obj [("name", `str "bar"); ("age", `num 23.0)]]])
            true;

      (* RFC 8785, sec3.2.2 for jcsnafi*)
      case (`obj [ ("numbers", `arr [`num 333333333.0; `num 4.0; `num 2e+3; `num 0.0]);
                    ("string", `str "\u{20ac}$\u{000F}\u{000a}A'\u{0042}\u{0022}\u{005c}\\\"/");
                    ("literals", `arr [`null; `bool true; `bool false])])
            true;

      (* RFC 8785, sec3.2.2 original *)
      case (`obj [ ("numbers", `arr [`num 333333333.33333329; `num 1E30; `num 4.50; `num 2e-3; `num 0.000000000000000000000000001]);
                    ("string", `str "\u{20ac}$\u{000F}\u{000a}A'\u{0042}\u{0022}\u{005c}\\\"/");
                    ("literals", `arr [`null; `bool true; `bool false])])
            false;
    ]

  let cases_for_is_encodable_str case =
    let case = case % Json_JCSnafi.is_encodable_str in
    [ case "\u{20ac}" true;
      case "$" true;	
      case "\u{000F}" true;
      case "\u{000a}" true; (* "\x0A" *)
      case "A" true;	
      case "'" true;
      case "\u{0042}" true;
      case "\u{0022}" true; (* "\x22" *)
      case "\u{005c}" true; (* "\x5C" *)
      case "\\" true;
      case "\"" true;
      case "/" true;
      case "\x08" true;
      case "\x09" true;
      case "\x0C" true;
      case "\x0D" true;

      (* Boundary of 1-byte characters |00..7F| *)
      case "\x00" true;
      case "\x7F" true;
      case "\x80" false;

      (* Boundary of 2-byte characters |C2..DF|80..BF| *)
      (*   1st byte check |C2..DF|<valid>| *)
      case "\xC1\x80" false;
      case "\xC2\x80" true;
      case "\xDF\x80" true;
      case "\xE0\x80" false;
      (*   2st byte check |C2 and DF|80..BF| *)
      case "\xC2\x7F" false;
      case "\xC2\xBF" true;
      case "\xC2\xC0" false;
      case "\xDF\x7F" false;
      case "\xDF\xBF" true;
      case "\xDF\xC0" false;

      (* Boundary of 3-byte characters |E0..EF|80..BF^|80..BF| *)
      (*   1st byte check |E0..EF|<valid>|<valid>| *)
      case "\xDF\x80\x80" false;
      case "\xE0\xA0\x80" true; (* ^if 1st byte is E0, 2nd byte must be outside the range E0..9F. *)
      case "\xEF\xBF\xBF" true;
      case "\xF0\x80\x80" false;
      (*   2nd byte check |<valid>|80..BF|<valid>|*)
      case "\xE1\x7F\x80" false;
      case "\xE1\x80\x80" true; (* include checking 3rd byte *)
      case "\xE1\xBF\x80" true;
      case "\xE1\xC0\x80" false;
      (*   3rd byte check |<valid>|<valid>|80..BF|*)
      case "\xE1\x80\x7F" false;
      case "\xE1\x80\xBF" true;
      case "\xE1\x80\xC0" false;
      (*   3-byte characters special check *)
      case "\xE0\x9F\xBF" false;
      case "\xED\x9F\xBF" true;
      case "\xED\xA0\x80" false;
      case "\xED\xBF\xBF" false;
      case "\xEE\x80\x80" true;

      (* Boundary of 4-byte characters |F0..F4|80..BF^|80..BF|80..BF| *)
      (*   1st byte check |F0..F4|<valid>|<valid>|<valid>| *)
      case "\xEF\xBF\xBF\xBF" false;
      case "\xF0\x90\x80\x80" true; (* ^if 1st byte is F0, 2nd byte is invalid if if it falls within the range 80..8F. *)
      case "\xF4\x8F\xBF\xBF" true; (* ^if 1st byte is F4, 2nd byte is invalid if it is 90 or greater. *)
      case "\xF5\x80\x80\x80" false;
      (*   2nd byte check |<valid>|80..BF|<valid>|<valid>| *)
      case "\xF1\x7F\x80\x80" false;
      case "\xF1\x80\x80\x80" true; (* include checking 3rd and 4th byte *)
      case "\xF1\xBF\xBF\xBF" true; (* include checking 3rd and 4th byte *)
      case "\xF1\xC0\x80\x80" false;
      (*   3rd byte check |<valid>|<valid>|80..BF|<valid>| *)
      case "\xF1\x80\x7F\x80" false;
      case "\xF1\x80\xC0\x80" false;
      (*   4th byte check |<valid>|<valid>|<valid>|80..BF| *)
      case "\xF1\x80\x80\x7F" false;
      case "\xF1\x80\x80\xC0" false;
      (*   4-byte characters special check *)
      case "\xF0\x8F\xBF\xBF" false;
      case "\xF4\x90\x80\x80" false;
    ]

  let cases_for_is_encodable_num case =
    let case = case % Json_JCSnafi.is_encodable_num in
    [ case min_fi_float true;
      case max_fi_float true;
      case (-0.) true;
      case (0.) true;
      case (+0.) true;
      case (SP.pred min_fi_float) false;
      case (SP.succ max_fi_float) false;
      case (-1.5) false;
      case 4.8 false;
    ]

  let cases_for_compare_field_name case =
    let case s1 s2 = case (Json_JCSnafi.compare_field_name s1 s2) in
    [ case "" "" 0;
      case "" "a" (-1);
      case "a" "" 1;
      case "a" "a" 0;
      case "a" "b" (-1);
      case "b" "a" 1;
      case "aa" "aa" 0;
      case "a" "aa" (-1);
      case "aa" "a" 1;
      case "あ" "あ" 0;
      case "あ" "い" (-1);
      case "い" "あ" 1;

      (* RFC 8785, Sec3.2.3 testcase *)
      case "\r" "1" (-1);
      case "\r" "\u{0080}" (-1);
      case "\r" "\u{00f6}" (-1);
      case "\r" "\u{20ac}" (-1);
      case "\r" "\u{1f600}" (-1);       (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\r" "\u{fb33}" (-1);
      case "\r" "\r" 0;      
      case "1" "\u{0080}" (-1);
      case "1" "\u{00f6}" (-1);
      case "1" "\u{20ac}" (-1);
      case "1" "\u{1f600}" (-1);        (* eqv with surrogate pair \uD83D\uDE00 *)
      case "1" "\u{fb33}" (-1);
      case "1" "1" 0;
      case "1" "\r" 1;
      case "\u{0080}" "\u{00f6}" (-1);
      case "\u{0080}" "\u{20ac}" (-1);
      case "\u{0080}" "\u{1f600}" (-1); (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{0080}" "\u{fb33}" (-1);
      case "\u{0080}" "\u{0080}" 0;
      case "\u{0080}" "\r" 1;
      case "\u{0080}" "1" 1;
      case "\u{00f6}" "\u{20ac}" (-1);
      case "\u{00f6}" "\u{1f600}" (-1); (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{00f6}" "\u{fb33}" (-1);
      case "\u{00f6}" "\u{00f6}" 0;
      case "\u{00f6}" "\r" 1;
      case "\u{00f6}" "1" 1;
      case "\u{00f6}" "\u{0080}" 1;
      case "\u{20ac}" "\u{1f600}" (-1); (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{20ac}" "\u{fb33}" (-1);
      case "\u{20ac}" "\u{20ac}" 0;
      case "\u{20ac}" "\r" 1;
      case "\u{20ac}" "1" 1;
      case "\u{20ac}" "\u{0080}" 1;
      case "\u{20ac}" "\u{00f6}" 1;
      case "\u{1f600}" "\u{fb33}" (-1); (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\u{1f600}" 0;   (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\r" 1;          (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "1" 1;           (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\u{0080}" 1;    (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\u{00f6}" 1;    (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{1f600}" "\u{20ac}" 1;    (* eqv with surrogate pair \uD83D\uDE00 *)
      case "\u{fb33}" "\u{fb33}" 0;
      case "\u{fb33}" "\r" 1;
      case "\u{fb33}" "1" 1;
      case "\u{fb33}" "\u{0080}" 1;
      case "\u{fb33}" "\u{00f6}" 1;
      case "\u{fb33}" "\u{20ac}" 1;
      case "\u{fb33}" "\u{1f600}" 1;    (* eqv with surrogate pair \uD83D\uDE00 *)
    ]

  let test_compare_field_name_rfc8785 check =
    let input = [
      ("\u{20ac}", "Euro Sign");
      ("\r", "Carriage Return");
      ("\u{fb33}", "Hebrew Letter Dalet With Dagesh");
      ("1", "One");
      ("\u{1f600}", "Emoji: Grinning Face"); (* eqv with surrogate pair \uD83D\uDE00 *)
      ("\u{0080}", "Control");
      ("\u{00f6}", "Latin Small Letter O With Diaeresis");
    ] in
    let expected = [
      "Carriage Return";
      "One";
      "Control";
      "Latin Small Letter O With Diaeresis";
      "Euro Sign";
      "Emoji: Grinning Face";
      "Hebrew Letter Dalet With Dagesh"
    ] in
    
    let actual =
      input
      |> List.sort (fun (k1, _) (k2, _) -> Json_JCSnafi.compare_field_name k1 k2)
      |&> snd
    in
    check ~actual ~expected ()
end
