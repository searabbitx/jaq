open OUnit2
open Jaq

let extract raw_json filter =
  let json = Yojson.Safe.from_string raw_json in
  Filter.exec filter json

let assert_str_equal = assert_equal ~printer:Fun.id
let assert_json_equal = assert_equal ~printer:Yojson.Safe.to_string

let tests =
  [
    "Filtering"
    >::: [
           ( "extract string field" >:: fun _ ->
             assert_json_equal (`String "bar") (extract {|{"foo":"bar"}|} "foo")
           );
           ( "extract string field sub" >:: fun _ ->
             assert_json_equal (`String "baz")
               (extract {|{"foo":{"bar":"baz"}}|} "foo.bar") );
           ( "extract string field sub sub" >:: fun _ ->
             assert_json_equal (`String "quix")
               (extract {|{"foo":{"bar":{"baz":"quix"}}}|} "foo.bar.baz") );
           ( "handle parens" >:: fun _ ->
             assert_json_equal (`String "baz")
               (extract {|{"foo":{"bar":"baz"}}|} "(foo).bar") );
           ( "extract string from array" >:: fun _ ->
             assert_json_equal
               (`List [ `String "baz1"; `String "baz2" ])
               (extract {|{"foo":[{"bar":"baz1"},{"bar":"baz2"}]}|} "foo.bar")
           );
           ( "select fields from array" >:: fun _ ->
             assert_json_equal
               (`List
                 [
                   `Assoc [ ("bar", `String "baz1") ];
                   `Assoc [ ("bar", `String "baz2") ];
                 ])
               (extract
                  {|{"foo":[{"bar":"baz1","quix":"quix1"},{"bar":"baz2","quix":"quix2"}]}|}
                  "foo.select(bar)") );
           ( "select multiple fields from array" >:: fun _ ->
             assert_json_equal
               (`List
                 [
                   `Assoc [ ("bar", `String "baz1"); ("quix", `String "quix1") ];
                   `Assoc [ ("bar", `String "baz2"); ("quix", `String "quix2") ];
                 ])
               (extract
                  {|{"foo":[{"bar":"baz1","quix":"quix1","zix":"zix1"},{"bar":"baz2","quix":"quix2","zix":"zix2"}]}|}
                  "foo.select(bar,quix)") );
           ( "select multiple sub-fields from array" >:: fun _ ->
             assert_json_equal
               (`List
                 [
                   `Assoc [ ("bar", `String "baz1"); ("zix", `String "zix1") ];
                   `Assoc [ ("bar", `String "baz2"); ("zix", `String "zix2") ];
                 ])
               (extract
                  {|{"foo":
                      [
                        {"bar":"baz1","quix":{"zix":"zix1"}},
                        {"bar":"baz2","quix":{"zix":"zix2"}}
                      ]
                    }|}
                  "foo.select(bar,quix.zix)") );
           ( "select fields with alias" >:: fun _ ->
             assert_json_equal
               (`List
                 [
                   `Assoc [ ("alias", `String "baz1") ];
                   `Assoc [ ("alias", `String "baz2") ];
                 ])
               (extract
                  {|{"foo":[{"bar":"baz1","quix":"quix1"},{"bar":"baz2","quix":"quix2"}]}|}
                  "foo.select(bar as alias)") );
           ( "alias multiple fields" >:: fun _ ->
             assert_json_equal
               (`List
                 [
                   `Assoc
                     [ ("alias1", `String "baz1"); ("alias2", `String "quix1") ];
                   `Assoc
                     [ ("alias1", `String "baz2"); ("alias2", `String "quix2") ];
                 ])
               (extract
                  {|{"foo":[{"bar":"baz1","quix":"quix1","zix":"zix1"},{"bar":"baz2","quix":"quix2","zix":"zix2"}]}|}
                  "foo.select(bar as alias1,quix as alias2)") );
           ( "alias multiple sub-fields" >:: fun _ ->
             assert_json_equal
               (`List
                 [
                   `Assoc
                     [ ("alias1", `String "baz1"); ("alias2", `String "zix1") ];
                   `Assoc
                     [ ("alias1", `String "baz2"); ("alias2", `String "zix2") ];
                 ])
               (extract
                  {|{"foo":
                      [
                        {"bar":"baz1","quix":{"zix":"zix1"}},
                        {"bar":"baz2","quix":{"zix":"zix2"}}
                      ]
                    }|}
                  "foo.select(bar as alias1,quix.zix as alias2)") );
           ( "string literal single quote" >:: fun _ ->
             assert_json_equal (`String "foo") (extract "{}" "'foo'") );
           ( "string literal double quote" >:: fun _ ->
             assert_json_equal (`String "foo") (extract "{}" "\"foo\"") );
           ( "filter match" >:: fun _ ->
             assert_json_equal
               (`Assoc [ ("foo", `String "bar") ])
               (extract {|{"foo":"bar"}|} "filter(foo == 'bar')") );
           ( "filter no match" >:: fun _ ->
             assert_json_equal `Null
               (extract {|{"foo":"bar"}|} "filter(foo == 'baz')") );
           ( "filter list" >:: fun _ ->
             assert_json_equal
               (`List [ `Assoc [ ("bar", `String "baz1") ] ])
               (extract {|{"foo":[{"bar":"baz1"},{"bar":"baz2"}]}|}
                  "foo.filter(bar == 'baz1')") );
           ( "neq operator" >:: fun _ ->
             assert_json_equal
               (`List [ `Assoc [ ("bar", `String "baz1") ] ])
               (extract {|{"foo":[{"bar":"baz1"},{"bar":"baz2"}]}|}
                  "foo.filter(bar != 'baz2')") );
           ( "gt operator" >:: fun _ ->
             assert_json_equal
               (`List [ `Assoc [ ("bar", `Int 20) ] ])
               (extract {|{"foo":[{"bar":10},{"bar":20}]}|}
                  "foo.filter(bar > 10)") );
           ( "regex match operator" >:: fun _ ->
             assert_json_equal
               (`List
                 [
                   `Assoc [ ("bar", `String "baz2") ];
                   `Assoc [ ("bar", `String "bqz3") ];
                 ])
               (extract
                  {|{"foo":[{"bar":"baz1"},{"bar":"baz2"},{"bar":"bqz3"}]}|}
                  "foo.filter(bar ~ /.z[2-3]/)") );
           ( "logical and" >:: fun _ ->
             assert_json_equal
               (`List [ `Assoc [ ("bar", `Int 20) ] ])
               (extract {|{"foo":[{"bar":10},{"bar":20},{"bar":30}]}|}
                  "foo.filter(bar > 10 && bar < 30)") );
           ( "logical or" >:: fun _ ->
             assert_json_equal
               (`List
                 [ `Assoc [ ("bar", `Int 10) ]; `Assoc [ ("bar", `Int 30) ] ])
               (extract {|{"foo":[{"bar":10},{"bar":20},{"bar":30}]}|}
                  "foo.filter(bar < 15 || bar > 25)") );
           ( "logical or / and precedence" >:: fun _ ->
             assert_json_equal
               (`List [ `Assoc [ ("bar", `Int 10) ] ])
               (extract {|{"foo":[{"bar":10},{"bar":20},{"bar":30}]}|}
                  "foo.filter(bar < 15 || bar > 25 && bar < 30)") );
           ( "index extraction" >:: fun _ ->
             assert_json_equal
               (`Assoc [ ("bar", `String "baz1") ])
               (extract {|{"foo":[{"bar":"baz1"},{"bar":"baz2"}]}|} "foo[0]") );
           ( "apply uppercase" >:: fun _ ->
             assert_json_equal (`String "BAR")
               (extract {|{"foo":"bar"}|} "foo.uppercase()") );
           ( "apply lowercase" >:: fun _ ->
             assert_json_equal (`String "bar")
               (extract {|{"foo":"BAR"}|} "foo.lowercase()") );
           ( "apply uppercase in select" >:: fun _ ->
             assert_json_equal
               (`Assoc [ ("foo", `String "BAR") ])
               (extract {|{"foo":"bar"}|} "select(foo.uppercase())") );
           ( "apply capitalize" >:: fun _ ->
             assert_json_equal (`String "Bar")
               (extract {|{"foo":"bar"}|} "foo.capitalize()") );
         ];
    "Coloring"
    >::: [
           ( "colorize key" >:: fun _ ->
             assert_str_equal {|{@{<blue>"foo"@}:123}|}
               (Print.colorize {|{"foo":123}|}) );
           ( "colorize val" >:: fun _ ->
             assert_str_equal {|{@{<blue>"foo"@}:@{<green>"bar"@}}|}
               (Print.colorize {|{"foo":"bar"}|}) );
           ( "ignore non string vals" >:: fun _ ->
             assert_str_equal {|{@{<blue>"foo"@}:123,@{<blue>"bar"@}:456}|}
               (Print.colorize {|{"foo":123,"bar":456}|}) );
           ( "ignore non string vals - sub object" >:: fun _ ->
             assert_str_equal {|{@{<blue>"foo"@}:123,@{<blue>"bar"@}:456}|}
               (Print.colorize {|{"foo":123,"bar":456}|}) );
           ( "ignore non string vals - array" >:: fun _ ->
             assert_str_equal
               {|{@{<blue>"foos"@}:[{@{<blue>"foo"@}:123},{@{<blue>"foo"@}:321]}|}
               (Print.colorize {|{"foos":[{"foo":123},{"foo":321]}|}) );
           ( "colorize array" >:: fun _ ->
             assert_str_equal {|[@{<green>"foo"@},@{<green>"bar"@}]|}
               (Print.colorize {|["foo","bar"]|}) );
           ( "colorize array with special chars" >:: fun _ ->
             assert_str_equal {|[@{<green>"{foo"@},@{<green>"bar"@}]|}
               (Print.colorize {|["{foo","bar"]|}) );
           ( "colorize val with special chars" >:: fun _ ->
             assert_str_equal {|{@{<blue>"foo["@}:@{<green>"bar"@}}|}
               (Print.colorize {|{"foo[":"bar"}|}) );
         ];
    "Coloring"
    >::: [
           ( "concat strings" >:: fun _ ->
             assert_str_equal "foo,bar"
               (Util.concat_strings "," [ "foo"; "bar" ]) );
         ];
  ]

let _ = tests |> List.map run_test_tt_main
