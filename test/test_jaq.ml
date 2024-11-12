open OUnit2

let extract raw_json filter =
  let json = Yojson.Safe.from_string raw_json in
  Filter.exec filter json

let tests =
  [
    "Filtering"
    >::: [
           ( "extract string field" >:: fun _ ->
             assert_equal (`String "bar") (extract {|{"foo":"bar"}|} "foo") );
           ( "extract string field sub" >:: fun _ ->
             assert_equal (`String "baz")
               (extract {|{"foo":{"bar":"baz"}}|} "foo.bar") );
           ( "extract string field sub sub" >:: fun _ ->
             assert_equal (`String "quix")
               (extract {|{"foo":{"bar":{"baz":"quix"}}}|} "foo.bar.baz") );
           ( "handle parens" >:: fun _ ->
             assert_equal (`String "baz")
               (extract {|{"foo":{"bar":"baz"}}|} "(foo).bar") );
           ( "extract string from array" >:: fun _ ->
             assert_equal
               (`List [ `String "baz1"; `String "baz2" ])
               (extract {|{"foo":[{"bar":"baz1"},{"bar":"baz2"}]}|} "foo.bar")
           );
         ];
  ]

let _ = tests |> List.map run_test_tt_main
