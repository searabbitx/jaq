open OUnit2

let extract raw_json filter =
  let json = Yojson.Safe.from_string raw_json in
  Filter.exec filter json

let tests =
  [
    "extract field"
    >::: [
           ( "extract string field" >:: fun _ ->
             assert_equal (`String "bar") (extract {|{"foo":"bar"}|} "foo") );
           ( "extract string field sub" >:: fun _ ->
             assert_equal (`String "baz")
               (extract {|{"foo":{"bar":"baz"}}|} "foo.bar") );
           ( "handle parens" >:: fun _ ->
             assert_equal (`String "baz")
               (extract {|{"foo":{"bar":"baz"}}|} "(foo).bar") );
         ];
  ]

let _ = tests |> List.map run_test_tt_main
