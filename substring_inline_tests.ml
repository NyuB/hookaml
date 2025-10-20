let%expect_test "substring_index" =
  let print_substring ~needle s =
    Printf.printf "'%s' in '%s' ? " needle s;
    match Substring.substring_index ~needle s with
    | None -> Printf.printf "No\n\n"
    | Some index ->
      Printf.printf "At index %d\n" index;
      print_endline s;
      print_endline
        (Printf.sprintf
           "%s%s\n"
           (String.make index ' ')
           (String.make (String.length needle) '^'))
  in
  print_substring ~needle:"tasty" "Some tasty burrito";
  print_substring ~needle:"" "Some tasty burrito";
  print_substring ~needle:"Some" "Some tasty burrito";
  print_substring ~needle:"burrito" "Some tasty burrito";
  print_substring ~needle:"burito" "Some tasty burrito";
  print_substring ~needle:"Somme" "Some tasty burrito";
  [%expect
    {|
    'tasty' in 'Some tasty burrito' ? At index 5
    Some tasty burrito
         ^^^^^

    '' in 'Some tasty burrito' ? At index 0
    Some tasty burrito


    'Some' in 'Some tasty burrito' ? At index 0
    Some tasty burrito
    ^^^^

    'burrito' in 'Some tasty burrito' ? At index 11
    Some tasty burrito
               ^^^^^^^

    'burito' in 'Some tasty burrito' ? No

    'Somme' in 'Some tasty burrito' ? No
    |}]
;;
