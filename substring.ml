let substring_index ~needle s =
  (* Yes, this can be faster ... *)
  let l = String.length s
  and ln = String.length needle in
  let rec aux i =
    if i + ln > l
    then None
    else (
      let sub = String.sub s i ln in
      if String.equal sub needle then Some i else aux (i + 1))
  in
  aux 0
;;

let contains ~needle s = substring_index ~needle s != None
