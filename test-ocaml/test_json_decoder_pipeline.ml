module D = Tea.Json.Decoder

type person = {
  name : string;
  age : int;
  is_developer : bool;
}
let person name age is_developer = { name; age; is_developer }

let decode_required = D.Pipeline.(
  decode person
  |> required "name" D.string
  |> required "age" D.int
  |> required "is_developer" D.bool
)

let default_age = 33
let decode_optional = D.Pipeline.(
  decode person
  |> required "name" D.string
  |> optional "age" D.int default_age
  |> required "is_developer" D.bool
)


let parse_required_success () =
  let json = {| { "name" : "grandpa", "age" : 84, "is_developer" : false } |} in
  match D.decodeString decode_required json with
  | Error err -> Js.log err; assert false
  | Ok p ->
    assert (p.name = "grandpa");
    assert (p.age = 84);
    assert (p.is_developer = false)


let parse_required_missing_field_fails () =
  let json = {| { "name" : "nobody", "is_developer" : false } |} in
  match D.decodeString decode_required json with
  | Error _err -> assert true
  | Ok p -> Js.log p; assert false


let parse_optional_all_fields_present () =
  let json = {| { "name" : "hacker", "age" : 15, "is_developer" : true } |} in
  match D.decodeString decode_optional json with
  | Error err -> Js.log err; assert false
  | Ok p ->
    assert (p.name = "hacker");
    assert (p.age = 15);
    assert (p.is_developer = true)


let parse_optional_default_missing_field () =
  let json = {| { "name" : "ageless", "is_developer" : false } |} in
  match D.decodeString decode_optional json with
  | Error err -> Js.log err; assert false
  | Ok p ->
    assert (p.name = "ageless");
    assert (p.age = default_age);
    assert (p.is_developer = false)


let another_way () =
  let decode = D.succeed in
  let (|<) f x = D.map2 (|>) x f in
  let hardcoded = D.succeed in
  let optional decoder fallback =
    D.maybe decoder
    |> D.andThen (function
      | None -> D.succeed fallback
      | Some value -> D.succeed value)
  in
  
  let data a b c d e = [a;b;c;d;e] in
  let d =
    decode data 
    |< D.field "a" (optional D.int 11)
    |< D.field "b" D.int
    |< D.field "c" D.int
    |< hardcoded 42
    |< optional (D.field "d" D.int) 555
  in
  let json = {| { "a" : "a", "b": 2, "c": 3 } |} in
  match D.decodeString d json with
  | Error err -> failwith err
  | Ok v -> assert (v = [11;2;3;42;555])


let all () =
  let run t = t () in
  [
    parse_required_success;
    parse_required_missing_field_fails;
    parse_optional_all_fields_present;
    parse_optional_default_missing_field;
    another_way;
  ] |> List.iter run
