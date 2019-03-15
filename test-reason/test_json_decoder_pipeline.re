module D = Tea.Json.Decoder;

type person = {
  name: string,
  age: int,
  is_developer: bool,
};
let person = (name, age, is_developer) => {name, age, is_developer};

let decode_required =
  D.Pipeline.(
    decode(person)
    |> required("name", D.string)
    |> required("age", D.int)
    |> required("is_developer", D.bool)
  );

let default_age = 33;
let decode_optional =
  D.Pipeline.(
    decode(person)
    |> required("name", D.string)
    |> optional("age", D.int, default_age)
    |> required("is_developer", D.bool)
  );

let parse_required_success = () => {
  let json = {| { "name" : "grandpa", "age" : 84, "is_developer" : false } |};
  switch (D.decodeString(decode_required, json)) {
  | Error(err) =>
    Js.log(err);
    assert(false);
  | Ok(p) =>
    assert(p.name == "grandpa");
    assert(p.age == 84);
    assert(p.is_developer == false);
  };
};

let parse_required_missing_field_fails = () => {
  let json = {| { "name" : "nobody", "is_developer" : false } |};
  switch (D.decodeString(decode_required, json)) {
  | Error(_err) => assert(true)
  | Ok(p) =>
    Js.log(p);
    assert(false);
  };
};

let parse_optional_all_fields_present = () => {
  let json = {| { "name" : "hacker", "age" : 15, "is_developer" : true } |};
  switch (D.decodeString(decode_optional, json)) {
  | Error(err) =>
    Js.log(err);
    assert(false);
  | Ok(p) =>
    assert(p.name == "hacker");
    assert(p.age == 15);
    assert(p.is_developer == true);
  };
};

let parse_optional_default_missing_field = () => {
  let json = {| { "name" : "ageless", "is_developer" : false } |};
  switch (D.decodeString(decode_optional, json)) {
  | Error(err) =>
    Js.log(err);
    assert(false);
  | Ok(p) =>
    assert(p.name == "ageless");
    assert(p.age == default_age);
    assert(p.is_developer == false);
  };
};

let all = () => {
  let run = t => t();
  [
    parse_required_success,
    parse_required_missing_field_fails,
    parse_optional_all_fields_present,
    parse_optional_default_missing_field,
  ]
  |> List.iter(run);
};
