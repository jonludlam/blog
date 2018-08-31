---
title: Native Records
date: 2018-08-28
layout: post
draft: false
path: "/posts/native-records/"
category: "Xapi Types"
tags:
  - "ocaml"
  - "types"
  - "database"
description: "Switching the records marshalling to use native OCaml records"
---

We saw last time how to describe a record as a collection of fields, but our marshalling code converted to and from a type that is awkwardly different from an actual OCaml record. Let's focus first on marshalling, and how we would write marshalling code.

Firstly, we need to alter our declaration of `Record` in the `typ` definition. Previously we had:

```ocaml
type _ typ =
...
  | Record : 'a record -> 'a rval typ
```

we now want to lose the intermediate `rval` and use the `'a` itself - remember that this is the type we want to marshal to and from. The declaration now becomes:

```ocaml
type _ typ =
...
  | Record : 'a record -> 'a typ
```

Now, in the marshalling code we've got to be able to find the values of each field of the record, when what we have in our hands is an `'a record` and an `'a`. The `'a record` has a list of fields in it, so it seems reasonable to extend the definition of the field to include a function to get the value of a field from the record. The current type of a field is:

```ocaml
type ('a, 'f) field = {
  f_name : string;
  f_ty : 'f typ;
}
```

what we need to do is extend this:

```ocaml
type ('a, 'f) field = {
  f_name : string;
  f_ty : 'f typ;
  f_get : 'a -> 'f;
}
```

with this in our hands, we can modify the marshalling code to get the field values directly from our record:

```ocaml
let rec to_value : type a. a typ -> a -> Value.t = fun typ v ->
  match typ with
...
  | Record r ->
    let d = List.fold_left
      (fun a f' ->
        match f' with
        | B f -> (f.f_name, to_value f.f_ty (f.f_get v))::a)
      [] r.r_fields in
    Value.Dict d
```

so here we're folding over the list of fields. There's a little bit of magic going on here. The fact that the `B` constructor hides one of the type variables when you create it means that when you pattern match on it that type variable has to come into existence again. This is an example of an 'existential' type - OCaml will figure out the type itself as part of the pattern match. The important thing is that the type mustn't 'escape' the match clause - so it can't be part of the return type - but within the clause itself we can use it. In our case here we can make the recursive call to `to_value` to marshal whatever type the field of the record happens to be. We then add the resulting `Value.t` to an association list with the field name as key, so the end result of this is a simple `(string * Value.t) list`, which we then put into a `Dict`.

To unmarshal from a `Value.t` back to a record involves the reverse process - we fold over the fields of the record, using a pattern match to record the types of each one, finding it in our association list to get the `Value.t` of the field and then recursively calling `of_value`. Once we get the unmarshalled value back we hit a problem - what do we do with it? We can't return it since that would violate the idea that the existentially quantified field type must not escape the match clause. What we can do instead is declare an `f_set` function equivalent to our `f_get`:

```ocaml
type ('a, 'f) field = {
  f_name : string;
  f_ty : 'f typ;
  f_get : 'a -> 'f;
  f_set : 'f -> 'a -> 'a;
}
```

with this, once we've got the unmarshalled value of the field and a convenient record, we can set the value of the field in the record. The record itself can then be the accumulator in the fold - once we've folded over all the fields we'll have set every field in the record. The only problem then is that we need an initial value for the record to pass to the fold function. For that we can extend the definition of our `'a record` type to include this:

```ocaml
type 'a record = {
  r_fields : 'a boxed_field list;
  r_empty : 'a;
}
```

and we now have all the pieces required to write our `of_value` function:

```ocaml
let rec of_value : type a. a typ -> Value.t -> a = fun typ v ->
  match typ, v with
...
  | Record r, Value.Dict fs ->
    let a =
      List.fold_left
        (fun a f' ->
          match f' with B f -> f.f_set (of_value f.f_ty (List.assoc f.f_name fs)) a
          )
        r.r_empty r.r_fields
    in a
  | _, _ -> raise RTTE
```
