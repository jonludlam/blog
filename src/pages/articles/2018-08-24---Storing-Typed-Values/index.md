---
title: Storing Typed Values
date: 2018-08-24
layout: post
draft: false
path: "/posts/storing-typed-values/"
category: "Xapi Types"
tags:
  - "ocaml"
  - "types"
  - "database"
description: "Storing typed values in our database rather than converting to untyped JSON"
---

Our previous implementation had to convert every value we wished to store in the database
into a string, which is obviously inefficient. Let's do a bit better by declaring a standard
ADT into which we can put the unmarshalled values and store this instead.

Previously the declaration of our `Row` module explicitly stated that we are storing 
strings in it.

```ocaml
module Row = struct
  include Make(String)
end
```

The signature of the module passed to `Make` is pretty simple, it only requires a `type t`.
Let's declare one of these that can store our values in. While we're at it, let's make a
function to convert a `t` to a `string` so we can print things.

```ocaml
module Value = struct
  type t =
  | Int : int -> t
  | Float : float -> t
  | String : string -> t
  | List : t list -> t
  | Map : (t * t) list -> t

  let rec to_string : t -> string = fun v ->
    match v with
    | String s -> Printf.sprintf "\"%s\"" s
    | Int i -> Printf.sprintf "%d" i
    | Float f -> Printf.sprintf "%f" f
    | List xs -> Printf.sprintf "[%s]" (String.concat ";" (List.map to_string xs))
    | Map kvs -> Printf.sprintf "[%s]" (String.concat ";" (List.map (fun (k,v) -> Printf.sprintf "(%s,%s)" (to_string k) (to_string v)) kvs))
end
```
Now we can have our rows store a `Value.t` rather than a `String.t`:

```ocaml
module Row = struct
  include Make(Value)
end
```

For example, to store the integer `5`, we would store `Int 5` rather than the string `"5"`
and avoid the whole `to_string`, `of_string` round trip - we just "wrap" each value to turn
it into a `Value.t`. Let's see what effect this has on the rest of the code.

Our `set_field` implementation took a default value to set when the field wasn't present.
This needs to be updated to take a default `Value.t` rather than a string:

```ocaml
let set_field tblname objref fldname v : Database.t -> Database.t =
  (function _ -> v)
  |> Row.update fldname (Value.String "")
  |> Table.update objref Row.empty
  |> TableSet.update tblname Table.empty
```

Previously, we had functions `to_string` and `of_string` that were used when putting values
in to, and getting values out of our database. Now we need simpler functions that turn our
values to and from `Value.t`:

```ocaml
let rec to_value : type a. a typ -> a -> Value.t = fun typ v ->
  match typ with
  | String -> Value.String v
  | Int -> Value.Int v
  | Refv _ -> Value.String (Ref.string_of v)
  | Set ty -> Value.List (List.map (to_value ty) v)
  | Map (ty1,ty2) ->
    let dict =
      List.map (fun (v1,v2) ->
        to_value ty1 v1,
        to_value ty2 v2) v
    in
    Value.Map dict

exception RTTE

let rec of_value : type a. a typ -> Value.t -> a = fun typ v ->
  match typ, v with
  | String, Value.String str -> str
  | Int, Value.Int i -> i
  | Refv cls, Value.String s -> Ref.of_string cls s
  | Set ty, Value.List vs -> List.map (of_value ty) vs
  | Map (ty1, ty2), Value.Map kvs ->
    List.map (fun (k,v) ->
      of_value ty1 k,
      of_value ty2 v
    ) kvs
  | _, _ -> raise RTTE
```

Here, `to_value` looks very similar to `to_string`. The `of_value` function deserves a
little more attention. We match on `typ` _and_ `v`, but we only expect to see cases where
the two match up. That is, if `typ` is `String`, we expect our database to contain a 
`Value.String`. Nothing in our types ensures this though, so we have to add the wildcard
pattern match at the bottom that raises `RTTE` (Run-Time Type Error). This isn't a
problem introduced here, since previously a type mismatch would have ended up raising
`Failure "int_of_string"` or worse, successfully convert without warning about it!
So we still have something to handle, but it's more consistent now.

Next we have to modify the `Field` module and database setter and getter to use these new functions:

```ocaml
module Field : sig
  type ('c, 'f) t
  val construct : string -> 'c cls -> 'f typ -> ('c, 'f) t
  val name_of : ('c, 'f) t -> string
  val table_of : ('c, 'f) t -> string
  val to_value : ('c, 'f) t -> 'f -> Value.t
  val of_value : ('c, 'f) t -> Value.t -> 'f
end = struct
  type ('c,'f) t = {
    name : string;
    cls : 'c cls;
    ty : 'f typ;
  }
  let name_of x = x.name
  let table_of : type c. (c, _) t -> string = fun x -> match x.cls with | VM -> "VM" | VBD -> "VBD"
  let construct name cls ty = {name; cls; ty}
  let to_value f v = to_value f.ty v
  let of_value f v = of_value f.ty v
end

let set : 'a Ref.t -> ('a,'b) Field.t -> 'b -> unit = fun ref field v ->
  let open Field in
  db := set_field (table_of field) (Ref.string_of ref) (name_of field) (to_value field v) !db

let get : 'a Ref.t -> ('a,'b) Field.t -> 'b = fun ref field ->
  let open Field in
  get_field (table_of field) (Ref.string_of ref) (name_of field) !db |> (of_value field)
```

That's essentially it. We've now removed the cost of converting to and from strings to store things in our database. Here's the final db6.ml:

```ocaml
(* DB6 - Store unmarshalled values *)

module StringMap = Map.Make(String)

module type VAL = sig
  type t
end

module Make (V : VAL) = struct
  type t = V.t StringMap.t
  let empty = StringMap.empty
  let add = StringMap.add
  let find = StringMap.find
  let mem = StringMap.mem
  let remove = StringMap.remove
  let update key default f t =
    let cur = if mem key t then find key t else default in
    let newv = f cur in
    StringMap.add key newv t
  let iter = StringMap.iter
end
module Value = struct
  type t =
    | Int : int -> t
    | Float : float -> t
    | String : string -> t
    | List : t list -> t
    | Map : (t * t) list -> t

  let rec to_string : t -> string = fun v ->
    match v with
    | String s -> Printf.sprintf "\"%s\"" s
    | Int i -> Printf.sprintf "%d" i
    | Float f -> Printf.sprintf "%f" f
    | List xs -> Printf.sprintf "[%s]" (String.concat ";" (List.map to_string xs))
    | Map kvs -> Printf.sprintf "[%s]" (String.concat ";" (List.map (fun (k,v) -> Printf.sprintf "(%s,%s)" (to_string k) (to_string v)) kvs))

end

module Row = struct
  include Make(Value)
end

module Table = struct
  include Make(Row)
end

module TableSet = struct
  include Make(Table)
end

module Database = struct
  type t = TableSet.t
  let empty = TableSet.empty
end

let get_field tblname objref fldname db =
  TableSet.find tblname db |>
  Table.find objref |>
  Row.find fldname

let set_field tblname objref fldname v : Database.t -> Database.t =
  (function _ -> v)
  |> Row.update fldname (Value.String "")
  |> Table.update objref Row.empty
  |> TableSet.update tblname Table.empty

let db = ref Database.empty

type _ cls =
  | VM : [`vm] cls
  | VBD : [`vbd] cls

(* Phantom-type-using references *)
module Ref : sig
  type 'c t = private S: string -> 'c t
  val string_of : 'c t -> string
  val of_string : 'c cls -> string -> 'c t
  val pp : Format.formatter -> 'c t -> unit
end = struct
  type 'c t = S : string -> 'c t
  let string_of (S v) = v
  let of_string _ v = (S v)
  let pp fmt (S v) = Format.pp_print_string fmt v
end

type _ typ =
  | String : string typ
  | Int : int typ
  | Refv : 'c cls -> 'c Ref.t typ
  | Set : 'a typ -> 'a list typ
  | Map : ('a typ * 'b typ) -> ('a * 'b) list typ

exception RTTE

let rec to_value : type a. a typ -> a -> Value.t = fun typ v ->
  match typ with
  | String -> Value.String v
  | Int -> Value.Int v
  | Refv _ -> Value.String (Ref.string_of v)
  | Set ty -> Value.List (List.map (to_value ty) v)
  | Map (ty1,ty2) ->
    let dict =
      List.map (fun (v1,v2) ->
        to_value ty1 v1,
        to_value ty2 v2) v
    in
    Value.Map dict

let rec of_value : type a. a typ -> Value.t -> a = fun typ v ->
  match typ, v with
  | String, Value.String str -> str
  | Int, Value.Int i -> i
  | Refv cls, Value.String s -> Ref.of_string cls s
  | Set ty, Value.List vs -> List.map (of_value ty) vs
  | Map (ty1, ty2), Value.Map kvs ->
    List.map (fun (k,v) ->
      of_value ty1 k,
      of_value ty2 v
      ) kvs
  | _, _ -> raise RTTE


(* Better: *)

module Field : sig
  type ('c, 'f) t
  val construct : string -> 'c cls -> 'f typ -> ('c, 'f) t
  val name_of : ('c, 'f) t -> string
  val table_of : ('c, 'f) t -> string
  val to_value : ('c, 'f) t -> 'f -> Value.t
  val of_value : ('c, 'f) t -> Value.t -> 'f
end = struct
  type ('c,'f) t = {
    name : string;
    cls : 'c cls;
    ty : 'f typ;
  }
  let name_of x = x.name
  let table_of : type c. (c, _) t -> string = fun x -> match x.cls with | VM -> "VM" | VBD -> "VBD"
  let construct name cls ty = {name; cls; ty}
  let to_value f v = to_value f.ty v
  let of_value f v = of_value f.ty v
end

let set : 'a Ref.t -> ('a,'b) Field.t -> 'b -> unit = fun ref field v ->
  let open Field in
  db := set_field (table_of field) (Ref.string_of ref) (name_of field) (to_value field v) !db

let get : 'a Ref.t -> ('a,'b) Field.t -> 'b = fun ref field ->
  let open Field in
  get_field (table_of field) (Ref.string_of ref) (name_of field) !db |> (of_value field)

(* Example showing how we would use these in practice *)
let dump db =
  TableSet.iter (fun tblname table ->
    Printf.printf "\n# TABLE: %s\n\n" tblname;
    Table.iter (fun objref row ->
      Printf.printf "## Object: %s\n" objref;
      Row.iter (fun fldname v ->
        Printf.printf "  %s: %s\n" fldname (Value.to_string v)) row) table) db

let vm1 = Ref.of_string VM "OpaqueRef:abcde"
let vbd1 = Ref.of_string VBD "OpaqueRef:fghij"
let vbd2 = Ref.of_string VBD "OpaqueRef:12345"

let name_label = Field.construct "name_label" VM String
let vBDs = Field.construct "VBDs" VM (Set (Refv VBD))
let vbd_vm = Field.construct "VM" VBD (Refv VM)
let memory = Field.construct "memory" VM Int

let _ =
  set vm1 name_label "my first vm";
  set vbd1 vbd_vm vm1;
  set vbd2 vbd_vm vm1;
  set vm1 vBDs [vbd1; vbd2];
  set vm1 memory 63356;
  dump !db
```
