---
title: "Puttin' on the Types"
draft: false
layout: post
path: "/posts/puttin-on-the-types/"
date: 2018-03-02
category: "Xapi Types"
tags:
  - "ocaml"
  - "types"
  - "database"
description: "Adding a typed layer on top of the untyped database introduced in part 1"

---

Last time we created a little database capable of storing information in a structured but untyped way. Of course, being strong, statically typed OCaml engineers rather than crazy dynamically typed python people we demand a typed interface to this database, so let's examine how we do this today in xapi.

## Datamodel

XenServer is controlled by connecting to xapi and sending it XMLRPC encoded requests. A fundamental component of xapi is therefore the set of OCaml modules that determine the API types, methods and the datamodel of database, including everything required to generate the [online documentation](https://xapi-project.github.io/xen-api/). These are all in the xen-api source tree under the [ocaml/idl](https://github.com/xapi-project/xen-api/tree/a3d339335fd6f4d1f649a40771f0847abdc10e63/ocaml/idl) directory. These are compiled into an executable that's used to generate a large amount of code that actually forms the core modules of the xapi daemon. For example, from these definitions we generate, amongst others, `aPI.ml` that contains the type declarations, `client.ml` that contains functions for invoking the XenAPI and `server.ml` that contain a dispatcher function into which the concrete implementation of the API method can be plumbed. The API is quite object oriented, so we declare classes such as `VM` and `host`, and have methods that operate on them, for example, `VM.suspend` or `host.set_name_label`. As well as methods, these classes also have fields that are defined by creating values of the [following type](https://github.com/xapi-project/xen-api/blob/a3d339335fd6f4d1f649a40771f0847abdc10e63/ocaml/idl/datamodel_types.ml#L341-L358):

```ocaml
type field = {
  release: release;
  lifecycle: lifecycle_transition list;
  field_persist: bool;
  default_value: api_value option;
  internal_only: bool;
  qualifier: qualifier;
  field_name: string;
  full_name: string list;
  ty: ty;
  field_description: string;
  field_has_effect: bool;
  field_ignore_foreign_key: bool;
  field_setter_roles: string list option;
  field_getter_roles: string list option;
  field_map_keys_roles: (string * (string list option)) list;
  field_doc_tags: doc_tag list;
}
```

and then these fields are gathered together along with the list of methods into a value that represents the whole class:

```ocaml
type obj = {
  name : string;
  description : string;
  obj_lifecycle: lifecycle_transition list;
  contents : content list;
  messages : message list;
  doccomments : (string * string) list;
  msg_lifecycles: ((string * (lifecycle_transition list)) list);
  gen_constructor_destructor: bool;
  force_custom_actions: qualifier option; (* None,Some(RW),Some(StaticRO) *)
  obj_allowed_roles: string list option; (* for construct, destruct and explicit obj msgs*)
  obj_implicit_msg_allowed_roles: string list option; (* for all other implicit obj msgs*)
  gen_events: bool;
  persist: persist_option;
  obj_release: release;
  in_database: bool; (* If the object is in the database *)
  obj_doc_tags: doc_tag list;
}
```

We'll be concentrating on the `contents` field of the object, which contains (slightly indirectly) objects of type `field`. Of the field object, we care most in this set of posts on the field `ty`.

We're also going to pick on [VMs](https://xapi-project.github.io/xen-api/classes/vm.html) (Virtual Machines) and [VBDs](https://xapi-project.github.io/xen-api/classes/vbd.html) (Virtual Block Devices - or disks!) as examples of the sorts of objects and fields we'll be storing in the database. For example, a VM will have a name, a field representing how much memory it should have, a list of disks (VBDs) associated with it, and other fields. A VBD might have a reference back to the VM it belongs to, a field delaring the device it should appear as, and so on.

Let's see an excerpt from the [definition of the VM object](https://github.com/xapi-project/xen-api/blob/bcd3f3b7a63b420aff3b27e2993566621eb2d559/ocaml/idl/datamodel_vm.ml#L1227-L1404):

```ocaml
let t =
    create_obj
        ~in_db:true
        ~in_product_since:rel_rio
        ~in_oss_since:oss_since_303
        ~internal_deprecated_since:None
        ~persist:PersistEverything
        ~gen_constructor_destructor:true
        ~name:_vm
        ~descr:"A virtual machine (or 'guest')."
        ~gen_events:true
        ~lifecycle:[
            Published, rel_rio, "";
        ]
        ~messages_default_allowed_roles:_R_VM_ADMIN
        ~messages:[
            snapshot;
            snapshot_with_quiesce;
...
        ]
        ~contents:([
            uid _vm;
...
            field ~qualifier:DynamicRO ~ty:(Set (Ref _vbd)) "VBDs" "virtual block devices";
            field  ~ty:(Map(String, String)) "other_config" "additional configuration";
            field ~qualifier:DynamicRO ~ty:Int "domid" "domain ID (if available, -1 otherwise)";
...
        ])
        ()
```

where `create_obj` creates an `obj` value, and `uid` and `field` are simply helper functions that construct values of type `field` as declared above.
We're simply creating a value representing the VM object using the [`create_obj`](https://github.com/xapi-project/xen-api/blob/a3d339335fd6f4d1f649a40771f0847abdc10e63/ocaml/idl/datamodel_common.ml#L484-L532) function, to which we pass some interesting bits of metadata associated with the object - we store it in the database, it's been in since the Rio release (4.0) and some other miscellaneous values. We also list the API methods we might use on a VM, and importantly for the database, the `contents` argument lists all of the fields in the object - for example we've got here a field called "VBDs" that has a type defined by the `ty` parameter as `Set (Ref _vbd)`, and a field `other_config` which has a type `Map (String, String)`, representing, as you might expect, a string to string map. These types are values of type [`Datamodel_types.ty`](https://github.com/xapi-project/xen-api/blob/a3d339335fd6f4d1f649a40771f0847abdc10e63/ocaml/idl/datamodel_types.ml), and are very important, so let's look at them more closely.

```ocaml
type ty =
  | String
  | Int
  | Float
  | Bool
  | DateTime
  | Enum of string * (string * string) list
  | Set of ty
  | Map of ty * ty
  | Ref of string
  | Record of string
```

The first few are obvious, then we've got:

1. An enumeration type that has a name and a list of named values it can take (e.g. the power_state of a VM might be `Running`, `Halted`, `Paused` or `Suspended`),

2. Sets and Maps that are used recursively,

3. References to particular object types (e.g. a "VM" reference or a "VBD" reference),

4. A named record, for example the VM we're constructing above.

This is the full set of different types of value we can currently store in our database.

These types all have some sort of representation in the generated OCaml code. Most of the `ty` type correspond as you might imagine to OCaml types, so `Strings` are simply OCaml strings, `Ints` are int64s and so on. We generate explicitly type declarations for Enums, Sets, Maps, Refs and Records.

Enums are expressed as polymorphic variants, so for example the `power_state` enum of a VM is declared like this:

```ocaml
let power_state =
    Enum ("vm_power_state", [ "Halted", "VM is offline and not using any resources";
                              "Paused", "All resources have been allocated but the VM itself is paused and its vCPUs are not running";
                              "Running", "Running";
                              "Suspended", "VM state has been saved to disk and it is nolonger running. Note that disks remain in-use while the VM is suspended."])
```

which is compiled into a type declaration in `aPI.ml` looking like this:

```ocaml
type vm_power_state = [ `Halted | `Paused | `Running | `Suspended ]
```

Sets and Maps are compiled into type declarations that look like this:

```ocaml
type vm_power_state_set = vm_power_state list
type string_to_string_map = (string * string) list
```

Records are turned into standard OCaml records, so our VM record type declaration in `aPI.ml` looks like this:

```ocaml
type vM_t = {
    vM_uuid : string;
    vM_name_label : string;
    vM_VBDs : ref_VBD_set;
...
    }
```

and Refs are turned into this:

```ocaml
type ref_VM = [`VM] Ref.t
```

This last one looks a bit unusual, so let's take a deeper look at how this works.

### Typesafe references

References are implemented as strings as we saw in the previous article, but they refer to different classes of objects, so we might have a reference to a VM or a reference to a Host. We'd like to distinguish at the type level between references to these different object types so the type checker would reject any use of a Host reference to look up a VM field. We do this by using phantom types.

```ocaml
module Ref : sig
  type 'a t
  val string_of : 'a t -> string
  val of_string : string -> 'a t
  val pp : Format.formatter -> 'a t -> unit
end = struct
  type 'a t = string
  let string_of v = v
  let of_string v = v
  let pp fmt v = Format.pp_print_string fmt v
end
```

Phantom type are those where a type is parameterized but the type parameter doesn't appear on the right hand side:

```ocaml
type 'a t = string
```

This says that all references are strings and actually have nothing to do with the `'a`. However, we hide the concrete representation outside of the module by using a signature that simply exposes the Ref.t as an _opaque_ parameterized type - that is, nobody who has a `Ref.t` in their hands knows that it's actually a string.

Let's see what happens when we try to create one of these:

```ocaml
utop # let x = Ref.of_string "OpaqueRef:foo";;
val x : '_a Ref.t = <abstr>
```

Since we haven't told OCaml what the type parameter ought to be it doesn't know and leaves it unspecified. However, note that this isn't a _polymorphic_ value, it's just we haven't yet figure out what monomorphic type it should be. It's not so nice to do this since buggy code elsewhere might lead to the OCaml deciding the type is something wacky, so let's constrain the type at the point we create the reference from a string. We'll use polymorphic variants because we don't need to declare or namespace them.

```ocaml
let vm : [`vm] Ref.t = Ref.of_string "OpaqueRef:abcde"
let vbd1 : [`vbd] Ref.t = Ref.of_string "OpaqueRef:fghij"
let vbd2 : [`vbd] Ref.t = Ref.of_string "OpaqueRef:12345"
```

and we can also use this to constrain the types our functions will accept. For example:

```ocaml
val function_for_VM_only : [`vm] Ref.t -> unit
```

We use this sort of thing extensively to ensure we're always using the right references in the right places.

### Database Accessors

As well as the `client.ml` and `server.ml` modules mentioned above, the code generator also generates modules to access the database in a typesafe fashion. These are:

1. `DM_to_String` - containing functions to convert to strings from the more complex OCaml representation of the datamodel types (e.g. `(Set (Ref _vbd))` or `Map(String,String)` from above). Note that for simple OCaml representations we'll inline the conversion.

2. `String_to_DM` - containing functions to convert from strings back to the OCaml types.

3. A module per class containing setters and getters for each field of the object.

In the VM object definition excerpt above we had a field called `VBDs` that had type `Set (Ref _vbd)`. The entry for that in the generated conversion modules might look like this:

```ocaml
module DM_to_String = struct
  let vbd_set : [`vbd] Ref.t list -> string =
    fun vbds ->
      String.concat "," (List.map Ref.string_of vbds)
end

module String_to_DM = struct
  let vbd_set : string -> [`vbd] Ref.t list =
    fun str ->
      Astring.String.cuts ~sep:"," str |> List.map Ref.of_string
end
```

and the setters and getters might look like this:

```ocaml
module VM : sig
  val set_name_label : [`vm] Ref.t -> string -> unit
  val get_name_label : [`vm] Ref.t -> string
  val set_memory : [`vm] Ref.t -> int -> unit
  val get_memory : [`vm] Ref.t -> int
  val set_VBDs : [`vm] Ref.t -> [`vbd] Ref.t list -> unit
  val get_VBDs : [`vm] Ref.t -> [`vbd] Ref.t list
end = struct
  let set_name_label self v =
    db := set_field "VM" (Ref.string_of self) "name_label" v !db

  let get_name_label self =
    get_field "VM" (Ref.string_of self) "name_label" !db

  let set_memory self v =
    db := set_field "VM" (Ref.string_of self) "memory" (string_of_int v) !db

  let get_memory self =
    get_field "VM" (Ref.string_of self) "memory" !db |> int_of_string

  let set_VBDs self v =
    db := set_field "VM" (Ref.string_of self) "VBDs" (DM_to_String.vbd_set v) !db

  let get_VBDs self =
    get_field "VM" (Ref.string_of self) "VBDs" !db |> String_to_DM.vbd_set
end

module VBD : sig
  val set_VM : [`vbd] Ref.t -> [`vm] Ref.t -> unit
  val get_VM : [`vbd] Ref.t -> [`vm] Ref.t
end = struct
  let set_VM self v =
    db := set_field "VBD" (Ref.string_of self) "VM" (Ref.string_of v) !db

  let get_VM self =
    get_field "VBD" (Ref.string_of self) "VM" !db |> Ref.of_string
end
```

There's no context on these setters and getters - they assume there's one global database into which we can set values and get them back later. Since the database itself is immutable we implement setters by having a global mutable variable that holds the current database. A setter then takes this, applies the setter to create a new database, and puts it back into the mutable variable. 

Putting this all together with the definitions from last time we get a complete example:

```ocaml
(* DB2 - roughly equivalent to what we have in xapi at time of writing. *)

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

module Row = struct
  include Make(String)
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
  |> Row.update fldname ""
  |> Table.update objref Row.empty
  |> TableSet.update tblname Table.empty

let dump db =
  TableSet.iter (fun tblname table ->
    Printf.printf "# TABLE: %s\n\n" tblname;
    Table.iter (fun objref row ->
      Printf.printf "## Object: %s\n" objref;
      Row.iter (fun fldname v ->
        Printf.printf "  %s: %s\n" fldname v) row) table) db


let db = ref Database.empty


(* Phantom-type-using references *)
module Ref : sig
  type 'a t
  val string_of : 'a t -> string
  val of_string : string -> 'a t
  val pp : Format.formatter -> 'a t -> unit
end = struct
  type 'a t = string
  let string_of v = v
  let of_string v = v
  let pp fmt v = Format.pp_print_string fmt v
end

(* The following 4 modules would normally be constructed from
   code generated by datamodel definitions *)

module DM_to_String = struct
  let vbd_set : [`vbd] Ref.t list -> string =
    fun vbds ->
      String.concat "," (List.map Ref.string_of vbds)
end

module String_to_DM = struct
  let vbd_set : string -> [`vbd] Ref.t list =
    fun str ->
      Astring.String.cuts ~sep:"," str |> List.map Ref.of_string
end

module VM : sig
  val set_name_label : [`vm] Ref.t -> string -> unit

  val get_name_label : [`vm] Ref.t -> string

  val set_memory : [`vm] Ref.t -> int -> unit

  val get_memory : [`vm] Ref.t -> int

  val set_VBDs : [`vm] Ref.t -> [`vbd] Ref.t list -> unit

  val get_VBDs : [`vm] Ref.t -> [`vbd] Ref.t list
end = struct
  let set_name_label self v =
    db := set_field "VM" (Ref.string_of self) "name_label" v !db

  let get_name_label self =
    get_field "VM" (Ref.string_of self) "name_label" !db

  let set_memory self v =
    db := set_field "VM" (Ref.string_of self) "memory" (string_of_int v) !db

  let get_memory self =
    get_field "VM" (Ref.string_of self) "memory" !db |> int_of_string

  let set_VBDs self v =
    db := set_field "VM" (Ref.string_of self) "VBDs" (DM_to_String.vbd_set v) !db

  let get_VBDs self =
    get_field "VM" (Ref.string_of self) "VBDs" !db |> String_to_DM.vbd_set
end

module VBD : sig
  val set_VM : [`vbd] Ref.t -> [`vm] Ref.t -> unit
  val get_VM : [`vbd] Ref.t -> [`vm] Ref.t
end = struct
  let set_VM self v =
    db := set_field "VBD" (Ref.string_of self) "VM" (Ref.string_of v) !db
  let get_VM self =
    get_field "VBD" (Ref.string_of self) "VM" !db |> Ref.of_string
end

(* Example showing how we would use these in practice *)

let vm : [`vm] Ref.t = Ref.of_string "OpaqueRef:abcde"
let vbd1 : [`vbd] Ref.t = Ref.of_string "OpaqueRef:fghij"
let vbd2 : [`vbd] Ref.t = Ref.of_string "OpaqueRef:12345"

let _ =
  VM.set_name_label vm "my first vm";
  VBD.set_VM vbd1 vm;
  VBD.set_VM vbd2 vm;
  VM.set_VBDs vm [vbd1; vbd2];
  VM.set_memory vm 63356;
```

