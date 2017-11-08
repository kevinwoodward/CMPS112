(* Kevin Woodward, keawoodw@ucsc.edu *)
(* Megan Sharp, mesharp@ucsc.edu *)

module Bigint : sig
   type bigint
   val bigint_of_string : string -> bigint
   val string_of_bigint : bigint -> string
   val string_formatted : string -> string
   val sanitize : bigint -> bigint
   val cmp : int list -> int list -> int
   val cmp_bigint : bigint -> bigint -> int
   val add : bigint -> bigint -> bigint
   val sub : bigint -> bigint -> bigint
   val mul : bigint -> bigint -> bigint
   val div : bigint -> bigint -> bigint
   val rem : bigint -> bigint -> bigint
   val pow : bigint -> bigint -> bigint
   val zero : bigint
end
