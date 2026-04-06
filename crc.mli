open! Core

(* This computes the standard preset and inverted CRC, as used by most networking
   standards. Note that this is a little-endian CRC, which is best used with data
   transmitted lsbit-first, and it should, itself, be appended to data in little-endian
   byte and bit order to preserve the property of detecting all burst errors of length 32
   bits or less. *)

val crc32 : string -> Int63.t
val bigstring_crc32 : Bigstring.t -> pos:int -> len:int -> Int63.t
val iobuf_crc32 : local_ ([> read ], _, Iobuf.global) Iobuf.t -> Int63.t

(** String version of the CRC, encoded in hex. *)
val crc32hex : string -> string

module With_custom_polynomial : sig
  type t

  val create : polynomial:Int32.t -> t

  (** Computes the 32-bit CRC. *)
  val crc32 : t -> string -> Int63.t

  val bigstring_crc32 : t -> Bigstring.t -> pos:int -> len:int -> Int63.t
  val iobuf_crc32 : t -> local_ ([> read ], _, Iobuf.global) Iobuf.t -> Int63.t

  (** String version of the CRC, encoded in hex. *)
  val crc32hex : t -> string -> string
end
