open! Core
open! Import

(* This file is based on code with the following header:

   Copyright abandoned; this code is in the public domain. Provided to GNUnet by
   peter@horizon.com *)

(* We don't seem to have a non-sign-extending conversion... *)
let int63_of_uint32 u32 =
  let open Int64 in
  of_int32_exn u32 land 0xFFFF_FFFFL |> Int63.of_int64_trunc
;;

module With_custom_polynomial = struct
  type t = (Int32.t, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t

  let create ~polynomial =
    let table = Bigarray.Array1.create Int32 C_layout 256 in
    Bigarray.Array1.fill table 0l;
    let i = ref 128 in
    let j = ref 0 in
    let h = ref 1l in
    while !i >= 1 do
      (h := Int32.((!h lsr 1) lxor (!h land 1l * polynomial)));
      (* [h] is now the [i]th element of [table] *)
      j := 0;
      while !j < 256 do
        let value_j = Bigarray.Array1.get table !j in
        Bigarray.Array1.set table (!i + !j) (Int32.( lxor ) value_j !h);
        j := !j + (2 * !i)
      done;
      i := !i lsr 1
    done;
    table
  ;;

  let[@inline always] crc32_kernel (t : t) ~buffer ~pos ~len ~unsafe_get =
    let crc_buf = ref 0xffff_ffffl in
    for pos = pos to pos + len - 1 do
      let c = unsafe_get buffer ~pos in
      let i = Int32.(!crc_buf lxor c land 0xffl) |> Int32.to_int_trunc in
      crc_buf := Int32.((!crc_buf lsr 8) lxor Bigarray.Array1.unsafe_get t i)
    done;
    Int32.(!crc_buf lxor 0xffff_ffffl) |> int63_of_uint32
  ;;

  let unsafe_bigstring_crc32 =
    (* Note that we call [check_pos_len_exn] before calling this function *)
    let unsafe_get buffer ~pos =
      Int32.of_int_trunc (Bigstring.unsafe_get_uint8 buffer ~pos)
    in
    fun (t : t) ~bstr ~pos ~len ->
      (* If we don't use flambda, [unsafe_get] is called via [caml_apply], introducing an
         extra indirection. *)
      crc32_kernel t ~buffer:bstr ~pos ~len ~unsafe_get
  ;;

  let crc32 =
    let unsafe_get buffer ~pos =
      String.unsafe_get buffer pos |> Char.to_int |> Int32.of_int_trunc
    in
    fun (t : t) str ->
      let len = String.length str in
      crc32_kernel t ~buffer:str ~pos:0 ~len ~unsafe_get
  ;;

  let bigstring_crc32 t bstr ~pos ~len =
    let total_length = Bigstring.length bstr in
    Ordered_collection_common.check_pos_len_exn ~pos ~len ~total_length;
    unsafe_bigstring_crc32 t ~bstr ~pos ~len
  ;;

  let crc32hex t s = Printf.sprintf "%08LX" (Int63.to_int64 (crc32 t s))

  let iobuf_crc32 t iobuf =
    let hi = Iobuf.Expert.hi iobuf in
    let lo = Iobuf.Expert.lo iobuf in
    bigstring_crc32 t (Iobuf.Expert.buf iobuf) ~pos:lo ~len:(hi - lo)
  ;;

  module%test [@name "crc32c"] _ = struct
    (* this is the polynomial for crc32c *)
    let t = create ~polynomial:0x82f6_3b78l
    let str = "The quick brown fox jumps over the lazy dog"
    let len = String.length str
    let crc = Int63.of_int64_trunc 0x2262_0404L
    let bstr = Bigstring.of_string str
    let%test_unit _ = [%test_result: Int63.Hex.t] (crc32 t str) ~expect:crc

    let%test_unit _ =
      [%test_result: Int63.Hex.t] (bigstring_crc32 t bstr ~pos:0 ~len) ~expect:crc
    ;;

    let%test_unit _ =
      [%test_result: Int63.Hex.t]
        (bigstring_crc32 t (Bigstring.of_string ("12345" ^ str ^ "12345")) ~pos:5 ~len)
        ~expect:crc
    ;;

    let%test _ = does_raise (fun () -> bigstring_crc32 t bstr ~pos:0 ~len:(-1))
    let%test _ = does_raise (fun () -> bigstring_crc32 t bstr ~pos:0 ~len:(len + 1))
    let%test _ = does_raise (fun () -> bigstring_crc32 t bstr ~pos:(-1) ~len:0)
    let%test _ = does_raise (fun () -> bigstring_crc32 t bstr ~pos:(len + 1) ~len:0)
  end
end

let t = With_custom_polynomial.create ~polynomial:0xedb88320l
let crc32 s : Int63.t = With_custom_polynomial.crc32 t s

let bigstring_crc32 bstr ~pos ~len : Int63.t =
  With_custom_polynomial.bigstring_crc32 t bstr ~pos ~len
;;

let crc32hex s = Printf.sprintf "%08LX" (Int63.to_int64 (crc32 s))

let iobuf_crc32 iobuf : Int63.t =
  let hi = Iobuf.Expert.hi iobuf in
  let lo = Iobuf.Expert.lo iobuf in
  bigstring_crc32 (Iobuf.Expert.buf iobuf) ~pos:lo ~len:(hi - lo)
;;

module%test [@name "crc32"] _ = struct
  let str = "The quick brown fox jumps over the lazy dog"
  let len = String.length str
  let crc = Int63.of_int64_exn 0x414fa339L
  let bstr = Bigstring.of_string str
  let%test_unit _ = [%test_result: Int63.Hex.t] (crc32 str) ~expect:crc

  let%test_unit _ =
    [%test_result: Int63.Hex.t] (bigstring_crc32 bstr ~pos:0 ~len) ~expect:crc
  ;;

  let%test_unit _ =
    [%test_result: Int63.Hex.t]
      (bigstring_crc32 (Bigstring.of_string ("12345" ^ str ^ "12345")) ~pos:5 ~len)
      ~expect:crc
  ;;

  let%test _ = does_raise (fun () -> bigstring_crc32 bstr ~pos:0 ~len:(-1))
  let%test _ = does_raise (fun () -> bigstring_crc32 bstr ~pos:0 ~len:(len + 1))
  let%test _ = does_raise (fun () -> bigstring_crc32 bstr ~pos:(-1) ~len:0)
  let%test _ = does_raise (fun () -> bigstring_crc32 bstr ~pos:(len + 1) ~len:0)
end
