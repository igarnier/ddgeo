let ( >>= ) x f = match x with Ok v -> f v | Error _ as e -> e

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let get_int f =
  let a = bigarray_create Bigarray.int32 1 in
  f a ;
  Int32.to_int a.{0}
