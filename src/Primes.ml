
type t =
    {
      data_len:  int;
      data: BatBitSet.t; (* This is a tradeoff between memory and speed!
                            a bitset is slower to access than a bool array
                            but is a win in term of memory and speed to
                            enumerate values.
                          *)
    }

module Eratosthenes =
struct

  let create n =
    let t =
      {
        data_len  = (n + 1);
        data = BatBitSet.create_full (n + 1);
      }
    in
    let cross start step =
      if start >= 0 then
        let idx = ref start in
          while !idx <= n do
            BatBitSet.unset t.data !idx;
            idx := !idx + step
          done
    in
      for i = 2 to n do
        if BatBitSet.mem t.data i then
          cross (i * i) i
      done;
      t

  let fold f a t =
    BatEnum.fold f a (BatBitSet.enum t.data)

  let list t =
    List.rev (fold (fun acc p -> p :: acc) [] t)

  let length t =
    t.data_len
end

module E = Eratosthenes

let is_prime t n =
  if n < E.length t then
    BatBitSet.mem t.data n
  else
    begin
      let idx = ref 2 in
      let res = ref true in
      let end_bitset = ref false in
      let sqrt = int_of_float (sqrt (float_of_int n)) in
        (* Fast scan of already generated prime numbers *)
        while !res && not !end_bitset do
          match  BatBitSet.next_set_bit t.data (!idx + 1) with
            | Some i ->
                idx := i;
                res := (n mod !idx) <> 0
            | None ->
                end_bitset := true
        done;
        (* We reach the end of primes table *)
        while !res && !idx <= sqrt do
          res := (n mod !idx) <> 0;
          incr idx; incr idx (* we can jump over even number *)
        done;
        !res
    end

let fold f a t =
  E.fold f a t

let list t =
  E.list t

