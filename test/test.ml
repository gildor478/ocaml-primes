open OUnit
open Primes

let _lst: 'a list =
  run_test_tt_main
    ("Primes" >:::
     [
       "Simple" >::
       (fun () ->
          let t =
            Eratosthenes.create 10000
          in
            List.iter
              (fun (i,p) ->
                 assert_equal
                   ~printer:(fun (i,p) ->
                               Printf.sprintf "(%d, %b)" i p)
                   (i, p)
                   (i, is_prime t i))
              [2,  true;
               3,  true;
               4,  false;
               5,  true;
               6,  false;
               7,  true;
               8,  false;
               9,  false;
               10, false;
               11, true;
               12, false;
               13, true;
               14, false;
               15, false;
               16, false;
               17, true;
               18, false;
               19, true;
               20, false])
     ])

