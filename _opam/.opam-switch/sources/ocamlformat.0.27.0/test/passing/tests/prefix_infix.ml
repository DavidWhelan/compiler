let _ = List.filter (( != ) e) l

let _ = List.map (( != ) x) l

let _ = x != y

let _ = - !e

let _ = - !e.f

let z = (( ! ) ~x:4) 1 2 ~c:3

let z = (( ! ) ~x:4 y z) 1 2 ~c:3

let z = (( ! ) ~x:4 [@attr]) 1 2 ~c:3

let z = (( ! ) [@attr]) 1 2 ~c:3

let z = ( ! ) [@attr]

let i x = (!r [@attr]) x

let _ = ( * ) [@attr]

let _ = f (( * ) [@attr]) ;;

( * ) [@attr]

(* TEST_BELOW *)

(* Int and float plus and minus operators *)
-1;;
+1;;
-1.0;;
+1.0;;
-.1.0;;
+.1.0;;

(* Prefix operator *)
~+2;;

(* With attributes attached to the argument *)
-(1[@foo]);;
+(1[@foo]);;
-(1.0[@foo]);;
+(1.0[@foo]);;
-.(1.0[@foo]);;
+.(1.0[@foo]);;

~+(2[@foo]);;

(* TEST
 flags = "-dparsetree -dno-locations -stop-after parsing";
 setup-ocamlc.byte-build-env;
 ocamlc.byte;
 check-ocamlc.byte-output;
*)
