(* This program prints generated code as string *)

open Fix_functor
open Fix

let print c = Codelib.print_code Format.std_formatter c ;;
print F.int ;;
print F.var ;;
print F.add ;;
print F.sub ;;
print F.mul ;;
print F.div ;;
print F.observe ;;
