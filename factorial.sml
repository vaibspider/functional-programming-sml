(* Pad the given string with zeros, if size of string is not a multiple of 4 *)
fun padZeros "" = ""
  | padZeros s  = 
        let
          fun padZ 0 = s
            | padZ 1 = "000"^s
            | padZ 2 = "00"^s
            | padZ 3 = "0"^s     (* non-exhaustive matching - but it's fine in this context *)
        in
          padZ (size s mod 4)
        end;

(* Convert a given string to a list of strings - each with exactly 4 characters *)
fun toStringList ""  = nil
  | toStringList str = 
        let
          val paddedStr = padZeros(str)
          fun toStringL("", alist) = alist
            | toStringL(s, alist)  = 
                toStringL(if (size s <> 0) then substring(s, 0, size(s)-4)
                                           else "", substring(s, size(s)-4, 4)::alist)
        in
          toStringL(paddedStr, [])
        end;

(* fromString: string -> int list : Convert a string of digits to an integer list with each element in base 10^4 *)
fun fromString ""  = nil
  | fromString str = 
        let
          val strL = toStringList str
          fun chartoInt ch = ord ch - ord #"0"
          fun strtoInt s = 
              let
                val chlist = explode s
                val diglist = map chartoInt chlist
                fun strtoIntAcc(nil, num)  = num
                  | strtoIntAcc(h::t, num) = 
                        strtoIntAcc(t, num*10+h)
              in
                strtoIntAcc(diglist, 0)
              end;
        in
          map strtoInt strL
        end;
