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
          (* strtoInt function can be replaced by a standard library function -
           * Int.fromString: which converts a string to integer *)
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

(* toString: int list -> string : Convert a list of integers into corresponding string *)
fun toString nil   = ""
  | toString alist =
        let
          val strList = map Int.toString alist
          fun toStr nil    = ""
            | toStr (h::t) = h ^ toStr(t)
        in
          toStr strList
        end;

(* karatsuba: int list -> int list -> int list : Take two large integers in the
 * integer list form and return their multiplication in the integer list form *)
fun karatsuba nil _         = nil
  | karatsuba _ nil         = nil
  | karatsuba [num1] [num2] =
        let
          val prod     = num1 * num2
          val prodStr  = Int.toString prod
          val prodSize = size prodStr
          val diff     = prodSize - 4
        in
          if diff > 0 then
            [valOf(Int.fromString(substring(prodStr, 0, diff))), valOf(Int.fromString(substring(prodStr, prodSize - 4, 4)))]
          else
            [prod]
        end;
  | karatsuba [xH, xL] [yL] =
        let
          val a        = [0]
          val d        = karatsuba [xL] [yL]
          val e        = subBigInt (karatsuba (addBigInt xH xL) [yL]) d
          val aShifted = [0]
          val eShifted = shift e 1
          val result   = addBigInt eShifted d
        in
          result
        end;
  | karatsuba [xL] [yH, yL] =
        let
          val a        = 0
          val d        = karatsuba [xL] [yL]
          val e        = subBigInt (karatsuba [xL] (addBigInt yH yL)) d
          val aShifted = [0]
          val eShifted = shift e 1
          val result   = addBigInt eShifted d
        in
          result
        end;
  | karatsuba [xH, xL] [yH, yL] =
        let
          val a = karatsuba [xH] [yH]
          val d = karatsuba [xL] [yL]
          val e = subBigInt (subBigInt (karatsuba (addBigInt xH xL) (addBigInt yH yL)) a) d
          val aShifted = shift a 2
          val eShifted = shift e 1
          val result   = addBigInt (addBigInt aShifted eShifted) d
        in
          result
        end;
  | karatsuba alist blist   =
        let
          val alistN = length alist
          val blistN = length blist
          val maxN   = max(alistN, blistN)
          val halfN  = (maxN+1) div 2
        in
          if (alistN <= 2 andalso blistN <=2) then

        end;

(* factorial: string -> string : Convert a number given as a string to its
 * factorial also in string *)
(*fun factorial "" = ""
  | factorial str =
        let
          val intList = fromString str
          *)
