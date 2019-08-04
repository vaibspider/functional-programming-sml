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

(* toBigInt: int -> int list : Convert an integer into a list of integers form,
 * where each integer is in base 10^4 *)
fun toBigInt num =
        let
          val numStr = Int.toString num
        in
          fromString numStr
        end;

(* shift: int list -> int -> int list : Take an integer list and a number, which
 * is to be considered as a power of Base (by default 10^4). Shift the integer
 * list by those many digits *)
fun shift nil _   = nil
  | shift alist 0 = alist
  | shift alist n = 
        let
          fun shiftAcc inli opli 0 = (inli @ opli)
            | shiftAcc inli opli i =
                shiftAcc inli (0000::opli) (i - 1)
        in
          shiftAcc alist [] n
        end;

fun zip f nil nil = nil
  | zip f (h::t) (i::u) = f(h, i)::zip f t u;

(* addBigInt: int list -> int list -> int list : Add two large integer lists and
 * return the resultant integer list *)
fun addBigInt nil _       = nil
  | addBigInt _ nil       = nil
  | addBigInt alist blist = 
        let
          val diff = (length alist) - (length blist)
          fun alignAcc (iolist, 0) = iolist
            | alignAcc (iolist, n) =
                alignAcc(0::iolist, n - 1)
          val alistNew = alignAcc(alist, if diff < 0 then ~diff else 0)
          val blistNew = alignAcc(blist, if diff > 0 then diff else 0)
          val intList    = zip (op +) alistNew blistNew
          val bigIntList = map toBigInt intList
          val revBigIntList = rev bigIntList
          fun getIntCarry [integer]        = (0, integer)
            | getIntCarry [carry, integer] = (carry, integer)
          fun flatten nil oplist carryPrev    = carryPrev::oplist
            | flatten (h::t) oplist carryPrev = 
                let
                  val (carry, integer) = getIntCarry(h)
                  val add = integer + carryPrev
                  val (car, res) = getIntCarry(toBigInt add)
                in
                  flatten t (res::oplist) (carry + car)
                end;
        in
          flatten revBigIntList nil 0
        end;

(* karatsuba: int list -> int list -> int list : Take two large integers in the
 * integer list form and return their multiplication in the integer list form *)
fun karatsuba nil _         = nil
  | karatsuba _ nil         = nil
  | karatsuba [num1] [num2] = toBigInt (num1 * num2)
  | karatsuba [xH, xL] [yL] =
        let
          val a        = 0
          val d        = xL * yL
          val e        = ((xH + xL) * yL) - d
          val aShifted = 0
          val eShifted = shift (toBigInt e) 1
        in
          addBigInt eShifted (toBigInt d)
        end
  | karatsuba [xL] [yH, yL] =
        let
          val a        = 0
          val d        = xL * yL
          val e        = (xL * (yH + yL)) - d
          val aShifted = 0
          val eShifted = shift (toBigInt e) 1
        in
          addBigInt eShifted (toBigInt d)
        end
  | karatsuba [xH, xL] [yH, yL] =
        let
          val a        = xH * yH
          val d        = xL * yL
          val e        = (xH + xL) * (yH + yL) - a - d
          val aShifted = shift (toBigInt a) 2
          val eShifted = shift (toBigInt e) 1
        in
          addBigInt (addBigInt aShifted eShifted) [d]
        end;
  (*| karatsuba alist blist   =
        let
          val alistN = length alist
          val blistN = length blist
          val maxN   = max(alistN, blistN)
          val halfN  = (maxN+1) div 2
        in
          if (alistN <= 2 andalso blistN <=2) then

        end;*)

(* factorial: string -> string : Convert a number given as a string to its
 * factorial also in string *)
(*fun factorial "" = ""
  | factorial str =
        let
          val intList = fromString str
          *)
