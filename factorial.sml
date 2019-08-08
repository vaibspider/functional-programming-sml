(* Program to calculate the Factorial of an arbitrary length integer number given as a string, using Karatsuba's algorithm for multiplication *)

(* padZeros: string -> string
 * Pad the given string with zeros (from left side), if size of string is not a multiple of 4 *)
fun padZeros "" = ""
  | padZeros s  = 
        let
          fun padZ 0 = s
            | padZ 1 = "000"^s
            | padZ 2 = "00"^s
            | padZ 3 = "0"^s
        in
          padZ (size s mod 4)
        end;

(* toStringList: string -> string list
 * Convert a given string to a list of strings - each with exactly 4 characters *)
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

(* removeZeros: string list -> string list
 * Remove leading zeros in a list of strings; Leave it as it is, if all the elements are zeros *)
fun removeZeros nil    = nil
  | removeZeros [one]  = [one]
  | removeZeros (h::t) =
      if ((h = "0000") orelse (h = "000") orelse (h = "00") orelse (h = "0")) then removeZeros t else (h::t)

(* chartoInt: char -> int
 * Convert a digit (in character form) to integer *)
fun chartoInt ch = ord ch - ord #"0"

(* strtoInt: string -> int
 * Convert a string into an integer *)
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

(* fromString: string -> int list
 * Convert a string of digits to an integer list with each element in base 10^4 *)
fun fromString ""  = nil
  | fromString str = 
        let
          val strL = toStringList str
          val strLR = removeZeros strL
       in
          map strtoInt strLR
        end;

(* toString: int list -> string
 * Convert a list of integers into corresponding string *)
fun toString nil   = ""
  | toString alist =
        let
          val strList = map Int.toString alist
          val strListPadded = map padZeros strList
          fun toStr nil acc _ = acc
            | toStr (h::t) acc 1 = toStr t (acc ^ Int.toString(valOf(Int.fromString h))) 0
            | toStr (h::t) acc _ = toStr t (acc ^ h) 0

        in
          toStr strListPadded "" 1
        end;

(* toBigInt: int -> int list
 * Convert an integer into a list of integers form, where each integer is in base 10^4 *)
fun toBigInt num = fromString (Int.toString num)

(* shift: int list -> int -> int list
 * Take an integer list and a number, which is to be considered as a power of Base (by default 10^4). Shift the integer list by those many digits *)
fun shift nil _   = nil
  | shift alist 0 = alist
  | shift alist n = 
        let
          fun shiftAcc inli opli 0 = (inli @ opli)
            | shiftAcc inli opli i =
                shiftAcc inli (0::opli) (i - 1)
        in
          shiftAcc alist [] n
        end;

(* zip: ('a * 'b -> 'c) -> 'a list -> 'b list -> 'c list
 * A higher order function, which takes a function and two lists; It zips the corresponding elements of the lists using the provided function *)
fun zip f nil _ = nil
  | zip f _ nil = nil
  | zip f (h::t) (i::u) = f(h, i)::zip f t u;

(* truncateZeros: int list -> int list
 * Truncate leading zero elements in an integer list *)
fun truncateZeros nil    = nil
  | truncateZeros [one]  = [one]
  | truncateZeros (h::t) = if h = 0 then truncateZeros t else (h::t)

(* alignAcc: int list * int -> int list
 * Align the integer list by adding 'n' zero elements to its front *)
fun alignAcc (iolist, 0) = iolist
  | alignAcc (iolist, n) =
      alignAcc(0::iolist, n - 1)

(* getIntCarry: int list -> (int, int)
 * Get an integer list, with or without the carry with the integer and return those accordingly in the tuple *)
fun getIntCarry [integer]        = (0, integer)
  | getIntCarry [carry, integer] = (carry, integer)

(* flatten: int int list -> int list -> int -> int list
 * Convert a list of list of integers to just a list of integers (in a way, flatten them); Also pass on the carry to one to another in the process *)
fun flatten nil oplist carryPrev  = (if (carryPrev > 0) then carryPrev::oplist else oplist)
| flatten (h::t) oplist carryPrev = 
    let
      val (carry, integer) = getIntCarry(h)
      val add = integer + carryPrev
      val (car, res) = getIntCarry(toBigInt add)
    in
      flatten t (res::oplist) (carry + car)
    end;

(* addBigInt: int list -> int list -> int list
 * Add two large integer lists and return the resultant integer list *)
fun addBigInt nil _       = nil
  | addBigInt _ nil       = nil
  | addBigInt [one] [two] = truncateZeros (toBigInt (one + two))
  | addBigInt anyList [0] = truncateZeros anyList
  | addBigInt [0] anyList = truncateZeros anyList
  | addBigInt alist blist = 
        let
          val diff     = (length alist) - (length blist)
          val alistNew = alignAcc(alist, if diff < 0 then ~diff else 0)
          val blistNew = alignAcc(blist, if diff > 0 then diff else 0)
          val intList  = zip (op +) alistNew blistNew
          val bigIntList    = map toBigInt intList
          val revBigIntList = rev bigIntList
        in
          truncateZeros (flatten revBigIntList nil 0)
        end;

(* convToPostveAcc: int list * int list * int -> int list
 * Convert an integer list possibly with some negative elements to a completely non-negative integer list *)
fun convToPostveAcc(nil, oplist, borrow)    = oplist (* borrow not used here *)
  | convToPostveAcc((h::t), oplist, borrow) =
      convToPostveAcc(t, (if h - borrow < 0 then 10000 + (h - borrow) else h - borrow)::oplist, 
                              (if h - borrow < 0 then 1 else 0))

(* subBigInt: int list -> int list -> int list
 * Subtract two large integer lists and return the resultant integer list *)
fun subBigInt nil _       = nil
  | subBigInt _ nil       = nil
  | subBigInt anyList [0] = anyList
  | subBigInt [one] [two] = [one - two]
  | subBigInt alist blist =
        let
          val diff     = (length alist) - (length blist)
          val alistNew = alignAcc(alist, if diff < 0 then ~diff else 0)
          val blistNew = alignAcc(blist, if diff > 0 then diff else 0)
          val intList  = zip (op -) alistNew blistNew
          val revIntList = rev intList
        in
          convToPostveAcc(revIntList, nil, 0)
        end;

(* takeFirstM: 'a list * int -> 'a list
 * Return the first M elements in a list of elements with arbitrary type *)
fun takeFirstM(nil, m) = nil
  | takeFirstM(li, m)  = 
      let
        fun takeFirstNAcc(ipli, 0, oplist)   = oplist
          | takeFirstNAcc((h::t), n, oplist) =
              takeFirstNAcc(t, n - 1, oplist @ [h])
      in
        takeFirstNAcc(li, m, nil)
      end;

(* dropFirstM: 'a list * int -> 'a list
 * Return the input list with its first M elements dropped off *)
fun dropFirstM(nil, m)    = nil
  | dropFirstM(li, 0)     = li
  | dropFirstM((h::t), m) =
      dropFirstM(t, m - 1)

(* karatsuba: int list -> int list -> int list
 * Take two large integers in the integer list form and return their multiplication in the integer list form *)
fun karatsuba nil _         = nil
  | karatsuba _ nil         = nil
  | karatsuba [1] any       = any
  | karatsuba any [1]       = any
  | karatsuba [0] _         = [0]
  | karatsuba _  [0]        = [0]
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
          addBigInt (addBigInt aShifted eShifted) (toBigInt d)
        end
  | karatsuba alist blist =
        let
          val diff     = length alist - length blist
          val maxN     = Int.max(length alist, length blist)
          val halfN    = (maxN + 1) div 2
          val alistNew = alignAcc(alist, if diff < 0 then ~diff else 0)
          val blistNew = alignAcc(blist, if diff > 0 then diff else 0)
          val xH = List.take(alistNew, maxN - halfN)
          val xL = List.drop(alistNew, maxN - halfN)
          val yH = List.take(blistNew, maxN - halfN)
          val yL = List.drop(blistNew, maxN - halfN)
          val a  = karatsuba xH yH
          val d  = karatsuba xL yL
          val e  = subBigInt (subBigInt (karatsuba (addBigInt xH xL) (addBigInt yH yL)) a) d
          val aShifted = shift a (2*halfN)
          val eShifted = shift e halfN
        in
          addBigInt (addBigInt aShifted eShifted) d
        end;

(* fact: int list -> int list
 * Calculate the factorial of a number represented as an integer list *)
fun fact nil = nil
  | fact [0] = [1]
  | fact num =
        let
          fun factFrom1(start, acc) = if (start = num) then karatsuba start acc else
            factFrom1(addBigInt start [1], karatsuba start acc)
        in
          factFrom1([1], [1])
        end;

(* isValid: string -> bool 
 * Check if a string is valid; It should only contain numbers from 0 to 9 and nothing else *)
fun isValid str =
        let
          val chars = explode str
          fun scanChars [one]  = if chartoInt one >= 0 andalso chartoInt one <= 9
                                 then true else false
            | scanChars (h::t) =
            if chartoInt h >= 0 andalso chartoInt h <= 9 then scanChars t else false
        in
          scanChars chars
        end;

(* factorial: string -> string
 * Convert a number given as a string to its factorial also in string *)
fun factorial "" = ""
  | factorial str =
        let
          exception Invalid_Input_exception of string
          val valid = isValid str
          fun checkIfValid vald = if not vald then raise Invalid_Input_exception("Invalid input! Please specify correct number!\n") else true
          val temp = checkIfValid valid
          val intList = fromString str
          val res = fact intList
        in
          toString res
        end;
