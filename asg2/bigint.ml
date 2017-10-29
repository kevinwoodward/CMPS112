(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf

module Bigint = struct

    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list
    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let charlist_of_string str =
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
        let len = strlen str
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)

    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        | value -> let reversed = reverse value
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))

    let rec sanitize' (Bigint (sign, value)) =
        match value with
        | 0::cdr       -> sanitize' (Bigint (sign, cdr))
        | _              -> (Bigint (sign, reverse value))

    let sanitize (Bigint (sign, value)) =
        sanitize' (Bigint (sign, reverse value))

    let rec sanitize_list' value =
        match value with
        | 0::cdr         -> sanitize_list' cdr
        | _              -> reverse value

    let sanitize_list value =
        sanitize_list' (reverse value)

    let rec cmp' list1 list2 carry = match (list1, list2) with
        | [], []                        -> 0
        | list1, []                     -> 1
        | [], list2                     -> (-1)
        | car1::[], car2::[]            ->
            if car1 > car2 then 1
            else if car1 < car2 then (-1)
            else carry
        | car1::cdr1, car2::cdr2        ->
            if car1 > car2 then cmp' cdr1 cdr2 1
            else if car1 < car2 then cmp' cdr1 cdr2 (-1)
            else cmp' cdr1 cdr2 carry

    let cmp list1 list2 =
        cmp' (sanitize_list list1) (sanitize_list list2) 0

    let cmp_bigint (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        cmp' (sanitize_list value1) (sanitize_list value2) 0

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> sub' list1 [carry] 0
        | [], list2, carry   -> sub' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let diff = car1 - car2 - carry
          in (if diff < 0 then (diff + radix) else diff) :: sub' cdr1 cdr2 ((if diff < 0 then 1 else 0))

    let double list1 =
        add' list1 list1 0

    let rec mul' (multiplier, powerof2, multiplicand') =
    match cmp powerof2 multiplier with
     | 1        -> multiplier, []
     | _        -> (let remainder, product =
                   mul' (multiplier, double powerof2, double multiplicand')
                   in (match cmp remainder powerof2 with
                        | (-1)      -> remainder, product
                        | _         -> sanitize_list (sub' remainder powerof2 0), add' product multiplicand' 0))


    let rec divrem' (dividend, powerof2, divisor') =
        match (cmp divisor' dividend) with
         | 1        -> [], dividend
         | _        -> (let quotient, remainder =
                            divrem' (dividend, double powerof2, double divisor')
                            in (match (cmp divisor' remainder) with
                                 | 1        -> quotient, remainder
                                 | _        -> (add' quotient powerof2 0), sanitize_list (sub' remainder divisor' 0)))

    let rec pow' (exp, base, result) =
    let exp_by_2, is_even = divrem' (exp, [1], [2]) in
    match (exp, is_even) with
     | [], _                   -> result
     | _, []                   -> let _, base_squared = (mul' (base, [1], base)) in
                                        pow' (exp_by_2, base_squared, result)
     | _, _                     -> let _, product = (mul' (result, [1], base)) in
                                    pow' (sanitize_list (sub' exp [1] 0), base, product)

    let add (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
    match (sign1, sign2) with
        | Pos, Pos              -> Bigint (Pos, add' value1 value2 0)
        | Neg, Neg              -> Bigint (Neg, add' value1 value2 0)
        | Pos, Neg              ->
           (match (cmp value1 value2) with
            | 1         -> Bigint (Pos, sub' value1 value2 0)
            | (-1)      -> Bigint (Neg, sub' value2 value1 0)
            | _         -> zero)
        | Neg, Pos              ->
           (match (cmp value1 value2) with
            | 1         -> Bigint (Neg, sub' value1 value2 0)
            | (-1)      -> Bigint (Pos, sub' value2 value1 0)
            | _         -> zero)

    let sub (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
    match (sign1, sign2) with
        | Pos, Pos              ->
            (match (cmp value1 value2) with
             | 1        -> Bigint (Pos, sub' value1 value2 0)
             | (-1)     -> Bigint (Neg, sub' value2 value1 0)
             | _        -> zero)
        | Neg, Neg              ->
            (match (cmp value1 value2) with
             | 1        -> Bigint (Neg, sub' value1 value2 0)
             | (-1)     -> Bigint (Pos, sub' value2 value1 0)
             | _        -> zero)
        | Pos, Neg              -> Bigint (Pos, add' value1 value2 0)
        | Neg, Pos              -> Bigint (Neg, add' value1 value2 0)

    let mul (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        let _, product =
            mul' (value1, [1], value2) in
                if sign1 = sign2
                then Bigint (Pos, product)
                else Bigint (Neg, product)

    let divrem (dividend, divisor') =
        divrem' (dividend, [1], divisor')

    let div (Bigint (sign1, dividend)) (Bigint (sign2, divisor)) =
        let quotient, _ = divrem (dividend, divisor) in
            if sign1 = sign2
            then Bigint (Pos, quotient)
            else Bigint (Neg, quotient)

    let rem (Bigint (sign1, dividend)) (Bigint (sign2, divisor)) =
        let _, remainder = divrem (dividend, divisor) in
            if sign1 = sign2
            then Bigint (Pos, remainder)
            else Bigint (Neg, remainder)

    let pow (Bigint (sign1, value1)) (Bigint (sign2, value2)) =
        let result_value = pow' (value2, value1, [1]) in
            match (sign1, value1, sign2, value2) with
             | _, _, Pos, [0]   -> Bigint (Pos, [1])
             | _, _, Neg, _     -> zero
             | _, [0], _, _     -> zero
             | Pos, _, Pos, _   -> Bigint (Pos, result_value)
             | Neg, _, Pos, _   -> (let _, result_sign = divrem (value2, [2]) in
                                       match result_sign with
                                        | [0]     -> Bigint (Pos, result_value)
                                        | [1]     -> Bigint (Neg, result_value)
                                        | _       -> Bigint (Neg, [-69]))


end
