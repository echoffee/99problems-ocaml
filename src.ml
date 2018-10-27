(* 1 *)
let rec last l = match l with
        | [] -> None
        | [x] -> Some x
        | x :: t -> last t;;

(* 2 *)
let rec last_two l = match l with
        | [] -> None
        | [x] -> None
        | [x; y] -> Some (x, y)
        | x :: t -> last_two t;;

(* 3 *)
let rec at i l = if l = [] then None else match i with
        | 1 -> Some (List.hd l)
        | _ -> at (i - 1) (List.tl l);;

(* 4 *)
let length l = 
        let rec len_acc l i = match l with
                | [] -> i
                | _ :: t -> len_acc t (i + 1)
        in len_acc l 0;;

(* 5 *)
let rev l =
        let rec rev_acc acc l = match l with 
                | [] -> acc
                | h :: t -> rev_acc (h :: acc) t
        in rev_acc [] l;;

(* 6 *)
let rec is_palindrome l = match l with
        | [] -> true
        | [x] -> true
        | [x; y] -> x = y
        | x :: t -> let rl = rev t in
                if x = List.hd rl then
                        is_palindrome (rev (List.tl rl))
                else
                        false;;

(* 6 but not retarded *)
let is_palindrome2 l = l = rev l;;

(* 7 *)
type 'a node =
        | Single of 'a
        | Multiple of 'a node list;;

let flatten n =
        let rec flatten_acc res n = match n with
                | [] -> res
                | Single x :: t -> flatten_acc (x :: res) t
                | Multiple y :: t -> flatten_acc (flatten_acc res y) t
        in rev (flatten_acc [] n);;

(* 8 *)
let compress l =
        let rec compress_acc res l = match l, res with
                | [], _ -> res
                | x :: t, [] -> compress_acc (x :: res) t
                | x :: t, y :: _ -> if x = y    then compress_acc res t
                                                else compress_acc (x :: res) t
        in rev (compress_acc [] l);;

(* 9 *)
let pack l = 
        let rec pack_acc res l = match l, res with
                | [], _ -> res
                | x :: t, [] -> pack_acc ([x] :: res) t
                | x :: t, y :: _ -> if x = (List.hd y)
                        then let nsres = x :: (List.hd res) in
                                pack_acc (nsres :: (List.tl res)) t
                        else pack_acc ([x] :: res) t
        in rev (pack_acc [] l);;

(* 10 *)
let encode l =
        let rec encode_acc res l = match l, res with
                | [], _ -> res
                | x :: t, [] -> encode_acc ((1, x) :: res) t
                | x :: t, y :: _ -> if x = (snd y)
                        then let nsres = ((fst y) + 1, x) in
                                encode_acc (nsres :: (List.tl res)) t
                        else encode_acc ((1, x) :: res) t
        in rev (encode_acc [] l);;

(* 11 *)
type 'a rle = 
        | Single of 'a
        | Multiple of int * 'a;;

let encode2 l =
        let rec encode_acc res l = match l, res with
                | [], _ -> res
                | x :: t, [] -> encode_acc ((Single x) :: res) t
                | x :: t, y :: ty -> match y with
                        | Single z -> if x = z then encode_acc (Multiple (2, z) :: ty) t
                                else encode_acc (Single (x) :: res) t
                        | Multiple (i, z) -> if x = z then encode_acc (Multiple (i + 1, z) :: ty) t
                                else encode_acc (Single (x) :: res) t
        in rev(encode_acc [] l);;

(* 12 *)
let decode l =
        let rec decode_acc res l = match l with
                | [] -> res
                | Single x :: t -> decode_acc (x :: res) t
                | Multiple (i, x) :: t -> if i > 2 then decode_acc (x :: res) ((Multiple ((i - 1), x)) :: t)
                        else decode_acc (x :: res) ((Single x) :: t)
                in rev(decode_acc [] l);;

(* 13 *)
(* Same as 10 I think ... *)

(* 14 *)
let duplicate l =
        let rec duplicate_acc res l = match l with
                | [] -> res
                | x :: t -> duplicate_acc (x :: (x :: res)) t
        in rev(duplicate_acc [] l);;

(* 15 *)
let replicate l n =
        let rec sub_rep res x n = if n = 0 then res else sub_rep (x :: res) x (n - 1)
        in
        let rec replicate_acc res l = match l with
                | [] -> res
                | x :: t -> replicate_acc (sub_rep res x n) t
        in rev (replicate_acc [] l);;

(* 16 *)
let drop l n =
        let rec drop_acc res l i = match l with
                | [] -> res
                | x :: t -> if i = 1 then drop_acc res t n else drop_acc (x :: res) t (i - 1)
        in rev (drop_acc [] l n);;

(* 17 *)
let split l n =
        let rec split_acc l1 l2 i = if i = 0 then (rev l1, l2) else split_acc ((List.hd l2) :: l1) (List.tl l2) (i - 1) in split_acc [] l n;;

(* 18 *)
let skip l n = 
        let rec skip_acc l i = if i = 0 then l else skip_acc (List.tl l) (i - 1) in skip_acc l n;;

let forget l n =
        let len = length l in 
        let rec forget_acc res l i = if i > 0 then forget_acc ((List.hd l) :: res) (List.tl l) (i - 1) 
                                        else res
        in rev (forget_acc [] l (len - n));;

let slice l i k = forget (skip l i) (k - i - 1);;

(*
slice [1; 2; 3; 4; 5; 6] 2 4;; (* = [3; 4; 5] *)
*)

(* 19 *)
let concatenate l1 l2 =
        let rec concatenate_acc res l = match l with
                | [] -> res
                | x :: t -> concatenate_acc (x :: res) t
        in concatenate_acc l2 (concatenate_acc [] l1);;

let rotate l n =
        let len = length l in
        let no = (len + n) mod len in
        let ls = split l no in
        concatenate (snd ls) (fst ls);;

(*
rotate [1; 2; 3; 4; 5; 6; 7; 8] 3;; (* = [4; 5; 6; 7; 8; 1; 2; 3] *)
*)
(*
rotate [1; 2; 3; 4; 5; 6; 7; 8] (-2);; (* = [7; 8; 1; 2; 3; 4; 5; 6] *)
*)

(* 20 *)
let remove_at i l =
        let rec remove_at_acc res i l =
                if i = 0 then concatenate (rev res) (List.tl l) else remove_at_acc ((List.hd l) :: res) (i - 1) (List.tl l) 
        in remove_at_acc [] i l;;
