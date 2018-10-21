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
        | One of 'a
        | Many of 'a node list;;

let flatten n =
        let rec flatten_acc res n = match n with
                | [] -> res
                | One x :: t -> flatten_acc (x :: res) t
                | Many y :: t -> flatten_acc (flatten_acc res y) t
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
