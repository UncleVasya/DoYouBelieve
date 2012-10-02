(* #load "str.cma";; *)  (* regexp *)
open Unix;; (* time detection *)
open Printf;;  

type card = card_rank * card_suit
and card_rank = Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
and card_suit = Heart | Club | Spade | Diamond;;

type gameState =
{
	hands : card list list;
	table : card list list;
	claim : card_rank option;
	active_hand : int;
	finished_hands : int list;
};;

type move = Check | New_move of card list * card_rank | Adding_move of card list;;

type player =
{
	name : string;
	nature : playerNature;
}
and playerNature = Human | Computer;;

let string_of_card_rank = function
	| Six -> "6" | Seven -> "7" | Eight -> "8" | Nine -> "9" | Ten -> "T"
	| Jack -> "J" | Queen -> "Q" | King -> "K" | Ace -> "A"
	
let card_rank_of_string = function
	| "6" -> Six | "7" -> Seven | "8" -> Eight | "9" -> Nine | "T" -> Ten
	| "J" -> Jack | "Q" -> Queen | "K" -> King | "A" -> Ace 
	| _ -> failwith "card_rank_of_string: wrong string"

let string_of_card_suit = function Heart -> "H" | Club -> "C" | Spade -> "S" | Diamond -> "D"

let card_suit_of_string = function "H" -> Heart | "C" -> Club | "S" -> Spade | "D" -> Diamond 
									| _ -> failwith "card_suit_of_string: wrong string"

let card_of_string s : card = 
	if String.length s = 2 then 
		let rank_s, suit_s = String.sub s 0 1, String.sub s 1 1 in 
		card_rank_of_string rank_s, card_suit_of_string suit_s
	else failwith "card_of_string: two symbols, dude"
	
let string_of_card (rank,suit) = string_of_card_rank rank ^ string_of_card_suit suit
let string_of_card_list = List.fold_left (fun s x -> s ^ "  " ^ string_of_card x) ""

let state_show state =
	printf "\n\n------STATE START------\n\n";
	printf "Hands: \n\n";
	ignore(List.fold_left (fun s x -> printf "%d.  %s \n" s (string_of_card_list x); s+1) 0 state.hands);
	
	printf "\nTable:  ";
	List.iter (fun x -> printf "[ %s ]  " (string_of_card_list x)) state.table;
	
	printf "\n\nClaim: %s" (match state.claim with None -> "None" | Some c -> string_of_card_rank c);
	printf "\nActive hand: %d" state.active_hand;
	
	printf "\n\nFinished_hands: "; 
	printf " [ %s ] \n" (List.fold_left (fun s x -> s ^ "  " ^ string_of_int x) "" state.finished_hands);
	printf "\n\n------STATE END------\n\n"

let get_players =
	[
		{name = "Player1"; nature = Human};
		{name = "Player2"; nature = Human};
		{name = "Player3"; nature = Human};
		{name = "Player4"; nature = Human}
	]

let create_deck ranks suits : card list = 
	List.concat (List.map (fun s -> List.map (fun n -> n,s) ranks) suits)
	
let shuffle_list lst =
	let rec shuffle_list acc = function
	| [] -> acc
	| lst -> 
		let n = (Random.int (List.length lst)) in
		let acc = (List.nth lst n) :: acc in
		let lst = remove_nth lst n in
		shuffle_list acc lst
		
	and remove_nth lst n =
		let rec remove_nth acc i = function
		| [] -> List.rev acc 
		| h::t ->
			if i = 0 then (List.rev acc) @ t
			else remove_nth (h::acc) (i-1) t
		in remove_nth [] n lst
	
	in shuffle_list [] lst
	
	
let divide_list lst n =
	let part_size = (List.length lst) / n in 
	let rec divide_list acc = function
	| [] -> acc
	| lst -> 
		let lst1, lst2 = list_split lst part_size in
		divide_list (lst1::acc) lst2
	
	and list_split lst n = 
		let rec list_split acc i = function
		| [] -> List.rev acc, []
		| h::t as lst -> 
			if i = 0 then List.rev acc, lst
			else list_split (h::acc) (i-1) t
		in list_split [] n lst
	
	in divide_list [] lst

	
(* TODO: sometimes 4 cards of the same rank may be in on hand. Check for this and shuffle again if needed *)
let create_state player_count = 
	let ranks = [Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace] in
	let suits = [Heart; Club; Spade; Diamond] in
	let deck = shuffle_list (create_deck ranks suits) in 
	let hands = divide_list deck player_count in
	{hands = hands; table = []; claim = None; active_hand = 0; finished_hands = []}
	
let play_game state players =	
	let rec play_game state =
		state_show state;
		if end_game state then List.rev state.finished_hands
		else 
			let new_state = 
				try  
					let move = get_move (info_to_player state) in
					do_move state move
				with Failure s -> printf "\n%s \n\n" s; state
			in play_game new_state
	
	and end_game state = 
		let hands_in_game = List.fold_left (fun s x -> if x = [] then s else s+1) 0 state.hands in
		hands_in_game < 2
	
	and info_to_player state = 
		(* 
			HINT: ask mcstar if he always writes tail-recursive functions? Is it a Greater Good?
			Because   List.map (fun x -> List.length x) state.hands  is prettier than the current one
			and I know that this function dealing with really small lists so there's no point
			in using tail-recursion except for Greater Good. Anyway, consult mcstar.
		*)
		let list_of_len = List.rev_map List.length in
		let hand_sizes = List.rev (list_of_len state.hands) in
		let prev_moves = List.rev (list_of_len state.table) in
		let claim = state.claim in
		let player_cards = List.nth state.hands state.active_hand in
		let player_num = state.active_hand in
		(* 
			TODO: 
			1. When you'll refactor project into different files, make a type (record) for this thing 
			2. Instead of giving player_num you can give hand_sizes in order that player at right
			   of this one is the first and player at left is the last one. Current player excluded 
			   from list.
		*)
		(hand_sizes, prev_moves, claim, player_cards, player_num) 		
	
	and get_move = function hand_sizes, prev_moves, claim, player_cards, player_num as info_to_player ->
		info_to_player_show info_to_player;
		let line = String.uppercase (ask_for_input ("Enter cards you wanna put on the table \n" ^ 
													 "(example: 7C AS 9D) or C for check: \n\n"))
		in 
			if line = "C" then Check
			else 
				let str_to_words = Str.split (Str.regexp " ") in 
				let cards = List.map card_of_string (str_to_words line) in
					if claim = None then 
						New_move (cards, 
								card_rank_of_string (String.capitalize (ask_for_input "Claim these cards as (example: 8): ") ))
					else Adding_move (cards)
					
		
		and ask_for_input s = print_string s; read_line ()
		
		and info_to_player_show = function hand_sizes, prev_moves, claim, player_cards, player_num ->
			printf "\n\n------INFO TO PLAYER START------\n\n";
			printf "Hand sizes: \n\n";
			ignore (List.fold_left (fun s x -> printf "%d.   %d \n" s x; s+1) 0 hand_sizes);
	
			printf "\nPrevious moves:  ";
			printf " [ %s ] \n" (List.fold_left (fun s x -> s ^ "  " ^ string_of_int x) "" prev_moves);
	
			printf "\n\nClaim: %s" (match claim with None -> "None" | Some c -> string_of_card_rank c);
			printf "\nPlayer number: %d" player_num;
	
			printf "\n\nPlayer cards:  %s" (string_of_card_list player_cards); 
			printf "\n\n------INFO TO PLAYER END------\n\n"
		
	(* TODO: shit-code detected! Refactor ASAP *)
	(* HINT: try references *)
	(* TODO: I must provide to all players info about cards in final move *)
	and do_move state move =
		let hand = (List.nth state.hands state.active_hand) in
		match state.claim, move with
		| _, New_move (_, Ace) ->  failwith "do_move: Ace cannot be a claim."
		| None, New_move (cards, claim) | Some claim, Adding_move(cards) -> 
			let hand, table = hand_cards_to_table hand cards state.table in					
			let finished = hand_finished hand cards claim in
				let hands = 
					let hands = list_replace_nth state.hands state.active_hand hand in
					if finished then
						let next_hand_num = next_hand_num state.active_hand state.hands in
						let next_hand = List.nth state.hands next_hand_num in
						let next_hand = table_to_hand table next_hand in
						list_replace_nth hands next_hand_num next_hand
					else hands
				in
				{
					hands = hands; 
					table = if finished then [] else table; 
					claim = if finished then None else Some claim;
					active_hand = if finished then next_hand_num (next_hand_num state.active_hand state.hands) state.hands
								  else next_hand_num state.active_hand state.hands;
					finished_hands = if finished then state.active_hand :: state.finished_hands 
									 else state.finished_hands
				}
		| Some claim, Check -> 
			if check_cards claim (List.hd state.table) then
				{state with hands = list_replace_nth state.hands state.active_hand (table_to_hand state.table hand);
							active_hand = next_hand_num state.active_hand state.hands;
							table = [];
							claim = None}
		    else 
				let hand_num = prev_hand_num state.active_hand state.hands in
				let hand = List.nth state.hands hand_num in
					{state with hands = list_replace_nth state.hands hand_num (table_to_hand state.table hand);
								table = [];
								claim = None}
					
		| _,_ -> failwith "do_move: Invalid move"
		
		and hand_finished hand moved_cards claim = match is_empty hand, check_cards claim moved_cards with
			| true, true -> true
			| true, false -> failwith "hand_finished: you cannot lie in your last move"
			| _ -> false
		
		and is_empty = function [] -> true | _ -> false
		and check_cards claim = List.for_all (fun (rank, suit) -> rank = claim) 
	
		and hand_cards_to_table hand cards table = list_remove_sublist hand cards, cards::table
		
		and table_to_hand table hand = 
			let hand = hand @ List.concat table in
			remove_gathered_ranks hand

		and remove_gathered_ranks hand = 
			List.filter (fun (rank,suit) -> list_item_count (fun (r,s) -> r = rank & r <> Ace) hand < 4) hand
		
		and list_item_count f = List.fold_left (fun s x -> if f x then s+1 else s) 0
		
		and list_remove_sublist lst slst = 
			let removed, new_lst = List.partition (fun x -> List.mem x slst) lst in
				if List.length removed = List.length slst then new_lst 
				else failwith "list_remove_sublist: this is not a sublist, it has unique elements" 
		
		and list_replace_nth lst n item = 
			let rec list_replace_nth acc n = function
			| [] -> failwith "list_replace_nth" 
			| h::t -> if n = 0 then (List.rev acc) @ (item::t) else list_replace_nth (h::acc) (n-1) t
			in list_replace_nth [] n lst
	
		(* TODO: optimize, add some checks *)
		and next_hand_num n = function 
			| [] -> failwith "next_hand_num"
			| hands ->
				let next_n = if n = (List.length hands) - 1 then 0 else n + 1 in
				if List.nth hands next_n = [] then next_hand_num next_n hands else next_n
	
		(* TODO: optimize, add some checks *)	
		and prev_hand_num n = function
			| [] -> failwith "prev_hand_num"
			| hands ->
				let prev_n = if n = 0 then (List.length hands) - 1 else n - 1 in
				if List.nth hands prev_n = [] then prev_hand_num prev_n hands else prev_n

	in play_game state
;;

(* #trace play_game;; *)

let main  =
	(* Random.self_init(); *)
	let players = get_players in
	let player_count = List.length players in
	let state = create_state player_count in
	play_game state players;
	
	(* let list_item_count f = List.fold_left (fun s x -> if f x then s+1 else s) 0 in
	
	let remove_gathered_ranks hand = 
			List.filter (fun (rank,suit) -> list_item_count (fun (r,s) -> r = rank) hand < 4) hand in
		
	let line = "7C 8H QD 6D QS AH KC JH 9H 8C 8D 8S" in
	let str_to_words = Str.split (Str.regexp " ") in 
	let cards = List.map card_of_string (str_to_words line) in
	remove_gathered_ranks cards *)
;;