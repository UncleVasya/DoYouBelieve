(* #load "str.cma";; *)  (* regexp *)
open Unix;; (* time detection *)
open Printf;;

type card = card_rank * card_suit
and card_rank = Six | Seven | Eight | Nine | Ten | Jack | Queen | King | Ace
and card_suit = Heart | Club | Spade | Diamond;;

type gameState =
{
	mutable hands : card list list;
	mutable table : card list list;
	mutable claim : card_rank option;
	mutable active_hand : int;
	mutable finished_hands : int list;
};;

type move = Check | New_move of card list * card_rank | Adding_move of card list;;

type player =
{
	name : string;
	nature : player_nature;
}
and player_nature = Human | Computer;;

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

let string_of_player_nature = function Human -> "Human" | Computer -> "Computer"

let state_show state =
	printf "\n\n------STATE START------\n\n";
	printf "Hands: \n\n";
	ignore(List.fold_left (fun s x -> printf "%d.  %s \n" s (string_of_card_list x); s+1) 1 state.hands);
	
	printf "\nTable:  ";
	List.iter (fun x -> printf "[ %s ]  " (string_of_card_list x)) state.table;
	
	printf "\n\nClaim: %s" (match state.claim with None -> "None" | Some c -> string_of_card_rank c);
	printf "\nActive hand: %d" (state.active_hand + 1);
	
	printf "\n\nFinished_hands: "; 
	printf " [ %s ] \n" (List.fold_left (fun s x -> s ^ "  " ^ string_of_int (x+1)) "" state.finished_hands);
	printf "\n\n------STATE END------\n\n"

let get_players =
	[
		{name = "Player 1"; nature = Computer};
		{name = "Player 2"; nature = Computer};
		{name = "Player 3"; nature = Computer};
		{name = "Player 4"; nature = Computer}
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

let list_split lst n = 
	let rec list_split acc i = function
	| [] -> List.rev acc, []
	| h::t as lst -> 
		if i = 0 then List.rev acc, lst
		else list_split (h::acc) (i-1) t
	in list_split [] n lst	
	
let divide_list lst n =
	let part_size = (List.length lst) / n in 
	let rec divide_list acc = function
	| [] -> acc
	| lst -> 
		let lst1, lst2 = list_split lst part_size in
		divide_list (lst1::acc) lst2
	in divide_list [] lst

	
(* TODO: sometimes 4 cards of the same rank may appear in one hand. Check for this and shuffle again if needed *)
let create_state player_count = 
	let ranks = [Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace] in
	let suits = [Heart; Club; Spade; Diamond] in
	let deck = shuffle_list (create_deck ranks suits) in 
	let hands = divide_list deck player_count in
	{hands = hands; table = []; claim = None; active_hand = 0; finished_hands = []}
	
let play_game state players =	
	let rec play_game state =
		(* state_show state; *)
		if end_game state then List.rev state.finished_hands
		else 
			let new_state = 
				try  
					let move = get_move (List.nth players state.active_hand) (info_to_player state) in
					let new_state, move_log = do_move state move in
					printf "\n%s" move_log;
					new_state
				with Failure s ->
						printf "\n%s \n\n" s; 
						state
			in play_game new_state
	
	and end_game state = 
		List.length state.hands - List.length state.finished_hands < 2
		or 
		List.for_all (fun (rank,_) -> rank = Ace) (List.concat state.hands @ List.concat state.table)
	
	and info_to_player state = 
		(* HINT: You can rewrite it to not tail-recursive. That's ok.*)
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
	
	and get_move player info_to_player = match player.nature with
		| Human -> human_get_move info_to_player
		| Computer -> computer_get_move info_to_player
		(* | Computer of Easy_AI  etc. *)
	
	and human_get_move (hand_sizes, prev_moves, claim, player_cards, player_num as info_to_player) =
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
								card_rank_of_string (String.uppercase (ask_for_input "Claim these cards as (example: 8): ") ))
					else Adding_move (cards)
	
		and ask_for_input s = print_string s; read_line ()
		
		and info_to_player_show = function hand_sizes, prev_moves, claim, player_cards, player_num ->
			(* printf "\n\n------INFO TO PLAYER START------\n\n"; *)
			printf "\n\n--------------------------------------------\n\n";
			printf "Hand sizes: \n\n";
			ignore (List.fold_left (fun s x -> printf "%d.   %d \n" s x; s+1) 0 hand_sizes);
	
			printf "\nPrevious moves:  ";
			printf " [ %s ] \n" (List.fold_left (fun s x -> s ^ "  " ^ string_of_int x) "" prev_moves);
	
			printf "\nClaim: %s" (match claim with None -> "None" | Some c -> string_of_card_rank c);
			printf "\nPlayer number: %d" player_num;
	
			printf "\n\nPlayer cards:  %s\n\n" (string_of_card_list player_cards); 
			(* printf "\n\n------INFO TO PLAYER END------\n\n" *)
	
	(* TODO: Another bad code. Refactor.  Yep, I'm still hoping you'll do it. *)
	and computer_get_move (hand_sizes, prev_moves, claim, hand, player_num) =
		let rnd = Random.int 100 in
		let truth, lie = true, false in
			
		let cards = cards_by_priority hand in
		let aces, normal_cards = List.partition (fun (rank,suit) -> rank = Ace) cards in
		
		if is_empty aces & can_finish normal_cards claim then finish normal_cards claim
		else
			let pick_cards cards claim say_truth =
				if say_truth then 
					let cards = List.filter (fun (rank,suit) -> rank = claim) cards in
					let num = 1 + Random.int (List.length cards) in
					let cards, _ = list_split cards num in 
					cards
				else
					let num = 1 + Random.int 3 in
					let cards, _ = list_split cards num in 
					cards
			in
			match claim with 
			| None -> 
				let claim = match List.rev normal_cards with 
					| [] -> Seven (* TODO: make random here: list_conditional_random (fun x -> x <> Ace) all_ranks *)
					| (rank,_)::_ -> rank
				in
				let chance_of_truth = 60 - 6 * List.length aces in
				let can_say_truth = not (is_empty normal_cards) in
				if can_say_truth & rnd < chance_of_truth 
					then New_move (pick_cards normal_cards claim truth, claim)
					else New_move (pick_cards (aces @ normal_cards) claim lie, claim)
			| Some claim ->
				let claim_cards = List.filter (fun (rank,suit) -> rank = claim) normal_cards in
				let prev_move = List.hd prev_moves in
				let prev_move_possible = prev_move <= 4 - List.length claim_cards in 
				let check_or_lose = List.fold_left (fun s x -> if x = 0 then s else s+1) 0 hand_sizes < 2 in
					
				if not prev_move_possible or check_or_lose then Check
				else
					let can_say_truth = List.exists (fun (rank,suit) -> rank = claim) normal_cards in
					if can_say_truth then 
						(* 10% check, 70% truth, 20% lie *)
						if rnd < 10 then Check 
						else if rnd > 30 then Adding_move (pick_cards normal_cards claim truth) 
						else Adding_move (pick_cards (aces @ normal_cards) claim lie)
					else 
						(* 60% check, 40% lie *)
						if rnd < 60 then Check else Adding_move (pick_cards (aces @ normal_cards) claim lie)
			
		and can_finish hand claim = match claim, hand with
		| None, (claim,_)::_ | Some claim, _ -> check_cards claim hand
		
		and finish hand claim = match claim, hand with
		| None, (rank,suit)::_ -> New_move (hand, rank)
		| Some claim, _ -> Adding_move (hand)
		
		(* and pick_claim hand =  *)
		
		and cards_by_priority hand = 
			let cards = cards_by_rank hand in
			let cards = List.sort (fun x y -> List.length x - List.length y) cards in
			List.concat cards
		
		and cards_by_rank hand = 
			let rec cards_by_rank cards hand = match hand with
			| [] -> cards
			| (rank,suit)::t -> 
				let same_rank, other_cards = List.partition (fun (r,s) -> r = rank) hand in
				cards_by_rank (same_rank::cards) other_cards
			in cards_by_rank [] hand
		
	(* TODO: shit-code detected! Refactor ASAP *)
	(* upd 04.10.2012: CODE BECAME VERY UGLY. DO NOT SHOW ANYONE! *)
	(* HINT: try references *)
	(* TODO: I must provide to all players info about cards in final move. And info about checked cards *)
	and do_move state move =
		let hand = ref (List.nth state.hands state.active_hand) in
		let prev_hand_n = prev_hand_num state.active_hand state.finished_hands in
		let prev_hand = ref (List.nth state.hands prev_hand_n) in
		
		let player = List.nth players state.active_hand in
		let prev_player = List.nth players prev_hand_n in
		
		let new_state = {state with table = state.table} in
		let move_log = ref (sprintf "%s (%s):  " player.name (string_of_player_nature player.nature)) in
		
		(
			match state.claim, move with
			| _, New_move (_, Ace) ->  failwith "do_move: Ace cannot be a claim."
			| None, New_move (cards, claim) | Some claim, Adding_move(cards) ->
				let hnd, tbl = hand_cards_to_table !hand cards new_state.table in
				hand := hnd; 
				new_state.table <- tbl;
				new_state.hands <- list_replace_nth new_state.hands new_state.active_hand !hand;
				new_state.claim <- Some claim ;
				new_state.active_hand <- next_hand_num state.active_hand state.finished_hands;
				
				move_log := !move_log ^
					(
						match move with
						| New_move (cards, claim) -> sprintf "%d cards of rank %s" (List.length cards) (string_of_card_rank claim)
						| Adding_move (cards) -> sprintf "%d more of rank %s" (List.length cards) (string_of_card_rank claim)
					)
			
			| Some claim, Check -> 
				let check_passed = check_cards claim (List.hd state.table) in
				if check_passed then 
				(
					new_state.hands <- list_replace_nth state.hands state.active_hand (table_to_hand state.table !hand);
					new_state.active_hand <- next_hand_num state.active_hand state.finished_hands;
				)
				else 
				(
					prev_hand := table_to_hand state.table !prev_hand;
					new_state.hands <- list_replace_nth state.hands prev_hand_n !prev_hand
				);
				new_state.table <- [];
				new_state.claim <- None;
				
				move_log := !move_log ^
					sprintf "Check \nCards: %s   Claim: %s. \n%s (%s) said %s. \n%s takes table.\n\n" 
							(string_of_card_list (List.hd state.table))
							(string_of_card_rank claim)											
							prev_player.name 
							(string_of_player_nature prev_player.nature)  
							(if check_passed then "truth" else "a lie")
							(if check_passed then player.name else prev_player.name)

			| _,_ -> failwith "do_move: Invalid move"
		); 
		if is_empty !prev_hand then
		(
			new_state.finished_hands <-  prev_hand_n :: state.finished_hands;
			move_log := !move_log ^ sprintf "\n%s (%s) has no more cards. HE FINISHED.\n\n" 
											prev_player.name 
											(string_of_player_nature prev_player.nature);
		);
		new_state, !move_log
		
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
	
		(* TODO: add some checks *)
		and next_hand_num n finished_hands =
			let next_n = if n = (List.length state.hands) - 1 then 0 else n + 1 in
			if List.mem next_n finished_hands then next_hand_num next_n finished_hands else next_n
	
		(* TODO: add some checks *)	
		and prev_hand_num n finished_hands =
			let prev_n = if n = 0 then (List.length state.hands) - 1 else n - 1 in
			if List.mem prev_n finished_hands then prev_hand_num prev_n finished_hands else prev_n

	in play_game state
;;

let main  =
	Random.self_init();
	let players = get_players in
	let player_count = List.length players in
	let state = create_state player_count in
	play_game state players;
	
	(* let list_item_count f = List.fold_left (fun s x -> if f x then s+1 else s) 0 in *)
		
	(* let line = "7C 8H QD 6D QS AH KC JH 9H 8C 8D 8S" in
	let str_to_words = Str.split (Str.regexp " ") in 
	let cards = List.map card_of_string (str_to_words line) in
	cards *)
;;