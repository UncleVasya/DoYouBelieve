#load "str.cma";;  (* regexp *)
#load "unix.cma";; (* time detection*)

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

type move = Check | Move of card list * card_rank;;

type player =
{
	name : string;
	nature : playerNature;
}
and playerNature = Human | Computer;;

let get_players =
	[
		{name = "Player1"; nature = Human};
		{name = "Player2"; nature = Computer};
		{name = "Player3"; nature = Computer};
		{name = "Player4"; nature = Human}
	]

let create_deck ranks suits : card list = 
	List.concat (List.map (fun s -> List.map (fun n -> n,s) ranks) suits)
	
let shuffle_list lst =
	Random.self_init () ;
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

	
let create_state player_count = 
	let ranks = [Six; Seven; Eight; Nine; Ten; Jack; Queen; King; Ace] in
	let suits = [Heart; Club; Spade; Diamond] in
	let deck = shuffle_list (create_deck ranks suits) in 
	let hands = divide_list deck player_count in
	{hands = hands; table = []; claim = None; active_hand = 0; finished_hands = []}
	
let play_game state players =	
	let rec play_game state =
		if end_game then List.rev state.finished_hands
		else begin
			let move = get_move info_to_player in
			let state = do_move move in
			play_game state
		end
	
	and end_game = 
		let hands_in_game = List.fold_left (fun s x -> if x = [] then s else s+1) 0 state.hands in
		hands_in_game < 2
	
	and info_to_player = 
		(* 
			HINT: ask mcstar if he _always_ writes tail-recursive functions? Is it a Greater Good?
			Because   List.map (fun x -> List.length x) state.hands  is prettier than the current one
			and I know that this function dealing with really small lists so there's no point
			in using tail-recrsion except for Greater Good. Anyway, consult mcstar.
		*)
		let list_of_len = List.rev_map (fun x -> List.length x) in
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
	
	and get_move info_to_player = 
		Check
		
	and do_move = function
		| Check -> state
	
		| Move (cards, claim) -> state
		
	
	in play_game state

;;

let main  =
	let players = get_players in
	let player_count = List.length players in
	let state = create_state player_count in
	(* play_game state players; *)
	state;
;;


