open Graphics;;

(* constantes et parametres *)

(* dimension fenetre graphique *)
let width = 1000;;
let height = 600;;
let pi = 4.0 *. atan 1.0;;


(* --- definition types pour etat du jeu --- *)

type coordonees = (int * int);; (* Abscisse et ordonnée sur la fenêtre *)
type orientation = int;; (* Angle par rapport à la normale en degrés *)
type vitesse = int;; (* coefficient multiplicateur *)
type taille = int;; (* entre 5 et 20 (Arbitraire) *)


type missile = {
    pos: coordonees;
  orient: orientation
};;


type vaisseau = {
  pos : coordonees;
  orient: orientation;
  vitesse: vitesse
};;


type asteroid = {
    pos: coordonees;
    orient: orientation;
    vitesse: vitesse;
    taille: taille;
		couleur: color
};;


type etat = {
    vaisseau: vaisseau;
    asteroids: asteroid list;
    missiles: missile list
};;


(* --- initialisations etat --- *)

(* A DEFINIR : generation positions, deplacements initiaux ... *)
(* on définit dans un enregistrement 'etat' les positions
de base des éléments constituant le jeu : *)

let init_etat = {vaisseau = {  
	(* position initiale du vaisseau : le milieu de l'écran : *)
	pos = ((width / 2),(height / 2)); 
	orient = 0; vitesse = 0.0 };
	asteroids = [
		{asteroid = {
				pos = (100, 100);
				orient = 50;
				taille = 15;
				couleur = green;
				vitesse = zero;
			}
		};
		{asteroid = {
				pos = (450, 700);
				orient = 120;
				taille = 24;
				couleur = blue;
				vitesse = zero;
			}
		}
	];
	missiles = []};;

(* --- changements d'etat --- *)

let rotation_gauche etat = { etat with vaisseau = {etat.vaisseau with orient = etat.vaisseau.orient + 5 }};;
let rotation_droite etat = { etat with vaisseau = {etat.vaisseau with orient = etat.vaisseau.orient - 5 }};;

(* acceleration du vaisseau *)
let acceleration etat = etat;;

(* rotation vers la gauche et vers la droite du vaisseau *)

(* cette fonction a pour but d'effectuer une rotation du point
p autour du point o , selon l'angle en radian ang *)

(* tir d'un nouveau projectile *)
let tir etat = etat;; (* A REDEFINIR *)

(* calcul de l'etat suivant, apres un pas de temps *)
let etat_suivant etat = etat;; (* A REDEFINIR *)


(* --- affichages graphiques --- *)

(* fonctions d'affichage du vaisseau, d'un asteroide, etc. *)

let draw_ship pos orient = 
	set_color blue;
	let ax_tmp = cos( ((float_of_int orient) *. pi) /. 180.0 ) *. 15.0 in
	let ay_tmp = sin( ((float_of_int orient) *. pi) /. 180.0 ) *. 15.0 in
	let ax = (int_of_float ax_tmp) + fst pos in
	let ay = (int_of_float ay_tmp) + snd pos in
	let orient_aux =  orient + 120 in
	let bx_tmp = cos( ((float_of_int orient_aux) *. pi) /. 180.0 ) *. 15.0 in
	let by_tmp = sin( ((float_of_int orient_aux) *. pi) /. 180.0 ) *. 15.0 in
	let bx = (int_of_float bx_tmp) + fst pos in
	let by = (int_of_float by_tmp) + snd pos in
	let orient_aux_2 = orient_aux + 120 in
	let cx_tmp = cos( ((float_of_int orient_aux_2) *. pi) /. 180.0 ) *. 15.0 in
	let cy_tmp = sin( ((float_of_int orient_aux_2) *. pi) /. 180.0 ) *. 15.0 in
	let cx = (int_of_float cx_tmp) + fst pos in
	let cy = (int_of_float cy_tmp) + snd pos in
	fill_poly [|(ax,ay);(bx,by);(cx,cy)|];;

let rec draw_asteroids asteroids = 
	match asteroids with 
		| ast::_ -> set_color ast.couleur;
								fill_circle (fst ast.pos) (snd ast.pos) ast.taille;;


let affiche_etat etat = 
	draw_ship etat.vaisseau.pos etat.vaisseau.orient;
	draw_asteroids etat.asteroids;;
	


(* --- boucle d'interaction --- *)

let rec boucle_interaction ref_etat =
    let status = wait_next_event [Key_pressed] in (* on attend une frappe clavier *)
    let etat = !ref_etat in (* on recupere l'etat courant *)
    let nouvel_etat = (* on definit le nouvel etat... *)
        match status.key with (* ...en fonction de la touche frappee *)
        | '1' | 'j' -> rotation_gauche etat (* rotation vers la gauche *)
        | '2' | 'k' -> acceleration etat (* acceleration vers l'avant *)
        | '3' | 'l' -> rotation_droite etat (* rotation vers la droite *)
        | ' ' -> tir etat (* tir d'un projectile *)
        | 'q' -> print_endline "Bye bye!"; exit 0 (* on quitte le jeux *)
        | _ -> etat in (* sinon, rien ne se passe *)
            ref_etat := nouvel_etat; (* on enregistre le nouvel etat *)
            boucle_interaction ref_etat;; (* on se remet en attente de frappe clavier *)

(* --- fonction principale --- *)

let main () =
    (* initialisation du generateur aleatoire *)
    Random.self_init ();
    (* initialisation de la fenetre graphique et de l'affichage *)
    open_graph (" " ^ string_of_int width ^ "x" ^ string_of_int height);
    auto_synchronize false;
    (* initialisation de l'etat du jeu *)
    (*let ref_etat = ref (init_etat ()) in *)
    let ref_etat = ref init_etat in
    (* programmation du refraichissement periodique de l'etat du jeu et de son affichage *)
    let _ = Unix.setitimer Unix.ITIMER_REAL
        { Unix.it_interval = 0.05; (* tous les 1/20eme de seconde... *)
          Unix.it_value = 0.05 } in
    Sys.set_signal Sys.sigalrm
    (Sys.Signal_handle (fun _ ->
			clear_graph ();
      affiche_etat !ref_etat; (* ...afficher l'etat courant... *)
			(* draw_ship !ref_etat.vaisseau.pos !ref_etat.vaisseau.orient; *)
      synchronize ();
      ref_etat := etat_suivant !ref_etat)); (* ...puis calculer l'etat suivant *)
  boucle_interaction ref_etat;; (* lancer la boucle d'interaction avec le joueur *)

let _ = main ();; (* demarrer le jeu *)
