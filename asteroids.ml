open Graphics;;

(* constantes et parametres *)

(* dimension fenetre graphique *)
let width = 1000;;
let height = 600;;
let pi = 4.0 *. atan 1.0;;


(* --- definition types pour etat du jeu --- *)

type coordonees = (int * int);; (* Abscisse et ordonnée sur la fenêtre *)
type orientation = float;; (* Angle par rapport à la normale en radian *)
type vitesse = float;; (* coefficient multiplicateur *)
type taille = int;; (* entre 1 et 4 (Arbitraire) *)
type couleur = Rouge | Bleu | Vert | Jaune;;


type missile = {
    pos: coordonees;
  orient: orientation
};;


type vaisseau = {
    (* les trois points du triangle plus le centre *)
    a : coordonees;
    b : coordonees;
    c : coordonees;
    d : coordonees;
    o : coordonees;
    orient: orientation;
    vitesse: vitesse
};;


type asteroid = {
    pos: coordonees;
    orient: orientation;
    vitesse: vitesse;
    taille: taille;
    couleur: couleur
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
    a = ((width / 2 - 10),(height / 2 + 10));
    b = ((width / 2 + 10),(height / 2 + 10));
    c = ((width / 2 + 10),(height / 2 - 10));
    d = ((width / 2 - 10),(height / 2 - 10));
    o = ((width / 2),(height / 2));
    orient = pi /. 2.0; vitesse = 0.0 };
    asteroids = [];
    missiles = []
};;


let draw_vaisseau a b c d color =
    set_color color;
    let tab = [|a;b;c;d|] in
    draw_poly tab;
    fill_poly tab;;


(* --- changements d'etat --- *)

(* acceleration du vaisseau *)
let acceleration etat = etat;; (* A REDEFINIR *)

(* rotation vers la gauche et vers la droite du vaisseau *)

(* cette fonction a pour but d'effectuer une rotation du point
p autour du point o , selon l'angle en radian ang *)
let rotate_point p o ang =
    let s = sin( ang ) in
    let c = cos( ang ) in
    let tmp_x = float_of_int ( fst p ) -. float_of_int ( fst o ) in
    let tmp_y = float_of_int ( snd p ) -. float_of_int ( snd o ) in
    let x_int = tmp_x *. c -. tmp_y *. s in
    let y_int = tmp_x *. s +. tmp_y *. c in
    ((int_of_float  (x_int +. float_of_int ( fst o ))),(int_of_float (y_int +. float_of_int ( snd o ))));;


let rotation_gauche etat =
    let new_a = rotate_point etat.vaisseau.a etat.vaisseau.o (pi /. 18.0) in
    let new_b = rotate_point etat.vaisseau.b etat.vaisseau.o (pi /. 18.0) in
    let new_c = rotate_point etat.vaisseau.c etat.vaisseau.o (pi /. 18.0) in
    let new_d = rotate_point etat.vaisseau.d etat.vaisseau.o (pi /. 18.0) in

    draw_vaisseau etat.vaisseau.a etat.vaisseau.b etat.vaisseau.c etat.vaisseau.d white;

    {etat with vaisseau =
        {etat.vaisseau with
            orient = etat.vaisseau.orient +. (pi /. 18.0);
            a = new_a;
            b = new_b;
            c = new_c;
            d = new_d
        }
    };;


let rotation_droite etat =
    let new_a = rotate_point etat.vaisseau.a etat.vaisseau.o (-.(pi /. 18.0)) in
    let new_b = rotate_point etat.vaisseau.b etat.vaisseau.o (-.(pi /. 18.0)) in
    let new_c = rotate_point etat.vaisseau.c etat.vaisseau.o (-.(pi /. 18.0)) in
    let new_d = rotate_point etat.vaisseau.d etat.vaisseau.o (-.(pi /. 18.0)) in

    draw_vaisseau etat.vaisseau.a etat.vaisseau.b etat.vaisseau.c etat.vaisseau.d white;

    {etat with vaisseau =
        {etat.vaisseau with
            orient = etat.vaisseau.orient -. (pi /. 18.0);
            a = new_a;
            b = new_b;
            c = new_c;
            d = new_d
        }
    };;

(* tir d'un nouveau projectile *)
let tir etat = etat;; (* A REDEFINIR *)

(* calcul de l'etat suivant, apres un pas de temps *)
let etat_suivant etat = etat;; (* A REDEFINIR *)


(* --- affichages graphiques --- *)

(* fonctions d'affichage du vaisseau, d'un asteroide, etc. *)



(* let affiche_etat etat = ();; (* A REDEFINIR *) *)
let affiche_etat etat =
    draw_vaisseau etat.vaisseau.a etat.vaisseau.b etat.vaisseau.c etat.vaisseau.d blue;;


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
        affiche_etat !ref_etat; (* ...afficher l'etat courant... *)
        (* test  draw_vaisseau (50,50) (60,50) (55,60);*)
        synchronize ();
        ref_etat := etat_suivant !ref_etat)); (* ...puis calculer l'etat suivant *)
    boucle_interaction ref_etat;; (* lancer la boucle d'interaction avec le joueur *)

let _ = main ();; (* demarrer le jeu *)
