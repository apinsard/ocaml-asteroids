open Graphics;;

(* constantes et parametres *)

(* dimension fenetre graphique *)
let width = 1000;;
let height = 600;;

(* autre def et types *)
let pi = 4.0 *. atan 1.0;;

type 'a gen = unit -> 'a;;
let lance gen = gen();;
let genInt i j = function() -> i + Random.int((j-i)+1);;


(* --- definition types pour etat du jeu --- *)

type coordonees = (int * int);; (* Abscisse et ordonnée sur la fenêtre *)
type orientation = int;; (* Angle par rapport à la normale en degrés *)
type vitesse = float;; (* coefficient multiplicateur *)
type taille = int;; (* entre 1 et 5 (Arbitraire) *)


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

let init_etat = {
  vaisseau = {
    (* position initiale du vaisseau : le milieu de l'écran : *)
    pos = ((width / 2),(height / 2));
    orient = 90;
    vitesse = 0.0;
  };
  asteroids = [
    {
      pos = (100, 100);
      orient = 50;
      taille = 1;
      couleur = green;
      vitesse = 2.0;
    };
    {
      pos = (450, 500);
      orient = 120;
      taille = 4;
      couleur = blue;
      vitesse = 5.0;
    };
  ];
  missiles = [];
};;

(* --- Autre --- *)

let modulo x m =
  let n = x mod m in
  if n >= 0 then n
  else n+m;;

(* --- changements d'etat --- *)

let rotation_gauche etat = { etat with vaisseau = {etat.vaisseau with orient = etat.vaisseau.orient + 5 }};;
let rotation_droite etat = { etat with vaisseau = {etat.vaisseau with orient = etat.vaisseau.orient - 5 }};;

(* acceleration du vaisseau *)
let acceleration etat = etat;;

(* rotation vers la gauche et vers la droite du vaisseau *)

(* cette fonction a pour but d'effectuer une rotation du point
p autour du point o , selon l'angle en radian ang *)

(* tir d'un nouveau projectile *)
let tir etat =
  (* on retrouve les coordonées de la tête de notre fameux vaisseau *)
  let ax_tmp = cos( ((float_of_int etat.vaisseau.orient) *. pi) /. 180.0 ) *. 15.0 in
  let ay_tmp = sin( ((float_of_int etat.vaisseau.orient) *. pi) /. 180.0 ) *. 15.0 in
  let ax = (int_of_float ax_tmp) + fst etat.vaisseau.pos in
  let ay = (int_of_float ay_tmp) + snd etat.vaisseau.pos in

  (* on lance le missile ! *)
  let new_missile = {pos=(ax, ay); orient = etat.vaisseau.orient} in
  { etat with missiles = new_missile::etat.missiles};;


(* calcul de l'etat suivant, apres un pas de temps *)

let rec etat_suivant_asteroids etat =
  match etat.asteroids with
    | ast::rest ->
        let tmp_x = ( cos( ((float_of_int ast.orient) *. pi) /. 180.0 )) *. ast.vitesse in
        let tmp_y = ( sin( ((float_of_int ast.orient) *. pi) /. 180.0 )) *. ast.vitesse in
        let tmp2_x = ( (fst ast.pos ) + (int_of_float(tmp_x)) ) in
        let tmp2_y = ( (snd ast.pos ) + (int_of_float(tmp_y)) ) in
        let new_x = modulo tmp2_x width in
        let new_y = modulo tmp2_y height in
        let new_ast = {ast with pos = (new_x, new_y)} in
        {etat with asteroids = new_ast::(etat_suivant_asteroids { etat with asteroids = rest}).asteroids};
    | _ -> etat;;

let rec etat_suivant_missiles etat =
  match etat.missiles with
    | miss::rest ->
        let tmp_x = ( cos( ((float_of_int miss.orient) *. pi) /. 180.0 )) *. 10.0 in
        let tmp_y = ( sin( ((float_of_int miss.orient) *. pi) /. 180.0 )) *. 10.0 in
        let new_x = ( (fst miss.pos ) + (int_of_float(tmp_x)) ) in
        let new_y = ( (snd miss.pos ) + (int_of_float(tmp_y)) ) in
        if new_x > width || new_x < 0 || new_y > height || new_y < 0 then
          etat_suivant_missiles { etat with missiles = rest}
        else
          let new_miss = {miss with pos = (new_x, new_y)} in
          {etat with missiles = new_miss::(etat_suivant_missiles { etat with missiles = rest}).missiles};
    | _ -> etat;;


let etat_suivant etat =
  etat_suivant_asteroids (etat_suivant_missiles etat);;


(* --- gestion des collisions --- *)

let rec eclate_asteroweed indice old_taille old_pos old_color =
  if indice = 0 then []
  else 
    let new_angle = lance( genInt 0 359) in
    let new_vitesse = float_of_int( lance( genInt 2 8 )) in
    let new_ast = {pos = old_pos; orient = new_angle; taille = (old_taille - 1); couleur = old_color; vitesse = new_vitesse } in
    new_ast::( eclate_asteroweed (indice-1) old_taille old_pos old_color);;
  

let rec handle_collisions_missiles_aux etat missile_pos = 
  match etat.asteroids with
    | ast::rest_ast ->
        let dist_miss_ast = int_of_float (sqrt (  ( (float_of_int (fst ast.pos)) ** 
                                                  (float_of_int (fst missile_pos)) ) +.
                                                  ( (float_of_int( snd ast.pos)) ** 
                                                  (float_of_int (snd missile_pos)) ) ) ) in
        if dist_miss_ast > (ast.taille + 3 ) then
          (* pas de collision entre ce missile et l'asteroid *)
          {etat with asteroids = ast::(handle_collisions_missiles_aux {etat with asteroids = rest_ast} missile_pos).asteroids}
        else
          (* collision : on éclate l'asteroid *)
          if ast.taille = 1 then
             {etat with asteroids = (handle_collisions_missiles_aux {etat with asteroids = rest_ast} missile_pos).asteroids}
          else
            let indice = lance(genInt 2 4 ) in
            let nouveaux_ast = eclate_asteroweed indice ast.taille ast.pos ast.couleur in
            {etat with asteroids = nouveaux_ast @ (handle_collisions_missiles_aux {etat with asteroids = rest_ast} missile_pos).asteroids};
    | _ -> etat;;  
          

let rec handle_collisions_missiles etat =
  match etat.missiles with
    | miss::rest_miss ->
        {etat with missiles = miss::(handle_collisions_missiles {etat with missiles = rest_miss}).missiles;
                   asteroids = (handle_collisions_missiles_aux etat miss.pos).asteroids};
    | _ -> etat;;

let handle_collisions etat = 
  handle_collisions_missiles etat;;

(* --- affichages graphiques --- *)

(* fonctions d'affichage du vaisseau, d'un asteroide, etc. *)

let draw_ship pos orient =
  set_color blue;
  let ax_tmp = cos( ((float_of_int orient) *. pi) /. 180.0 ) *. 20.0 in
  let ay_tmp = sin( ((float_of_int orient) *. pi) /. 180.0 ) *. 20.0 in
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

let rec draw_asteroids etat =
  match etat.asteroids with
    | ast::rest -> set_color ast.couleur;
                fill_circle (fst ast.pos) (snd ast.pos) (ast.taille*8);
                draw_asteroids {etat with asteroids = rest};
    | _ -> ();;

let rec draw_missiles etat =
  match etat.missiles with
    | miss::rest -> set_color red;
                    fill_circle (fst miss.pos) (snd miss.pos) 3;
                    draw_missiles {etat with missiles = rest};
    | _ -> ();;


let affiche_etat etat =
  draw_ship etat.vaisseau.pos etat.vaisseau.orient;
  draw_missiles etat;
  draw_asteroids etat;;


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
      ref_etat := etat_suivant !ref_etat;
      ref_etat := handle_collisions !ref_etat)); (* ...puis calculer l'etat suivant *)
      (* test *)
  boucle_interaction ref_etat;; (* lancer la boucle d'interaction avec le joueur *)

let _ = main ();; (* demarrer le jeu *)
