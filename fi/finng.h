!   finng.h
!   =======
!   Suomenkieliset verbit. 
! ==============================================================================
!   GRAMMAR:  Grammar table entries for the standard verbs library.
!	
!   Supplied for use with Inform 6 -- Release 6/11 -- Serial number 040227
!
!   Copyright Graham Nelson 1993-2004 but freely usable (see manuals)
!
!   In your game file, Include three library files in this order:
!       Include "Parser";
!       Include "VerbLib";
!       Include "Grammar"; (Suomeksi: Include "finng";)
! ==============================================================================

System_file;


!! VerbDepot voi auttaa verbien tulostamisessa
!! ks. verbit.h
Object VerbDepot;


[off_noun; return c_token (0, csOff); ];

    

[nom_noun; return c_token (NOUN_TOKEN, csNom); ];
[gen_noun; return c_token (NOUN_TOKEN, csGen); ];
[par_noun; return c_token (NOUN_TOKEN, csPar); ];
[ine_noun; return c_token (NOUN_TOKEN, csIne); ];
[ela_noun; return c_token (NOUN_TOKEN, csEla); ];
[ill_noun; return c_token (NOUN_TOKEN, csIll); ];
[ade_noun; return c_token (NOUN_TOKEN, csAde); ];
[abl_noun; return c_token (NOUN_TOKEN, csAbl); ];
[all_noun; return c_token (NOUN_TOKEN, csAll); ];

!held 
[nom_held; return c_token (HELD_TOKEN, csNom); ];
[par_held; return c_token (HELD_TOKEN, csPar); ];
[ill_held; return c_token (HELD_TOKEN, csIll); ];
[ade_held; return c_token (HELD_TOKEN, csAde); ];

!worn (worn-tokenia ei ole)
[nom_worn; return c_token (HELD_TOKEN, csNom); ]; 

!creature
[nom_creat; return c_token (CREATURE_TOKEN, csNom); ];
[par_creat; return c_token (CREATURE_TOKEN, csPar); ];
[abl_creat; return c_token (CREATURE_TOKEN, csAbl); ];
[all_creat; return c_token (CREATURE_TOKEN, csAll); ];

!multi
[nom_multi; return c_token (MULTI_TOKEN, csNom); ];
[par_multi; return c_token (MULTI_TOKEN, csPar); ];

!multiheld
[nom_multiheld;	return c_token (MULTIHELD_TOKEN, csNom); ];

!multiexcept (nom, par?)
[nom_multiexcept; return c_token (MULTI_TOKEN, csNom);	];

!multiinside?


! ------------------------------------------------------------------------------
!  The "meta-verbs", commands to the game rather than in the game, come first:
! ------------------------------------------------------------------------------

Verb meta 'brief' 'perus'
    *                                      -> LMode1;
Verb meta 'laaja' 'pitk�'
    *                                      -> LMode2;
Verb meta 'lyhyt'
    *                                      -> LMode3;
Verb meta 'notify'
    *                                      -> NotifyOn
    * 'on'                                 -> NotifyOn
    * 'off'                                -> NotifyOff;
Verb meta 'pronominit' 'nominit'
    *                                      -> Pronouns;
Verb meta 'lopeta' 'lopu' 'loppu' 'kuole' 'q//' 'quit'
    *                                      -> Quit;
Verb meta 'recording' 'nauhoitus'
    *                                      -> CommandsOn
    * 'on'                                 -> CommandsOn
    * 'off'                                -> CommandsOff;
Verb meta 'replay'
    *                                      -> CommandsRead;
Verb meta 'aloita' 'restart' 'alusta' 'uusi' 'alkuun'
    *                                      -> Restart
    * 'alusta'                             -> Restart;
Verb meta 'lataa' 'restore'
    *                                      -> Restore;
Verb meta 'tallenna' 'tallennus' 'save' 
    *                                      -> Save;
Verb meta 'pisteet'
    *                                      -> Score;
Verb meta 'maksimi' 'maksimipi' 't�ydet'
    *                                      -> FullScore
    * 'pisteet'                            -> FullScore;
Verb meta 'script' 'kirjaus'
    *                                      -> ScriptOn
    * 'p��lle'                             -> ScriptOn
    * 'pois'                               -> ScriptOff
    * 'off'                                -> ScriptOff;
Verb meta 'noscript' 'unscript'
    *                                      -> ScriptOff;
Verb meta 'verifioi'
    *                                      -> Verify;
Verb meta 'versio'
    *                                      -> Version;

#Ifndef NO_PLACES;
Verb meta 'esineet'
    *                                      -> Objects;
Verb meta 'paikat'
    *                                      -> Places;
#Endif; ! NO_PLACES

! ------------------------------------------------------------------------------
!  Debugging grammar
! ------------------------------------------------------------------------------

#Ifdef DEBUG;
! anything rutiini on 
Verb meta 'sijat'
    * scope=anything                       -> PrintSijat;
Verb meta 'actions'
    *                                      -> ActionsOn
    * 'on'                                 -> ActionsOn
    * 'off'                                -> ActionsOff;
Verb meta 'changes'
    *                                      -> ChangesOn
    * 'on'                                 -> ChangesOn
    * 'off'                                -> ChangesOff;
Verb meta 'gonear'
    * nom_noun                                 -> Gonear;
Verb meta 'goto'
    * number                               -> Goto;
Verb meta 'random'
    *                                      -> Predictable;
Verb meta 'routines' 'messages'
    *                                      -> RoutinesOn
    * 'on'                                 -> RoutinesOn
    * 'off'                                -> RoutinesOff;
Verb meta 'scope'
    *                                      -> Scope
    * nom_noun                                 -> Scope;
Verb meta 'showobj'
    *                                      -> Showobj
    * number                               -> Showobj
    * nom_multi                                -> Showobj;
Verb meta 'showverb'
    * special                              -> Showverb;
Verb meta 'timers' 'daemons'
    *                                      -> TimersOn
    * 'on'                                 -> TimersOn
    * 'off'                                -> TimersOff;
Verb meta 'trace'
    *                                      -> TraceOn
    * number                               -> TraceLevel
    * 'on'                                 -> TraceOn
    * 'off'                                -> TraceOff;
Verb meta 'abstract'
    * noun 'to' noun                       -> XAbstract;
Verb meta 'purloin'
    * multi                                -> XPurloin;
Verb meta 'tree'
    *                                      -> XTree
    * noun                                 -> XTree;

#Ifdef TARGET_GLULX;
Verb meta 'glklist'
    *                                      -> Glklist;
#Endif; ! TARGET_

#Endif; ! DEBUG

! ------------------------------------------------------------------------------
!  And now the game verbs.
! ------------------------------------------------------------------------------


[ ADirection; if (noun in compass) rtrue; rfalse; ];


! vajaaseen sy�tteeseen oletuksena kysyt��n "mit�?"
! esim.
!   >tutki
!   Tutki mit�?
! PrintKysymys (finnish.h) tulostaa muita

Verb 'vastaa' 'sano' 'puhu'
    * topic				-> Answer
    * all_creat topic			-> Answer
    * all_creat ela_noun topic		-> Answer reverse;
Verb 'kysy' 'pyyd�' ! 'tiedustele'
! ristolta pallosta
    * abl_creat topic			-> Ask 
!   * abl_creat nom_noun                   -> AskFor
! ristolta palloa
    * abl_creat par_noun		-> AskFor
! ristoa olemaan hiljaa
    * par_creat   topic			-> AskTo;
!   * abl_creat   topic                    -> AskTo;

!Object 	"kysy" VerbDepot
! with 	name 'kysy' 'tiedustele' 'pyyd�';
! kys_a "kenelt�k�", kys_b "mittee"; ! miksei?



Verb 'hy�kk��' 'riko' 's�rje' 'murskaa' 'tuhoa' 'potkaise' 'potki'
     'ly�' 'tapa' 'murhaa' 'iske'
     'hajota' 'hakkaa' 'piekse' 'kiduta' 'romuta'
    * nom_noun                             -> Attack
    * par_noun                             -> Attack
    * gen_noun 'kimppuun'                  -> Attack; 
Verb 'puhalla'
    * par_held                             -> Blow
    * ill_held                             -> Blow;
Verb '�h' 'pahus' 'piru' 'hitsi' 'hitto'
    *                                      -> Mild
    * topic                                -> Mild;
Verb 'polta' 'sytyt�'
    * nom_noun                             -> Burn
    * nom_noun ade_held                    -> Burn;
Verb 'osta'
    * nom_noun                             -> Buy;
Verb 'kiipe�'
    * nom_noun                             -> Climb
    * par_noun                             -> Climb
    * ill_noun                             -> Climb
    * all_noun                             -> Climb
    * gen_noun 'yli'/'p��lle'              -> Climb
    * 'yl�s' par_noun                      -> Climb;
Verb 'sulje' 'peit�'
    * nom_noun                             -> Close
    * 'off' nom_noun                       -> SwitchOff;
Verb 'hae'
    * par_noun topic                       -> Consult;
Verb 'leikkaa' 'katko' 'katkaise' 'viipaloi'
    * nom_noun                             -> Cut
    * par_noun                             -> Cut;
Verb 'kaiva' 'kaivaudu'
    * nom_noun                             -> Dig
    * ill_noun                             -> Dig;
Verb 'juo' 'h�rpp��' 'niele'
    * nom_noun                             -> Drink
    * ela_noun                             -> Drink
    * par_noun                             -> Drink;
Verb 'pudota' 'laske' 'tiputa' 'poista' 'hylk��'
    * multiheld                            -> Drop
    * multiheld 'maahan'                   -> Drop;
Verb 'j�t�'
    * multiheld                            -> Drop
    * multiexcept ill_noun                 -> Insert
    * multiexcept all_noun                 -> PutOn
    * nom_noun                             -> Exit;
Verb 'heit�' 'viskaa'
    * par_multi ade_held                   -> ThrowAt reverse
    * nom_multi                            -> Drop
    * nom_held ill_noun                    -> ThrowAt
    * ade_held par_noun                    -> ThrowAt
    * ade_held ill_noun                    -> ThrowAt
    * par_noun ade_held                    -> ThrowAt reverse
    * ill_noun ade_held                    -> ThrowAt reverse
    * all_noun par_held                    -> ThrowAt reverse
    * all_noun nom_held			   -> ThrowAt reverse
    * par_held all_noun                    -> ThrowAt
    * nom_held all_noun                    -> ThrowAt;
Verb 'sy�'
    * nom_held                             -> Eat
    * par_held                             -> Eat;
Verb 'tyhj��' 'tyhjenn�'
    * nom_noun                             -> Empty
    * nom_noun ill_noun                    -> EmptyT
    * nom_noun all_noun                    -> EmptyT;
Verb 'tutki' 't//' 'x//' 'tarkasta' 'kuvaile'
    * par_noun                          -> Examine
    * nom_noun                          -> Examine;

Object 	"tutki" VerbDepot
 with 	name 'tutki' 't//' 'x//' 'tarkasta' 'kuvaile';

Verb 'exit' 'ulos'
    *                                      -> Exit
    * ela_noun                             -> Exit;
Verb 't�yt�'
    * nom_noun                             -> Fill;
! mit� - kenelle
Verb 'anna' 'sy�t�' 'tarjoa' 'maksa' 'lahjoita'
    * nom_held all_creat		-> Give 
    * par_held all_creat		-> Give
    * all_creat nom_held		-> Give reverse
    * all_creat par_Held		-> Give reverse;


Object 	"anna" VerbDepot
 with 	name 'anna' 'sy�t�' 'tarjoa' 'maksa' 'lahjoita';

! mill� - kenet
Verb 'lahjo' 'ruoki'
    * ade_held creature                    -> Give
    * creature ade_held                    -> Give reverse;
Verb 'tunge'
    * multiexcept ill_noun                 -> Insert;
Verb 'inv' 'tavarat' 'inventoi' 'inventaario' 'tarvikkeet' 'lista'
    'm//' 'v//'
    *                                      -> Inv
    * 'pitk�'/'p'                          -> InvTall
    * 'leve�'/'l'                          -> InvWide;
Verb 'hypp��' 'loikkaa' 
    *                                      -> Jump
    * all_noun                             -> Enter
    * ill_noun                             -> Enter    
    * gen_noun 'yli'                       -> JumpOver
    * 'yli' gen_noun                       -> JumpOver;
Verb 'suutele' 'pussaa' 'halaa' 'syleile'
    * par_creat                            -> Kiss
    * par_noun                             -> Kiss;
Verb 'mene' 'juokse' 'k�vele' 'k�y' 'astu'
    *                                      -> VagueGo
    * noun=ADirection                      -> Go
    * ill_noun                             -> Enter
    * par_noun                             -> Enter
    * ela_noun                             -> Enter
    * all_noun                             -> Enter
    * 'sis�lle'/'sis��n' ill_noun          -> Enter
    * gen_noun 'sis�lle'/'sis��n'/'p��lle' -> Enter
    * 'pois'                               -> Exit;
Verb 'poistu' 'tule' 'l�hde'
    *                                      -> Exit
    * abl_noun                             -> Exit
    * ela_noun                             -> Exit
    * nom_noun 'p��lt�'/'sis�lt�'          -> Exit
    * 'pois'                               -> Exit
    * 'pois' gen_noun 'p��lt�'/'sis�lt�'   -> Exit
    * gen_noun 'p��lt�'/'sis�lt�'          -> Exit
    * gen_noun 'pois'/'ulos'               -> Exit
    * 'pois'/'ulos' ela_noun               -> Exit
    * 'pois'/'ulos' abl_noun               -> Exit
    * ela_noun 'pois'/'ulos'               -> Exit
    * abl_noun 'pois'/'ulos'               -> Exit
    * noun=ADirection                      -> Go;
Verb 'kuuntele'
    *                                      -> Listen
    * nom_noun                             -> Listen
    * par_noun                             -> Listen
    * ela_noun                             -> Listen;

! mik� mill�
Verb 'lukitse'
    * nom_noun ade_held                    -> Lock;

Object 	"lukitse" VerbDepot
 with 	name 'lukitse';


Verb 'katso' 'ks' 'k//'
    *                                      -> Look
    * par_noun                             -> Examine
    * ill_noun                             -> Search
    * gen_noun 'sis��n'/'sis�lle'          -> Search
    * gen_noun 'alle'                      -> LookUnder
    * noun=ADirection                      -> Examine;
Verb 'ei' 'en' '�l�' 
    *                                      -> No;
Verb 'avaa'
    * nom_noun				-> Open
    * gen_noun 'lukitus' ade_held	-> Unlock
    * gen_noun 'lukko' ade_held		-> Unlock
    * nom_noun ade_held                 -> Unlock;

    
 Object "avaa" VerbDepot
 with 	name 'avaa',
 	kysymys "mill�";

Verb 'poimi' 'ker��'
    * nom_noun                             -> Take;
Verb 'rukoile'
    *                                      -> Pray;
Verb 'tiirikoi' 'murra'
    * nom_noun ade_held                    -> Unlock
    * nom_noun 'auki' ade_held             -> Unlock;
Verb 'ved�' 'kisko'
    * par_noun                             -> Pull
    * ela_noun				-> Pull;
Verb 'paina'
    * par_noun				-> Push
    * nom_noun				-> Push;
Verb 'ty�nn�' 'puske' ! 'liikuta'
!    * par_noun				-> Push
    * par_noun noun=ADirection		-> PushDir
    * nom_noun noun=ADirection		-> PushDir;
! push rfalse "Se pysyy paikallaan."

! (ehk�) mit� - mihin
Verb 'laita' 'pane' 'pist�' ! 'aseta'

! * nom_noun 'p��lle'
    * nom_multiexcept ill_noun		-> Insert 
    * par_noun ill_noun			-> Insert
    * nom_multiexcept all_noun		-> PutOn
    * par_noun all_noun			-> PutOn
    * nom_noun gen_noun 'sis��n'	-> Insert
    * nom_noun gen_noun 'p��lle'	-> PutOn 
    * all_noun nom_multiexcept		-> PutOn reverse
    * 'pois' nom_multiheld		-> Drop
    * nom_multiheld 'pois'		-> Drop; 


    !* nom_multiexcept off_noun          -> Insert;
    !* multiheld noun                    -> Insert;

Object 	"laita" VerbDepot
 with	
	!inf_ "taa", ind_y "taa", ind_m "tavat",
	name 'laita' 'pane' 'pist�' 'aseta';



! siirr� voisi olla my�s pushdir
Verb 'siirr�'
    * nom_noun all_noun                    -> Transfer
    * nom_noun ill_noun                    -> Transfer;
Verb 'lue'
    * nom_noun                             -> Consult
    * par_noun                             -> Consult
    * ela_noun topic ela_noun              -> Consult;
Verb 'irrota'
    * multi                                -> Take
    * multiinside ela_noun                 -> Remove;
Verb 'hankaa' 'siisti' 'siivoa' 'p�lyt�' 'kiillota'
     'kuuraa' 'hinkkaa' 'pyyhi' 'hiero'
    * nom_noun                             -> Rub
    * par_noun                             -> Rub;
Verb 'etsi'
    * ela_noun                             -> Search;
Verb 's��d�' 'm��rit�'
    * nom_noun                             -> Set
    * noun special                         -> SetTo;
Verb 'riisu' 'riisuudu'
    * held				   -> Disrobe;
! mit� - kenelle
Verb 'n�yt�' 'esit�'
    * par_held all_creat                   -> Show
    * nom_held all_creat                   -> Show
    * all_creat par_held                   -> Show reverse
    * all_creat nom_held                   -> Show reverse;
   
Object 	"n�yt�" VerbDepot
 with 	name 'n�yt�' 'esit�';


Verb  'saatana' 'helvetti' 'jumalauta' 'vittu' 'perkele'
    *                                      -> Strong           
    * topic                                -> Strong;
Verb 'laula' 'huuda'
    *                                      -> Sing;
Verb 'istu' 'makaa'
    * all_noun                             -> Enter
    * ade_noun                             -> Enter
    * ill_noun                             -> Enter;

Object 	"istu" VerbDepot
 with 	name 'istu' 'makaa';


Verb 'nuku' 'koisaa'
    *                                      -> Sleep;
Verb 'haista' 'haistele' 'nuuhki'
    *                                      -> Smell
    * par_noun                             -> Smell
    * nom_noun                             -> Smell;
Verb 'sori' 'pahoittelen' 'anteeksi'
    *                                      -> Sorry;
Verb 'purista' 'litist�'
    * nom_noun                             -> Squeeze
    * par_noun                             -> Squeeze;
Verb 'nouse'
    * 'seisomaan'                          -> Exit
    * 'yl�s'/'pystyyn'                     -> Exit
    * all_noun                             -> Enter
    * ill_noun                             -> Enter
    * ade_noun                             -> Enter;
Verb 'seiso'
    *                                      -> Exit;
Verb 'ui' 'sukella'
    *                                      -> Swim;
Verb 'keinu' 'heilu'       
    * nom_noun                             -> Swing
    * ade_noun                             -> Swing
    * ine_noun                             -> Swing;	
Verb 'kytke'
    * nom_noun                             -> Switchon
    * nom_noun 'p��lle'                    -> Switchon
    * nom_noun 'p��lt�'                    -> Switchoff
    * nom_noun 'pois' 'p��lt�'             -> Switchoff;
Verb 'ota'
    * nom_multi                            -> Take
    * par_multi                            -> Take;
Verb 'tee'
    * 'inventaario'                        -> InvTall;
Verb 'maista'
    * par_noun                             -> Taste;
Verb 'kerro'
    * creature topic			-> Tell; ! all_creat
Verb 'ajattele'
    *                                      -> Think;
Verb 'sido' 'kiinnit�'
    * nom_noun                             -> Tie
    * nom_noun ill_noun                    -> Tie;
Verb 'koske' 'kosketa' 'koskettele' 'tunnustele'
    * par_noun                             -> Touch;
Verb 'k��nn�' 'kierr�' 'v��nn�'
    * par_noun                             -> Turn
    * nom_noun 'p��lle'                    -> Switchon
    * nom_noun 'p��lt�'                    -> Switchoff
    * nom_noun 'pois' 'p��lt�'             -> Switchoff
    * nom_noun 'p��lt�' 'pois'             -> Switchoff;
Verb 'sammuta' 
    * nom_noun                             -> Switchoff;
Verb 'heiluta' 'vilkuta'
    *                                      -> WaveHands
    * par_noun                             -> Wave
    * nom_noun                             -> Wave;
Verb 'pukeudu' 'pue'
    * nom_held                             -> Wear
    * nom_held 'p��lle'                    -> Wear
    * 'p��lle' nom_held                    -> Wear
    * ill_noun nom_held                    -> Wear;
Verb 'kyll�' 'joo' 'ok'
    *                                      -> Yes;
Verb 'odota' 'z//' '�//' 
    *                                      -> Wait;
Verb 'her��' 'her�t�' 'her�tys'
    *                                      -> Wake
    * nom_creat                            -> WakeOther;

! ------------------------------------------------------------------------------
!  This routine is no longer used here, but provided to help existing games
!  which use it as a general parsing routine:

[ ConTopic w;
    consult_from = wn;
    do w = NextWordStopped();
    until (w == -1 || (w == 'to' && action_to_be == ##Answer));
    wn--;
    consult_words = wn - consult_from;
    if (consult_words == 0) return -1;
    if (action_to_be == ##Answer or ##Ask or ##Tell) {
	w = wn; wn = consult_from; parsed_number = NextWord();
	if (parsed_number == 'the' && consult_words > 1) parsed_number = NextWord();
	wn = w;
	return 1;
    }
    return 0;
];

! ------------------------------------------------------------------------------
!  Final task: provide trivial routines if the user hasn't already:
! ------------------------------------------------------------------------------

#Stub AfterLife         0;
#Stub AfterPrompt       0;
#Stub Amusing           0;
#Stub BeforeParsing     0;
#Stub ChooseObjects     2;
#Stub DarkToDark        0;
#Stub DeathMessage      0;
#Stub GamePostRoutine   0;
#Stub GamePreRoutine    0;
#Stub InScope           1;
#Stub LookRoutine       0;
#Stub NewRoom           0;
#Stub ParseNumber       2;
#Stub ParserError       1;
#Stub PrintTaskName     1;
#Stub PrintVerb         1;
#Stub TimePasses        0;
#Stub UnknownVerb       1;

#Ifdef TARGET_GLULX;
#Stub HandleGlkEvent    2;
#Stub IdentifyGlkObject 4;
#Stub InitGlkWindow     1;
#Endif; ! TARGET_GLULX

#Ifndef PrintRank;
! Constant Make__PR;
! #Endif;
! #Ifdef Make__PR;
[ PrintRank; "."; ];
#Endif;

#Ifndef ParseNoun;
! Constant Make__PN;
! #Endif;
! #Ifdef Make__PN;
[ ParseNoun obj; obj = obj; return -1; ];
#Endif;

#Default Story 0;
#Default Headline 0;

#Ifdef INFIX;
#Include "infix";
#Endif;

! ==============================================================================

Constant LIBRARY_GRAMMAR;       ! for dependency checking

! ==============================================================================
