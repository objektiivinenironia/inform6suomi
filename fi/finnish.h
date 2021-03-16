! ==============================================================================
!   FINNISH:  Language Definition File
!					
!   Supplied for use with Inform 6 -- Release 6/11 -- Serial number 040227
!
!   Copyright Graham Nelson 1993-2004 but freely usable (see manuals)
!
!   This file is automatically Included in your game file by "parserm".
!   Strictly, "parserm" includes the file named in the "language__" variable,
!   whose contents can be defined by+language_name=XXX compiler setting (with a
!   default of "english").
!
!   Define the constant DIALECT_US before including "Parser" to obtain American
!   English.
! ==============================================================================

System_file;

! ------------------------------------------------------------------------------
!   Part I.   Preliminaries
! ------------------------------------------------------------------------------

Replace NounDomain; 
Replace Adjudicate; 
Replace PrintCommand; 
Replace CantSee;
Replace Refers;
Replace ScoreMatchL;
Replace TryGivenObject;
Replace ParseToken;
Replace ParseToken__;
Replace ResetDescriptors;
Replace Descriptors;
Replace PSN__; ! "sinä itse" jne.



Constant LanguageVersion = "Finnish";


! asiayhteys on rutiinin PrintKysymys
! arvailua varten ks. adjudicate (context)
Global asiayhteys = 0; 

Constant LanguageCases = 11;	

Constant csOff =	0;

Constant csNom =	1;
Constant csGen =	2;
Constant csPar =	3;
Constant csIne =	4;
Constant csEla =	5;
Constant csIll =	6;
Constant csAde =	7;
Constant csAbl =	8;
Constant csAll =	9;
Constant csEss =	10;
Constant csTra =	11;

Global csDflt = csNom;

Constant ocS =		1;
Constant ocP =		2;

! Verbin tai nominin alkuosa ennen "/"
Constant Alku =		20; !pu

! Verbit...

Constant vbImp = 0; ! oletus

!Constant vbImp = 21; !pue
!Constant vbInf = 22; !pukea
!Constant vbY1 =	23; !puen
!Constant vbY2 =	24; !puet
!Constant vbInd = 25; !oletus 'pukee' (=vbY3)
!Constant vbY3 =	25; !pukee
!Constant vbM1 =	26; !puemme
!Constant vbM2 =	27; !puette
!Constant vbM3 =	28; !pukevat 



! ei artikkeleita...
  
[ LtoU ch;

  return
  UpperCase(ch);
 
  ];

! ParserTrace: 0=off, 1=print string after LanguageToInformese,
!                     2=ASCII-List (useful for character hacking)

#ifdef DEBUG;
  Constant ParserTrace = 2;
#endif;


Class   CompassDirection
  with  number 0,
	description [;
	    if (location provides compass_look && location.compass_look(self)) rtrue;
	    if (self.compass_look()) rtrue;
	    L__M(##Look, 7, self);
	],
	compass_look false,
  has   scenery;

Object Compass "compass" has concealed;


#Ifndef WITHOUT_DIRECTIONS;

CompassDirection -> n_obj "pohjoi/nen" 
    with door_dir n_to, name 'p//' 'pohjoinen' 'pohjoi' 'pohjoiseen' 'pohjois',
    gen "sen", par "sta", ess "sena", ill "seen";
CompassDirection -> s_obj "etelä/"
		    with door_dir s_to, name 'e//' 'etelä' 'etelään'
    	    'eteläi',
	gen "n", par "ä", ess "nä", ill "stä";
CompassDirection -> e_obj "i/tä" 
		    with door_dir e_to, name 'i//' 'itä' 'itään' 'itäi',
	gen "dän", par "tää", ess "tänä", ill "tään";
CompassDirection -> w_obj "län/si"
		    with door_dir w_to, name 'l//' 'länsi' 'länt'
	    'länteen' 'länti',
	gen "nen", par "ttä", ess "tenä", ill "teen";
CompassDirection -> ne_obj "koilli/nen"
		    with door_dir ne_to, name 'ko' 'koillis' 'koillise',
	gen "sen", par "sta", ess "sena", ill "seen"; 
CompassDirection -> nw_obj "luo/de"
		    with door_dir nw_to, name 'lu' 'luode' 'luoteeseen' 'luote' 'luoteis',
	gen "teen", par "detta", ess "teena", ill "teeseen";
CompassDirection -> sw_obj "louna/s" 
		    with door_dir sw_to, name 'lo' 'lounaa' 'lounaaseen' 'lounais',
	gen "an", par "sta", ess "ana", ill "aseen";
CompassDirection -> se_obj "kaak/ko"
		    with door_dir se_to, name 'ka' 'kaakko' 'kaakkoon' 'kaakkois',
	gen "on", par "koa", ess "kona", ill "koon";

CompassDirection -> u_obj "ylös"
		    with door_dir u_to, name 'y//' 'ylös' 'ylhää' 'katto' 'kato' 'taivas';
CompassDirection -> d_obj "alas"
 with 	door_dir d_to, name 'a//' 'alas' 'alhaa' 'lattia' 'maa'
    	    'maa-taso';

#endif; ! WITHOUT_DIRECTIONS

CompassDirection -> in_obj "sisä/"
		    with door_dir in_to, name 'sisään' 'sisä'
	    'sisäpuole' 'sis',
	gen "n", par "ä", ess "nä", ill "än";
CompassDirection -> out_obj "ul/ko"
 with 	door_dir out_to, name 'ulos' 'ulkopuole',
	gen "on", par "koa", ess "kona", ill "os";

! ------------------------------------------------------------------------------
!   Part II.   Vocabulary
! ------------------------------------------------------------------------------

!!!# ongelmia "again"-, "oops"- ja "undo"-sanoista?

Constant AGAIN1__WD     = 'toista';
Constant AGAIN2__WD     = 'u';
Constant AGAIN3__WD     = 'uudestaan';
Constant OOPS1__WD      = 'oho';
Constant OOPS2__WD      = 'o//';
Constant OOPS3__WD      = 'hups';
Constant UNDO1__WD      = 'peru';
Constant UNDO2__WD      = 'peru';
Constant UNDO3__WD      = 'peru';

Constant ALL1__WD       = 'kaikki';
Constant ALL2__WD       = 'kaikkia';!¤
Constant ALL3__WD       = 'kumpaakin';!¤
Constant ALL4__WD       = 'molempia';!¤
Constant ALL5__WD       = 'molemmat';
Constant AND1__WD       = 'ja';
Constant AND2__WD       = 'ynnä';
Constant AND3__WD       = 'sekä';
Constant BUT1__WD       = 'paitsi';
Constant BUT2__WD       = 'paitsi';
Constant BUT3__WD       = 'mutta';
Constant ME1__WD        = 'minä';
Constant ME2__WD        = 'minu';
Constant ME3__WD        = 'itse';	
!Constant ME4__WD	= 'minut';	
Constant OF1__WD        = 'of';
Constant OF2__WD        = 'of';
Constant OF3__WD        = 'of';
Constant OF4__WD        = 'of';
Constant OTHER1__WD     = 'toinen';
Constant OTHER2__WD     = 'muu';
Constant OTHER3__WD     = 'eri';
Constant THEN1__WD      = 'sitten';
Constant THEN2__WD      = 'sitten';
Constant THEN3__WD      = 'sitten';

Constant NO1__WD        = 'e//';
Constant NO2__WD        = 'ei';
Constant NO3__WD        = 'en';
Constant YES1__WD       = 'k//';
Constant YES2__WD       = 'kyllä';
Constant YES3__WD       = 'ok';

Constant AMUSING__WD    = 'hupaisia';
Constant FULLSCORE1__WD = 'täydet';
Constant FULLSCORE2__WD = 'maksimi';
Constant QUIT1__WD      = 'q//';
Constant QUIT2__WD      = 'lopeta';
Constant RESTART__WD    = 'aloita';
Constant RESTORE__WD    = 'lataa';

Array LanguagePronouns table

  ! word        possible GNAs              connected
  !             to follow:                 to:
  !                 a     i
  !                 s  p  s  p
  !                 mfnmfnmfnmfn

  !     'it'      $$001000111000                NULL
  !     'him'     $$100000000000                NULL
  !     'her'     $$010000000000                NULL
  !     'them'    $$000111000111                NULL;

	'se'      $$001000111000                NULL
        'si'      $$001000111000                NULL
        'sii'     $$001000111000                NULL
	'hän'     $$110000000000                NULL
	'ne'      $$000001000001                NULL
	'nii'     $$000001000001                NULL
	'he'      $$000110000000                NULL;


Array LanguageDescriptors table

  ! word         possible GNAs   descriptor      connected
  !              to follow:      type:           to:
  !                a     i
  !                s  p  s  p
  !                mfnmfnmfnmfn

   'minun'       $$111111111111    POSSESS_PK           0
   'oma'         $$111000111000    POSSESS_PK           0
   'omat'        $$000111000111    POSSESS_PK           0
   'tämä'        $$111000111000    POSSESS_PK           0
   'nämä'        $$000111000111    POSSESS_PK           0
   'tuo'         $$111111111111    POSSESS_PK           1
   'nuo'         $$000111000111    POSSESS_PK           1
   'hänen'       $$111111111111    POSSESS_PK           'hän'
   'heidän'      $$111111111111    POSSESS_PK           'he'
   'sen'         $$111111111111    POSSESS_PK           'se'
   'niiden'      $$111111111111    POSSESS_PK           'ne'
  !'a'           $$111111111111    INDEFART_PK          NULL
   'joitain'     $$000111000111    INDEFART_PK          NULL
   'muutama'     $$000111000111    INDEFART_PK          NULL
   'valaistu'    $$111111111111    light                NULL
   'palava'      $$111111111111    light                NULL
   'sytytetty'   $$111111111111    light                NULL
   'sammunut'    $$111111111111    (-light)             NULL
   'palamaton'   $$111111111111    (-light)             NULL
   'sytyttämätön'$$111111111111    (-light)             NULL;

!    'my'      $$111111111111    POSSESS_PK      0
!    'this'    $$111111111111    POSSESS_PK      0
!    'these'   $$000111000111    POSSESS_PK      0
!    'that'    $$111111111111    POSSESS_PK      1
!    'those'   $$000111000111    POSSESS_PK      1
!    'his'     $$111111111111    POSSESS_PK      'him'
!    'her'     $$111111111111    POSSESS_PK      'her'
!    'their'   $$111111111111    POSSESS_PK      'them'
!    'its'     $$111111111111    POSSESS_PK      'it'
!    'the'     $$111111111111    DEFART_PK       NULL
!    'a//'     $$111000111000    INDEFART_PK     NULL
!    'an'      $$111000111000    INDEFART_PK     NULL
!    'some'    $$000111000111    INDEFART_PK     NULL
!    'lit'     $$111111111111    light           NULL
!    'lighted' $$111111111111    light           NULL
!    'unlit'   $$111111111111    (-light)        NULL;

Array LanguageNumbers table
    'yksi' 1 'kaksi' 2 'kolme' 3 'neljä' 4 'viisi' 5
    'kuusi' 6 'seitsemän' 7 'kahdeksan' 8 'yhdeksän' 9 'kymmenen' 10
    'yksitoista' 11 'kaksitoista' 12 'kolmetoista' 13 'neljätoista' 14
	'viisitoista' 15 'kuusitoista' 16 'seitsemäntoista' 17                                  
	'kahdeksantoista' 18 'yhdeksäntoista' 19 'kaksikymmentä' 20;

! --------------------------------------------------------------------
!   Part III.   translation... 
! --------------------------------------------------------------------

Include "puklu";	! parsimista ja tulostamista
Include "parsermfi";    ! "Parserm"-tiedoston muokatut rutiinit

! "arse parse"
[ LanguageToInformese;
]; 


! ------------------------------------------------------------------------------
!   Part IV.   Printing
! ------------------------------------------------------------------------------


Constant LanguageAnimateGender   = male;	!neuter
Constant LanguageInanimateGender = neuter;

Constant LanguageContractionForms = 1;     ! English has two:
					   ! 0 = starting with a consonant
					   ! 1 = starting with a vowel

[ LanguageContraction;! text;
    !if (text->0 == 'a' or 'e' or 'i' or 'o' or 'u'
	!	or 'A' or 'E' or 'I' or 'O' or 'U') return 1;
    return 0;
];


Array LanguageArticles -->


 !   Contraction form 0:     Contraction form 1:
 !   Cdef   Def    Indef     Cdef   Def    Indef

	"" "" "" ;      

		       !         a     i
		       !         s     p     s     p
       		       !         m f n m f n m f n m f n

Array LanguageGNAsToArticles --> 0 0 0 0 0 0 0 0 0 0 0 0;

[ LanguageDirection d;    
    switch (d) {
      n_to:    print "pohjoiseen";  
      s_to:    print "etelään";
      e_to:    print "itään";
      w_to:    print "länteen";
      ne_to:   print "koilliseen";
      nw_to:   print "luoteeseen";
      se_to:   print "kaakkoon";
      sw_to:   print "lounaaseen";
      u_to:    print "ylös";
      d_to:    print "alas";
      in_to:   print "sisään";
      out_to:  print "ulos";
      default: return RunTimeError(9,d);
    }
];

[ LanguageNumber n f;
    if (n == 0)    { print "nolla"; rfalse; }
    if (n < 0)     { print "miinus "; n = -n; }
    if (n >= 1000) { print (LanguageNumber) n/1000, " thousand"; n = n%1000; f = 1; }
    if (n >= 100)  {
	if (f == 1) print ", ";
	print (LanguageNumber) n/100, " sata"; n = n%100; f = 1;
    }
    if (n == 0) rfalse;
    #Ifdef DIALECT_US;
    if (f == 1) print " ";
    #Ifnot;
    if (f == 1) print " and ";
    #Endif;
    switch (n) {
      1:    print "yksi";
      2:    print "kaksi";
      3:    print "kolme";
      4:    print "neljä";
      5:    print "viisi";
      6:    print "kuusi";
      7:    print "seitsemän";
      8:    print "kahdeksan";
      9:    print "yhdeksän";
      10:   print "kymmenen";
      11:   print "yksitoista";
      12:   print "kaksitoista";
      13:   print "kolmetoista";
      14:   print "neljätoista";
      15:   print "viisitoista";
      16:   print "kuusitoista";
      17:   print "seitsemäntoista";
      18:   print "kahdeksantoista";
      19:   print "yhdeksäntoista";
      20 to 99: switch (n/10) {
	2:  print "kaksikymmentä";
	3:  print "kolmekymmentä";
	4:  print "neljäkymmentä";
	5:  print "viisikymmentä";
	6:  print "kuusikymmentä";
	7:  print "seitsemänkymmentä";
	8:  print "kahdeksankymmentä";
	9:  print "yhdeksänkymmentä";
	}
	!if (n%10 ~= 0) print "-", (LanguageNumber) n%10;
    }
];

[ LanguageTimeOfDay hours mins i;
    i = hours%12;
    if (i == 0) i = 12;
    if (i < 10) print " ";
    print i, ":", mins/10, mins%10;
    if ((hours/12) > 0) print " pm"; else print " am";
];

![ LanguageVerb i;
!    switch (i) {
!      'm//','inv','inventory':
!	       print "take inventory";
!      'k//':   print "katsoa";
!      't//':   print "tutkia";
!      'z//':   print "odottaa";
!      default: rfalse;
!    }
!    rtrue;
!];

! (ks. VerbDepot finng.h) 
[ LanguageVerb verbi obj;
    
    objectloop (obj in VerbDepot) {
	if (WordInProperty (verbi, obj, name))
	    
	{
	    ! "Risto, laita (jotain?)"
	    if (actor ~= player) {print (name) obj;
	    	rtrue;
	    }
	    ! "Laita (jotain?)"
	    else
	    PrintCapitalised(obj); rtrue;	    	    
	}
    	! jos ei VerbDepotissa, tulostetaan verbin osoite
	! pienellä alkukirjaimella... 
 	else if (actor ~= player) { print (address) verbi; rtrue;
	}
        ! isolla...
      	else {verbikap(verbi); rtrue;
	}	    	
    	rfalse;
  	
    }  
];



! tänne tullaan PrintCommandista (parsermfi.h)...
[ PrintKysNomini i from k taivuta; ! asia;

       
    !asia = asiayhteys; !(ks. PrintKysymys) 
    !asiayhteys = 0;
    
    
#ifdef DEBUG;
    if (parser_trace > 0) {
	print "^* PrintKysNomini: ", (address)verb_word, "! from ", from,
	    " k(ysymys) #", k, ", CaseIs ", CaseIs, ", csLR ", csLR, "*^";
	if (action_reversed) print "* action reversed! *^";
    }    
#endif;

    ! 1. nomini taipuu jos: "kysy -Ristolta- mitä?"
    if (k == 1 && action_to_be == ##Ask or ##AskFor)
	! && action_reversed == false?
    taivuta = true;
    
    ! 1. nomini ei lähtökohtaisesti taivu
    ! paitsi partitiivissa: "näytä -palloa- kenelle?"
    else if (k == 1 && action_reversed == false && CaseIs ~= csPar) 
	taivuta = false;
    ! k>2 TAI action_reversed TAI csPar 
    else taivuta = true;
    

	
		if (taivuta) 
             		switch (CaseIs) {
                 	 csNom: print (nominatiivi) i; ! +jos oletus_par?
             		csGen: print (genetiivi) i;
             		csPar: print (partitiivi) i; 
			csIne: print (inessiivi) i;
			csIll: print (illatiivi) i;
			csAde: print (adessiivi) i; 
			csAbl: print (ablatiivi) i;
			csAll: print (allatiivi) i; 
			csEss: print (essiivi) i;
			csTra: print (translatiivi) i;}
	
 		else print (the) i;

];


! [esim. lm misc 48 49]
! Alla ajatuksena se että jos pelaaja antaa vajaan komennon
! tarkentava kysymys johon pelaaja vastaa sijam.
!
!   >istu
!   (What do you want to sit on top of?)
!   Istu mihin?
!   >tuoliin/tuolille
!
! ("mihin" tulee action_to_be  ##Enter:istä)
	
! myös tänne tullaan PrintCommandista... k on sen laskuri
[ PrintKysymys verbi from k obj kys _kys asia;
    !   print "! from ", from, ", k ", k, ", etype ", etype, "^";

    ! asiayhteys on token
    ! adjudicate (context), sillä arvataan
    ! kysytäänkö esim. "mille"/"kenelle"

    asia = asiayhteys;
    asiayhteys = 0; ! ...nollataan se.
    
     
        
    kys = "mitä";
    _kys = 0;
    
    if (asia == CREATURE_TOKEN) !(== 6)
    	switch (csLR) 
    	{
	    !Nominatiivi 1: Risto
     	 1: _kys = "kenet";!kuka?	
	    !Genetiivi   2: Riston
     	 2: _kys = "kenen";!kenet?
   	    !Partitiivi  3: Ristoa
     	 3: _kys = "ketä";
	    !Inessiivi   4: Ristossa
     	 4: _kys = "kenessä";
      	    !Elatiivi    5: Ristosta
     	 5: _kys = "kenestä";
   	    !Illatiivi   6: Ristoon
     	 6: _kys = "keneen";
      	    !Adessiivi   7: Ristolla
     	 7: _kys = "kenellä";
   	    !Ablatiivi   8: Ristolta
     	 8: _kys = "keneltä";
   	    !Allatiivi   9: Ristolle
     	 9: _kys = "kenelle";
   	    !Essiivi    10: Ristona
     	 10: _kys = "kenenä";
      	    !Translat.  11: Ristoksi
     	 11: _kys = "keneksi";
   	    
    	}
    
    else
    	switch (csLR) 
    	{
	    !Nominatiivi 1: pallo
     	 1: _kys = "mikä";	
	    !Genetiivi   2: pallon
     	 2: _kys = "minkä";
   	    !Partitiivi  3: palloa
     	 3: _kys = "mitä";
	    !Inessiivi   4: pallossa
     	 4: _kys = "missä";
      	    !Elatiivi    5: pallosta
     	 5: _kys = "mistä";
   	    !Illatiivi   6: palloon
     	 6: _kys = "mihin";
      	    !Adessiivi   7: pallolla
     	 7: _kys = "millä";
   	    !Ablatiivi   8: pallolta
     	 8: _kys = "miltä";
   	    !Allatiivi   9: pallolle
     	 9: _kys = "mille";
   	    !Essiivi    10: pallona
     	 10: _kys = "minä";
      	    !Translat.  11: palloksi
     	 11: _kys = "miksi";
   	    
    	}
    
    
    
    
    if (action_to_be == ##Fill or ##Unlock or ##Open or ##Take)
	kys = "mikä";
    ! kysy keneltä mitä -- taivuta (poikkeus on sääntö?)
    ! kysy ristolta mitä
    if (action_to_be == ##Ask or ##AskFor)
    { kys = "keneltä"; _kys = "mitä"; }
    
    ! "näytä mitä (oletus) kenelle (_kys)"  
    if (action_to_be == ##Show or ##Give) _kys = "kenelle";
    if (action_to_be == ##Lock or ##Unlock) _kys = "millä";
    ! esim. laita mitä - mihin?
    !    if (action_to_be == ##Insert) _kys = "mihin";
    
    ! VerbDepotissa voi antaa omat kys_a ja kys_b
    ! esim.
    !   Object "kysy" VerbDepot with name 'kysy',
    !   kys_a "keneltäkö", kys_b "mittee";
    !	
    !    >kysy
    !     Kysy keneltäkö?
    !
    !     >kysy ristolta
    !     Kysy Ristolta mittee?
    
    objectloop (obj in VerbDepot) 
    { if (WordInProperty (verbi, obj, name))
    {
	if (obj provides kys_a) kys = obj.kys_a;
	if (obj provides kys_b) _kys = obj.kys_b;
    }	
    }
    
#ifdef DEBUG;
    if (parser_trace > 0) {
	print "^* PrintKysymys: ", (address)verbi, "! (asia=", asia,
	    ") from ", from, ", k(ysymys) #", k, ", CaseIs ", CaseIs,
	    ", csLR ", csLR, " *^";
	if (action_reversed) print "* action reversed! *^";
    }    
#endif;        
    
    
    
    print " ";
    ! ok, ao. on aikamoista suttua
    !
    ! jos kysymys #2, eikä käänteinen järjestys
    ! (action_reversed), tulosta _kys,
    ! muuten tulosta kys (taivuta, ks. PrintKysNomini)
    !
    ! -- _kys täytyy olla merkkijono 
    !
    ! ehto: asia == NOUN_TOKEN on nolla,
    ! jotta esim.
    ! ">istu" ei vastaisi "istu mitä?" vaan "istu mille?"
    if (k > 1 && _kys > 0 && action_reversed == false)
       	! print "*A* ", (string)_kys;
       	print (string)_kys;
    else if (asia == NOUN_TOKEN && from == k && _kys > 0)
       	! print "*B* ", (string)_kys;
       	print (string)_kys;
    else
       	! print "*C* ", (string)kys;
        print (string)kys;
    rtrue;
    
      
];


! Anything (ks. DM4) debug verbi 'sijat')

[ Anything i;
    
    if (scope_stage == 1) rfalse;
    if (scope_stage == 2) 
    {
	objectloop (i ofclass Object) PlaceInScope(i);
	rtrue;
	
    }
    "Sitä ei löydy pelistä.";
    
];	

! debug-verbi sijat

[ PrintSijatSub;
  print ! "^Artikkeli?:", (a) x1,
  	"^Oletus (name): ", (name) noun,
  	"^Nominatiivi 1: ", (nominatiivi) noun, 
  	"^Partitiivi  3: ", (partitiivi) noun, 
  	"^Genetiivi   2: ", (genetiivi) noun, 
  	"^Essiivi    10: ", (essiivi) noun,	
  	"^Inessiivi   4: ", (inessiivi) noun, 
  	"^Illatiivi   6: ", (illatiivi) noun, 
  	"^Elatiivi    5: ", (elatiivi) noun, 
  	"^Adessiivi   7: ", (adessiivi) noun, 
  	"^Ablatiivi   8: ", (ablatiivi) noun,
	"^Translat.  11: ", (translatiivi) noun, 
	"^Allatiivi   9: ", (allatiivi) noun, "^";

 ];


! ----------------------------------------------------------------------------
!  LanguageVerbIsDebugging is called by SearchScope.  It should return true
!  if word w is a debugging verb which needs all objects to be in scope.
! ----------------------------------------------------------------------------

#Ifdef DEBUG;
[ LanguageVerbIsDebugging w;
    if (w == 'purloin' or 'tree' or 'abstract'
		       or 'gonear' or 'scope' or 'showobj')
	rtrue;
    rfalse;
];
#Endif;

! ----------------------------------------------------------------------------
!  LanguageVerbLikesAdverb is called by PrintCommand when printing an UPTO_PE
!  error or an inference message.  Words which are intransitive verbs, i.e.,
!  which require a direction name as an adverb ('walk west'), not a noun
!  ('I only understood you as far as wanting to touch /the/ ground'), should
!  cause the routine to return true.
! ----------------------------------------------------------------------------

[ LanguageVerbLikesAdverb w;
    if (w == 'katso' or 'mene' or 'työnnä' or 'kävele') !!!???
	rtrue;
    rfalse;
];

! ----------------------------------------------------------------------------
!  LanguageVerbMayBeName is called by NounDomain when dealing with the
!  player's reply to a "Which do you mean, the short stick or the long
!  stick?" prompt from the parser. If the reply is another verb (for example,
!  LOOK) then then previous ambiguous command is discarded /unless/
!  it is one of these words which could be both a verb /and/ an
!  adjective in a 'name' property.
! ----------------------------------------------------------------------------

[ LanguageVerbMayBeName w;
    if (w == 'pitkä' or 'lyhyt' or 'vakio'
		    or 'laaja' or 'normaali' or 'perus')
	rtrue;
    rfalse;
];

Constant NKEY__TX       = "S) seuraava aihe";
Constant PKEY__TX       = "E) edellinen";
Constant QKEY1__TX      = "P) palaa peliin";
Constant QKEY2__TX      = "P) palaa aiempaan";
Constant RKEY__TX       = "RETURN = lue aiheesta";

Constant NKEY1__KY      = 'S';
Constant NKEY2__KY      = 's';
Constant PKEY1__KY      = 'E';
Constant PKEY2__KY      = 'e';
Constant QKEY1__KY      = 'P';
Constant QKEY2__KY      = 'p';

Constant SCORE__TX      = "Pisteet: ";
Constant MOVES__TX      = "Vuorot: ";
Constant TIME__TX       = "Aika: ";
Constant CANTGO__TX     = "Et pääse siihen suuntaan.";
Constant FORMER__TX     = "aiempi itsesi";
Constant YOURSELF__TX   = "sinä itse"; !! pitäisi taivuttaa
!Constant ITSELLESI__TX  = "itsellesi";
!Constant ITSEASI_TX     = "itseäsi";
Constant YOU__TX        = "Sinä"; !! kuin myös?
Constant DARKNESS__TX   = "Pimeys";

Constant THOSET__TX     = "niitä"; 
Constant THAT__TX       = "tuo";       
Constant OR__TX         = " vai ";   
Constant TAI__TX        = " tai ";      
Constant NOTHING__TX    = "ei mitään"; 
Constant IS__TX         = " on";   
Constant ARE__TX        = " ovat";  
Constant IS2__TX        = "on ";   
Constant ARE2__TX       = "on ";                     
Constant AND__TX        = " ja "; 
Constant WHOM__TX       = "joka ";     
Constant WHICH__TX      = " "; ! ks. ListMiscellany 19 ja 21 ("jossa" vai "jolla")   
Constant COMMA__TX      = ", ";

!! huono: ao. pitäisi olla "tai" eikä "ja" - esim. "SitataiNiita"

! ThatorThose (partitiivi)
[ SitajaNiita obj;      ! Used in the accusative
    if (obj == player)      { print "itseäsi"; return; }
    if (obj has pluralname)       { print "niitä"; return; }
    if (obj has animate) {
	if (obj has female)       { print "häntä"; return; }
	else
	    if (obj hasnt neuter) { print "häntä"; return; }
    }
    print "sitä";
];

! ItorThem
[ SejaNe obj;
    if (obj == player)      { print "sinä"; return; }
    if (obj has pluralname)       { print "ne"; return; }
    if (obj has animate) {
	if (obj has female)       { print "hän"; return; }
	else
	    if (obj hasnt neuter) { print "hän"; return; }
    }
    print "se";
];


! IsorAre

![ OnjaOvat obj;
!    if (obj has pluralname && obj hasnt oletus_par) print "ovat"; else print "on";
!	if (object == player) print "olet"; ];						

[ OnjaOvat obj;
    if (obj has pluralname) print "ovat"; else print "on";
	if (object == player) print "olet"; ];

[ EijaEivat obj;
	if (obj has pluralname) print "eivät"; else print "ei";
	if (object == player) print "et"; ];

[ JokajaJotka obj;
	if (obj has pluralname) print "jotka"; else print "joka"; ];

[ SenjaNiiden obj;
if (obj == player)      { print "sinun"; return; }
    if (obj has pluralname)       { print "niiden"; return; }
    if (obj has animate) {
	if (obj has female)       { print "hänen"; return; }
	else
	    if (obj hasnt neuter) { print "hänen"; return; }
    }
    print "sen";
];

! Heidän / heihin jne..

[ SiihenjaNiihin obj;
if (obj == player)      { print "itseesi"; return; }
    if (obj has pluralname)       { print "niihin"; return; }
    if (obj has animate) {
	if (obj has female)       { print "häneen"; return; }
	else
	    if (obj hasnt neuter) { print "häneen"; return; }
    }
    print "siihen";
];

[ Sentaine obj;
if (obj == player)      { print "itsesi"; return; }
    if (obj has pluralname)       { print "ne"; return; }
    if (obj has animate) {
	if (obj has female)       { print "hänet"; return; }
	else
	    if (obj hasnt neuter) { print "hänet"; return; }
    }
    print "sen";
];

! Samat isolla alkukirjaimella...

[ KSitajaNiita obj;      ! Used in the accusative
    if (obj == player)      { print "Itseäsi"; return; }
    if (obj has pluralname)       { print "Niitä"; return; }
    if (obj has animate) {
	if (obj has female)       { print "Häntä"; return; }
	else
	    if (obj hasnt neuter) { print "Häntä"; return; }
    }
    print "Sitä";
];

! ItorThem
[ KSejaNe obj;
    if (obj == player)      { print "Sinä"; return; }
    if (obj has pluralname)       { print "Ne"; return; }
    if (obj has animate) {
	if (obj has female)       { print "Hän"; return; }
	else
	    if (obj hasnt neuter) { print "Hän"; return; }
    }
    print "Se";
];

[ KSenjaNiiden obj;
if (obj == player)      { print "Sinun"; return; }
    if (obj has pluralname)       { print "Niiden"; return; }
    if (obj has animate) {
	if (obj has female)       { print "Hänen"; return; }
	else
	    if (obj hasnt neuter) { print "Hänen"; return; }
    }
    print "Sen";
];

[ KSiihenjaNiihin obj;
if (obj == player)      { print "Itseesi"; return; }
    if (obj has pluralname)       { print "Niihin"; return; }
    if (obj has animate) {
	if (obj has female)       { print "Häneen"; return; }
	else
	    if (obj hasnt neuter) { print "Häneen"; return; }
    }
    print "Siihen";
];

! monikko_vai_yks
!
! "laitat munan laatikkoon" (yksikkö taipuu genetiivinä)
! "laitat munat laatikkoon" (monikko ei...)
! - pitäisikö olla vielä partitiivi? "laitat munaa laatikkoon?"

[ monikko_vai_yks obj;  
	
	 if (obj has pluralname)
		 CCase (obj, csNom, false);  
	 else
		 CCase (obj, csGen, false);  
];

! [ oletussija obj;	CCase (obj, csOff, false); ];

[ nominatiivi obj;  	CCase (obj, csNom, false); ];

[ genetiivi obj; 	CCase (obj, csGen, false); ];

[ partitiivi obj; 	CCase (obj, csPar, false); ];

[ essiivi obj; 		CCase (obj, csEss, false); ];

[ inessiivi obj;	CCase (obj, csIne, false); ];

[ illatiivi obj; 	CCase (obj, csIll, false); ];

[ elatiivi obj;		CCase (obj, csEla, false); ];

[ adessiivi obj;	CCase (obj, csAde, false); ];

[ ablatiivi obj;	CCase (obj, csAbl, false); ];

[ translatiivi obj; 	CCase (obj, csTra, false); ];

[ allatiivi obj;	CCase (obj, csAll, false); ];


!isot alkukirjaimet 

[ k_nominatiivi obj;  	CCase (obj, csNom, true); ];

[ k_genetiivi obj; 	CCase (obj, csGen, true); ];

[ k_partitiivi obj; 	CCase (obj, csPar, true); ];

[ k_essiivi obj; 	CCase (obj, csEss, true); ];

[ k_inessiivi obj;	CCase (obj, csIne, true); ];

[ k_illatiivi obj; 	CCase (obj, csIll, true); ];

[ k_elatiivi obj;	CCase (obj, csEla, true); ];

[ k_adessiivi obj;	CCase (obj, csAde, true); ];

[ k_ablatiivi obj;	CCase (obj, csAbl, true); ];

[ k_translatiivi obj; 	CCase (obj, csTra, true); ];

[ k_allatiivi obj;	CCase (obj, csAll, true); ];

!! verbilöitä

!Constant vbImp = 21; !pue
!Constant vbInf = 22; !pukea
!Constant vbY1 =	23; !puen
!Constant vbY2 =	24; !puet
!Constant vbY3 =	25; !pukee
!Constant vbM1 =	26; !puemme
!Constant vbM2 =	27; !puette
!Constant vbM3 =	28; !pukevat 

[ imp obj;	CCase (obj, name, false); ];
![ ind obj;	CCase (obj, vbInd, false); ];
![ inf obj;	CCase (obj, vbInf, false); ];
![ Y1 obj;	CCase (obj, vbY1, false); ];
![ Y2 obj;	CCase (obj, vbY2, false); ];
![ Y3 obj;	CCase (obj, vbY3, false); ];
![ M1 obj;	CCase (obj, vbM1, false); ];
![ M2 obj;	CCase (obj, vbM2, false); ];
![ M3 obj;	CCase (obj, vbM3, false); ];

[ k_imp obj;	CCase (obj, name, true); ];
![ k_ind obj;	CCase (obj, vbInd, true); ];
![ k_inf obj;	CCase (obj, vbInf, true); ];
![ k_Y1 obj;	CCase (obj, vbY1, true); ];
![ k_Y2 obj;	CCase (obj, vbY2, true); ];
![ k_Y3 obj;	CCase (obj, vbY3, true); ];
![ k_M1 obj;	CCase (obj, vbM1, true); ];
![ k_M2 obj;	CCase (obj, vbM2, true); ];
![ k_M3 obj;	CCase (obj, vbM3, true); ];



!--------------------------------------------------------------

[ LanguageLM n x1;


  Answer, Ask:  "Ei vastausta.";
  Attack:   "Väkivalta ei ole ratkaisu tähän.";
  Blow:     print (KSitajaNiita) x1, " ei voi kunnolla puhaltaa.";
  Burn:     "Siitä ei seuraisi mitään hyvää.";       
  Buy:      "Mitään ei ole myytävänä.";
  Climb:    "Luulen ettet saavuttaisi mitään.";
  Close: switch (n) {
	1:  print_ret (ksitajaniita) x1, " ei voi sulkea.";
	2:  print_ret (ksejane) x1, " ", (onjaovat) x1, " jo kiinni.";
	3:  "Suljet ", (genetiivi) x1, ".";
    }
  CommandsOff: switch (n) {
	1: "[Komentotallennus pois päältä.]";
	#Ifdef TARGET_GLULX;
	2: "[Komentotallennus on jo pois päältä.]";
	#Endif; ! TARGET_
    }
  CommandsOn: switch (n) {
	1: "[Command recording on.]";
	#Ifdef TARGET_GLULX;
	2: "[Commands are currently replaying.]";
	3: "[Command recording already on.]";
	4: "[Command recording failed.]";
	#Endif; ! TARGET_
    }
  CommandsRead: switch (n) {
	1: "[Replaying commands.]";
	#Ifdef TARGET_GLULX;
	2: "[Commands are already replaying.]";
	3: "[Command replay failed.  Command recording is on.]";
	4: "[Command replay failed.]";
	5: "[Command replay complete.]";
	#Endif; ! TARGET_
    }

  Consult:  "Et löydä mitään kiinnostavaa ", (elatiivi) x1, ".";
  Cut:      print_ret (ksenjaniiden) x1, " katkaisemisesta olisi hyvin vähän hyötyä.";
  Dig:      "Kaivaminen on tässä ihan turhaa.";
  Disrobe: switch (n) {
	1:  print (ksejane) x1, " ", (eijaeivat) x1, " ole päälläsi.";
	2:  "Riisut ", (genetiivi) x1, ".";
    }
  Drink:    "Täällä ei ole mitään juotavaksi kelpaavaa.";
  Drop: switch (n) {
	1:  if (x1 has pluralname) print (nominatiivi) x1, " ovat "; else print (nominatiivi) x1, " on ";
		"jo täällä.";
	2:  "Sinulla ei ole ", (SitajaNiita) x1, ".";
	3:  "(Ensin riisuen ", (genetiivi) x1, ")";  
	4:  "Pudotettu.";
    }
  Eat: switch (n) {
	1:  print_ret (ksejane) x1, " ", (eijaeivat) x1, " selvästi kelpaa syötäväksi.";
	2:  "Syöt ", (genetiivi) x1, ". Ei hullumpaa.";
    }
  EmptyT: switch (n) {
	1:  print_ret (k_nominatiivi) x1, " ", (eijaeivat) x1, " toimi säiliönä.";
	2:  print_ret (k_nominatiivi) x1, " ", (OnjaOvat) x1, " kiinni.";
	3:  if (x1 has pluralname) print_ret (k_nominatiivi) x1, " on jo tyhjiä."; 
		else print_ret (k_nominatiivi) x1, " on jo tyhjä.";
	4:  "Se tuskin tyhjentäisi mitään.";
    }

  Enter: switch (n) {
	1:  print "Mutta sinä olet jo "; if (x1 has supporter) print (adessiivi) x1, "."; else print (inessiivi) x1; ".";
	2:  if ((verb_word) ~= 'seiso' or 'istu' or 'makaa') 
		{if (x1 has pluralname) print "Niihin ei "; else print "Sinne ei ";}
	    else if (x1 has pluralname) print "Niissä ei "; else print "Siinä ei ";
	    switch (verb_word) {
	      'seiso':  "voi seistä.";
	      'istu':   "voi istua.";
	      'makaa':  "voi maata.";
	      default:  "voi mennä.";
	    }
	3:  "Et pääse suljettuun ", (illatiivi) x1, "."; ! entä monikko?
	4:  "Voit mennä vain johonkin mikä on vapaana.";
	5:  print "Menet "; if (x1 has supporter) print (allatiivi) x1;
		else print (illatiivi) x1; print_ret ".";
	6:  print "(poistut ", (genetiivi) x1;
	    if (x1 has supporter) print " päältä)"; else print " sisältä)";
	7:  if (x1 has supporter) print "(menet ", (genetiivi) x1, " päälle)^";
	    if (x1 has container) print "(menet ", (genetiivi) x1, " sisään)^";
	    print "(menet ", (genetiivi) x1, "sisään)^";
    }
  Examine: switch (n) {
	1:  "Pimeys, subst. Valonpuutteesta johtuva pimeä tilanne."; !! etsi sanakirjaselitys?
	!!! "Darkness, noun.  An absence of light to see by.";
	2:  "Et näe mitään erikoista ", (inessiivi) x1, "."; 

	3:  print (nominatiivi) x1, " on tällä hetkellä kytketty ";
	    if (x1 has on) "päälle."; else "pois päältä.";
    }
  Exit: switch (n) {
	1:  "Et ole minkään sisällä juuri nyt.";
	2:  "Et pääse pois suljetusta ", (elatiivi) x1, ".";
	3:  print "Poistut ";
	    if (x1 has supporter) print (ablatiivi) x1; else print (elatiivi) x1;
	    print_ret ".";
	4:  print "Mutta et ole ";
	    if (x1 has supporter) print (adessiivi) x1, " juuri nyt.";
	    else print (inessiivi) x1, " juuri nyt.";
    }
  Fill:     "Mutta täällä ei ole vettä kannettavaksi.";
  FullScore: switch (n) {
	1:  if (deadflag) print "Sait pisteitä "; else print "Olet saanut pisteitä ";
	    "seuraavasti:^";
	2:  "erilaisten esineiden löytämisestä";
	3:  "erilaisissa paikoissa käymisestä";
	4:  print "yhteensä (maksimista ", MAX_SCORE; ")";
    }
  GetOff:   "Mutta et ole ", (adessiivi) x1, " juuri nyt.";
  Give: switch (n) {
	1:  "Et pitele ", (partitiivi) x1, ".";
	2:  "Temppuilet hetken ", (genetiivi) x1, " kanssa, muttet saa paljon aikaan.";
	3:  print (k_nominatiivi) x1;
	    if (x1 has pluralname) print " eivät"; else print " ei";
	    " tunnu kiinnostuvan.";
    }
  Go: switch (n) {
	1:  print "Sinun täytyy ensin poistua ";
	    if (x1 has supporter) print_ret (ablatiivi) x1, "."; else print_ret (elatiivi) x1, ".";
	2:  print_ret (string) CANTGO__TX;   ! "You can't go that way."
	3:  "Et voi kiivetä ", (partitiivi) x1, ".";
	4:  "Et voi mennä alas ", (elatiivi) x1, "."; !?
	5:  "Et pääse, sillä ", (nominatiivi) x1, " ", (OnjaOvat) x1, " tielläsi.";
	6:  print "Et voi, sillä ", (nominatiivi) x1, " ", (eijaeivat) x1;
	    print " johda mihinkään.";
    }
  Insert: switch (n) {
	1:  "Sinun täytyy pidellä ", (partitiivi) x1, " ennenkuin voit laittaa ", (sitajaniita) x1,
	    " mihinkään.";
	2:  print_ret (ksejane) x1, " ", (eijaeivat) x1, " toimi säiliönä.";
	3:  print (k_nominatiivi) x1; if (x1 has pluralname) print
	    " ovat suljetut.^";	else print " on kiinni.^";
	4:  "Sinun täytyy ensin ottaa ", (sejane) x1, " pois.";
	5:  "Et voi laittaa sitä itsensä sisään.";
	6:  "(ottamalla ", (sentaine) x1, " ensin)^";
	7:  print_ret (k_inessiivi) x1, " ei ole enää tilaa.";   
	8:  "Tehty.";
	9:  "Laitat ", (monikko_vai_yks) x1, " ", (illatiivi) second, ".";
    }
  Inv: switch (n) {
	1:  "Sinulla ei ole mukanasi mitään.";
	2:  print "Mukanasi on";
	3:  print ":^";
	4:  print ".^";
    }
  Jump:     "Hyppäät paikallasi, mutta se on hyödytöntä.";     ! KÖKÖHKÖ
  JumpOver,Tie:
	    "Et saavuttaisi sillä mitään.";
  Kiss:     "Yritä keskittyä seikkailuun."; !vapaa käännös
  Listen:   "Et kuule mitään outoa.";
  !Listen: PrintCommand();
 ListMiscellany: switch (n) {
	1:  print " (valaisee)";
	2:  print " (", (jokajajotka) x1, " on suljettu)";
	3:  print " (suljettu ja valaisee)";
	4:  print " (", (jokajajotka) x1, " ", (OnjaOvat) x1; if (x1 has pluralname) print " tyhjiä)"; else print " tyhjä)";
	5:  print " (tyhjä ja valaisee)";
	6:  print " (", (jokajajotka) x1, " on suljettu ja "; if (x1 has pluralname) print "tyhjiä)"; else print " tyhjä)";
	7:  print " (suljettu, tyhjä ja valaisee)";
	8:  print " (valaisee ja on puettu";
	9:  print " (valaisee";
	10: print " (puettu";
	11: print " (", (jokajajotka) x1, " on ";               !?! ON/OVAT
	12: print "auki";
	13: print "auki mutta "; if (x1 has pluralname) print "tyhjiä"; else print "tyhjä";
	14: print "kiinni";
	15: print "kiinni ja lukossa";
	16: print " ja "; if (x1 has pluralname) print "tyhjiä"; else print "tyhjä";
	17: print " (", (jokajajotka) x1, " ", (onjaovat) x1; if (x1 has pluralname) print " tyhjiä)"; else print " tyhjä)";
	18: print " sisältää ";         ! sisältää/sisältävät?
	19: print " (jonka päällä ";                 ! taso
	20: print ", jonka päällä ";
	21: print " (jossa";                 ! säiliö
	22: print ", sisällä ";
    }
  LMode1:   " is now in its normal ~brief~ printing mode, which gives long descriptions
	     of places never before visited and short descriptions otherwise.";
  LMode2:   " is now in its ~verbose~ mode, which always gives long descriptions
	     of locations (even if you've been there before).";
  LMode3:   " is now in its ~superbrief~ mode, which always gives short descriptions
	     of locations (even if you haven't been there before).";
  Lock: switch (n) {
	1:  if (x1 has pluralname) print "Ne eivät "; else print "Se ei ";
	    "tunnu olevan lukittavissa.";
	2:  print_ret (ksejane) x1, " ", (onjaovat) x1, " lukossa
	    tällä hetkellä.";
	3:  "Sinun täytyy ensin sulkea ", (nominatiivi) x1, ".";
	4:  if (x1 has pluralname) print "Ne eivät "; else print "Se ei ";
	    "tunnu sopivan lukkoon.";
	5:  "Lukitset ", (genetiivi) x1, ".";
    }
  Look: switch (n) {
	1:  print " (", (adessiivi) x1, ")";
	2:  print " (", (inessiivi) x1, ")";
	3:  print " (", (essiivi) x1, ")";    
	4:  print "^", (k_adessiivi) x1, " on ";                 
	    WriteListFrom(child(x1),

ENGLISH_BIT+RECURSE_BIT+PARTINV_BIT+TERSE_BIT+CONCEAL_BIT);  !+ISARE_BIT);
	    ".";
!       5,6:
!           if (x1 ~= location) {
!               if (x1 has supporter) print "^On "; else print "^In ";
!               print (the) x1, " you";
!           }
!           else print "^Sinä";
!           print " voit ";
!           if (n == 5) print "myös ";         !Tämä täytyy järpätä
!           print "nähdä ";
!           WriteListFrom(child(x1), !ENGLISH_BIT+RECURSE_BIT+PARTINV_BIT+TERSE_BIT+CONCEAL_BIT+WORKFLAG_BIT);
!           if (x1 ~= location) "."; else " täällä.";


	5,6:
	    if (x1 ~= location) {
		print "^"; if (x1 has supporter) print (k_adessiivi) x1; else print (k_inessiivi) x1;
	    }
	    else print "^Täällä";
	    print " on ";
	    if (n == 5) print "myös ";
	    WriteListFrom(child(x1), ENGLISH_BIT+RECURSE_BIT+PARTINV_BIT+TERSE_BIT+CONCEAL_BIT+WORKFLAG_BIT);
	    ".";


	7:  "Et näe siellä mitään yllättävää.";
    }
  LookUnder: switch (n) {
	1:  "On liian pimeää.";
	2:  "Et löydä mitään mielenkiintoista.";
    }
  Mild:     "Älä muuta sano.";
  Miscellany: switch (n) {
	1:  "(huomioiden vain ensimmäiset kuusitoista)^";
	2:  "Ei mitään tekemistä!";
	3:  print " Olet kuollut ";
	4:  print " Olet voittanut ";
	5:  print "^ALOITAtko alusta, LATAAtko tallennuksen";
	    #Ifdef DEATH_MENTION_UNDO;
	    print ", PERUtko viimeisen siirtosi";
	    #Endif;
	    if (TASKS_PROVIDED == 0) print ", katsotko tämän pelin MAKSIMIpisteet";
	    if (deadflag == 2 && AMUSING_PROVIDED == 0)
		print ", haluatko HUPAISIA ehdotuksia";
	    " vai LOPETAtko?";
	6:  "[Tulkissasi ei ole ~peruutustoimintoa~, pahoittelut!]";
	    #Ifdef TARGET_ZCODE;
	7:  "~Peruminen~ epäonnistui.  [Kaikki tulkit eivät tue toimintoa.]";
	    #Ifnot; ! TARGET_GLULX
	7:  "[Et voi ~perua~ pidemmälle.]";
	    #Endif; ! TARGET_
	8:  "Anna jokin ylläolevista vastauksista, kiitos.";
	9:  "^Täällä on nyt pilkkopimeää!";
	10: "Anteeksi?"; ! "I beg your pardon?"
	11: "[Et voi ~perua~ sitä mitä ei ole tehty!]";
	12: "[Kahdesti peräkkäin ei voi ~perua~, sori!]";
	13: "[Aiempi vuoro peruttu.]";
	14: "Sori, sitä ei voi oikaista.";
	15: "Eipä kestä.";
	16: "~Hups~ korjaa vain yhden sanan.";
	17: "On pilkkopimeää, et näe mitään.";
	18: print "sinä itse";                          ! Missä yhtydessä???
	19: "Aina yhtä hauskannäköinen.";
	20: "Toistaaksesi komennon ~sammakko, loikkaa~, sano vain ~toista~ (tai ~u~), ei: ~sammakko, uudestaan~.";
	21: "Sitä ei voi toistaa.";
	22: "Et voi aloittaa pilkulla.";
	23: "Haluat ilmeisesti puhua jonkun kanssa, mutta ketään ei näy.";
! genetiivi
	24: "Et voi puhua ", (genetiivi) x1, " kanssa.";
	25: "Puhuaksesi jollekulle, kokeile ~joku, terve~ tms.";
	26: "(ottamalla ensin ", (monikko_vai_yks) not_holding, ")";
	27: "En käsittänyt tuota lausetta.";
	28: print "Ymmärsin vain: ";
	29: "En käsittänyt tuota numeroa.";
	30: "Et näe mitään sellaista.";
	31: "Kenties et sanonut tarpeeksi!";
	32: "Et pitele sitä!";
	33: "Tuota verbiä ei voi käyttää samanaikaisesti eri kohteisiin.";     
	34: "Voit käyttää montaa esinettä vain kerran rivillä.";       ! Kökköä
	35: "En ole varma mihin ~", (address) pronoun_word, "~ viittaa.";
! Mihin/keneen/keihin?
	36: "Paitsisi koski jotain mitä ei ole!";                      ! ???
	37: "Voit tehdä niin vain jollekin elolliselle.";
	    #Ifdef DIALECT_US;
	38: "En tunne tuota verbiä.";
	    #Ifnot;
	38: "En tunne tuota verbiä.";
	    #Endif;
	39: "Siihen ei tarvitse viitata tässä pelissä.";
! (Siihen/niihin) ??? sitä sanaa ei tarvita tässä pelissä?
	40: "Et näe ~", (address) pronoun_word, "~ (", (the) pronoun_obj,
	    ") tällä hetkellä.";
!? Sitä/niitä/häntä/heitä ? (partitiivi)
	41: "En käsittänyt miten tuo loppui.";
	42: if (x1 == 0) print "Ainuttakaan "; else print "Vain ", (number) x1;
	   ! print " niistä ";
	    if (x1 == 0) print "ei ole "; else print "on";
	    " saatavilla.";
	43: "Ei mitään tekemistä!";
	44: "Ainuttakaan ei ole saatavilla!";
	
	45: ;!print "Tarkoitatko "; ks. parsermfi, r. 1560+
	46: ;!print "Tarkoitatko "; ks. parsermfi, r. 1560+
	47: "Tällä kertaa voit ottaa vain yhden, minkä?";

!       48: print "Whom do you want";
!           if (actor ~= player) print " ", (the) actor;
        48: ! print "Ketä haluat ";
           if (actor ~= player) {print (The) actor;
           print " ";} PrintCommand(); print "?^";
!       49: print "What do you want";
!            if (actor ~= player) print " ", (the) actor;
!            print " to "; PrintCommand(); print "?^";

   49: if (actor ~= player) print (The) actor, ", ";
      PrintCommand(); print "?^";
      
	50: !print "Pisteesi ovat juuri ";
	    print ""; if (x1 > 0) print "Sait juuri "; else { x1 = -x1; print "Menetit juuri "; }
	     if (x1 == 1) print "pisteen"; else print (number) x1, " pistettä";           
	51: "(Koska jotain yllättävää tapahtui, .)";
	52: "^Anna luku väliltä 1 - ", x1, ", 0 -to redisplay- tai paina ENTER.";
	53: "^[Paina välilyöntiä.]";
	54: "[Kommentti talletettu.]";
	55: "[Kommentti EI tallentunut.]";  
	56: print ".^";
	57: print "?^";
	
    }
  No,Yes:   "Se oli retorinen kysymys.";
  NotifyOff:
	    "Pistenäyttö pois päältä.";
  NotifyOn: "Pistenäyttö päällä.";
  Objects: switch (n) {
	1:  "Esineet joita olet käsitellyt:^";
	2:  "-Et mitään-.";
	3:  print "   (puettu)";
	4:  print "   (pidelty)";
	5:  print "   (annettu pois)";
	6:  print "   (", (name) x1, " sisällä)";                   
	7:  print "   (", (genetiivi) x1, " sisällä)";                 
	8:  print "   (", (inessiivi) x1, ")";
	9:  print "   (", (adessiivi) x1, ")"; 
	10: print "   (poissa)";
    }
  Open: switch (n) {
	1:  print_ret (ksitajaniita) x1, " ei voi avata.";
	2:  if (x1 has pluralname) print "Ne tuntuvat"; else print "Se tuntuu"; " olevan lukossa.";
	3:  print_ret (ksejane) x1, " ", (onjaovat) x1, " jo auki.";
	4:  print "Avaat ", (genetiivi) x1; if (x1 has pluralname) print ". Niissä"; else print ". Siellä";
	 
	 ! ISARE_BIT added, Writelistfrom in verblib modified (-> are__tx)
	 if (WriteListFrom(child(x1), ISARE_BIT+ENGLISH_BIT+TERSE_BIT+CONCEAL_BIT) == 0) " ei ole mitään.";  
	 !   if (WriteListFrom(child(x1), ENGLISH_BIT+TERSE_BIT+CONCEAL_BIT) ~= 0) " on ";
	    ".";
	5:  "Avaat ", (genetiivi) x1, ".";
    }

  Order:  print (k_adessiivi) x1; " on parempaa tekemistä.";  
  Places: switch (n) {
	1:  print "Olet käynyt näissä paikoissa: ";
	2:  print ".^";
    }
  Pray:     "Et saa vastausta rukouksiisi.";
  Prompt:   print "^>";
  Pronouns: switch (n) {
	1:  print "Tällä hetkellä, ";
	2:  print "on ";
	3:  print "ei ole asetettu";
	4:  "peli ei tunne pronomineja.";
	5:  ".";
    }
  Pull,Push,Turn: switch (n) {
	1:  if (x1 has pluralname) print "Ne pysyvät "; else print "Se pysyy ";
	    "paikallaan.";
	2:  "Et pysty.";
	3:  "Mitään ilmeistä ei tapahdu.";
	4:  "Se ei olisi kohteliasta.";
    }
! Push: see Pull
  PushDir: switch (n) {
	1:  "Etkö keksi muuta?";
	2:  "Se ei ole suunta.";
	3:  "Et voi siihen suuntaan.";
    }
  PutOn: switch (n) {
	1:  "Sinun täytyy pidellä ", (partitiivi) x1, " ennenkuin voit laittaa ",
		(sitajaniita) x1, " minkään päälle.";
	2:  "Se ei mene itsensä päälle.";
	3:  "Asioiden laittaminen ", (genetiivi) x1, " päälle ei hyödyttäisi mitään.";
	4:  "Et ole riittävän notkea.";
	5:  "(ottaen ensin ", (sentaine) x1, " päältä)^";      
	6:  print (k_adessiivi) x1, " ei ole enää tilaa.";                
	7:  "Tehty.";
	8:  "Laitat ", (genetiivi) x1, " ", (allatiivi) second, ".";
    }
  Quit: switch (n) {
	1:  print "Vastaa kyllä tai ei.";
	2:  print "Oletko varma? ";
    }
  Remove: switch (n) {
	1:  if (x1 has pluralname) print "Ne ovat"; else print "Se on";
	    " harmillista kyllä kiinni.";
	2:  if (x1 has pluralname) print "Mutta ne eivät"; else print "Mutta se ei"; print "ole siellä nyt.";
	3:  "Selvä."; ! "Removed."
    }
  Restart: switch (n) {
	1:  print "Haluatko varmasti aloittaa alusta? ";
	2:  "Ei onnistunut.";
    }
  Restore: switch (n) {
	1:  "Lataus epäonnistui.";
	2:  "Ok.";
    }
  Rub:      "Et saavuta sillä mitään.";
  Save: switch (n) {
	1:  "Talletus epäonnistui.";
	2:  "Ok.";
    }
  Score: switch (n) {
	1:  if (deadflag) print "Tässä pelissä sait pisteitä "; else print "Tähän mennessä olet saanut pisteitä ";
	    print score, " (maksimi ", MAX_SCORE, "), peli "; if (deadflag) print "kesti "; else print "on kestänyt "; 
	    print turns, " vuoroa";
	    !! if (turns ~= 1) print "/vuoron";     
	    !! return;
	2:  "Tässä tarinassa ei ole pisteytystä.";
    }
  ScriptOff: switch (n) {
	1:  "transkriptio on jo pois päältä.";
	2:  "^transkription loppu.";
	3:  "transkription lopettaminen epäonnistui.";
    }
  ScriptOn: switch (n) {
	1:  "transkriptio on jo päällä.";
	2:  "transkription alku";
	3:  "transkription aloittaminen ei onnistunut.";
    }
  Search: switch (n) {
	1:  "On liian pimeää.";
	2:  print (k_adessiivi) x1, " ei ole mitään.";                       
	3:  print (k_adessiivi) x1, " on ";    !? tulostuuko oikein?                               
	    WriteListFrom(child(x1), ENGLISH_BIT+TERSE_BIT+CONCEAL_BIT); ! +ISARE_BIT); pois
	    ".";
	4:  "Et löydä mitään.";
! Yksikkö JA monikko?
	5:  "Et näe sisälle, koska ", (nominatiivi) x1, " on suljettu.";
	6:  print (k_nominatiivi) x1; if (x1 has pluralname) print_ret " ovat tyhjiä."; else print_ret " on tyhjä.";
	7:  print (k_inessiivi) x1, " on ";     !? tulostuuko oikein?			
	    WriteListFrom(child(x1), ENGLISH_BIT+TERSE_BIT+CONCEAL_BIT);   !+ISARE_BIT); pois
	    ".";
    }
  Set:      "Et voi säätää ", (SitajaNiita) x1, ".";
  SetTo:    "Et voi säätää ", (SitajaNiita) x1, " mihinkään.";
  Show: switch (n) {
	1:  "Et pitele ", (partitiivi) x1, ".";
	2:  print (k_nominatiivi) x1; if (noun has pluralname) print_ret " eivät ole vaikuttuneita."; else print_ret " ei ole vaikuttunut.";
    }
  Sing:     "Laulantasi on kammottavaa.";
  Sleep:    "Et tunne itseäsi kovin uniseksi.";
  Smell:    "Et haista mitään yllättävää.";
	    #Ifdef DIALECT_US;
  Sorry:    "Mitäpä tuosta.";
	    #Ifnot;
  Sorry:    "Mitäpä turhia.";
	    #Endif;
  Squeeze: switch (n) {
	1:  "Pidä kädet itselläsi.";
	2:  "Et saavuta sillä mitään.";
    }
  Strong:   "Todellinen seikkailija ei käytä tuollaista kieltä.";
  Swim:     "Vettä ei ole riittävästi uimiseen.";
  Swing:    "Täällä ei ole mitään missä voisit keinua.";
  SwitchOff: switch (n) {
	1:  print_ret (ksitajaniita) x1, " ei voi kytkeä.";
	2:  print_ret (ksejane) x1, " ", (onjaovat) x1, " jo päällä.";
	3:  "Kytket ", (genetiivi) x1, " pois päältä.";
    }
  SwitchOn: switch (n) {
	1:  print_ret (ksitajaniita) x1, " ei voi kytkeä päälle.";
	2:  print_ret (ksejane) x1, " on jo kytketty päälle.";
	3:  "Kytket ", (genetiivi) x1, " päälle.";
    }
  Take: switch (n) {
	1:  "Otettu."; ! "Taken."
	2:  "Olet aina täynnä itseäsi.";
	3:  "En usko että ", (nominatiivi) x1, " on kiinnostunut siitä.";
	4:  print "Sinun täytyy ensin poistua ";
	    if (x1 has supporter) print_ret (ablatiivi) x1, ".^";
	    else print_ret (elatiivi) x1, ".";
	5:  "Sinulla on jo ", (sejane) x1, ".";
	6:  if (noun has pluralname) print "Ne näyttävät "; else print "Se näyttää ";
	    "kuuluvan ", (allatiivi) x1, ".";
	7:  if (noun has pluralname) print "Ne näyttävät "; else print "Se näyttää ";
	    "olevan osa ", (partitiivi) x1, ".";
	8:  print_ret (Ksitajaniita) x1, " ei ole saatavilla.";
	9:  print_ret (k_nominatiivi) x1, " ", (eijaeivat) x1, " ole auki.";
	10: if (x1 has pluralname) print_ret "Ne eivät taida olla siirrettävissä."; 
		else print_ret "Se ei taida olla siirrettävissä.";                     !?! Kannettava?
	11: if (x1 has pluralname) print "Ne eivät "; else print "Se ei ";
	    "irtoa."; ! "theyre / that's fixed in place."
 	12: "Sinulla on jo liikaa kantamuksia.";
	13: print "(laitat "; if (x1 has pluralname) print (nominatiivi) x1; 
		else print (genetiivi) x1; " ", (illatiivi) SACK_OBJECT, " tilan saamiseksi)";
    }
  Taste:    "Et maista mitään yllättävää.";
  Tell: switch (n) {
	1:  "Puhut hetken itseksesi.";
	2:  "Ei reaktiota.";
    }
  Think:    "Hieno ajatus.";
  ThrowAt: switch (n) {
	1:  "Hyödytöntä.";
	2:  "Viimehetkellä et uskallakaan.";
    }
! Tie:  see JumpOver.
  Touch: switch (n) {
	1:  "Pidä kätesi kurissa!";
	2:  "Et tunne mitään odottamatonta.";
	3:  "Jos uskot siitä olevan apua...";
    }
! Turn: see Pull.
  Unlock:  switch (n) {
	1:  if (x1 has pluralname) print "Ne eivät "; else print "Se ei ";
	    "tunnu olevan lukittavissa.";
! Se ei / ne eivät
	2:  print_ret (ksejane) x1, " ", (eijaeivat) x1, " ole lukossa.";
	3:  if (x1 has pluralname) print "Ne eivät "; else print "Se ei ";
	    "tunnu sopivan lukkoon.";
	4:  "Avaat ", (genetiivi) x1, " lukituksen.";
    }
  VagueGo:  "Mihin ilmansuuntaan haluat mennä?"; !mihin suuntaan haluat mennä?
  
  Verify: switch (n) {
	1:  "Pelitiedosto on ok.";
	2:  "Pelitiedosto voi olla huono.";
    }
  Wait:     "Aika kuluu.";
  Wake:     "Karmea tosiasia on, ettet näe unta.";
  WakeOther:"Se ei tunnu olevan tarpeen.";
  Wave: switch (n) {
	! 1:  "Mutta ", (SejaNe) x1, " ", (eijaeivat) x1, " ole käsissäsi.";
	1:  "Mutta et pitele ", (SitajaNiita) x1, ".";
	2:  "Heiluttelet ", (partitiivi) x1, ".";
    }
  WaveHands:"Heilutat, typeränä.";
  Wear: switch (n) {
	1:  "Et voi pukea ", (SitajaNiita) x1, "!";
	2:  "Et pitele ", (SitajaNiita) x1, "!";
	3:  "Sinulla on jo ", (sejane) x1, " päälläsi!";
	4:  "Puet päällesi ", (monikko_vai_yks) x1, ".";
    }
! Yes:  see No.

];

! ==============================================================================

Constant LIBRARY_FINNISH;       ! for dependency checking.

! ==============================================================================
