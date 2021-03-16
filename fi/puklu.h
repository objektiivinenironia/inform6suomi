! puklu.h -- Parsimista ja Tulostamista
! =====================================
! * Parsimista
! ------------

System_file;

Constant Tlimit = 31;
Array  Tbuffer -> 3+Tlimit;
Array  Tparse  -> 6;


! ** Dictinary Lookup

 [DL buf len
     i;
     if (len == 0 || len > Tlimit) return 0;
     Tbuffer->1 = len;
     for (i = 0: i ~= len: i ++) Tbuffer->(2+i) = buf->i;    
     Tbuffer->(2+len) = 0;
     Tparse->0 = 1;
     @tokenise Tbuffer Tparse;
     return Tparse-->1;
 ];


Attribute oletus_par;

! ** yksikkö S_Req
[ S_Req csID nreq;

    switch (csID) {

     csNom:	switch (nreq) {
      0: 	return 0;
     }
     csGen:	switch (nreq) {
      0: 	return 'n//';
     ! 1:        return 'en';   ! sekoittuu illatiiviin
     }
     csPar:	switch (nreq) {
      0:	return 'a//';
      1:	return 'ä//';
      2:	return 'ta';
      3:	return 'tä';
      4:	return 'tta';
      5: 	return 'ttä';
     }
     csIne:	switch (nreq) {
      0:	return 'ssa';
      1:	return 'ssä';
      2:        return 'essä'; ! pitkä 'ee'

     }
     csEla:	switch (nreq) {
      0:	return 'sta';
      1:	return 'stä';
      3:        return 'estä'; ! pitkä 'ee'

     }
     csIll:	switch (nreq) {
      0:	return 'an';
      1:	return 'en';
      2:	return 'in';
      3:	return 'on';
      4:	return 'un';
      5:	return 'yn';
      6:	return 'än';
      7:	return 'ön';
      8:	return 'han';
      9:	return 'hun';
      10:	return 'seen';
      11:       return 'teen';
      12:	return 'eseen'; ! pitkä 'ee'

     }
     csAde:	switch (nreq) {
      0:	return 'lla';
      1:        return 'llä';
      2:        return 'ellä'; ! pitkä 'ee'
     }
     csAbl:	switch (nreq) {
      0:	return 'lta';
      1:	return 'ltä';
      2:        return 'eltä'; ! pitkä 'ee'

     }
     csAll:	switch (nreq) {
      0:	return 'lle';
      1:	return 'elle'; ! pitkä 'ee'
     }
    }
    return -1;

];

! ** monikko P_Req

[ P_Req csID nreq;

    switch (csID) {

     csNom: 	switch (nreq) {
      0:	return 0;
      1: 	return 't//';
     }
     csGen:	switch (nreq) {
      0: 	return 'en';
      1:	return 'in';
      2:	return 'den';
      3:	return 'ten';
      4:	return 'tten';
     }
     csPar:	switch (nreq) {
      0:	return 'ta';
      1:	return 'tä';
      2:	return 'ja';
      3:	return 'jä';
     }
     csIne:	switch (nreq) {
      0:	return 'ssa';
      1:	return 'ssä';
     }
     csEla:	switch (nreq) {
      0:	return 'sta';
      1:	return 'stä';
     }
     csIll:	switch (nreq) {
      0:	return 'siin';
      1:	return 'yn';
      2:	return 'hyn';
      3:	return 'ihin';
     }
     csAde:	switch (nreq) {
      0:	return 'lla';
     }
     csAbl:	switch (nreq) {
      0:	return 'lta';
      1:	return 'ltä';
      2:	return 'ilta';
      3:	return 'iltä';
     }
     csAll:	switch (nreq) {
      0:	return 'lle';
      1:	return 'ille';
     }
    }
    return -1;
];

! ** global muu_sija (ks. PrintCommand)
global muu_sija = 0;

! ** Parser Error (error code)
!
! jos sija ON olemassa (esim. ei "pallorz%s5t"),
! ja esine/asia ON paikalla,
! mutta konteksti väärä (">ota palloLLE"),
! ei sanota "Et näe mitään sellaista" (error_code 4),
! vaan:
!
!   >ota pallosta
!   En ihan käsittänyt.
!
! ...ja myös:
!
!   >ota pallox
!	En ihan käsittänyt.
!
! menee sijamuotona koska "x//" on sanakirjassa
! (samoin "pallosyö" ("syö" on sanak) -> "En ihan käsittänyt"
! mikä on ihan hyvä vastaus jos pallo on huoneessa).
! Se on käytännössä sama kuin etype 1
! (error type 1 == STUCK_PE == "En käsittänyt tuota lausetta.")

[ ParserError error_code;

#Ifdef DEBUG;
    if (parser_trace > 1)
    { print "^ ParserError: etype ", etype, "  muu_sija: ", muu_sija,
	" ecode: ", error_code, " ^";
    	if (error_code == 2) print "vähennetään UPTO_PE -> STUCK_PE:ksi^";
    	if (muu_sija == true && error_code == 4) print
	    "~Et näe mitään sellaista.~ (ecode 4) -> ~En ihan
    	    käsittänyt.~ (käytännössä sama kuin ecode 1)^";
    }
#Endif;

    ! vähennetään UPTO_PE -> STUCK_PE:ksi
    if (error_code == 2) etype = 1;

    if (muu_sija == true && error_code == 4)
	print_ret "En ihan käsittänyt.";

    ! (vai annetaanko merkkijono?)
    if (error_code ofclass String) print_ret (string) error_code;

    rfalse; ! Print standard parser error message
];


! ** Ending Lookup etsii sijapäätettä syötteestä

global luku = 0;

[ EndingLookup addr len csID
    v u Nomini i;

    muu_sija = 0;

    if (csID == 0) rtrue;

!    ! "len" on haettavan päätteen pituus
!    if (len ~= 0) {v = DL (addr, len); 	
!   	    
!    	if (v == 0) rfalse;

    ! "len" on haettavan päätteen pituus -- ONKO? (on) $$$
    if (len ~= 0) {
	v = DL (addr, len);
	
!	if (v ~= 0 && ) 
   	    
    	if (v == 0) rfalse;

	! print " len ", len;!!$$$$
	

    

    } ! jos pääte ei löydy sanakirjasta, rfalse

    else v = 0; ! ei päätettä

    Nomini = S_Req; ! etsii yksikön päätettä

    for (::) {
	for (i = 0: : ++i) {
	    u = indirect (Nomini, csID, i);

	    ! jos 'u' on 0 tai pääte löytyy sanakirjasta (DL) rtrue

	    if (u == v && Nomini == P_Req) luku = 2;
            if (u == v && Nomini == S_Req) luku = 1;

	    if (u == v) rtrue;

	    else if (u == -1) break;

	}

        ! sijapääte on oudossa asiayhteydessä 
	! esim. "tutki uomasta" (mutta ei "tutki uomarrr") 

	muu_sija = true;

	! yksikön jälkeen etsitään monikkoa 

	switch (Nomini) {

	 S_Req: Nomini = P_Req;
	 P_Req: rfalse;
	}
    }

    rfalse;
];

! Tämä on rikki !
!!!!!!!!!!!!!!!!!
! ** NextWordLyh (???) on tämäkin! 
!??? jos olio on in scope ja sillä on parse_namessa allaoleva
!??? niin ajaa tämän aina

[ NextWordLyh i j adr len end w;

    !syötetty sana
    adr = WordAddress(wn);
    !syötetyn sanan pituus
    len = WordLength(wn);

    for (end = len: end ~= 0 : --end)
    {
	! yritetään parsia syötetty sana
	! lopusta alkuun
	w = DL(adr, end);

	!print len, "*", end, "/";

	! jos syöte kelpaa DL:lle...
	if (w ~= 0)
	{
	    j = w; 

	    break; 
	}
    }
    if (wn > parse->1) { wn++; rfalse; }
    i = wn*2-1; wn++;
    ! jos sanaa ei löytynyt, parsitaan i...
    if (j ~= w) j = parse-->i;
    if (j == ',//') j = comma_word;
    if (j == './/') j = THEN1__WD;

    return j;
];

! ** LanguageRefers (ja globaaleja muuttujia)

Global csLR = 0;
! syötteen tulostusta varten sijamuoto PrintCommand
Global CaseIs; 
! tulostusta varten 
Global sija; 

! LanguageRefers 
!
! !! Tämäkin pitäisi kirjoittaa uudelleen!
! !! nimien astevaihtelun erottelu (vahva_a, heikko_b)
! !! on hankalaa.
! !! "jokeri" (lyh) vielä samassa niin tulee takkua...
!
! Parseri kysyy languagerefersiltä kelpaako syöte sanakirjasanaksi
! languagerefers vastaa sen perusteella mitä endinglookup
! kertoo sijapäätteestä.
!
! lyh on "jokeri"
! ---------------
! kelpuuttaa mitä vain
! sanakirjasanan ja sijapäätteen väliin. esim.
! luolasusi luolasutta luolasudelle luolasudesta luolasuteen
! (muista sanakirjasanat ''-sisään ei toimi muuten)
!
! Object -> luolis "luolasu/si"
!  with 	lyh 'luolasu' 'su'
!  etsitään sanakirjasanan
!  jälkeen ja rtrue kun pääte löytyy
!  välissä voi olla mitä tahansa köntsää
!    >anna pallo sudelle
!  ja
!    >anna pallo surtkgjkjkrgjegeglle
!  sama lopputulos
!
!???
! vahva /heikko astevaihtelu?
! ---------------------------
! jos nimet (name) sekoittuvat toisiinsa astevaihtelun takia, voi antaa
! esim. 'mato', 'madot' / 'matto', 'matot'; 'pato', 'padot' / 
! 'patto', 'patot',  jne...
!
! esim. vahva_a 'Maukka' / mon.: 'maukko'
! (Monikkovartalon perään kelpaa genetiivi-, partitiivi-, illatiivi-, 
! ja essiivipääte.
! Yksikön nominatiivi kelpaa, ja partitiivi-, essiivi- tai illatiivipääte)

property lyh;

[ LanguageRefers obj wnum adr len end w csID; !paska; !$$$$ 
    
    adr = WordAddress(wnum); len = WordLength(wnum);
    
    csID = csLR; 
    
    for (end = len: end ~= 0 : --end) 
    {
	!paska = DL (adr, end-1); !$$$$

	! * rikki * $$$$$
	! tässä oli *lyh* joka on *rikki*
	! if (w ~= 0 && WordInProperty (w, obj, lyh))
	!if (paska ~= 0 && WordInProperty (paska, obj, name)) !! && ?? $$$
	!{

	    !$$$ if boardtext->i~=' ' or 0) f=1
  	    !$$$ i = WordAddress(wn++); i=i-buffer;
            !$$$ if (buffer->i=='"')

	    ! print " ", (address)paska, " ", end;
	    ! if (paska->end=='*')!$$$ houno!!!
	    !	print "*";
	    
	    
	    !while (end < len)
	    !{
	!	end++;		
	!	if (EndingLookup (adr+end, len-end, csID))
	!	{
	!	    rtrue;
	!	    				    
	!	}		
	!    	
	!    }
	!}
	
	w = DL (adr, end); 
	
	if (parent (obj) == Compass)
   	{ if (w ~= 0 && WordInProperty (w, obj, name) && EndingLookup
	      (adr+end, len-end, csID)) rtrue;

		    ! hyväksytään vain partitiivi
		    ! ">t ptä" ">mene pohjoista"
		    ! muuten esim. ">lu" (luode) vastaa:
		    ! "Länsi vai luode?"
		    ! if (csID ~= 3) rfalse;

	    if (csID ~= 3) rfalse; 
	}	

	
 	!???
	! property *taipumaton*
	! esimerkiksi genetiiviattribuutti
	! "pöydän" -> "pöydän antimet"
	
	if ( end == len && w ~= 0 && WordInProperty (w, obj, taipumaton)) 
	    
	{
#Ifdef DEBUG;				
	    if (parser_trace >= 5)
	    {print "^        [* Taipumaton * ]^";
		debugsijat(adr, wnum, len, end, w, csID);
	    }
#Endif;
	    rtrue; 
	}; 
	!??? täh?... jos Endinglookup on tosi, se on yksikkö
 	if ( w ~=0 && WordInProperty (w, obj, name) && EndingLookup
	    (adr+end, len-end, csID)) 
	    
	{
#Ifdef DEBUG;
	    if (parser_trace >= 4)
		print "    [* LanguageRefers *]^";
	    !???
	    !  switch (luku)
	    !  {
	    !   0: print "    [* LanguageRefers: luku ? (0)]^";
	    !   1: print "    [* LanguageRefers: luku yksikkö (1)]^";
	    !   2: print "    [* LanguageRefers: luku monikko (0)]^";
	    ! }
	    
	    
	    
	    if (parser_trace >= 5)
		debugsijat(adr, wnum, len, end, w, csID);
	    
#Endif;
	    rtrue;
	    
	};
	!??? astevaihtelun vahva muoto?
	
	if (w ~=0 && WordInProperty (w, obj, vahva_a) && EndingLookup (adr+end, len-end, csID) )
	{
#Ifdef DEBUG;
	    if (parser_trace >= 5) debugsijat(wnum, len, end, w,
					      csID);
#Endif;
	    !???  monikko ja gen, par, ill tai ess
	    
	    if (obj provides pluralname && csID == 2 or 3 or 6 or 10 )
	    	rtrue;
	    !??? yksikkö ja nom, par, ess tai ill
	    else if (csID == 0 or 1 or 3 or 6 or 10)  
		rtrue; 
	};
	
	!??? esim. 'Mauka' (mon. 'Maukat', 'Mauko')
	!??? (kelpaa muut sijapäätteet kuin edellisessä)
	
	if (w ~=0 && WordInProperty (w, obj, heikko_a) && EndingLookup (adr+end, len-end, csID) )
	{
#Ifdef DEBUG;
	    if (parser_trace >= 5) debugsijat(wnum, len, end, w, csID);
#Endif;
	    !???  monikko ja ei 0, gen, par, ill tai ess
	    
	    if (obj provides pluralname && csID ~= 0 or 2 or 3 or 6 or
	    	10 ) rtrue;
	    !??? yksikkö ja ei nom, par, ess tai ill
	    
	    else if (csID ~= 0 or 1 or 3 or 6 or 10)  
		rtrue; 
	};
	!??? esim. vahva_b 'maukka' 'maukkaa'
	!??? (kaikki paitsi yksikön nominatiivi
	!??? 'maukka' + partitiivipääte 'ta' kelpaa)
	
	if (w ~=0 && WordInProperty (w, obj, vahva_b) && EndingLookup (adr+end, len-end, csID) )
	{
#Ifdef DEBUG;
	    if (parser_trace >= 5) debugsijat(wnum, len, end, w, csID);
#Endif;
	    !??? ei 0 tai nom tai par
	    if (obj hasnt pluralname && csID ~= 0 or 1 or 3 ) 
		rtrue; 
	};
	!??? esim. heikko_b 'maukas'
	!??? (kelpaa muut kuin edellisessä, ts. vain yksikön
	!??? nominatiivi
	!??? ja partitiivi kelpaa)
	
	
	
	if (w ~=0 && WordInProperty (w, obj, heikko_b) && EndingLookup (adr+end, len-end, csID) )
	{
#Ifdef DEBUG;
	    if (parser_trace >= 5) debugsijat(wnum, len, end, w, csID);
#Endif;
	    !??? nom tai par
	    
	    if (obj hasnt pluralname && csID == 0 or 1 or 3 ) 
	       	rtrue; 
	};
	
    }
    
    rfalse; 
];


! * Tulostamista
! --------------

! ** sijat-verbi testitulostaa 
[ debugsijat adr wnum len end w csID;

    print
	"^-- Debug (parsiminen, LR) --^
	etype == ", etype, "^
	wnum: ", wnum, " / len-end (pääte): ", (len-end),")^",
	"(adr: ", adr, ")^",
	"(end: ", end, ")^",
	"(len: ", len, ")^   ",
	"(address w: ", (address) w, ", ", (the)w, ")^   ";
    switch (csID) {
     0: "- csID 0 -";
     1: "Nominatiivi";
     2: "Genetiivi";
     3: "Partitiivi";
     4: "Inessiivi";
     5: "Elatiivi";
     6: "Illatiivi";
     7: "Adessiivi";
     8: "Ablatiivi";
     9: "Allatiivi";
     10: "Essiivi";
     11: "Translatiivi";
    };  print "^";
];

! ** c_token ks. finng.h - etsii sijapäätteen
!??? CaseIs = komennon verbin tulostamiseen 
!??? multiflag jottei luule montaa asiaa haettavan
!??? nyt kys. onkin onko edes tämä "ei MULTI_" ehto paikallaan

[ c_token  idtok csID retval;

    csLR = csID;

    retval = ParseToken (ELEMENTARY_TT, idtok);

    !??? mitä täällä tapahtuu?
    if (retval == 10000) sija = 10000; else sija = 0;

    CaseIs = csID; 

    !??? ANIMA_PE -> MULTI_PE bugi
  if (idtok ~= MULTI_TOKEN || MULTIHELD_TOKEN)
  multiflag = 1;
    
#Ifdef DEBUG;
    if (parser_trace >= 2)
    {
	print "^[!* c_token! idtok (", idtok, "): ";
    	switch (idtok) {
     	 NOUN_TOKEN: print "NOUN_TOKEN^";
     	 HELD_TOKEN: print "HELD_TOKEN^";
     	 CREATURE_TOKEN: print "CREATURE_TOKEN^";
     	 MULTI_TOKEN: print "MULTI_TOKEN^";
     	 MULTIHELD_TOKEN: print "MULTIHELD_TOKEN^";
	    
    	}
	if (idtok ~= MULTI_TOKEN || MULTIHELD_TOKEN)
    	    print "- Asetetaan *multiflag* koska muuten luulee moneksi -^";
	" CaseIs: ", CaseIs,
	    "^ csLR: ", csLR,
	    "^ csID: ", csID,
	    "^ sija: ", sija,
	    "^ retval: ", retval, "]^";
    }
    
#Endif;

    csLR = 0;

    return retval;

];

! ** LanguagePrintShortName(jossa jotain oletus_par)
[ LanguagePrintShortName obj sn;
    
    sn = short_name;
    
    if (sija == 0 or 10000 && obj hasnt oletus_par)
	CCase (obj, csNom, false);
    !??? nominatiivi
    if (sija == 0 or 10000 && obj has oletus_par)
	CCase (obj, csPar, false);
    !??? LPSN ei tee mitään jos globaali sija on muuta kuin nolla
    if (sija ~= 0) rfalse; 
    
    rtrue;
];

! hyhhyh! vaikka itse sanonkin (tai juuri siksi että)
! -------
!
! '>/' tulostaa '/' olion nimessä ('>>' tulostaa '>').
! tulostettu "/" tarkoittaa sanan vaihtumista kuten " ".
! Esim. "komero/>/putka/" tulostuu "komerossa/putkassa" (ine).
!
!??? nominatiivi (1), csDflt
!??? myös verbien tulostusta, imperatiivi


Constant SutLen = 200;
Array Suttu --> SutLen;

[ CCase obj csID ucase i dlm limit at vart;

    sija = csID;

    if (csID ~= 0) { 

	at = 0;

	Suttu-->0 = SutLen-1;

	@output_stream 3 Suttu;

 	if (obj provides short_name)
	    printshortname(obj);
	else
!??? jos vain tämä, olion nimi ei tulostu oikein
	    print (object) obj;

	@output_stream -3;

	if (ucase) Suttu->2 = LtoU (Suttu->2);

	dlm = 0;
	limit = (Suttu-->0) + 2;

	sija = 0;

	vart = 0;

	if (csID == csIne) vart = 1;

	if (csID < 2 || csID == vbImp)
	    for (i = 2: i ~= limit: ++ i) {
		if (Suttu->i ~= '/' or '>') print (char) (Suttu->i);
 	        if (Suttu->i == '>' && Suttu->(i+1) == '/') print "/";
	        if (Suttu->i == '>' && Suttu->(i+1) == '>') print ">";}

	!??? nomini ei nominatiivi tai 0 (csDflt)
	!??? verbi ei myöskään imperatiivi
	if (csID > 1 && csID ~= vbImp)
	    for (i = 2: i ~= limit: ++ i)
	    {    if (Suttu->i == '/' && Suttu->(i-1) ~= '>')
	    { if (dlm == 0) { dlm = Suttu+i; }
	    else { at++; CaseEnd (obj, csID, at);
		dlm = 0;
	    }
	    }

	    else { if (dlm ~= 0 && Suttu->i == ' ' or '/')
	    { at++;
		!??? verbien tulostus (?) VerbEnd
		if (csID > 20) VerbEnd(obj, csID,at);
		else CaseEnd(obj, csID, at);
		dlm = 0;
	    }
		if (dlm == 0 && Suttu->i ~= '>') print (char) (Suttu->i);
	    }
	    } ! for

	if (dlm ~= 0) { at++;
	    !??? verbi ei ole infinitiivi (?)
	    if (csID > 20 && csID ~= csInf) VerbEnd(obj, csID,at);
	    else CaseEnd(obj, csID, at);
	}
    }

    else
	print (object) obj;

];

!??? 

Constant ParLen = 50;
Array ParArr --> ParLen;

Constant JutLen = 100;
Array Juttu --> JutLen;

! tämä "juttu" pitäisi kirjoittaa uudelleen
! -----------------------------------------
! ps = printtaussääntö, onko yksikkö, monikko vai monikko ja "ine"-ohje.
! 0 on yksikkö
! 1 jos monikko
! 2 jos monikko ja ine tulostusohje.
!
!??? 's' ei tulostu ine-tulostusohjeen takia
!??? ps on 1 jos (monikko)objektilla *ei* ole ine-ohjetta (esim. "susilla")
!??? ps on 2 jos (monikko)objektilla on ine-ohje (esim. "pöydillä")

[ CaseEnd obj csID at num limit i ps a paate_isolla;

    paate_isolla = 0; 

    ps = 0; 
    if (obj has pluralname) (ps = 1); 
    if ((obj has pluralname) &&
	(obj provides ine)) (ps = 2); 

    Juttu-->0 = JutLen-1;

    @output_stream 3 Juttu;

    if (csID == csIll)
    { if (obj provides ill) print (string) obj.ill;
    else print (string) obj.ess; };

    if (obj hasnt pluralname)
	switch (csID) {
	 csGen: print (string) obj.gen;
	 csPar: print (string) obj.par;
	 csEss: print (string) obj.ess;
	 csIne: print (string) obj.gen;
	 csEla: print (string) obj.gen;
	 csAde: print (string) obj.gen;
	 csAbl: print (string) obj.gen;
	 csTra: print (string) obj.gen;
	 csAll: print (string) obj.gen;
	};

    !??? monikon astevaihtelu esim. "reikien"
    !??? oliolle on annettu ine "jissä"

    if ((obj provides ine) && (obj has pluralname) &&
	(csID ~= csNom or csPar or csGen or csEss or csIll))
	print (string) obj.ine;
    else if ((obj has pluralname) && (csID ~= csIll))
    {
	if (csID == csGen) print (string) obj.gen;
	if (csID == csPar) print (string) obj.par;
	if (csID ~= csGen or csPar) print (string) obj.ess;
    };

    @output_stream -3;

    num = 0;
    limit = (Juttu-->0) + 2;

    if ((csID == csIll) && (obj provides Ill))
    {
	for (i = 2: i ~= limit: ++ i) 	{
	    if ((num == at-1) && (Juttu->i ~= '/')) print (char) (Juttu->i);
	    if (juttu->i == '/') num++;
	}
    }

    if (csID == csGen or csPar or csEss)
    {
	for (i = 2: i ~= limit: ++ i) 	{
	    if ((num == at-1) && (Juttu->i ~= '/')) print (char) (Juttu->i);
	    if (juttu->i == '/') num++;
	}
    }

    if (csID ~= csIll or csGen or csPar or csEss)
    {
	!??? 's' ei tulostu ine-tulostusohjeen takia
	!??? ps on 1 jos (monikko)objektilla *ei* ole ine (esim. "susilla")
	!??? ps on 2 jos (monikko)objektilla on ine-ohje (esim. "pöydillä")

  	if (ps == 2) !??? monikko ja ine-ohje
	    for (i = 2: i ~= limit: ++ i) {
		if ((num == at-1)
		    &&
		    (Juttu->i ~= 's' or 'a' or 'ä' or '/' or 'S' or 'A' or 'Ä'))
		    print (char) (Juttu->i);

		if (Juttu->i == 'S' or 'A' or 'Ä' ) paate_isolla = 1; 
		if (Juttu->i == '/') num++;
	    };

   	if (ps == 1) !??? monikko
 	    for (i = 2: i ~= limit: ++ i) {

 	  	if ((num == at-1)
		    &&
		    (Juttu->i ~= 'n' or 'a' or 'ä' or '/' or 'N' or 'A' or 'Ä'))
		    print (char) (Juttu->i);

		if (Juttu->i == 'N' or 'A' or 'Ä') paate_isolla = 1; 
		if (Juttu->i == '/') num++;

 	    };

	!??? olio ei ole monikollinen
    	if (obj hasnt pluralname) 
	    for (i = 2: i ~= limit: ++ i) {
		
		!??? (gen-päätteestä) jos kirjain on 'n' tai 'N',
	     	!??? eikä sitä seuraa '/' tai jonon loppu, se tulostetaan.
		!??? Esim.: "ont/to kan/to"  gen "on/non"
		!??? genetiivin "non" ensimmäinen "n" tulostetaan.
		!??? <limit ~= (i+1) or (i+2)> (?) eli merkkijonon
		!??? loppu ei tule heti '/' jälkeenkään?
		
		if ((num == at-1) && (Juttu->i == 'n' or 'N')
		    && (Juttu->(i+1) ~= '/'))
		{ if (limit ~= (i+1) or (i+2)) print (char) (Juttu->i);};

		!??? tulosta kaikki kirjaimet paitsi '/', 'n' tai 'N'.
		if  ((num == at-1) && (Juttu->i ~= '/' or 'n' or 'N'))
		    print (char) (Juttu->i);

	    	if (Juttu->i == 'N') paate_isolla = 1; 
	    	if (Juttu->i == '/') num++;

	    };

	if (paate_isolla == 0)
	    switch (csID) {
	     csIne: print "ss";
		
	     csEla: print "st";
	     csAde: print "ll";
	     csAbl: print "lt";
		
	     csTra: print "ksi";
	     csAll: print "lle"; 
	    }
	
	else switch (csID) {
	 csIne: print "SS";
	    
	 csEla: print "ST";
	 csAde: print "LL";
	 csAbl: print "LT";
	    
	 csTra: print "KSI";
	 csAll: print "LLE"; 
	}
	if (csID ~= csTra or csAll) {
	    ParArr-->0 = ParLen-1;
	    @output_stream 3 ParArr;
	    print (string) obj.par;
	    @output_stream -3;
	    
	    num = 0;
	    limit = (ParArr-->0) +2;
	    a = 0;
	    
	    for (i = 2: i ~= limit: i++)
		
	    {
		if ((ParArr->(i+1) == '/') || (i == limit-1))
	    	{a++;
 	 	    if (a == at) print (char) (ParArr->i);} }
	}
    }
    
];

! ** Tulosta verbi isolla alkukirjaimella

Constant verbi_pituus = 39;
Array verbi_array --> verbi_pituus;

[ VerbiKap w i k iso;
    @output_stream 3 verbi_array;
    print (address) w;
    @output_stream -3;

    k = verbi_array->2;

    switch (k)
 { 155, 158: iso = 158; ! ä -> Ä
   156, 159: iso = 159; ! ö -> Ö
   201, 202: iso = 202; ! å -> Å
   default: iso = k-32;
    }
    print (char) iso;

    for (i=2:i<=verbi_array-->0:i++)
  { print (char) verbi_array->(i+1);
    }
    ! ao. ei tarvita?
    return verbi_array-->0;
 ];

! ** verbin loppuosan tulostus

[ VerbEnd obj csID;
    
    switch (csID) {
     vbInf: print (string) obj.inf_;	
     	! vbInd: print (string) obj.ind_y;
 	!     vbY2: print "t";
	!     vbY3: print (string) obj.ind_y;
	!     vbM3: print (string) obj.ind_m;
     default: print "^[ !verbin tulostusvirhe! ]^";
    }
];
