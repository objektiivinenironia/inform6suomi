! puklu.h 
! =======
! Parsimiseen ja tulostamiseen.  

! Parsimista
! ----------

System_file;

Constant Tlimit = 31;
Array  Tbuffer -> 3+Tlimit;
Array  Tparse  -> 6;




! RusMCE:n Dictinary Lookup

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


! Attribute isot_kirjaimet; 
Attribute oletus_par; ! tulostaa objektin oletuksena partitiivissa 

! ao. taitaa olla ven. tulostussääntö
! (se ei taida tehdä mitään täällä)
[ objID  obj;
    
    if (obj has pluralname) return ocP;
    else return ocS;
];

! yksikkö
[ S_Req  csID nreq;
    
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


! monikko

[ P_Req  csID nreq;
    
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

global muu_sija = 0;


! TODO: "Tuota verbiä ei voi käyttää useisiin kohteisiin
    ! kerralla." on melko kökköä... LibraryMessages (tai parsererror?) voisi
    ! sanoa jos esim actiontobe == examine
    ! "Voit tutkia vain yhtä asiaa kerrallaan."

[ ParserError error_code en_k;
    
    en_k = 0;    
    
    if (muu_sija == 1) en_k = 1;
    muu_sija = 0;    
    
    !! vähennetään UPTO_PE -> STUCK_PE:ksi
    ! if (error_code == 2) etype = 1;    
    if (error_code == 2) etype = 1;

    ! jos sija on olemassa, mutta väärä konteksti,
    ! ei sanota "Et näe mitään sellaista" (4), vaan...

    if (en_k == true && error_code == 4) print_ret "En ihan käsittänyt.";
	
    !! (vai annetaanko merkkijono?) 
    if (error_code ofclass String) print_ret (string) error_code;

    rfalse; ! Print standard parser error message
    
];

global monikko = 0;

! ao. etsii sijamuodon päätteen

[ EndingLookup   addr len csID 
    v u ocFN i;


      
    if (csID == 0) rtrue;    
    
    if (len ~= 0) {v = DL (addr, len); 	! "len" on haettavan sijamuodon päätteen pituus
	    
    	if (v == 0) rfalse;
		

    } ! jos sijamuodon päätettä ei löydy sanakirjasta, rfalse
    
    else v = 0; ! ei sijamuodon päätettä, v = 0
    
    ocFN = S_Req; ! etsii yksikön päätteitä

!        monikko = 0;        
    
    for (::) {
	for (i = 0: : ++i) { 
	    u = indirect (ocFN, csID, i);	! 'i' on 'nreq' arvo

	    
		    
	    ! jos 'u' on 0 tai löytyy sanakirjasta (DL) rtrue

            ! ao. OLI:   if (ocFN == P_Req) monikko = true;
	    
	    if (ocFN == P_Req) monikko = true;
	    
	    if (u == v) rtrue;	    
	    
	    else if (u == -1) break;	! jos lista (csID nreq) valmis, break
	    
	}
	
        ! sijapääte on eri kontekstissa, esim "tutki uomasta"
	! (mutta ei "tutki uomarrro") ks. ParserError yllä
 	
	muu_sija = true;
	
	! jos yksikkölista on käyty läpi, siirry monikkolistaan
	! rfalse jos monikkolista on käyty läpi (ilman osumaa)

        !??? if (ocFN == P_Req) monikko = true; else monikko = false;

!	monikko = true;
	
	if (ocFN == S_Req) ocFN = P_Req; else rfalse;
	
    }
    
    rfalse;
];

Global csLR = 0;
Global CaseIs; ! kertoo PrintCommandille mikä on syötteen sijamuoto
Global sija; ! tulostusta varten


! LanguageRefers
!
! Parseri kysyy languagerefersiltä kelpaako syöte sanakirjasanaksi
! languagerefers vastaa sen perusteella mitä endinglookup
! kertoo sijapäätteestä. 

[ LanguageRefers  obj wnum adr len end w csID; 

    
    adr = WordAddress(wnum); len = WordLength(wnum);

       
    if (parent (obj) == Compass) 
    {
	w = DL (adr, len);
	if (w ~= 0 && WordInProperty (w, obj, name)) rtrue;
	rfalse;
    }
    
    csID = csLR; 
    
    
    for (end = len: end ~= 0 : --end) 
    {
	w = DL (adr, end); 



	!! (property) taipumaton
	!! esimerkiksi genetiiviattribuutti "pöydän" -> "pöydän antimet"	   
	
	if ( end == len && w ~= 0 && WordInProperty (w, obj, taipumaton)) 
	    !! && EndingLookup (adr+end, len-end, -1))
	    
	{
           #Ifdef DEBUG;				
	    if (parser_trace > 0)
	    {print "^[ * Taipumaton * ]^";
		debugsijat(adr, wnum, len, end, w, csID);
       	
	   
	    }
            #Endif;
	    rtrue; 
	}; 
	
	
	

	if ( w ~=0 && WordInProperty (w, obj, name) && EndingLookup
	    (adr+end, len-end, csID))

	    	
	{
		
            #Ifdef DEBUG;				
	    if (parser_trace > 1)
		debugsijat(adr, wnum, len, end, w, csID);
	    !	print "% LANGUAGEREFERS! parser action:",  parser_action, " indef_type: ", indef_type, " ";
	    ! if (monikko == true) print " % LanguageRefers: MONIKKO!^";
	    ! else print " % LanguageRefers: YKSIKKÖ!^";	
              #Endif;
		    rtrue; 
	};
	
	
	!! jos nimet (name) sekoittuvat toisiinsa astevaihtelun takia, voi antaa   	
	!! esim. 'mato', 'madot' / 'matto', 'matot'; 'pato', 'padot' / 'patto', 'patot',  jne... 
	
	!! esim. vahva_a 'Maukka' / mon.: 'maukko' 
	!! (Monikkovartalon perään kelpaa genetiivi-, partitiivi-, illatiivi-, ja essiivipääte.
	!! Yksikön nominatiivi kelpaa, ja partitiivi-, essiivi- tai illatiivipääte) 
	if (w ~=0 && WordInProperty (w, obj, vahva_a) && EndingLookup (adr+end, len-end, csID) )!!
	{ #Ifdef DEBUG;	if (parser_trace > 0) debugsijat(wnum, len, end, w, csID);
#Endif;
	    if (obj provides pluralname && csID == 2 or 3 or 6 or 10 ) rtrue; !  monikko ja gen, par, ill tai ess 
	    else if (csID == 0 or 1 or 3 or 6 or 10)  !yksikkö ja nom, par, ess tai ill
		rtrue; 
	};
	
	!! esim. 'Mauka' (mon. 'Maukat', 'Mauko')
	!! (kelpaa muut sijapäätteet kuin edellisessä)
	if (w ~=0 && WordInProperty (w, obj, heikko_a) && EndingLookup (adr+end, len-end, csID) )!!
	{ #Ifdef DEBUG;	if (parser_trace > 0) debugsijat(wnum, len, end, w, csID);
#Endif;
	    if (obj provides pluralname && csID ~= 0 or 2 or 3 or 6 or 10 ) rtrue; !  monikko ja ei 0, gen, par, ill tai ess 
	    else if (csID ~= 0 or 1 or 3 or 6 or 10)  !yksikkö ja ei nom, par, ess tai ill
		rtrue; 
	};
	!! esim. vahva_b 'maukka' 'maukkaa' 
	!! (kaikki paitsi yksikön nominatiivi 'maukka' + partitiivipääte 'ta' kelpaa)
	if (w ~=0 && WordInProperty (w, obj, vahva_b) && EndingLookup (adr+end, len-end, csID) )!!
	{ #Ifdef DEBUG;	if (parser_trace > 0) debugsijat(wnum, len, end, w, csID);
#Endif;
	    if (obj hasnt pluralname && csID ~= 0 or 1 or 3 ) ! ei 0 tai nom tai par 
		rtrue; 
	};
	!! esim. heikko_b 'maukas'  
	!! (kelpaa muut kuin edellisessä, ts. vain yksikön nominatiivi ja partitiivi kelpaa)
	if (w ~=0 && WordInProperty (w, obj, heikko_b) && EndingLookup (adr+end, len-end, csID) )!!
	{ #Ifdef DEBUG;	if (parser_trace > 0) debugsijat(wnum, len, end, w, csID);
#Endif;
	    if (obj hasnt pluralname && csID == 0 or 1 or 3 ) ! nom tai par 
	       rtrue; 
	};
	
    }
    
    rfalse; 
];


! Tulosta (address) verbi isolla alkukirjaimella
! (ks. sanakirja.h)
Constant verbi_pituus = 39;
Array verbi_array --> verbi_pituus;

[ VerbiKap w i k iso;
    @output_stream 3 verbi_array;
    print (address) w;
    @output_stream -3;

    k = verbi_array->2;

    ! å,ä ja ö on sijoitettu toisin kuin a-z
    switch (k)
 { 155, 158: iso = 158; ! ä, Ä -> Ä
   156, 159: iso = 159; ! ö. Ö -> Ö 	
   201, 202: iso = 202; ! å, Å -> Å	
   default: iso = k-32;   
    }    
    print (char) iso;
    
    for (i=2:i<=verbi_array-->0:i++) 
  { print (char) verbi_array->(i+1);
    }
    ! ao. ei tarvita?
    return verbi_array-->0;
 ];


! Tulostusta
! ----------

!! debug parsimiseen									 
[ debugsijat adr wnum len end w csID;
    
    print
	"^-- Debug (parsiminen, LR) --^
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

! (RusMCE) - tätä ei ole vielä käytetty...
[ WriteListFromCase obj flag csID
    rval csSV;
    
    csSV = csDflt; csDflt = csID;
    
    rval = WriteListFrom (obj, flag);
    
    csDflt = csSV;
    return rval;
];


! ks. finng.h - etsii sijapäätteen 
[ c_token  idtok csID
    retval;

!#Ifdef DEBUG;			     
    if (parser_trace > 1) !% 

!    print   "%     [0] C_TOKEN ", 
!	    " found_ttype: ", found_ttype, 
!	    " found_tdata: ", found_tdata,
!	    
!	    " CaseIs: ", CaseIs,
!	    " csLR: ", csLR,
!	    " csID: ", csID,
!	    " sija: ", sija, "]^";
    
! #Endif;	

! print "%     [1] C_TOKEN return value: ", retval, " monikko == ",
!    monikko, "^";

    
    csLR = csID;

!    print "%     [1.5] C_TOKEN -> ParseToken(",ELEMENTARY_TT, ", ",
! idtok,")^";
    
    retval = ParseToken (ELEMENTARY_TT, idtok);
    
    if (retval == 10000) sija = 10000; else sija = 0; !! mikä tämä on?
!    print "%     [2] C_TOKEN return value: ", retval, " monikko == ",
!    monikko, "^";

    
    
    CaseIs = csID; !? komennon verbin tulostamiseen (hmm!)  
    
    csLR = 0;
    
    return retval;
    
];



[ LanguagePrintShortName obj
    sn;
    
    sn = short_name;
    
    ! if (obj provides sn && PrintOrRun(obj, sn, 1) ~= 0) rtrue;
    
    if (sija ~= 0) rfalse; !!!# LPSN ei tee mitään jos globaali sija on muuta kuin nolla 
    if (sija == 0 && obj hasnt oletus_par) CCase (obj, csNom, false);	!!!# nominatiivi 
    if (sija == 0 && obj has oletus_par) CCase (obj, csPar, false); 
    rtrue;
];


Constant SutLen = 200; 
Array Suttu --> SutLen;

[ CCase obj csID ucase i dlm limit at vart; 
    
    sija = csID;
    
    
    
    if (csID ~= 0) { 	!!!# jättää käsittelemättä sijamuodon oletuksen (0)   
	
	at = 0;
	
	Suttu-->0 = SutLen-1;
	
	@output_stream 3 Suttu; 
	
 	if (obj provides short_name) 
	    printshortname(obj); 
	else
	    print (object) obj;
	! jos vain tämä, olion nimi ei tulostu oikein 
	
	@output_stream -3;
	
	if (ucase) Suttu->2 = LtoU (Suttu->2);
	
	dlm = 0;
	limit = (Suttu-->0) + 2;
	
	sija = 0;
	
	vart = 0;
	
	if (csID == csIne) vart = 1;
	
    	
	!! '>/' tulostaa '/' olion nimessä ('>>' tulostaa '>').  
	!! tulostettu "/" tarkoittaa sanan vaihtumista kuten " ". 	    
	!! Esim. "komero/>/putka/" tulostuu "komerossa/putkassa" (ine).
	
	!!!# nominatiivi (1), csDflt
	! myös verbien tulostusta, imperatiivi
	if (csID < 2 || csID == vbImp) 
	    for (i = 2: i ~= limit: ++ i) {
		if (Suttu->i ~= '/' or '>') print (char) (Suttu->i);
 	        if (Suttu->i == '>' && Suttu->(i+1) == '/') print "/";
	        if (Suttu->i == '>' && Suttu->(i+1) == '>') print ">";}                
	
	
	! nomini ei nominatiivi tai 0 (csDflt)
	! verbi ei myöskään imperatiivi
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
		! verbien tulostus (?) VerbEnd
		if (csID > 20) VerbEnd(obj, csID,at); 
		else CaseEnd(obj, csID, at);
		dlm = 0; 
	    }
		if (dlm == 0 && Suttu->i ~= '>') print (char) (Suttu->i);
	    }
	    } !! for	
	
	if (dlm ~= 0) { at++;
	    ! verbi ei ole infinitiivi (?)
	    if (csID > 20 && csID ~= csInf) VerbEnd(obj, csID,at);
	    else CaseEnd(obj, csID, at);
	}
	
    } !! (csID ~= 0)
    
    
    
    
    else
	print (object) obj;
    
 
];


Constant ParLen = 50; 
Array ParArr --> ParLen;

Constant JutLen = 100; 
Array Juttu --> JutLen;

!! Verbin loppuosa?
[ VerbEnd obj csID;

    switch (csID) {	
    vbInf: print (string) obj.inf_;
!     vbInd: print (string) obj.ind_y;	
!     vbY2: print "t";
!     vbY3: print (string) obj.ind_y;
	!     vbM3: print (string) obj.ind_m;
     default: print "^^**** Verbin Tulostushäiriö ***^^";	
    }
    
];



!! Vrt. CCaseF RusMCE:ssä 
!! Ps = printtaussääntö, onko yksikkö, monikko vai monikko ja "ine"-ohje.

[ CaseEnd obj csID at num limit i Ps a paate_isolla;  	
    
    paate_isolla = 0; !! kun tulostetaan pieniä kirjaimia
    
    Ps = 0; 		!! 0 on yksikkö
    
    if (obj has pluralname) (Ps = 1); !! Ps on 1 jos monikko
    if ((obj has pluralname) && 
	(obj provides ine)) (Ps = 2); !! Ps on 2 jos monikko ja ine tulostusohje.
    
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
    
    !!!# Alla monikon astevaihtelu esim. "reikien"
    !!!# (Objektille on määritelty ** ine "jissä" ** )
    
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
  	
  	
	!!!# 's' ei tulostu ine-tulostusohjeen takia   
	!!!# Ps on 1 jos (monikko)objektilla *ei* ole ine-ohjetta (esim. "susilla")
	!!!# Ps on 2 jos (monikko)objektilla on ine-ohje (esim. "pöydillä")
  	
  	if (ps == 2) ! monikko ja ine-ohje
	    for (i = 2: i ~= limit: ++ i) { 
		
		if ((num == at-1) && (Juttu->i ~= 's' or 'a' or 'ä' or '/' or 'S' or 'A' or 'Ä')) 
		    print (char) (Juttu->i); 
		if (Juttu->i == 'S' or 'A' or 'Ä' ) paate_isolla = 1; !!!#! tulosta myös sijapääte isolla
		if (Juttu->i == '/') num++;
		
				
	    }; 
 	
   	if (ps == 1) ! monikko
 	    for (i = 2: i ~= limit: ++ i) { 
 	  	
 	  	if ((num == at-1) && (Juttu->i ~= 'n' or 'a' or 'ä' or '/' or 'N' or 'A' or 'Ä'))
		    print (char) (Juttu->i); 
		if (Juttu->i == 'N' or 'A' or 'Ä') paate_isolla = 1; !!!#! tulosta myös sijapääte isolla 
		if (Juttu->i == '/') num++;
		
		 		
 	    }; 
 	
 	
	
    	if (obj hasnt pluralname) ! yksikkö				       
	    for (i = 2: i ~= limit: ++ i) { 
		
		!! (gen-päätteestä) jos kirjain on 'n' tai 'N', eikä sitä seuraa '/' tai jonon loppu, se tulostetaan.
		!! Esim.: "ont/to kan/to"  gen "on/non" - genetiivin "non" ensimmäinen "n" tulostetaan.
		!! ??? <limit ~= (i+1) or (i+2)> (?) -eli- merkkijonon loppu ei tule heti '/' jälkeenkään - ?
		
		if ((num == at-1) && (Juttu->i == 'n' or 'N') && (Juttu->(i+1) ~= '/')) 
		{ if (limit ~= (i+1) or (i+2)) print (char) (Juttu->i);};
		
		!! tulosta kaikki kirjaimet paitsi '/', 'n' tai 'N'.
		if  ((num == at-1) && (Juttu->i ~= '/' or 'n' or 'N')) 
		    print (char) (Juttu->i);
		
		
	    	if (Juttu->i == 'N') paate_isolla = 1; !!!#! tulosta myös sijapääte isolla
	    	if (Juttu->i == '/') num++; 
	    	
	    }; 		
	if (paate_isolla == 0) 
	    switch (csID) {
	     csIne: print "ss"; 
		
	     csEla: print "st";  
	     csAde: print "ll"; 
	     csAbl: print "lt"; 
		
	     csTra: print "ksi";
	     csAll: print "lle"; }
	
	else switch (csID) {
	 csIne: print "SS"; 
	    
	 csEla: print "ST";  
	 csAde: print "LL"; 
	 csAbl: print "LT"; 
	    
	 csTra: print "KSI";
	 csAll: print "LLE"; }
	if (csID ~= csTra or csAll) {
	    ParArr-->0 = ParLen-1;
	    @output_stream 3 ParArr;
	    print (string) obj.par;
	    @output_stream -3;
	    
	    
	    num = 0;
	    limit = (ParArr-->0) +2;
	    a = 0;
	    
	    
	    for (i = 2: i ~= limit: i++) 
		
	    {if ((ParArr->(i+1) == '/') || (i == limit-1)) 
	    {a++;
 	 	if (a == at) print (char) (ParArr->i);} }
	    
	    
	    
	    
	    
	}
	
    }
    
    
];


! verbien tulostus tulisi tänne?

! debug-verbi sijat

[ PrintSijatSub; L__M(##PrintSijat, 1, noun); ];


! käänteisiä sanajärjestyksiä
! "laita X pöydälle" - "laita pöydälle X" 

[ PutOnRSub; <PutOn second noun>; ];

[ InsertRSub; <Insert second noun>; ];

[ TransferRSub; <Transfer second noun>; ];

[ AnswerRSub; <Answer second noun>; ];

[ ThrowAtRSub; <ThrowAt second noun>; ];

