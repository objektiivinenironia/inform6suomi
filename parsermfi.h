! parsermfi.h
! ===========
! Muokattuja rutiineja parserm:stä (finnish korvaa alkuperäiset näillä).
! Seuraavia rutiineja on muokattu:
!
!     * NounDomain 
!     * PrintCommand
!     * CantSee
!


! ----------------------------------------------------------------------------
!  ScoreMatchL  scores the match list for quality in terms of what the
!  player has vaguely asked for.  Points are awarded for conforming with
!  requirements like "my", and so on.  Remove from the match list any
!  entries which fail the basic requirements of the descriptors.
! ----------------------------------------------------------------------------

[ ScoreMatchL context its_owner its_score obj i j threshold met a_s l_s;

    !   if (indef_type & OTHER_BIT ~= 0) threshold++;
    if (indef_type & MY_BIT ~= 0)    threshold++;
    if (indef_type & THAT_BIT ~= 0)  threshold++;
    if (indef_type & LIT_BIT ~= 0)   threshold++;
    if (indef_type & UNLIT_BIT ~= 0) threshold++;
    if (indef_owner ~= nothing)      threshold++;

    #Ifdef DEBUG;
    if (parser_trace >= 4) print "   Scoring match list: indef mode ", indef_mode, " type ",
      indef_type, ", satisfying ", threshold, " requirements:^";
    #Endif; ! DEBUG

    a_s = SCORE__NEXTBESTLOC; l_s = SCORE__BESTLOC;
    if (context == HELD_TOKEN or MULTIHELD_TOKEN or MULTIEXCEPT_TOKEN) {
        a_s = SCORE__BESTLOC; l_s = SCORE__NEXTBESTLOC;
    }

    for (i=0 : i<number_matched : i++) {
        obj = match_list-->i; its_owner = parent(obj); its_score=0; met=0;

        !      if (indef_type & OTHER_BIT ~= 0
        !          &&  obj ~= itobj or himobj or herobj) met++;
        if (indef_type & MY_BIT ~= 0 && its_owner == actor) met++;
        if (indef_type & THAT_BIT ~= 0 && its_owner == actors_location) met++;
        if (indef_type & LIT_BIT ~= 0 && obj has light) met++;
        if (indef_type & UNLIT_BIT ~= 0 && obj hasnt light) met++;
        if (indef_owner ~= 0 && its_owner == indef_owner) met++;

        if (met < threshold) {
            #Ifdef DEBUG;
            if (parser_trace >= 4) print "   ", (The) match_list-->i, " (", match_list-->i, ") in ",
              (the) its_owner, " is rejected (doesn't match descriptors)^";
            #Endif; ! DEBUG
            match_list-->i = -1;
        }
        else {
            its_score = 0;
            if (obj hasnt concealed) its_score = SCORE__UNCONCEALED;

            if (its_owner == actor) its_score = its_score + a_s;
            else
                if (its_owner == actors_location) its_score = its_score + l_s;
                else
                    if (its_owner ~= compass) its_score = its_score + SCORE__NOTCOMPASS;

            its_score = its_score + SCORE__CHOOSEOBJ * ChooseObjects(obj, 2);

            if (obj hasnt scenery) its_score = its_score + SCORE__NOTSCENERY;
            if (obj ~= actor) its_score = its_score + SCORE__NOTACTOR;

            !   A small bonus for having the correct GNA,
            !   for sorting out ambiguous articles and the like.

            if (indef_cases & (PowersOfTwo_TB-->(GetGNAOfObject(obj))))
                its_score = its_score + SCORE__GNA;

            match_scores-->i = match_scores-->i + its_score;
            #Ifdef DEBUG;
            if (parser_trace >= 4) print "     ", (The) match_list-->i, " (", match_list-->i,
              ") in ", (the) its_owner, " : ", match_scores-->i, " points^";
            #Endif; ! DEBUG
        }
     }

    for (i=0 : i<number_matched : i++) {
        while (match_list-->i == -1) {
            if (i == number_matched-1) { number_matched--; break; }
            for (j=i : j<number_matched : j++) {
                match_list-->j = match_list-->(j+1);
                match_scores-->j = match_scores-->(j+1);
            }
            number_matched--;
        }
    }
];



! adjudicate kutsuu rutiinia ChooseObjects jos sellainen löytyy
! ks DM4 A5 EntryPointRoutines
!

[ ChooseObjects obj code;
    obj = obj; 
#Ifdef DEBUG;	
    if (parser_trace >= 5) print "[ChooseObjects ", code,"]^";
#Endif;
    
    return 0; ! 0 hyväxyy parserin ratkaisun
    
 ];

! ----------------------------------------------------------------------------
!  TryGivenObject tries to match as many words as possible in what has been
!  typed to the given object, obj.  If it manages any words matched at all,
!  it calls MakeMatch to say so, then returns the number of words (or 1
!  if it was a match because of inadequate input).
! ----------------------------------------------------------------------------

[ TryGivenObject obj threshold k w j;
    #Ifdef DEBUG;
    if (parser_trace >= 5) print "    Trying ", (the) obj, " (", obj, ") at word ", wn, "^";
    #Endif; ! DEBUG

    dict_flags_of_noun = 0;

!  If input has run out then always match, with only quality 0 (this saves
!  time).

    if (wn > num_words) {
        if (indef_mode ~= 0)
            dict_flags_of_noun = $$01110000;  ! Reject "plural" bit
        MakeMatch(obj,0);
        #Ifdef DEBUG;
        if (parser_trace >= 5) print "    Matched (0)^";
        #Endif; ! DEBUG
        return 1;
    }

!  Ask the object to parse itself if necessary, sitting up and taking notice
!  if it says the plural was used:

    if (obj.parse_name~=0) {
        parser_action = NULL; j=wn;
        k = RunRoutines(obj,parse_name);
        if (k > 0) {
            wn=j+k;

            .MMbyPN;

	    ! *? kuutio ja kuutiota tulee tänne

	    ! *? kummassakaan parser_action ei ole ##PluralFound
	    if (parser_action == ##PluralFound)
	       dict_flags_of_noun = dict_flags_of_noun | 4;

	    ! *? vain kuutiota tulee tänne
            if (dict_flags_of_noun & 4) {
		! print "***???";
		
                if (~~allow_plurals) k = 0;
                else {
                    if (indef_mode == 0) {
			print " *****????? TryGivenObject
			    indef_mode = 1 ";
			
                        indef_mode = 1; indef_type = 0; indef_wanted = 0;
                    }
                    indef_type = indef_type | PLURAL_BIT;
		    print " *? indef_type = ", indef_type, "!^";		    
                    if (indef_wanted == 0) indef_wanted = 100;
                }
            }

            #Ifdef DEBUG;
            if (parser_trace >= 5) print "    Matched (", k, ")^";
            #Endif; ! DEBUG
            MakeMatch(obj,k);
            return k;
        }
        if (k == 0) jump NoWordsMatch;
    }

    ! The default algorithm is simply to count up how many words pass the
    ! Refers test:

    parser_action = NULL;

    w = NounWord();

    if (w == 1 && player == obj) { k=1; jump MMbyPN; }

    if (w >= 2 && w < 128 && (LanguagePronouns-->w == obj)) { k = 1; jump MMbyPN; }

    j=--wn;
    threshold = ParseNoun(obj);
    #Ifdef DEBUG;  
    if (threshold >= 0 && parser_trace >= 5) print "    ParseNoun returned ", threshold, "^";
    #Endif; ! DEBUG
    if (threshold < 0) wn++;
    if (threshold > 0) { k = threshold; jump MMbyPN; }

    if (threshold == 0 || Refers(obj,wn-1) == 0) {
      .NoWordsMatch;
        if (indef_mode ~= 0) {
            k = 0; parser_action = NULL;
	    ! *? kuutio EI tee tätä hyppyä
	    ! *? tämä hyppy on viimeinen jonka "kuutiota" tekee	    
     		jump MMbyPN;
        }
        rfalse;
    }

    if (threshold < 0) {	
        threshold = 1;
        dict_flags_of_noun = (w->#dict_par1) & $$01110100;
        w = NextWord();
        while (Refers(obj, wn-1)) {
            threshold++;
            if (w)
               dict_flags_of_noun = dict_flags_of_noun | ((w->#dict_par1) & $$01110100);
            w = NextWord();
        }
    }

    k = threshold;
    ! *? kuutiota ja kuutio tekevät tämän hypyn 
    jump MMbyPN;
];



[ Adjudicate context i j k good_flag good_ones last n flag offset
    sovert;

! Tämä ei tälläisenään toimi
! --------------------------    
!    if (indef_mode == 1 && indef_type && PLURAL_BIT ~= 0 && luku == 1) ! action_to_be ~= ##XPurloin)
!    {  indef_mode = 0;
!	indef_type = 0;
!	 flag = 1;	
!    }
    
#Ifdef DEBUG; 
    if (parser_trace >= 1)	
	switch (luku) 
	{	    
	 0: print "[* Adjudicate: päätteen luku tuntematon!]^";
	 1: print "[* Adjudicate: pääte yksikkö!]^";
	 2: print "[* Adjudicate: pääte monikko!]^";
		 
	    } 
	
    if (parser_trace >= 4) {
        print "   [!* Adjudicate. indef_mode ", indef_mode,
	", indef_type ", indef_type, " indef_type & PLURAL_BIT ",
	(indef_type & PLURAL_BIT), ", luku ", luku, "]^";
    } 
#Endif; ! DEBUG
    
    #Ifdef DEBUG;
    if (parser_trace >= 4) {
        print "   [Adjudicating match list of size ", number_matched, " in context ", context, "^";
        print "   ";

        if (indef_mode) {
            print "indefinite type: ";
            if (indef_type & OTHER_BIT)  print "other ";
            if (indef_type & MY_BIT)     print "my ";
            if (indef_type & THAT_BIT)   print "that ";
            if (indef_type & PLURAL_BIT) print "plural ";
            if (indef_type & LIT_BIT)    print "lit ";
            if (indef_type & UNLIT_BIT)  print "unlit ";
            if (indef_owner ~= 0) print "owner:", (name) indef_owner;
            new_line;
            print "   number wanted: ";
            if (indef_wanted == 100) print "all"; else print indef_wanted;
            new_line;
            print "   most likely GNAs of names: ", indef_cases, "^";
        }
        else print "definite object^";
    }
    #Endif; ! DEBUG

    j = number_matched-1; good_ones = 0; last = match_list-->0;
    for (i=0 : i<=j : i++) {
        n = match_list-->i;
        match_scores-->i = 0;

        good_flag = false;

        switch (context) {
          HELD_TOKEN, MULTIHELD_TOKEN:
            if (parent(n) == actor) good_flag = true;
          MULTIEXCEPT_TOKEN:
            if (advance_warning == -1) {
                good_flag = true;
            }
            else {
                if (n ~= advance_warning) good_flag = true;
            }
          MULTIINSIDE_TOKEN:
            if (advance_warning == -1) {
                if (parent(n) ~= actor) good_flag = true;
            }
            else {
                if (n in advance_warning) good_flag = true;
            }
          CREATURE_TOKEN:
            if (CreatureTest(n) == 1) good_flag = true;
          default:
            good_flag = true;
        }

        if (good_flag) {
            match_scores-->i = SCORE__IFGOOD;
            good_ones++; last = n;
        }
    }
    if (good_ones == 1) return last;
    

    ! If there is ambiguity about what was typed, but it definitely wasn't
    ! animate as required, then return anything; higher up in the parser
    ! a suitable error will be given.  (This prevents a question being asked.)

    if (context == CREATURE_TOKEN && good_ones == 0) return match_list-->0;

    if (indef_mode == 0) indef_type=0;

    ScoreMatchL(context);
    if (number_matched == 0)
	{ print "[ ****???? adjudicate palaa -1 (number_matched == 0) ]^";
    return -1;
    }


    if (indef_mode == 0) {
        !  Is there now a single highest-scoring object?
        i = SingleBestGuess();
        if (i >= 0) {

            #Ifdef DEBUG;
            if (parser_trace >= 4) print "   Single best-scoring object returned.]^";
            #Endif; ! DEBUG
            return i;
        }
    }

    if (indef_mode == 1 && indef_type & PLURAL_BIT ~= 0) {
        if (context ~= MULTI_TOKEN or MULTIHELD_TOKEN or MULTIEXCEPT_TOKEN
                     or MULTIINSIDE_TOKEN) {
            etype = MULTI_PE;
	    print "[ ****???? adjudicate sanoo etype = MULTI_PE ja
        palaa -1 ]";
	    return -1;
	   	    
	    
	    
        }
        i = 0; offset = multiple_object-->0; sovert = -1;
        for (j=BestGuess() : j~=-1 && i<indef_wanted && i+offset<63 : j=BestGuess()) {
            flag = 0;
            if (j hasnt concealed && j hasnt worn) flag = 1;
            if (sovert == -1) sovert = bestguess_score/SCORE__DIVISOR;
            else {
                if (indef_wanted == 100 && bestguess_score/SCORE__DIVISOR < sovert)
                    flag = 0;
            }
            if (context == MULTIHELD_TOKEN or MULTIEXCEPT_TOKEN && parent(j) ~= actor)
                flag = 0;
            if (action_to_be == ##Take or ##Remove && parent(j) == actor)
                flag = 0;
            k = ChooseObjects(j, flag);
            if (k == 1)
                flag = 1;
            else {
                if (k == 2) flag = 0;
            }
            if (flag == 1) {
                i++; multiple_object-->(i+offset) = j;
                #Ifdef DEBUG;
                if (parser_trace >= 4) print "   Accepting it^";
                #Endif; ! DEBUG
            }
            else {
                i = i;
                #Ifdef DEBUG;
                if (parser_trace >= 4) print "   Rejecting it^";
                #Endif; ! DEBUG
            }
        }
        if (i < indef_wanted && indef_wanted < 100) {
            etype = TOOFEW_PE; multi_wanted = indef_wanted;
            multi_had=i;
            return -1;
        }
        multiple_object-->0 = i+offset;
        multi_context = context;
        #Ifdef DEBUG;
        if (parser_trace >= 4)
            print "   Made multiple object of size ", i, "]^";
        #Endif; ! DEBUG
        return 1;
    }

    for (i=0 : i<number_matched : i++) match_classes-->i = 0;

    n = 1;
    for (i=0 : i<number_matched : i++)
        if (match_classes-->i == 0) {
            match_classes-->i = n++; flag = 0;
            for (j=i+1 : j<number_matched : j++)
                if (match_classes-->j == 0 && Identical(match_list-->i, match_list-->j) == 1) {
                    flag=1;
                    match_classes-->j = match_classes-->i;
                }
            if (flag == 1) match_classes-->i = 1-n;
        }
     n--; number_of_classes = n;

    #Ifdef DEBUG;
    if (parser_trace >= 4) {
        print "   Grouped into ", n, " possibilities by name:^";
        for (i=0 : i<number_matched : i++)
            if (match_classes-->i > 0)
                print "   ", (The) match_list-->i, " (", match_list-->i, ")  ---  group ",
                  match_classes-->i, "^";
    }
    #Endif; ! DEBUG

    if (indef_mode == 0) {
        if (n > 1) {
            k = -1;
            for (i=0 : i<number_matched : i++) {
                if (match_scores-->i > k) {
                    k = match_scores-->i;
                    j = match_classes-->i; j = j*j;
                    flag = 0;
                }
                else
                    if (match_scores-->i == k) {
                        if ((match_classes-->i) * (match_classes-->i) ~= j)
                            flag = 1;
                    }
            }

        if (flag) {
            #Ifdef DEBUG;
            if (parser_trace >= 4) print "   Unable to choose best group, so ask player.]^";
            #Endif; ! DEBUG
            return 0;
        }
        #Ifdef DEBUG;
        if (parser_trace >= 4) print "   Best choices are all from the same group.^";
        #Endif; ! DEBUG
        }
    }

    !  When the player is really vague, or there's a single collection of
    !  indistinguishable objects to choose from, choose the one the player
    !  most recently acquired, or if the player has none of them, then
    !  the one most recently put where it is.

    if (n == 1) dont_infer = true;
    return BestGuess();

]; ! Adjudicate

! ----------------------------------------------------------------------------
!  Refers works out whether the word at number wnum can refer to the object
!  obj, returning true or false.  The standard method is to see if the
!  word is listed under "name" for the object, but this is more complex
!  in languages other than English.
! ----------------------------------------------------------------------------



[ Refers obj wnum   wd k l m;

    
    if (obj == 0) rfalse;

    #Ifdef LanguageRefers;
    k = LanguageRefers(obj,wnum);
    if (k >= 0) return k;
    #Endif; ! LanguageRefers

    k = wn; wn = wnum; wd = NextWordStopped(); wn = k;

    if (parser_inflection >= 256) {
        k = indirect(parser_inflection, obj, wd);
        if (k >= 0) return k;
        m = -k;
    }
    else
        m = parser_inflection;
    k = obj.&m; l = (obj.#m)/WORDSIZE-1;
    for (m=0 : m<=l : m++)
        if (wd == k-->m) rtrue;
    rfalse;
];




! ==============================================================================================================
! General bug fix, which became necessary with the swedish grammar
! ==============================================================================================================

! ----------------------------------------------------------------------------
!  The CantSee routine returns a good error number for the situation where
!  the last word looked at didn't seem to refer to any object in context.
!
!  The idea is that: if the actor is in a location (but not inside something
!  like, for instance, a tank which is in that location) then an attempt to
!  refer to one of the words listed as meaningful-but-irrelevant there
!  will cause "you don't need to refer to that in this game" rather than
!  "no such thing" or "what's 'it'?".
!  (The advantage of not having looked at "irrelevant" local nouns until now
!  is that it stops them from clogging up the ambiguity-resolving process.
!  Thus game objects always triumph over scenery.)
! ----------------------------------------------------------------------------

[ CantSee  i w e;
    
    saved_oops=oops_from;

    if (scope_token ~= 0) {
        scope_error = scope_token;
        return ASKSCOPE_PE;
    }

    wn--; w = NextWord();
    e = CANTSEE_PE;
! ¤#¤ One line changed
!    if (w==pronoun_word)
    if (w==pronoun_word && ~~ TestScope(pronoun_obj))  ! TestScope condition added
    {
        pronoun__word = pronoun_word; pronoun__obj = pronoun_obj;
        e = ITGONE_PE;
    }
    i = actor; while (parent(i) ~= 0) i = parent(i);

    wn--;
    if (i has visited && Refers(i,wn) == 1) e = SCENERY_PE;
    else {
        Descriptors();  ! skip past THE etc
        if (i has visited && Refers(i,wn) == 1) e = SCENERY_PE;
    }
    wn++;
    if (etype > e) return etype;
    return e;
];





! ----------------------------------------------------------------------------
!  NounDomain does the most substantial part of parsing an object name.
!
!  It is given two "domains" - usually a location and then the actor who is
!  looking - and a context (i.e. token type), and returns:
!
!   0    if no match at all could be made,
!   1    if a multiple object was made,
!   k    if object k was the one decided upon,
!   REPARSE_CODE if it asked a question of the player and consequently rewrote
!        the player's input, so that the whole parser should start again
!        on the rewritten input.
!
!   In the case when it returns 1<k<REPARSE_CODE, it also sets the variable
!   length_of_noun to the number of words in the input text matched to the
!   noun.
!   In the case k=1, the multiple objects are added to multiple_object by
!   hand (not by MultiAdd, because we want to allow duplicates).
! ----------------------------------------------------------------------------


[ NounDomain domain1 domain2 context    first_word i j k l
    answer_words marker;


#Ifdef DEBUG;
    if (parser_trace >= 4) 
    { print "   [!* NounDomain. indef_mode ", indef_mode,
	", indef_type ", indef_type, " indef_type & PLURAL_BIT ",
	(indef_type & PLURAL_BIT), ", luku ", luku, "]^";
    } 
#Endif; ! DEBUG


    #Ifdef DEBUG;
    if (parser_trace >= 4) {
        print "   [NounDomain called at word ", wn, "^";
        print "   ";
        if (indef_mode) {
            print "seeking indefinite object: ";
            if (indef_type & OTHER_BIT)  print "other ";
            if (indef_type & MY_BIT)     print "my ";
            if (indef_type & THAT_BIT)   print "that ";
            if (indef_type & PLURAL_BIT) print "plural ";
            if (indef_type & LIT_BIT)    print "lit ";
            if (indef_type & UNLIT_BIT)  print "unlit ";
            if (indef_owner ~= 0) print "owner:", (name) indef_owner;
            new_line;
            print "   number wanted: ";
            if (indef_wanted == 100) print "all"; else print indef_wanted;
            new_line;
            print "   most likely GNAs of names: ", indef_cases, "^";
        }
        else print "seeking definite object^";
    }
    #Endif; ! DEBUG

    match_length = 0; number_matched = 0; match_from = wn; placed_in_flag = 0;

    SearchScope(domain1, domain2, context);

    #Ifdef DEBUG;
    if (parser_trace >= 4) print "   [ND made ", number_matched, " matches]^";
    #Endif; ! DEBUG
  
    
    wn = match_from+match_length;

    ! If nothing worked at all, leave with the word marker skipped past the
    ! first unmatched word...

    if (number_matched == 0) { wn++; rfalse; }

    ! Suppose that there really were some words being parsed (i.e., we did
    ! not just infer).  If so, and if there was only one match, it must be
    ! right and we return it...

    if (match_from <= num_words) {
        if (number_matched == 1) {
            i=match_list-->0;
            return i;
        }

        ! ...now suppose that there was more typing to come, i.e. suppose that
        ! the user entered something beyond this noun.  If nothing ought to follow,
        ! then there must be a mistake, (unless what does follow is just a full
        ! stop, and or comma)

        if (wn <= num_words) {
            i = NextWord(); wn--;
            if (i ~=  AND1__WD or AND2__WD or AND3__WD or comma_word
                   or THEN1__WD or THEN2__WD or THEN3__WD
                   or BUT1__WD or BUT2__WD or BUT3__WD) {
                if (lookahead == ENDIT_TOKEN) rfalse;
            }
        }
    }

    ! Now look for a good choice, if there's more than one choice...

     
    
    number_of_classes = 0;

    if (number_matched == 1) i = match_list-->0;
    if (number_matched > 1) {
        i = Adjudicate(context);
	
        if (i == -1) rfalse;	
        if (i == 1) rtrue;       !  Adjudicate has made a multiple
                             !  object, and we pass it on
    }

    ! If i is non-zero here, one of two things is happening: either
    ! (a) an inference has been successfully made that object i is
    !     the intended one from the user's specification, or
    ! (b) the user finished typing some time ago, but we've decided
    !     on i because it's the only possible choice.
    ! In either case we have to keep the pattern up to date,
    ! note that an inference has been made and return.
    ! (Except, we don't note which of a pile of identical objects.)

    if (i ~= 0) {
        if (dont_infer) return i;
        if (inferfrom == 0) inferfrom=pcount;
        pattern-->pcount = i;
        return i;
    }

    ! If we get here, there was no obvious choice of object to make.  If in
    ! fact we've already gone past the end of the player's typing (which
    ! means the match list must contain every object in scope, regardless
    ! of its name), then it's foolish to give an enormous list to choose
    ! from - instead we go and ask a more suitable question...

    if (match_from > num_words) jump Incomplete;

    ! Now we print up the question, using the equivalence classes as worked
    ! out by Adjudicate() so as not to repeat ourselves on plural objects...
  
   if (context==CREATURE_TOKEN) L__M(##Miscellany, 45);
   else		                L__M(##Miscellany, 46);

  
    j = number_of_classes; marker = 0;
    for (i=1 : i<=number_of_classes : i++) {
        while (((match_classes-->marker) ~= i) && ((match_classes-->marker) ~= -i)) marker++;
        k = match_list-->marker;


!! Vastaukset 45 ja 46 - "ota ruokaa"... "Tarkoitatko pahaa ruokaa vai hyvää ruokaa?",
!! tai "ota ruoka"... "Paha ruoka vai hyvä ruoka?"
!! jos syöte on nominatiivi, ei tulosta "Tarkoitatko ", vaan ensimmäinen sana isolla alkuk.

        if ((match_classes-->marker > 0) && (csLR == 0 or 1)) 
         	{ if (i == 1) print (k_nominatiivi) k; else print (nominatiivi) k; }; 		

!! jos syöte on muu kuin nominatiivi...

        if ((i == 1) && (csLR ~= 0 or 1)) print "Tarkoitatko ";
  
        if ((match_classes-->marker > 0) && (csLR ~= 0 or 1))
        	switch (csLR) {
        !!-	0: print (nominatiivi) k; 
		2: print (genetiivi) k;
		3: print (partitiivi) k;
		4: print (inessiivi) k;
		5: print (elatiivi) k;
		6: print (illatiivi) k;
		7: print (adessiivi) k;
		8: print (ablatiivi) k;
		9: print (allatiivi) k;
		10: print (essiivi) k;
		11: print (translatiivi) k;
		};

        if (i < j-1)  print (string) COMMA__TX;

         if (i == j-1) print (string) OR__TX; !!!# OR__TX on "vai"
        
    } 
    L__M(##Miscellany, 57); !!!# kysymysmerkki ja rivinvaihto
    ! ...and get an answer:

  .WhichOne;
    #Ifdef TARGET_ZCODE;
    for (i=2 : i<INPUT_BUFFER_LEN : i++) buffer2->i = ' ';
    #Endif; ! TARGET_ZCODE
    
    answer_words=Keyboard(buffer2, parse2);
    ! Conveniently, parse2-->1 is the first word in both ZCODE and GLULX.
    first_word = (parse2-->1);

    ! Take care of "all", because that does something too clever here to do
    ! later on:

    if (first_word == ALL1__WD or ALL2__WD or ALL3__WD or ALL4__WD or ALL5__WD) {
        if (context == MULTI_TOKEN or MULTIHELD_TOKEN or MULTIEXCEPT_TOKEN or MULTIINSIDE_TOKEN) {
            l = multiple_object-->0;
            for (i=0 : i<number_matched && l+i<63 : i++) {
                k = match_list-->i;
                multiple_object-->(i+1+l) = k;
            }
            multiple_object-->0 = i+l;
            rtrue;
        }
        L__M(##Miscellany, 47);
        jump WhichOne;
    }

    ! If the first word of the reply can be interpreted as a verb, then
    ! assume that the player has ignored the question and given a new
    ! command altogether.
    ! (This is one time when it's convenient that the directions are
    ! not themselves verbs - thus, "north" as a reply to "Which, the north
    ! or south door" is not treated as a fresh command but as an answer.)

    #Ifdef LanguageIsVerb;
    if (first_word == 0) {
        j = wn; first_word = LanguageIsVerb(buffer2, parse2, 1); wn = j;
    }
    #Endif; ! LanguageIsVerb
    if (first_word ~= 0) {
        j = first_word->#dict_par1;
        if ((0 ~= j&1) && ~~LanguageVerbMayBeName(first_word)) {
            CopyBuffer(buffer, buffer2);
            return REPARSE_CODE;
        }
    }

    ! Now we insert the answer into the original typed command, as
    ! words additionally describing the same object
    ! (eg, > take red button
    !      Which one, ...
    !      > music
    ! becomes "take music red button".  The parser will thus have three
    ! words to work from next time, not two.)

    #Ifdef TARGET_ZCODE;
    k = WordAddress(match_from) - buffer; l=buffer2->1+1;
     for (j=buffer + buffer->0 - 1 : j>=buffer+k+l : j--) j->0 = 0->(j-l);
     for (i=0 : i<l : i++) buffer->(k+i) = buffer2->(2+i);

    buffer->(k+l-1) = ' '; 
    buffer->1 = buffer->1 + l;
    if (buffer->1 >= (buffer->0 - 1)) buffer->1 = buffer->0;
    #Ifnot; ! TARGET_GLULX
    k = WordAddress(match_from) - buffer;
    l = (buffer2-->0) + 1;
    for (j=buffer+INPUT_BUFFER_LEN-1 : j>=buffer+k+l : j--) j->0 = j->(-l);
    for (i=0 : i<l : i++) buffer->(k+i) = buffer2->(WORDSIZE+i);
    buffer->(k+l-1) = ' ';
    buffer-->0 = buffer-->0 + l;
    if (buffer-->0 > (INPUT_BUFFER_LEN-WORDSIZE)) buffer-->0 = (INPUT_BUFFER_LEN-WORDSIZE);
    #Endif; ! TARGET_

    ! Having reconstructed the input, we warn the parser accordingly
    ! and get out.

    return REPARSE_CODE;

    ! Now we come to the question asked when the input has run out
    ! and can't easily be guessed (eg, the player typed "take" and there
    ! were plenty of things which might have been meant).

  .Incomplete;

    if (context == CREATURE_TOKEN) L__M(##Miscellany, 48);
    else                           L__M(##Miscellany, 49);

    #Ifdef TARGET_ZCODE;
    for (i=2 : i<INPUT_BUFFER_LEN : i++) buffer2->i=' ';
    #Endif; ! TARGET_ZCODE
    answer_words = Keyboard(buffer2, parse2);

    first_word=(parse2-->1);
    #Ifdef LanguageIsVerb;
    if (first_word==0) {
        j = wn; first_word=LanguageIsVerb(buffer2, parse2, 1); wn = j;
    }
    #Endif; ! LanguageIsVerb

    ! Once again, if the reply looks like a command, give it to the
    ! parser to get on with and forget about the question...

    if (first_word ~= 0) {
        j = first_word->#dict_par1;
        if (0 ~= j&1) {
            CopyBuffer(buffer, buffer2);
            return REPARSE_CODE;
        }
    }

    ! ...but if we have a genuine answer, then:
    !
    ! (1) we must glue in text suitable for anything that's been inferred.

    if (inferfrom ~= 0) {
        for (j=inferfrom : j<pcount : j++) {
            if (pattern-->j == PATTERN_NULL) continue;
            #Ifdef TARGET_ZCODE;
            i = 2+buffer->1; (buffer->1)++; buffer->(i++) = ' ';
            #Ifnot; ! TARGET_GLULX
            i = WORDSIZE + buffer-->0;
            (buffer-->0)++; buffer->(i++) = ' ';
            #Endif; ! TARGET_

            #Ifdef DEBUG;
            if (parser_trace >= 5) print "[Gluing in inference with pattern code ", pattern-->j, "]^";
            #Endif; ! DEBUG

            ! Conveniently, parse2-->1 is the first word in both ZCODE and GLULX.

            parse2-->1 = 0;

            ! An inferred object.  Best we can do is glue in a pronoun.
            ! (This is imperfect, but it's very seldom needed anyway.)

            if (pattern-->j >= 2 && pattern-->j < REPARSE_CODE) {
                PronounNotice(pattern-->j);
                for (k=1 : k<=LanguagePronouns-->0 : k=k+3)
                    if (pattern-->j == LanguagePronouns-->(k+2)) {
                        parse2-->1 = LanguagePronouns-->k;
                        #Ifdef DEBUG;
                        if (parser_trace >= 5) print "[Using pronoun '", (address) parse2-->1, "']^";
                        #Endif; ! DEBUG
                        break;
                    }
            }
            else {
                ! An inferred preposition.
                parse2-->1 = No__Dword(pattern-->j - REPARSE_CODE);
                #Ifdef DEBUG;
                if (parser_trace >= 5) print "[Using preposition '", (address) parse2-->1, "']^";
                #Endif; ! DEBUG
            }

            ! parse2-->1 now holds the dictionary address of the word to glue in.

            if (parse2-->1 ~= 0) {
                k = buffer + i;
                #Ifdef TARGET_ZCODE;
                @output_stream 3 k;
                 print (address) parse2-->1;
                @output_stream -3;
                k = k-->0;
                for (l=i : l<i+k : l++) buffer->l = buffer->(l+2);
                i = i + k; buffer->1 = i-2;
                #Ifnot; ! TARGET_GLULX
                k = PrintAnyToArray(buffer+i, INPUT_BUFFER_LEN-i, parse2-->1);
                i = i + k; buffer-->0 = i - WORDSIZE;
                #Endif; ! TARGET_
            }
        }
    }

    ! (2) we must glue the newly-typed text onto the end.

    #Ifdef TARGET_ZCODE;
    i = 2+buffer->1; (buffer->1)++; buffer->(i++) = ' ';
    for (j=0 : j<buffer2->1 : i++,j++) {
        buffer->i = buffer2->(j+2);
        (buffer->1)++;
        if (buffer->1 == INPUT_BUFFER_LEN) break;
    }
    #Ifnot; ! TARGET_GLULX
    i = WORDSIZE + buffer-->0;
    (buffer-->0)++; buffer->(i++) = ' ';
    for (j=0 : j<buffer2-->0 : i++,j++) {
        buffer->i = buffer2->(j+WORDSIZE);
        (buffer-->0)++;
        if (buffer-->0 == INPUT_BUFFER_LEN) break;
    }
    #Endif; ! TARGET_

    ! (3) we fill up the buffer with spaces, which is unnecessary, but may
    !     help incorrectly-written interpreters to cope.

    #Ifdef TARGET_ZCODE;
    for (: i<INPUT_BUFFER_LEN : i++) buffer->i = ' ';
    #Endif; ! TARGET_ZCODE

    return REPARSE_CODE;

]; ! end of NounDomain

!!!# PrintCommand 

! ----------------------------------------------------------------------------
!  PrintCommand reconstructs the command as it presently reads, from
!  the pattern which has been built up
!
!  If from is 0, it starts with the verb: then it goes through the pattern.
!  The other parameter is "emptyf" - a flag: if 0, it goes up to pcount:
!  if 1, it goes up to pcount-1.
!
!  Note that verbs and prepositions are printed out of the dictionary:
!  and that since the dictionary may only preserve the first six characters
!  of a word (in a V3 game), we have to hand-code the longer words needed.
!
!  (Recall that pattern entries are 0 for "multiple object", 1 for "special
!  word", 2 to REPARSE_CODE-1 are object numbers and REPARSE_CODE+n means the
!  preposition n)
! ----------------------------------------------------------------------------

!! kys, sijamuotoja yms
[ PrintCommand from i k spacing_flag kys; 

  kys = from;    
    
    if (from == 0) {
       	i = verb_word;  
        if (LanguageVerb(i) == 0)
            if (PrintVerb(i) == 0) print (address) i;
        from++; spacing_flag = true;
	
    }

    for (k=from : k<pcount : k++) { 	
        i = pattern-->k;
        if (i == PATTERN_NULL) continue;
        if (spacing_flag) print (char) ' ';
        if (i ==0 ) { print (string) THOSET__TX; jump TokenPrinted; }
        if (i == 1) { print (string) THAT__TX;   jump TokenPrinted; }
        if (i >= REPARSE_CODE)     
                    print (address) No__Dword(i-REPARSE_CODE);
        else
	
            if (i in compass && LanguageVerbLikesAdverb(verb_word))
                LanguageDirection (i.door_dir); ! the direction name
	! as adverb
            else
	  
		
		!! print "** k ", k, ", kys ", kys, ", caseis ", CaseIs,
		!!    " verb_word ", (address)
		!!    verb_word;

	! jos, 1. sana nom. 2. sana taipuu	
		if (CaseIs ~= 0 && k >= 2) 
             		switch (CaseIs) {
                 	csNom: print (nominatiivi) i;
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
	
    .TokenPrinted;
	spacing_flag = true;

	
    }

    ! kys
    ! 1: (lasille siniselle pöydälle)
    ! 0: laita kuutio mihin?"
       
    if (kys == 0) PrintKysymys(verb_word, from, k); 	

    
];
