! fialpha.h
! =========
! From Swedish Library by Fredrik Ramsberg, except the banner translation
! which is from verblibm (Library 611):
!
! Contains a Z-machine alphabet optimised for Swedish. 
! Must be included before any text in encoded, even before declaring 
! the constants Story and Headline.
!
! It is not absolutely necessary to include this file, but without it, 
! dictionary words containing "åäö" may be reduced to the 2-6 first 
! characters, rather than 9. This file is only relevant for Zcode games,
! not Glulx.
!
! If you don't agree with this Zcharacter directive, make your own instead. 
! Just make sure that åäö is included in the table, and that line 2 is an 
! upper case version of line 1.



#ifdef TARGET_ZCODE;
Zcharacter "abcdefghijklmnoprstuvxyåäö"
           "ABCDEFGHIJKLMNOPRSTUVXYÅÄÖ"
              "012345.,!?'/-:()wqzWQZé";
#endif;

! The Inform banner in Finnish 

Replace Banner;

[ Banner i;
   if (Story ~= 0) {
        #Ifdef TARGET_ZCODE;
        #IfV5; style bold; #Endif;
        print (string) Story;
        #IfV5; style roman; #Endif;
        #Ifnot; ! TARGET_GLULX;
        glk($0086, 3); ! set header style
        print (string) Story;
        glk($0086, 0); ! set normal style
        #Endif; ! TARGET_
    }
    if (Headline ~= 0) print (string) Headline;
    #Ifdef TARGET_ZCODE;
    !!!#print "Release ", (HDR_GAMERELEASE-->0) & $03ff, " / Serial number ";
    print "Julkaisu ", (HDR_GAMERELEASE-->0) & $03ff, " / Sarjanumero ";
    for (i=0 : i<6 : i++) print (char) HDR_GAMESERIAL->i;
    #Ifnot; ! TARGET_GLULX;
    print "Release ";
    @aloads ROM_GAMERELEASE 0 i;
    print i;
    print " / Serial number ";
    for (i=0 : i<6 : i++) print (char) ROM_GAMESERIAL->i;
    #Endif; ! TARGET_
    print " / Inform v"; inversion;
    print " Kirjasto ", (string) LibRelease, " ";
    #Ifdef STRICT_MODE;
    print "S";
    #Endif; ! STRICT_MODE
    #Ifdef INFIX;
    print "X";
    #Ifnot;
    #Ifdef DEBUG;
    print "D";
    #Endif; ! DEBUG
    #Endif; ! INFIX
    new_line;
];
