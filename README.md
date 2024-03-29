# Inform 6 kielimäärittelyt
Finnish definitions for Inform 6
---

Esimerkkipelit löytyvät täältä:
<https://github.com/objektiivinenironia/i6-esimerkit>

Tarvitset myös kirjaston Inform 6/11, kääntäjän ja tulkkiohjelman.

### Kirjasto 6/11:

<http://www.ifarchive.org/if-archive/infocom/compilers/inform6/library/old/inform_library611.zip>


### Kääntäjiä:

<http://www.ifarchive.org/indexes/if-archiveXinfocomXcompilersXinform6Xexecutables.html>

Kääntämisen pitäisi onnistua komentorivillä kun lisäät lähdetiedoston ja hakemistojen polut riville, esimerkiksi: 

```  
  inform6 [lelukauppa.inf] [valitsimet] +include_path=[tämä hakemisto], [inform_library611]
```

Lähdetiedostoissa käytetään oletuksena merkistöä ISO-8859. Kääntäjässä on versiosta 6.33 lähtien valitsin -Cu joka hyväksyy UTF-8:n. Jos haluaa käyttää lähdetiedostossa merkistöä UTF-8, täytyy myös kielimäärittelyjen merkistö muuntaa. 

Valitsin -G kääntää Glulx-virtuaalikoneelle.

### Tulkkeja:

<http://ifarchive.org/indexes/if-archive/infocom/interpreters/>


## Puutteita

 - Ei toteutettu: nominien monikon ja yksikön taivutuksen tulostus samassa nimessä. Esimerkiksi "nakit ja muusi" olisi kiva tulostaa.
    - *TODO: Taivutukset tulostavain pätkäin korjaaminen.*
 - Verbien tulostuksessa ei taivutusta.
 - Ei toteutettu: komitatiivi, abessiivi ja instruktiivi.
   Järkeilynä ollut että asian voi sanoa myös toisin 
   esim. abessiivi "aivoitta" -> "ilman aivoja".
 - Parsittaessa ilmeisesti tarvitaan vieläkin tällaista: 
   with name 'punainen' 'punaise' 'punais'. 

## Kehittely

Nopsa lista englanninkielistä aineistoa:

<https://intfiction.org/t/i6-the-list-of-inform-6-documentation/48409>


### Nominien taivutuksen tulostaminen

Jotta nimi saadaan taivutettua ja tulostettua, annetaan ohjelmalle taivutusohje. Taivutusohjeen voi yrittää tuottaa apuohjelmalla:

<https://github.com/objektiivinenironia/i6-taivutusapu>

Esimerkiksi jos halutaan tulostaa *"nakki ja muusi"*:

```
"nak/ki muusi/", 
  gen "in/n", par "kia/a", ess "kina/na", ill "kiin/in",
```

Taivutusohjetta voi muokata tai sen voi tehdä käsin, jolloin nomini jaetaan kauttaviivalla "/" ennen astevaihtelua, tai jos
astevaihtelua ei esiinny, menee "/" nominin loppuun. Jos kauttaviivaa ei ole, sanaa ei taivuteta.
">/" tulostaa kauttaviivan "/".

```
"nak/ki ja muusi/"
```

Merkkijonoihin *gen, par, ess* ja *ill* annetaan astevaihtelun jälkeinen sanavartalo ja vastaava sijapääte (genetiivi, partitiivi, essiivi ja illatiivi).

Monikko tulostetaan kun objektilla on määrite *pluralname*. Monikko tarvitsee joskus myös merkkijonon *ine*, jotta taivutus tulee oikein. 


### Omistusmuotojen tulostaminen

Omistusmuotoja tulostettaessa lisätään kauttaviiva ja
omistusliite nimen perään, mutta esimerkiksi "kapsäkkisi" tarvitsee lisäapua (ks. Lelukauppa):

```
  Object satchel "kapsäk/ki/si"
  
    gen "in", par "kiä", ess "kinä", ill "kiin",
    	short_name
    	  [; if (Sija == csGen) {print "kapsäkkisi"; rtrue;};
    		if (Sija == csIll) {print "kapsäkkiisi"; rtrue;};
    		if (Sija == csTra) {print "kapsäkiksesi"; rtrue;};
    		rfalse;],
```

### Oletus partitiivi

Antamalla määritteen `oletus_par` nimi tulostuu oletuksena partitiivissa - esim. "maukasta ruokaa".

### Debug tulostus

Taivutuksen tulostusta voi testata komennolla *sijat [olion nimi]*.

---

### Parsiminen: vahva vai heikko astevaihtelu

Esimerkiksi jos pelissä on "Maukka" ja "maukasta ruokaa", käsky
"anna maukalle maukasta"
Ei välttämättä toimi ilman parsimisohjeita tyyliin:

Maukka
```
vahva_a 'maukka', heikko_a 'mauka'
```
maukas ruoka
```
vahva_b 'maukkaa', heikko_b 'maukas'
```

### Pläp

Inform-kirjasto 6/11 (c) Graham Nelson 1993-2004.

Osia ruotsinkielisestä (c) Fredrik Ramsberg ja venäjänkielisestä (c) Denis Gayev kotoistuksesta on käytetty tekijöiden luvalla.

Tämä kotoistus (c) satunnainenmerkkijono 2010-2023. Samat käyttöehdot kuin Inform.

Muita Inform-kotoistuksia eri kielille

<https://www.ifarchive.org/indexes/if-archive/infocom/compilers/inform6/library/translations/>

Esimerkkipelien lähdetiedostoissa lisätietoja.














