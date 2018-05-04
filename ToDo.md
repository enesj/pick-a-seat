**Commands:**

1. editing - drawing
2. undo
3. redo
4. show-hide (tables)
5. **zoom**


**modes:**
1. move,
2. copy
3. delete
4. merge /spajanje dva objektakoji imaju zajedničku liniju/
5. **move and resize all**


**Varijante za promjenu moda:**

- meni
- klik na selektovani objekt
- kontekst meni




**Bugs:**



**Rezervacije**

- **osnovni templejt**
1. Postoji jedan osnovni templejt za svaku prostoriji
2. Promjena templejta se može skedžulirati unaprijed ili izvršiti trenutno
3. Trajanje templejta je od dana postavljanja pa nadalje
4. Kod izmjene templejta ne smiju se brisati stolovi koji su rezervisani u bilo kom predstojećem eventu.
   Ako želimo da izbrišemo ovakve stolove potrebno je da te rezerevacije predhodno prebacimo na drugi sto ili
   u status čekanja za dodijelu stola

- **privremeni templejti**
1. Vezani su za određeni event i traju dok traje event
2. Ako event nema svoj (privremeni) templejt koristi se osnovni teplejt

- **eventi**

1. Eventi su termini u kojima je moguće izvršiti rezervaciju.
2. Vezani su za datum i imaju početak i kraj (u satima i minutama)
3. Može biti nijedan, jedan ili više *eventa* vezanih za jedan datum (ručak, večera, muzika ...).
4. Jedan event može biti vezan samo za jedan datum.
5. Eventi se mogu ponavljati sedmično (svaki dan u sedmici ili u neke određene dane)
6. Ponavljanje eventa se može blokirati za određene dane ili periode (dani kada se dešavaju neki posebni eventi)
7. Vlasnik može defnisati neradne dane (nema nikakvih eventa).
8. Valsnik može odrediti koliko dana unaprijed prihvata rezervacije.
9. Eventi mogu imati svoj (priveremeni) templjet, a ako nemaju koristi se osnovni templejt.
10. Prevremeni templejt se može ponavljati sa eventom a može biti i jednokratan

- **rezervacije**

1. Rezervacija je vezana za event i ID stola (u posebnim slučajevima može bit vezana za stolicu)
2. Rezervacija takođe je vezana za osobu koja vrši rezervaciju (u posebnim slučajevima i sve osobe koje dolaze na event - stolice)


