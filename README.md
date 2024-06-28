# Functional Images

În literatura de specialitate, embedded domain-specific languages (EDSL) sunt considerate unele dintre principalele aplicații ale programării funcționale. Sintagma merită explicată pe componente. Un domain-specific language (DSL) este un limbaj de programare restrâns, dedicat unui domeniu particular; exemple bine-cunoscute sunt HTML (pentru descrierea paginilor web) și SQL (pentru interogarea bazelor de date). Acestea se opun așa-ziselor general-purpose languages, care pot fi utilizate într-o gamă largă de domenii, fiind cazul limbajelor C, Java, Python, și al limbajelor studiate la PP. Deși restrânse, DSL-urile sunt înzestrate cu o serie de operații expresive dedicate, care simplifică masiv modelarea în cadrul domeniului specific către care au fost țintite.

Pentru implementarea unui DSL, există următoarele două variante:

stand-alone: un interpretor sau un compilator sunt scrise de la zero pentru limbajul respectiv, necesitând o cantitate mare de efort
embedded: limbajul este integrat sub forma unei biblioteci într-un limbaj existent (de exemplu, Haskell), valorificându-se astfel construcțiile limbajului gazdă, precum și interpretorul și/sau compilatorul existent.
Continuând pe varianta embedded (de exemplu, în Haskell), un DSL poate utiliza două tipuri de reprezentări în cadrul limbajului gazdă:

shallow embeddings: obiectele din cadrul DSL-ului au reprezentări concrete în limbajul gazdă; de exemplu, dacă dorim să creăm un mic limbaj pentru definirea expresiilor aritmetice, fiecare expresie ar putea fi reprezentată prin valoarea ei (Int etc.), iar operațiile pe expresii (adunare etc.) manipulează valorile acestora
deep embeddings: obiectele din cadrul DSL-ului au reprezentări abstracte, ca descrieri ale operațiilor care construiesc acele obiecte; continuând exemplul expresiilor aritmetice de mai sus, fiecare expresie poate fi reprezentată prin arborele ei sintactic, care poate fi interpretat ulterior în diverse moduri; o posibilă interpretare vizează obținerea valorii expresiei, realizând astfel corespondența cu shallow embedding-ul de mai sus.

Se propune ca studiu de caz un mic DSL embedded în Haskell pentru lucrul cu imagini. Pe parcursul etapelor, vom explora ambele modalități de reprezentare de mai sus (shallow, respectiv deep), avantajele și dezavantajele fiecăreia, precum și corespondența lor conceptuală.
