## TODO: 

- Alle bsp durchlaufen lassen
- System.out.println in Konstantenpool


## TODO (Lara):

- Char zu char
- init Funktion immer reinhauen "<init>", und dann Vars aus Init als Field
- Void -> VoidT!!!

- Semantikcheck

    - welche Regeln fehlen noch?

        immer mit aktuellen Type vergleichen!! 

        - BlockStmts type -> check
        - While 
        - Funktionstypen prüfen?
        - Assign korrekt machen

    - Beispiele
    - return Regel
- advancedExamples/Addn.minijava prüfen:
  - Constructor
  - bei n + x: Parser macht x zu this.x
- examples/explFieldRef3.minijava prüfen
- this.method prüfen
- String parsen können
    - System.out.println mit " " statt ~. 
    - System.out.println als Methode

## Nice to have:

- different access flags für fields (bis jetzt alles 0x0000)
- classfile automatisch ausführen
- [Class]
- access flags klassen bis auf generisch und abstract
- Visibility upgrade (final, public, private)
