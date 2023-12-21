# Aufgabenverteilung

## Gemeinsame Aufgaben
- GIT-Repository: Einrichten eines GIT-Repositories bei GitHUB
- Abstrakte Syntax: Aufbau der abstrakten Syntax aus dem Parsetree
- Dokumentation: Erstellen der Dokumentation

### Scannen/Parsen/Grammatik

    Frage:
    Das evtl. noch zu Aufgaben von allen zählen? 
    
- Scannen: alex–File oder Scanner von Hand programmieren
- Grammatik: Erstellen einer Mini-Java-Grammatik an Hand der Spezifikation
- Parsen: Erstellen des happy–Files oder des Kombinator–Parsers und Aufbau
des abstrakten Syntaxbaums

## Aufgaben vergeben:

### Semantische Analyse: Typisierung der abstrakten Syntax (1 Person)

### Aufbau eines abstrakten ClassFiles (1 Person)
### Konstantenpool (1 Person)
### Nur bei Bearbeitung durch 3 Personen: Umwandlung des ClassFiles in Bytecode (1 Person)
### ggf. Tester (1 Person)
– Testsuite von Java–Files, die alle implementierten Features abdecken.

– Händische Übersetzung aller Java-Files der Testsuite in die abstrakte Syntax
(als Test–Eingaben für den Typ-Checker)


– Händische Übersetzung aller Testfälle der abstrakten Syntax in getypte ab-
strakte Syntax (als Test–Eingaben für den Code-Generierer).

– Händische Übersetzung aller Testfälle der getypten abstrakten Syntax in
abstrakten Bytecode.

– Automatische Tests, die die jeweiligen Testsuite mit den implementierten
Funktionen des Teams vergleichen

    Ideen zum Testen:
    - pro Type je ein funktionierendes und ein nicht funktionierendes Bsp möglichst minimal
    - 3-5 größere funktionierende Beispiele
