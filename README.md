# SPLCompiler
SPL Compiler entstand im Rahmen des Moduls "Compilerbau" an der Technischen Hochschule Mittelhessen.

Der Compiler ist in **Scala** geschrieben
und verarbeitet Programme die in SPL(Simple Programm Language) erstellt wurden. Er prüft ob die Syntax korrekt ist, in dem ein abstrakter Sytanxbaum (AST) aufgbaut wird. Wenn die Syntax mit anschließender Typprüfung erfolgreich geprüft sind, geht es weiter mit der Verarbeitung. Abschließend wird der Maschinencode für die Zielmaschine, in diesem Fall die virtuelle Puck Maschine, generiert.
