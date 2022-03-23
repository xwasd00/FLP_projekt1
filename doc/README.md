# Funkcionální projekt do FLP: bkg-2-cnf
- Autor: Michal Sova (xsovam00@stud.fit.vutbr.cz)
## Implementace
Implementace je rozdělena do několika souborů v `src/`.
- `Main.hs` - Zpracování argumentů a výpis výsledných gramatik na standardní výstup.
- `Types.hs` - Deklarace vlastních datových typů potřebné pro zpracování gramatiky.
- `Parser.hs` - Parsování gramatiky z formy textu do dané datové struktury.
- `Algorithm1.hs` - Nahrazování jednoduchých pravidel.
- `Algorithm2.hs` - Převedení gramatiky do CNF formy.
- `Helper.hs` - Pomocné funkce.
## Testy
V adresáři `test/` se nachází pár vzorových testovacích souborů. Test se spustí pomocí `make test`, přičemž se vytvoří adresář `test-diff/`. Pokud výstup nesouhlasí se vzorovým, je vytvořena adresář `test-diff/` a je vytvořen soubor s rozdílem výstupu od vzorového pomocí programu diff. V případě `make clean` se odstraní s istatními soubory i adresář `test-diff/`.