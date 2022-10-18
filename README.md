![KL+RP+IBE+EFS](inst/Belka-Losy-absolwentow-Kolor-PL.png)

# MLASZdaneAdm2

Pakiet został opracowany w ramach projektu *Monitorowanie losów edukacyjno-zawodowych absolwentów i młodych dorosłych* (POWR.02.15.00-IP.02-00-004/16) prowadzonego w Instytucie Badań Edukacyjnych w ramach działania 2.15. Kształcenie i szkolenie zawodowe dostosowane do potrzeb zmieniającej się gospodarki II. osi priorytetowej Efektywne polityki publiczne dla rynku pracy, gospodarki i edukacji Programu Operacyjnego Wiedza, Edukacja, Rozwój.

Pakiet `MLASZdaneAdm2` zawiera zbiór funkcji służących do obliczania wskaźników dla 2. edycji Monitoringu Losów Absolwentów z użyciem danych administracyjnych (resjestry: CIE, ZUS, POLON i CKE). Przy pomocy tego pakietu oraz drugiego, będącego silnikiem agregacji - [`MLASZdane`](https://github.com/bartplat/MLASZdane), można tworzyć zbiory wskaźników na zadanym poziomie agregacji.

# Instalacja / aktualizacja

Pakiet nie jest dostępny na CRAN, więc trzeba instalować go ze źródeł.

Instalację najprościej przeprowadzić wykorzystując pakiet *devtools*:

```r
install.packages('devtools') # potrzebne tylko, gdy nie jest jeszcze zainstalowany
devtools::install_github('losyabsolwentow/MLASZdaneAdm2', build_opts = c("--no-resave-data"))
```

Pakiet `MLASZdaneAdm2` jest zależny od pakietu `MLASZdane`, ale nie ma potrzeby go dodatkowo instalować, ponieważ dzieje się to podczas instalacji pakietu `MLASZdaneAdm2`.

Dokładnie w ten sam sposób można przeprowadzić aktualizację pakietu do najnowszej wersji.
