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

# Użycie

Jako, że pakiet `MLASZdaneAdm2` zawiera zbiór funkcji służących do liczenia wskaźników dla specyficznych danych w specyficzny sposób, poniżej opisano kolejne kroki, które należy przejść, aby otrzymać zbiór wskaźników zagregowanych.

## Przygotowanie danych administracyjnych

Dla każdego typu szkoły zbiór wskaźników zagregowanych liczony jest oddzielnie. W przypadku tej edycji dla 4 typów szkół: branżowych szkół pierwszego stopnia, techników, szkół policealnych oraz liceów ogólnokształcących. Taka konstrukcja pakietu bierze się z dwóch powodów:

  1. Część wskaźników jest specyficzna dla danego typu szkoły - np. obliczane są tylko dla techników i szkół policealnych z pominięciem branżowych szkół pierwszego stopnia;
  2. Pakiet `MLASZraportyAdm2`, który wykorzysuje zbiory wskaźników na poziomie zagregowanym stworzone przy użyciu pakiety `MLASZdaneAdm2`, zawiera oddzielne szabolny dla każdego z typów szkoły. Wynika to zarówno ze złożoności raportów jak i tego, że nie w każdym typie szkoły prezentowane są te same wskaźniki - raporty różnią się wyglądem i zawartością w zależności od typu szkoły.

Zbiór wskaźników liczony jest z tabel pośrednich tworzonych za pomocą pakietu [`MLASdaneAdm`](https://github.com/tzoltak/MLASdaneAdm). W pierwszym kroku należy z tabel pośrednich odfiltrować obserwacje (przykład dla abolswentów branżowych szkół pierwszego stopnia z 2020 roku).

```r
library(dplyr)
p4 = p4 %>%
  filter(ROK_ABS %in% 2020,
         TYP_SZK %in% c("Branżowa szkoła I stopnia"))
p3 = p3 %>%
  filter(ROK_ABS %in% 2020,
         ROK %in% 2020,
         TYP_SZK %in% c("Branżowa szkoła I stopnia"))
p2 = p2 %>%
  filter(ROK_ABS %in% 2020,
         TYP_SZK %in% c("Branżowa szkoła I stopnia"))
```

## Definicja grup odniesienia

Do funkcji agregujących, jako jeden z argumentów, należy podać ramkę danych będącą definicją grupy oraz grupy odniesienia, czyli de facto poziomy agregacji dla grup prezentowanych w raportach oraz ich grup odniesienia. Można to osiągnąć na 2 sposoby: albo za pomocą funkcji `utworz_grupowanie_ze_zmiennej()` z pakietu [`MLASZdane`](https://github.com/bartplat/MLASZdane) albo samodzielnie. Istotne jest, aby wynikiem była ramka danych zawierająca kolumny o wymaganych nazwach. Pierwsza opcja ma jednak pewne ograniczenia: grupę odniesienia można definiować tylko za pomocą jednej nazwy zmiennej. W przypadku potrzeby użycia więcej niż jednej zmiennej ramkę danych zawierającą definicje grup i grup odniesienia należy przygotować samodzielnie.

```r
grupy_bs1 = p4 %>%
  select(ID_SZK, TYP_SZK, TERYT_WOJ_SZK) %>%
  mutate(grupa = paste0("ID_SZK %in% ", ID_SZK),
         odniesienie = paste0("!(ID_SZK %in% ", ID_SZK, ") & ",
                              "(TERYT_WOJ_SZK %in% ", TERYT_WOJ_SZK, ") & ",
                              "(TYP_SZK %in% \"", TYP_SZK, "\")")) %>% 
  distinct()
```

## Obliczanie wskaźników na poziomie zagregowanym

Ostatnim krokiem, mając już odpowiednio okrojone tabele pośrednie oraz ramkę danych z definicja grup odniesienia, jest wygenerowanie wskaźników na danym poziomie agregacji.

```r
library(MLASZdaneAdm2)
wskazniki_bs1 = agreguj_bs1_admin_1(p2, p3, p4, grupy_bs1, 0, 2021)

szk = wskazniki_bs1$grupy
woj = wskazniki_bs1$grupyOdniesienia
```

Wynikiem działania funkcji `agreguj_bs1_admin_1()` (oraz analogicznych `agreguj_tech_admin_1()` i `agreguj_spolic_admin_1()`) jest lista o dwóch elementach: ramkach danych zagregwanych dla grupy oraz dla grupy odniesienia.
