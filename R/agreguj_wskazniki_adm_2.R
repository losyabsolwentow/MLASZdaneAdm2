#' @title Obliczanie wskaznikow na poziomie zagregowanym dla 2. edycji
#' monitoringu na danych administracyjnych
#' @description Funkcja obliczająca wskaźniki na poziomie zagregowanym na
#' potrzeby raportów "1rokpo"
#' @param wsk2 ramka danych z tabeli pośredniej nr 2 (P2) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param wsk3 ramka danych z tabeli pośredniej nr 3 (P3) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param wsk4 ramka danych z tabeli pośredniej nr 4 (P4) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @param rok rok, w którym grupa absolwentów uzyskała status absolwenta. Może
#' to być więcej niż 1 wartość.
#' @param duplikaty wartość logiczna określająca czy należy odfiltrować ze
#' zbioru duplikaty przy liczeniu niektórych wskaźników. Przyjmuje wartości:
#' \itemize{
#'  \item{TRUE}{wartość domyślna; duplikaty nie zostaną odfiltrowane ze zbioru}
#'  \item{FALSE}{duplikaty zostaną odfiltrowane ze zbioru}
#'  }
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{dane_szkoly}},}
#'  \item{\code{\link{l_abs}},}
#'  \item{\code{\link{l_kobiet}},}
#'  \item{\code{\link{l_abs_zrodla}},}
#'  \item{\code{\link{S3_mies}},}
#'  \item{\code{\link{zawody_S3}},}
#'  \item{\code{\link{E2_nauka_kontyn}},}
#'  \item{\code{\link{liczebnosc_branze_ucz}},}
#'  \item{\code{\link{liczebnosc_branze_kont}},}
#'  \item{\code{\link{Z4_ods_prac_mies}},}
#'  \item{\code{\link{Z8_formy_prac_mies}},}
#'  \item{\code{\link{Z9_kont_mlod}},}
#'  \item{\code{\link{W3_sr_doch_uop}},}
#'  \item{\code{\link{B2_ods_bezrob}},}
#'  \item{\code{\link{N2_ods_biernosc}},}
#'  \item{\code{\link{licz_zawody}},}
#'  \item{\code{\link{liczebnosc_dyscypliny}},}
#'  \item{\code{\link{liczebnosc_dziedziny}}}
#' }
#' @export
#' @importFrom dplyr %>% filter .data left_join
agreguj_1rokpo_adm_2 = function(wsk2, wsk3, wsk4, grupy, rok, duplikaty = TRUE) {
  stopifnot(is.data.frame(wsk2),
            is.data.frame(wsk3),
            is.data.frame(wsk4),
            is.data.frame(grupy),
            rok %in% c(2020, 2021),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj", "branza") %in% names(wsk2),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj", "branza") %in% names(wsk3),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj", "branza") %in% names(wsk4),
            is.logical(duplikaty))
  
  if (rok %in% c(2020, 2021)) {
    wsk4 = wsk4 %>%
      filter(.data$rok_abs %in% c(rok, rok - 1))
    wsk3 = wsk3 %>%
      filter(.data$rok_abs %in% c(rok, rok - 1),
             .data$rok %in% c(rok, rok - 1))
    wsk2 = wsk2 %>%
      filter(.data$rok_abs %in% c(rok, rok - 1))
  } else if (.data$rok %in% 2020) {
    wsk4 = wsk4 %>%
      filter(.data$rok_abs %in% (rok - 1))
    wsk3 = wsk3 %>%
      filter(.data$rok_abs %in% (rok - 1),
             .data$rok %in% (rok - 1))
    wsk2 = wsk2 %>%
      filter(.data$rok_abs %in% (rok - 1))
  } else if (.data$rok %in% 2021) {
    wsk4 = wsk4 %>%
      filter(.data$rok_abs %in% (rok))
    wsk3 = wsk3 %>%
      filter(.data$rok_abs %in% (rok),
             .data$rok %in% (rok))
    wsk2 = wsk2 %>%
      filter(.data$rok_abs %in% (rok))
  }
  
  if (duplikaty) {
    dup = NULL
  } else {
    dup = wsk4 %>% 
      count(.data$id_abs, .data$rok_abs) %>% 
      filter(n > 1) %>% 
      pull(id_abs)
  }
  
  wskazniki_4 = agreguj_wskazniki(
    wsk4, grupy,
    dane_szkoly = dane_szkoly(.data),
    l_abs = l_abs(.data),
    l_kobiet = l_kobiet(.data),
    l_abs_zrodla = l_abs_zrodla(.data),
    liczebnosc_branze_ucz = liczebnosc_branze_ucz(.data))
  
  wskazniki_3 = agreguj_wskazniki(
    wsk3, grupy, list("rok" = rok, "wsk2" = wsk2, "dup" = dup),
    S3_01 = S3_mies(.data, min(rok), 1, max(rok), 1, dup),
    S3_02 = S3_mies(.data, min(rok), 2, max(rok), 2, dup),
    S3_03 = S3_mies(.data, min(rok), 3, max(rok), 3, dup),
    S3_04 = S3_mies(.data, min(rok), 4, max(rok), 4, dup),
    S3_05 = S3_mies(.data, min(rok), 5, max(rok), 5, dup),
    S3_06 = S3_mies(.data, min(rok), 6, max(rok), 6, dup),
    S3_07 = S3_mies(.data, min(rok), 7, max(rok), 7, dup),
    S3_08 = S3_mies(.data, min(rok), 8, max(rok), 8, dup),
    S3_09 = S3_mies(.data, min(rok), 9, max(rok), 9, dup),
    S3_10 = S3_mies(.data, min(rok), 10, max(rok), 10, dup),
    S3_11 = S3_mies(.data, min(rok), 11, max(rok), 11, dup),
    S3_12 = S3_mies(.data, min(rok), 12, max(rok), 12, dup),
    tab_s3_zaw = zawody_S3(.data, min(rok), 12, max(rok), 12, dup),
    E2_nauka_kontyn = E2_nauka_kontyn(.data, rok, 12),
    Z4_ucz = Z4_ods_prac_mies(.data, min(rok), 9, max(rok), 12, TRUE),
    Z4_nie_ucz = Z4_ods_prac_mies(.data, min(rok), 9, max(rok), 12, TRUE),
    Z8_formy_ucz = Z8_formy_prac_mies(.data, rok, 12, TRUE),
    Z8_formy_nie_ucz = Z8_formy_prac_mies(.data, rok, 12, FALSE),
    Z9_mlod_wrz = Z9_kont_mlod(.data, rok, 9),
    W3_ucz = W3_sr_doch_uop(.data, rok, 9, 12, TRUE),
    W3_nie_ucz = W3_sr_doch_uop(.data, rok, 9, 12, FALSE),
    B2_bezrob = B2_ods_bezrob(.data, rok, 9, 12),
    N2_biernosc = N2_ods_biernosc(.data, rok, 9, 12),
    liczebnosc_branze_kont = liczebnosc_branze_kont(.data, wsk2, rok, 12),
    liczebnosc_dziedziny = liczebnosc_dziedziny(.data, wsk2, rok, 12),
    liczebnosc_dyscypliny = liczebnosc_dyscypliny(.data, wsk2, rok, 12)
  )
  
  wskazniki_4$grupy = wskazniki_4$grupy %>%
    left_join(wskazniki_3$grupy, by = names(grupy))
  wskazniki_4$grupyOdniesienia = wskazniki_4$grupyOdniesienia %>%
    left_join(wskazniki_3$grupyOdniesienia, by = names(grupy))
  
  wskazniki = list(grupy = wskazniki_4$grupy, grupyOdniesienia = wskazniki_4$grupyOdniesienia)
  return(wskazniki)
}
#' @title Obliczanie wskaznikow na poziomie zagregowanym dla 2. edycji
#' monitoringu na danych administracyjnych
#' @description Funkcja obliczająca wskaźniki na poziomie zagregowanym na
#' potrzeby Aneksu w raportach "1rokpo" - liczy mniej wskaźników niż
#' \code{\link{agreguj_1rokpo_adm_2}}
#' @param wsk2 ramka danych z tabeli pośredniej nr 2 (P2) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param wsk3 ramka danych z tabeli pośredniej nr 3 (P3) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param wsk4 ramka danych z tabeli pośredniej nr 4 (P4) z wynikami z 2.
#' rundy monitoringu na danych administracyjnych
#' @param grupy ramka danych zawierająca definicje podziałów na grupy -
#' np. zwrócona przez funkcję \code{\link{utworz_grupowanie_ze_zmiennej}}
#' @param rok rok, w którym grupa absolwentów uzyskała status absolwenta. Może
#' to być więcej niż 1 wartość.
#' @param duplikaty wartość logiczna określająca czy należy odfiltrować ze
#' zbioru duplikaty przy liczeniu niektórych wskaźników. Przyjmuje wartości:
#' \itemize{
#'  \item{TRUE}{wartość domyślna; duplikaty nie zostaną odfiltrowane ze zbioru}
#'  \item{FALSE}{duplikaty zostaną odfiltrowane ze zbioru}
#'  }
#' @return data frame
#' @seealso \code{\link{agreguj_wskazniki}} oraz przekazywane do niej funkcje
#' używane do obliczania konkretnych wskaźników zagregowanych:
#' \itemize{
#'  \item{\code{\link{l_abs}},}
#'  \item{\code{\link{l_kobiet}},}
#'  \item{\code{\link{S3_mies}},}
#'  \item{\code{\link{zawody_S3}},}
#'  \item{\code{\link{E2_nauka_kontyn}},}
#'  \item{\code{\link{liczebnosc_branze_kont}},}
#'  \item{\code{\link{Z4_ods_prac_mies}},}
#'  \item{\code{\link{Z8_formy_prac_mies}},}
#'  \item{\code{\link{Z9_kont_mlod}},}
#'  \item{\code{\link{W3_sr_doch_uop}},}
#'  \item{\code{\link{B2_ods_bezrob}},}
#'  \item{\code{\link{N2_ods_biernosc}},}
#'  \item{\code{\link{licz_zawody}},}
#'  \item{\code{\link{liczebnosc_dyscypliny}},}
#'  \item{\code{\link{liczebnosc_dziedziny}}}
#' }
#' @export
#' @importFrom dplyr %>% filter .data left_join
agreguj_aneks_1rokpo_adm_2 = function(wsk2, wsk3, wsk4, grupy, rok, duplikaty = TRUE) {
  stopifnot(is.data.frame(wsk2),
            is.data.frame(wsk3),
            is.data.frame(wsk4),
            is.data.frame(grupy),
            rok %in% c(2020, 2021),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj") %in% names(wsk2),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj") %in% names(wsk3),
            c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj") %in% names(wsk4),
            is.logical(duplikaty))
  
  if (rok %in% c(2020, 2021)) {
    wsk4 = wsk4 %>%
      filter(.data$rok_abs %in% c(rok, rok - 1))
    wsk3 = wsk3 %>%
      filter(.data$rok_abs %in% c(rok, rok - 1),
             .data$rok %in% c(rok, rok - 1))
    wsk2 = wsk2 %>%
      filter(.data$rok_abs %in% c(rok, rok - 1))
  } else if (.data$rok %in% 2020) {
    wsk4 = wsk4 %>%
      filter(.data$rok_abs %in% (rok - 1))
    wsk3 = wsk3 %>%
      filter(.data$rok_abs %in% (rok - 1),
             .data$rok %in% (rok - 1))
    wsk2 = wsk2 %>%
      filter(.data$rok_abs %in% (rok - 1))
  } else if (.data$rok %in% 2021) {
    wsk4 = wsk4 %>%
      filter(.data$rok_abs %in% (rok))
    wsk3 = wsk3 %>%
      filter(.data$rok_abs %in% (rok),
             .data$rok %in% (rok))
    wsk2 = wsk2 %>%
      filter(.data$rok_abs %in% (rok))
  }
  
  if (duplikaty) {
    dup = NULL
  } else {
    dup = wsk4 %>% 
      count(.data$id_abs, .data$rok_abs) %>% 
      filter(n > 1) %>% 
      pull(id_abs)
  }
  
  wskazniki_4 = agreguj_wskazniki(
    wsk4, grupy,
    l_abs = l_abs(.data),
    l_kobiet = l_kobiet(.data))
  
  wskazniki_3 = agreguj_wskazniki(
    wsk3, grupy, list("rok" = rok, "wsk2" = wsk2, "dup" = dup),
    S3_12 = S3_mies(.data, min(rok), 12, max(rok), 12, dup),
    tab_s3_zaw = zawody_S3(.data, min(rok), 12, max(rok), 12, dup),
    E2_nauka_kontyn = E2_nauka_kontyn(.data, rok, 12),
    Z4_ucz = Z4_ods_prac_mies(.data, min(rok), 9, max(rok), 12, TRUE),
    Z4_nie_ucz = Z4_ods_prac_mies(.data, min(rok), 9, max(rok), 12, TRUE),
    Z8_formy_ucz = Z8_formy_prac_mies(.data, rok, 12, TRUE),
    Z8_formy_nie_ucz = Z8_formy_prac_mies(.data, rok, 12, FALSE),
    Z9_mlod_wrz = Z9_kont_mlod(.data, rok, 9),
    W3_ucz = W3_sr_doch_uop(.data, rok, 9, 12, TRUE),
    W3_nie_ucz = W3_sr_doch_uop(.data, rok, 9, 12, FALSE),
    B2_bezrob = B2_ods_bezrob(.data, rok, 9, 12),
    N2_biernosc = N2_ods_biernosc(.data, rok, 9, 12),
    liczebnosc_branze_kont = liczebnosc_branze_kont(.data, wsk2, rok, 12),
    liczebnosc_dziedziny = liczebnosc_dziedziny(.data, wsk2, rok, 12),
    liczebnosc_dyscypliny = liczebnosc_dyscypliny(.data, wsk2, rok, 12)
  )
  
  wskazniki_4$grupy = wskazniki_4$grupy %>%
    left_join(wskazniki_3$grupy, by = names(grupy))
  wskazniki_4$grupyOdniesienia = wskazniki_4$grupyOdniesienia %>%
    left_join(wskazniki_3$grupyOdniesienia, by = names(grupy))
  
  wskazniki = list(grupy = wskazniki_4$grupy, grupyOdniesienia = wskazniki_4$grupyOdniesienia)
  return(wskazniki)
}
