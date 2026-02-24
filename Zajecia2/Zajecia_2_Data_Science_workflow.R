#"/home/rarety"
#install.packages("writexl")
#2.1. Import danych----
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)
getwd()
# Prawidłowy import plików CSV z folderu - wykonaj:
kraje_1 = read.table("kraje_makro_1.csv", header=TRUE, sep=",", dec=".")
kraje_2 = read.table("kraje_makro_2.csv", header=TRUE, sep=",", dec=".")

# 2.2. Przygotowanie danych ----

# Pierwsze
head(kraje_1)	# pierwsze 6 wierszy (obserwacji)
head(kraje_2)
# Ostatnie wiersze
tail(kraje_1, 5)	# ostatnie 5 wierszy (obserwacji)
tail(kraje_2, 5)


# Podstawowe statystyki wszystkich kolumn (zmiennych)
summary(kraje_1)	# min, max, średnia, mediana, kwantyle
summary(kraje_2)

# Statystyki pojedynczej kolumny (zmiennej)
mean(kraje_1$Przyrost_populacji)		# średnia
median(kraje_1$Przyrost_populacji)	# mediana
min(kraje_1$Przyrost_populacji)		# minimum
max(kraje_1$Przyrost_populacji)		# maksimum



# Usuwanie zbędnej kolumny
kraje_1$X = NULL
kraje_2$X = NULL

# Zmiana nazw kolumn z angielskich na polskie
colnames(kraje_2) = c("Kod_kraju", "Nazwa", "Region", "Urbanizacja_proc.", "Internet_proc.")


# W ramce danych kraje_2 sprawdź typ zmiennej Region 
is.numeric(kraje_2$Region) 	# czy zmienna jest liczbowa? Odp. Nie.
is.character(kraje_2$Region) 	# czy zmienna jest tekstowa? Odp. Tak.


# Region to zmienna kategorialna, więc nadajemy jej typ factor:
kraje_2$Region = as.factor(kraje_2$Region)


# Sprawdzenie kategorii:
summary(kraje_2)
levels(kraje_2$Region)


# Porządkowanie braków danych
# Szybka kontrola braków danych we wszystkich kolumnach:
colSums(is.na(kraje_1))	# nie ma braków danych
colSums(is.na(kraje_2))	# są 4 braki danych w kolumnie (zmiennej) Internet_proc.

# Liczba braków w konkretnej kolumnie:
sum(is.na(kraje_2$Internet_proc.)) 	# 4 braki


# Zobaczmy te 4 wiersze, w których brakuje wartości:
kraje_2[is.na(kraje_2$Internet_proc.), ]

levels(kraje_2$Region)

kraje_2$Region  = gsub("&", "and", kraje_2$Region)
kraje_2$Region = as.factor(kraje_2$Region)
levels(kraje_2$Region)

#2.3. Łączenie (scalanie) ramek danych w jedną ----

kraje = merge(kraje_1, kraje_2, by.x="Kod", by.y="Kod_kraju")
kraje$Nazwa = NULL

summary(kraje)
str(kraje)
#2.4. Podstawowa analiza danych----
# Tworzenie nowej zmiennej Populacja_w_mln w dplyr:
kraje = kraje %>%
  mutate(Populacja_mln = Populacja / 1e6)

# Tworzenie nowej zmiennej PKB_per_capita w dplyr:
kraje = kraje %>%
  mutate(PKB_per_capita = PKB / Populacja)

# Wyświetl kraje, w których % poziom urbanizacji jest większy niż 50
head(kraje %>%
  filter(Urbanizacja_proc. > 50))

# Wyświetl tylko dane pokazujące zmienne Panstwo, Region, PKB, Populacja_mln
head(kraje %>%
  select(Panstwo, Region, PKB, Populacja_mln))

# Posortuj kraje według przyrostu populacji rosnąco
head(kraje %>%
  arrange(Przyrost_populacji))

# Wybierz kraje z PKB większym niż 1 bilion, posortuj je rosnąco względem PKB 
# i wyświetl nazwę państwa, PKB i PKB per capita. Ile jest takich krajów?
head(kraje %>%
       filter(PKB > 1e12) %>%
       arrange(PKB) %>%
       select(Panstwo, PKB, PKB_per_capita))
# Wybierz kraje z regionu Afryki Subsaharyjskiej, 
# wybierz zmienne Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja,
# a następnie posortuj malejąco po PKB per capita
head(kraje %>%
  filter(Region == "Sub-Saharan Africa") %>%
  select(Panstwo, PKB_per_capita, Populacja_mln, Urbanizacja_proc.) %>%
  arrange(desc(PKB_per_capita)))


bogate = kraje %>%
       group_by(Region) %>%
       filter(PKB_per_capita > mean(PKB_per_capita, na.rm = TRUE))


# Znajdź największą wartość PKB per capita w całym zbiorze krajów
kraje %>%
  summarise(max_PKB_per_capita = max(PKB_per_capita, na.rm = TRUE))


# Znajdź największą i najmniejszą wartość Populacji w mln w całym zbiorze krajów
kraje %>%
  summarise(
    min_populacja = min(Populacja_mln, na.rm = TRUE),
    max_populacja = max(Populacja_mln, na.rm = TRUE))

# Oblicz średnią populację w całym zbiorze krajów (jedna liczba dla całej ramki)
kraje %>%
  summarise(srednia_populacja = mean(Populacja_mln, na.rm = TRUE))

# Ile krajów jest w całym zbiorze danych?
kraje %>%
  summarise(liczba_krajow = n())

# Policz, ile krajów jest w każdym regionie
kraje %>%
  group_by(Region) %>%
  summarise(liczba_krajow = n())

# Dla każdego regionu świata: oblicz liczbę krajów (n), średni % dostęp do internetu i średni % poziom urbanizacji, a następnie posortuj regiony malejąco wg średniego % dostępu do internetu
kraje %>%
  group_by(Region) %>%
  summarise(
    liczba_krajow = n(),
    sredni_internet = mean(Internet_proc., na.rm = TRUE),
    srednia_urbanizacja = mean(Urbanizacja_proc., na.rm = TRUE)
  ) %>%
  arrange(desc(sredni_internet))











#2.5. Wizualizacja [* zaawansowane *]----
# Wizualizacja danych także pozwala zidentyfikować wzorce i zależności w zbiorze danych.

# install.packages("ggplot2")
library(ggplot2)
#2.6. Eksport----
write.csv(kraje, "kraje_analiza.csv") 
write_xlsx(kraje, "kraje_wynik.xlsx")


