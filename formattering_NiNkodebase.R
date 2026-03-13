#'#################################################
#' formattering av koblingstabell og NiN kodebase #
#'#################################################

# startet 2025-09-12
# av Eva Lieungh

Sys.setlocale('LC_ALL', 'nb-NO.utf-8') # les norske tegn riktig
library(dplyr)
library(tidyr)
library(openxlsx)
`%nin%` <- Negate(`%in%`) # a function for negation of %in% function

# last ned siste versjon av NiN kodebase fra Artsdatabanken (sist: 2025-09-12)
download.file(url = "https://nin-kode-api.artsdatabanken.no/v3.0/rapporter/kodeoversikt",
              destfile = "kodeoversikt.csv")

# les inn som R-objekt
nin_kodebase <- read.csv("kodeoversikt.csv", sep = ";")

# lag liste over terrestriske hovedtyper
hovedtyper_TV <- subset(nin_kodebase, 
                        Klasse == "Hovedtype" & 
                          grepl("^NA-[TV]", Kortkode))

hovedtyper_natursystem <- subset(nin_kodebase, 
                        Klasse == "Hovedtype" & 
                          grepl("^NA-", Kortkode))

# lag liste over kartleggingsenheter
KE20_TV <- subset(nin_kodebase,
                Klasse == "Kartleggingsenhet" & # alle kartleggingsenheter
                grepl("^NiN-3.0-T-C-PE-NA-MB-[TV]", Langkode) & # med T eller V i langkoden
                grepl("M020", Kortkode)) # i 1:20 000 skala

# Se koblinger mellom kartleggingsenheter og grunntyper ----------
# splitt langkode i bestanddeler
splittet_temp <- strsplit(nin_kodebase$Langkode, "-", fixed = TRUE)
antall_kolonner <- max(lengths(splittet_temp))
splittet_nin_kodebase <- t(vapply(
  splittet_temp,
  function(x) { length(x) <- antall_kolonner; x },
  FUN.VALUE = character(antall_kolonner)
))
kolonnenavn <- c(
  "NiN", "versjon",
  "variabel_eller_type",
  "naturegenskap_nivaa1",
  "naturegenskap_nivaa2",
  "naturegenskap_nivaa3",
  "naturegenskap_nivaa4",
  "hovedtypegruppe_eller_generaliteringsnivaa",
  "prosesskategori_eller_generaliseringsnivaa_2A",
  "",
)
# subset Naturtystem fastmark og våtmark
na_T_V <- hovedtyper_TV %>%
  left_join(nin_kodebase)

test <- nin_kodebase %>% 
  pivot_wider(names_from = Klasse,
              values_from = Navn)


# kombinasjon av koblingstabell, ny kopi av rødlista, og kodebasen -----------
gammel_koblingstabell <- read.csv2("Notat_Format_på_kartleggingen_vedlegg_koblingstabell.csv")
rodlista <- read.csv2("rødlista-naturtyper-2025-12-11.csv")
names(gammel_koblingstabell)
names(rodlista)
names(nin_kodebase)
rodlista$RL_id <- rodlista$Id.for.vurderingen

length(rodlista$Id.for.vurderingen)
length(unique(na.omit(gammel_koblingstabell$RL_id)))
mangler_i_RL <- which(unique(na.omit(gammel_koblingstabell$RL_id)) %nin% rodlista$Id.for.vurderingen)
gammel_koblingstabell[gammel_koblingstabell$RL_id %in% mangler_i_RL, ]

ny_koblingstabell <-
  full_join(gammel_koblingstabell, 
            rodlista, 
            by = "RL_id")

KE20_TV$Kartleggingsenhet_20 <- KE20_TV$Kortkode
ny_koblingstabell2 <-
  full_join(ny_koblingstabell, KE20_TV, by = "Kartleggingsenhet_20") %>%
  select(
    !c(
      "X",
      "X.1",
      "Mørketall.forekomster",
      "Beregnet.forekomstareal..km2.",
      "Regioner.og.havområder",
      "Mørketall.utbredelsesareal",
      "Beregnet.utbredelsesareal..km2.",
      "Antall.forekomstruter",
      "Mørketall.totalareal",
      "Beregnet.totalareal..km2.",
      "Kjent.utbredelsesareal..km2.",
      "Påvirkningsfaktorer",
      "Areal.kommentar",
      "Kjent.totalareal..km2.",
      "Påvirkningsfaktor",
      "Sitering"
    )
  )

write.csv2(ny_koblingstabell2, 
           "Koblingstabell_kombinert.csv",
           row.names = FALSE, fileEncoding = "native.enc") # evt UTF-8

write.xlsx(ny_koblingstabell2,
           "Koblingstabell_kombinert.xlsx")

# Legge til kartleggingsenheter i rødlistevarsel
rl_matrise <- read.xlsx("Rødlistematrise_NiN_3.xlsx")
rl_matrise$kartleggingsenhet <- nin_kodebase[match(rl_matrise$`NiN-kode`, nin_kodebase$Kortkode), 2]

write.xlsx(rl_matrise,
           "Rødlistematrise_NiN3_medKE.xlsx")


# stuff ----- 
Kortkode <- data.frame(Kortkode = c(
"TA01-M020-07",
"TA02-M020-01",
"TA03-M020-07",
"TC01-M020-01",
"TC01-M020-02",
"TC02-M020-01",
"TC02-M020-02",
"TC02-M020-03",
"TC02-M020-04",
"TC03-M020-03",
"TC03-M020-04",
"TC03-M020-05",
"TD01-M020-01",
"TD01-M020-02",
"TD01-M020-03",
"TD01-M020-04",
"TD01-M020-05",
"TD01-M020-06",
"TD01-M020-07",
"TD01-M020-08",
"TD01-M020-09",
"TD02-M020-01",
"TD05-M020-01",
"TD05-M020-02",
"TD06-M020-04",
"TG01-M020-01",
"TG01-M020-04",
"TG01-M020-05",
"TG01-M020-06",
"TG01-M020-07",
"TG01-M020-08",
"TG01-M020-09",
"TG01-M020-10",
"TG01-M020-11",
"TG01-M020-12",
"TG01-M020-13",
"TG01-M020-14",
"TG01-M020-15",
"TG01-M020-16",
"TG01-M020-17",
"TG01-M020-18",
"TG01-M020-19",
"TI01-M020-01",
"TI01-M020-02",
"TI01-M020-03",
"TI01-M020-04",
"TL01-M020-01",
"TL01-M020-02",
"TL01-M020-03",
"TM01-M020-01",
"TM01-M020-02",
"TM01-M020-03",
"TM02-M020-01",
"TM02-M020-02",
"TM03-M020-01",
"TM03-M020-02",
"TM03-M020-03",
"TM03-M020-04",
"TM03-M020-05",
"TM03-M020-06",
"TM03-M020-07",
"TM04-M020-01",
"TM04-M020-02",
"TM05-M020-01",
"TM06-M020-01",
"TM06-M020-02",
"TN01-M020-01",
"TN03-M020-01",
"TO01-M020-01",
"TO02-M020-01",
"TO03-M020-01",
"TO04-M020-01",
"VA01-M020-01",
"VA01-M020-02",
"VC02-M020-01",
"VC03-M020-01",
"VC05-M020-01",
"VG01-M020-01",
"VG01-M020-02",
"VG02-M020-01",
"VG02-M020-02",
"VI01-M020-01",
"VI01-M020-02",
"VI01-M020-03",
"VI01-M020-04",
"VM01-M020-01",
"VM01-M020-02",
"VM01-M020-03",
"VM01-M020-04",
"VM01-M020-05",
"VM02-M020-01",
"VM03-M020-01",
"VM04-M020-01",
"VM05-M020-01",
"VM06-M020-01",
"VO01-M020-01"))

ke1 <- left_join(Kortkode, KE20_TV, by = "Kortkode")
write.xlsx(ke1, "temp.xlsx")



