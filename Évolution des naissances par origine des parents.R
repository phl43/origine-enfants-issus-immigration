library(tidyverse)
library(memisc)
library(data.table)

# convertit le code de pays de la variable "pnai28" ou "natmer" dans la nomenclature de la
# variable "nat14" qui est diffusée à la place de "nat28" à partir de l'enquête de 2013
# note : si "pnai28"/"natmer" a une valeur correspondant à la France, la fonction renvoie NA
convertir_code_pays <- function(code, source) {
  if (source == "pnai28") {
    case_when(code == 11 ~ 21L,
              code == 12 ~ 23L,
              code == 13 ~ 22L,
              code == 14 ~ 24L,
              code == 15 ~ 32L,
              code == 21 ~ 12L,
              (code >= 22 & code <= 29) | code %in% c(42, 43, 47) ~ 14L,
              code == 31 ~ 11L,
              code == 32 ~ 13L,
              code %in% c(41, 44, 46, 48) ~ 15L,
              code == 45 ~ 31L,
              code %in% c(51, 52) ~ 41L,
              code == 60 ~ 51L,
              code == 61 ~ 99L,
              TRUE ~ NA_integer_)
  } else if (source == "natmer") {
    # la nomenclature de la variable "natmer" est trop différente de celle de la variable "nat14", donc on assigne
    # toutes les personnes nées dans un pays européen autre que la France dans la catégorie "Autres pays de l'UE à 28",
    # toutes les personnées nées au Maghreb dans la catégorie "Algérie", toutes les personnes nées au Proche-Orient dans
    # la catégorie "Turquie" et toutes les personnes nées dans le reste du monde dans la catégorie "Autres pays (d'Asie
    # ou Océanie) ou apatride"
    case_when(code %in% c(3, 4, 5) ~ 14L,
              code == 6 ~ 21L,
              code == 7 ~ 24L,
              code == 8 ~ 31L,
              code == 9 ~ 32L,
              code == 10 ~ 51L,
              code == 99 ~ 99L,
              TRUE ~ NA_integer_)
  } else {
    NA_integer_
  }
}

# détermine l'origine des parents et des grand-parents des enfants en appariant les données dans le fichier
# sur les enfants avec celles dans le fichier sur les adultes
apparier_enfants_parents <- function(logement, noi, anais, noi_père, noi_mère, poids, année_enquête) {
  père <- adultes[adultes$ident == logement & adultes$noi == noi_père, ]
  mère <- adultes[adultes$ident == logement & adultes$noi == noi_mère, ]
  
  if (dim(père)[1] == 1) {
    if (année_enquête <= 2012) {
      painp <- père$pnai28
      
      # on fait l'hypothèse que les personnes ayant acquis la nationalité française
      # avaient la nationalité de leur pays de naissance avant ça
      natnp <- case_when(père$nfr == 1 ~ 10L,
                         père$nfr == 2 ~ père$pnai28,
                         père$nfr == 3 ~ père$nat28)
      natngpp <- père$natperc
      natngmp <- père$natmerc
      paingpp <- père$paiperc
      paingmp <- père$paimerc
    } else {
      # la variable "pnai28" n'a pas été diffusée en 2013 et 2014, donc pour ces années on fait l'hypothèse que les
      # personnes ayant acquis la nationalité française avaient la nationalité de naissance de leur mère avant ça
      if (année_enquête < 2015) {
        painp <- NA_integer_
        
        natnp <- case_when(père$nfrred == 1 ~ 10L,
                           père$nfrred == 2 ~ convertir_code_pays(père$natmer, "natmer"),
                           père$nfrred == 3 ~ père$nat14)
      } else {
        painp <- père$pnai28
        
        # on fait l'hypothèse que les personnes ayant acquis la nationalité française
        # avaient la nationalité de leur pays de naissance avant ça
        natnp <- case_when(père$nfrred == 1 ~ 10L,
                           père$nfrred == 2 ~ convertir_code_pays(père$pnai28, "pnai28"),
                           père$nfrred == 3 ~ père$nat14)
      }
      natngpp <- père$natper
      natngmp <- père$natmer
      paingpp <- père$paiper
      paingmp <- père$paimer
    }
  } else {
    natnp <- NA_integer_
    painp <- NA_integer_
    natngpp <- NA_integer_
    natngmp <- NA_integer_
    paingpp <- NA_integer_
    paingmp <- NA_integer_
  }
  
  if (dim(mère)[1] == 1) {
    if (année_enquête <= 2012) {
      painm <- mère$pnai28
      
      # on fait l'hypothèse que les personnes ayant acquis la nationalité française
      # avaient la nationalité de leur pays de naissance avant ça
      natnm <- case_when(mère$nfr == 1 ~ 10L,
                         mère$nfr == 2 ~ mère$pnai28,
                         mère$nfr == 3 ~ mère$nat28)
      natngpm <- mère$natperc
      natngmm <- mère$natmerc
      paingpm <- mère$paiperc
      paingmm <- mère$paimerc
    }
    else {
      painm <- NA_integer_
      
      # la variable "pnai28" n'a pas été diffusée en 2013 et 2014, donc pour ces années on fait l'hypothèse que les
      # personnes ayant acquis la nationalité française avaient la nationalité de naissance de leur mère avant ça
      if (année_enquête < 2015) {
        natnm <- case_when(mère$nfrred == 1 ~ 10L,
                           mère$nfrred == 2 ~ convertir_code_pays(mère$natmer, "natmer"),
                           mère$nfrred == 3 ~ mère$nat14)
      } else {
        painm <- mère$pnai28
        
        # on fait l'hypothèse que les personnes ayant acquis la nationalité française
        # avaient la nationalité de leur pays de naissance avant ça
        natnm <- case_when(mère$nfrred == 1 ~ 10L,
                           mère$nfrred == 2 ~ convertir_code_pays(mère$pnai28, "pnai28"),
                           mère$nfrred == 3 ~ mère$nat14)
      }
      natngpm <- mère$natper
      natngmm <- mère$natmer
      paingpm <- mère$paiper
      paingmm <- mère$paimer
    }
  } else {
    natnm <- NA_integer_
    painm <- NA_integer_
    natngpm <- NA_integer_
    natngmm <- NA_integer_
    paingpm <- NA_integer_
    paingmm <- NA_integer_
  }
  
  tibble(logement = logement,
         noi = noi,
         anais = anais,
         natnp = natnp,
         natnm = natnm,
         painp = painp,
         painm = painm,
         natngpp = natngpp,
         natngmp = natngmp,
         natngpm = natngpm,
         natngmm = natngmm,
         paingpp = paingpp,
         paingmp = paingmp,
         paingpm = paingpm,
         paingmm = paingmm,
         poids = poids,
         année_enquête = année_enquête)
}

# renvoie l'origine de l'enfant en fonction de la nationalité de naissance de ses grand-parents
# note : on hiérarchise les origines avec Maghreb > Reste de l'Afrique > Turquie > Cambodge/Laos/Vietnam > Europe
origine_enfant <- function(natnp, natnm, natngpp, natngmp, natngpm, natngmm, année_enquête) {
  if (année_enquête <= 2012) {
    # on fait l'hypothèse que tous les gens ayant la nationalité d'un pays du Proche-Orient à la naissance viennent
    # de Turquie car les nomenclatures de "nat28" et "natmerc"/"natperc" ne correspondent pas exactement
    case_when(natngpp == 1 & natngmp == 1 & natngpm == 1 & natngmm == 1 ~ "France",
              natnp %in% c(11, 12, 13) | natnm %in% c(11, 12, 13) | natngpp == 6 |
              natngmp == 6 | natngpm == 6 | natngmm == 6 ~ "Maghreb",
              natnp == 14 | natnm == 14 | natngpp == 7 | natngmp == 7 | natngpm == 7 | natngmm == 7 ~ "Reste de l'Afrique",
              natnp == 45 | natnm == 45 | natngpp == 8 | natngmp == 8 | natngpm == 8 | natngmm == 8 ~ "Turquie",
              natnp == 15 | natnm == 15 | natngpp == 9 | natngmp == 9 | natngpm == 9 | natngmm == 9 ~ "Cambodge/Laos/Vietnam",
              natnp %in% c(51, 52, 60) | natnm %in% c(51, 52, 60) | natngpp == 10 | natngmp == 10 |
              natngpm == 10 | natngmm == 10 ~ "Reste du monde",
              (natnp >= 21 & natnp <= 48 & natnp != 45) | (natnm >= 21 & natnm <= 48 & natnm != 45) | natngpp %in% c(2,3,4) |
              natngmp %in% c(2,3,4) | natngpm %in% c(2,3,4) | natngmm %in% c(2,3,4) ~ "Europe",
              TRUE ~ NA_character_)
  } else {
    # on fait l'hypothèse que tous les gens ayant la nationalité d'un pays du Proche-Orient à la naissance viennent
    # de Turquie car les nomenclatures de "nat14" et "natmer"/"natper" ne correspondent pas exactement
    case_when(natngpp == 1 & natngmp == 1 & natngpm == 1 & natngmm == 1 ~ "France",
              natnp %in% c(21, 22, 23) | natnm %in% c(21, 22, 23) | natngpp == 6 |
              natngmp == 6 | natngpm == 6 | natngmm == 6 ~ "Maghreb",
              natnp == 24 | natnm == 24 | natngpp == 7 | natngmp == 7 | natngpm == 7 | natngmm == 7 ~ "Reste de l'Afrique",
              natnp == 31 | natnm == 31 | natngpp == 8 | natngmp == 8 | natngpm == 8 | natngmm == 8 ~ "Turquie",
              natnp == 32 | natnm == 32 | natngpp == 9 | natngmp == 9 | natngpm == 9 | natngmm == 9 ~ "Cambodge/Laos/Vietnam",
              natnp %in% c(41, 51) | natnm %in% c(41, 51) | natngpp == 10 | natngmp == 10 |
              natngpm == 10 | natngmm == 10 ~ "Reste du monde",
              (natnp >= 11 & natnp <= 15) | (natnm >= 11 & natnm <= 15) | natngpp %in% c(2,3,4) |
              natngmp %in% c(2,3,4) | natngpm %in% c(2,3,4) | natngmm %in% c(2,3,4) ~ "Europe",
              TRUE ~ NA_character_)
  }
}

# renvoie TRUE ssi l'un des parents était de nationalité étrangère à la naissance
# et la nationalité à la naissance des parents de l'autre parent est connue
# note : on fait l'hypothèse que, si un des parents est né étranger, ses parents étaient
# aussi de nationalité étrangère à la naissance
enfant_immigré <- function(natnp, natnm, natngpp, natngmp, natngpm, natngmm) {
  case_when(natnp != 10 &
              ((!is.na(natnm) & natnm != 10) | (!is.na(natngpm) & natngpm != 99) & (!is.na(natngmm) & natngmm != 99)) ~ TRUE,
            natnm != 10 &
              ((!is.na(natnp) & natnp != 10) | (!is.na(natngpp) & natngpp != 99) & (!is.na(natngmp) & natngmp != 99)) ~ TRUE,
            TRUE ~ FALSE)
}

# renvoie TRUE ssi l'un des parents est enfant d'au moins une personne de nationalité étrangère
# à la naissance et la nationalité à la naissance des parents de l'autre parent est connue
# note : on fait l'hypothèse que, si un des parents est né étranger, ses parents étaient
# aussi de nationalité étrangère à la naissance
petit_enfant_immigré <- function(natnp, natnm, natngpp, natngmp, natngpm, natngmm) {
  case_when(natnp == 10 & ((natngpp != 1 & natngpp != 99) | (natngmp != 1 & natngmp != 99)) &
            ((!is.na(natnm) & natnm != 10) | (!is.na(natngpm) & natngpm != 99) & (!is.na(natngmm) & natngmm != 99)) ~ TRUE,
            natnm == 10 & ((natngpm != 1 & natngpm != 99) | (natngmm != 1 & natngmm != 99)) &
            ((!is.na(natnp) & natnp != 10) | (!is.na(natngpp) & natngpp != 99) & (!is.na(natngmp) & natngmp != 99)) ~ TRUE,
            TRUE ~ FALSE)
}

# renvoie TRUE ssi les deux parents d'au moins un des parents étaient français à la naissance
parent_natif_carré <- function(natngpp, natngmp, natngpm, natngmm) {
  (natngpp == 1 & natngmp == 1) | (natngpm == 1 & natngmm == 1)
}

# crée des listes dans lesquelles seront stockées les structures de données créées au fur et à mesure
proportion_mixité_enfants_natif_carré <- list()
proportion_mixité_enfants_immigré <- list()
proportion_mixité_petit_enfants_immigré <- list()

# boucle de chargement et d'analyse des données de l'enquête emploi en continue entre 2005 et 2017
for (i in 5:17) {
  enfants <- list()
  adultes <- list()
  
  # boucle de chargement des données de chaque round d'interviews dans l'année
  for (j in 1:4) {
    
    a <- str_pad(i, 2, pad = "0")
    
    system_file <- spss.system.file(paste0("20", a, "/spss/enf", a, j, ".sav"))
    
    enfants[[j]] <- subset(system_file,
                           select = c(extri,
                                      ident,
                                      noi,
                                      naia,
                                      noiper,
                                      noimer))
    
    system_file <- spss.system.file(paste0("20", a, "/spss/indiv", a, j, ".sav"))
    
    # les variables ne sont pas les mêmes et/ou n'ont le même nom selon les éditions de l'enquête
    if (i <= 12) {
      adultes[[j]] <- subset(system_file,
                             select = c(ident,
                                        noi,
                                        nfr,
                                        nat28,
                                        pnai28,
                                        natperc,
                                        natmerc,
                                        paiperc,
                                        paimerc))
    } else if (i < 15) {
      adultes[[j]] <- subset(system_file,
                             select = c(ident,
                                        noi,
                                        nfrred,
                                        nat14,
                                        natper,
                                        natmer,
                                        paiperc,
                                        paimerc))
    } else {
      adultes[[j]] <- subset(system_file,
                             select = c(ident,
                                        noi,
                                        nfrred,
                                        nat14,
                                        pnai28,
                                        natper,
                                        natmer,
                                        paiperc,
                                        paimerc))
    }
  }
  
  # la plupart des individus ayant été interviewés plusieurs fois, on ne garde
  # qu'une ligne par individu dans chaque table
  enfants <- rbindlist(enfants) %>%
    distinct(ident, noi, .keep_all = TRUE)
  adultes <- rbindlist(adultes) %>%
    distinct(ident, noi, .keep_all = TRUE)
  
  adultes$extri <- as.numeric(adultes$extri)
  adultes$ident <- as.character(adultes$ident)
  adultes$noi <- as.integer(adultes$noi)
  enfants$ident <- as.character(enfants$ident)
  enfants$noi <- as.integer(enfants$noi)
  enfants$naia <- as.integer(enfants$naia)
  enfants$noiper <- as.integer(enfants$noiper)
  enfants$noimer <- as.integer(enfants$noimer)
  
  # les variables ne sont pas les mêmes et/ou n'ont le même nom selon les éditions de l'enquête
  if (i <= 12) {
    adultes$nfr <- as.integer(adultes$nfr)
    adultes$nat28 <- as.integer(adultes$nat28)
    adultes$pnai28 <- as.integer(adultes$pnai28)
    adultes$natperc <- as.integer(adultes$natperc)
    adultes$natmerc <- as.integer(adultes$natmerc)
    adultes$paiperc <- as.integer(adultes$paiperc)
    adultes$paimerc <- as.integer(adultes$paimerc)
  } else if (i < 15) {
    adultes$nfrred <- as.integer(adultes$nfrred)
    adultes$nat14 <- as.integer(adultes$nat14)
    adultes$natper <- as.integer(adultes$natper)
    adultes$natmer <- as.integer(adultes$natmer)
    adultes$paiper <- as.integer(adultes$paiper)
    adultes$paimer <- as.integer(adultes$paimer)
  } else {
    adultes$nfrred <- as.integer(adultes$nfrred)
    adultes$nat14 <- as.integer(adultes$nat14)
    adultes$pnai28 <- as.integer(adultes$pnai28)
    adultes$natper <- as.integer(adultes$natper)
    adultes$natmer <- as.integer(adultes$natmer)
    adultes$paiper <- as.integer(adultes$paiper)
    adultes$paimer <- as.integer(adultes$paimer)
  }
  
  # liste qui contient les arguments pour la fonction apparier_enfants_parents en vue de l'appel à pmap_df
  args <- list(
    logement = enfants$ident,
    noi = enfants$noi,
    anais = enfants$naia,
    noi_père = enfants$noiper,
    noi_mère = enfants$noimer,
    poids = enfants$extri,
    année_enquête = 2000L + i
  )
  
  # apparie les données sur les enfants avec celles sur les adultes pour déterminer l'origine des enfants et
  # créer les variables qu'on utilise plus loin pour l'analyse
  enfants <- pmap_df(args, apparier_enfants_parents) %>%
    mutate(enfant_immigré = enfant_immigré(natnp, natnm, natngpp, natngmp, natngpm, natngmm),
           petit_enfant_immigré = petit_enfant_immigré(natnp, natnm, natngpp, natngmp, natngpm, natngmm),
           parent_natif_carré = parent_natif_carré(natngpp, natngmp, natngpm, natngmm),
           origine = origine_enfant(natnp, natnm, natngpp, natngmp, natngpm, natngmm, 2000L + i))
  
  # estime la proportion d'enfants, parmi ceux ayant au moins un parent natif au carré, dont l'autre parent
  # est de chaque origine par année de naissance des enfants
  proportion_mixité_enfants_natif_carré[[i - 4]] <- enfants  %>%
    filter(parent_natif_carré == TRUE & !is.na(origine)) %>%
    group_by(année_enquête, anais, origine) %>%
    summarize(weighted_n = sum(poids), n = n()) %>%
    mutate(prop_mixte = weighted_n / sum(weighted_n))
  
  # estime la proportion d'enfants, parmi ceux ayant au moins un parent de nationalité étrangère à la naissance,
  # dont l'autre parent est natif au carré pour chaque origine par année de naissance de l'enfant
  proportion_mixité_enfants_immigré[[i - 4]] <- enfants  %>%
    filter(enfant_immigré == TRUE) %>%
    group_by(année_enquête, anais, origine) %>%
    summarize(prop_mixte = sum(parent_natif_carré * poids) / sum(poids), n = n())
  
  # estime la proportion d'enfants, parmi ceux dont au moins un parent était de nationalité française à la naissance
  # mais avait lui-même au moins un parent de nationalité étrangère à la naissance, dont l'autre parent est natif au
  # carré pour chaque origine par année de naissance de l'enfant
  proportion_mixité_petit_enfants_immigré[[i - 4]] <- enfants  %>%
    filter(petit_enfant_immigré == TRUE) %>%
    group_by(année_enquête, anais, origine) %>%
    summarize(prop_mixte = sum(parent_natif_carré * poids) / sum(poids), n = n())
}

# combine les structures de données créées plus haut à partir de chaque édition de l'enquête
proportion_mixité_enfants_natif_carré <- rbindlist(proportion_mixité_enfants_natif_carré)
proportion_mixité_enfants_immigré <- rbindlist(proportion_mixité_enfants_immigré)
proportion_mixité_petit_enfants_immigré <- rbindlist(proportion_mixité_petit_enfants_immigré)

# crée un fichier csv avec la taille des échantillons pour chaque années de naissance et groupe d'origine
taille_échantillon_enfants_natif_carré <- proportion_mixité_enfants_natif_carré %>%
  group_by(anais) %>%
  summarize(n = sum(n)) %>%
  write_csv("taille_échantillon_enfants_natif_carré.csv")
taille_échantillon_enfants_immigré <- proportion_mixité_enfants_immigré %>%
  filter(origine != "France") %>%
  group_by(anais, origine) %>%
  summarize(n = sum(n)) %>%
  write_csv("taille_échantillon_enfants_immigré.csv")
taille_échantillon_petit_enfants_immigré <- proportion_mixité_petit_enfants_immigré %>%
  filter(origine != "France") %>%
  group_by(anais, origine) %>%
  summarize(n = sum(n)) %>%
  write_csv("taille_échantillon_petit_enfants_immigré.csv")

# fait la moyenne pondérée des estimations obtenues à partir des différentes éditions de l'enquête
# pour chaque année de naissance et groupe d'origine des enfants
évolution_mixité_enfants_natif_carré <- proportion_mixité_enfants_natif_carré %>%
  filter(origine != "France") %>%
  group_by(anais, origine) %>%
  summarize(prop_mixte = sum(prop_mixte * n) / sum(n), total_n = sum(n)) %>%
  na.omit()
évolution_mixité_enfants_immigré <- proportion_mixité_enfants_immigré %>%
  filter(origine != "France") %>%
  group_by(anais, origine) %>%
  summarize(prop_mixte = sum(prop_mixte * n) / sum(n), total_n = sum(n)) %>%
  na.omit()
évolution_mixité_petit_enfants_immigré <- proportion_mixité_petit_enfants_immigré %>%
  filter(origine != "France") %>%
  group_by(anais, origine) %>%
  summarize(prop_mixte = sum(prop_mixte * n) / sum(n), total_n = sum(n)) %>%
  na.omit()

ggplot(évolution_mixité_enfants_natif_carré, aes(x = anais, y = prop_mixte, group = origine, color = origine)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Proportion d'enfants, parmi ceux ayant au moins un parent dont les deux parents sont français de naissance, selon l'origine de l'autre parent\n(calcul basé uniquement sur les enfants vivant avec leurs deux parents au moment de l'enquête)") +
  xlab("Année de naissance") +
  ylab("Proportion") +
  scale_color_discrete(name = "Origine") +
  scale_x_continuous(breaks = seq(1991, 2017, 1)) +
  scale_y_continuous(breaks = seq(0, max(évolution_mixité_enfants_natif_carré$prop_mixte), 0.01), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Enquête emploi en continu de l'INSEE (2005-2017) - Calculs de Philippe Lemoine") +
  ggsave("Enfants ayant au moins un parent dont les deux parents sont français de naissance.png", width = 15, height = 7)

ggplot(évolution_mixité_enfants_immigré, aes(x = anais, y = prop_mixte, group = origine, color = origine)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Proportion d'enfants, parmi ceux ayant au moins un parent immigré, dont l'autre parent a deux parents qui étaient français à la naissance\n(calcul basé uniquement sur les enfants vivant avec leurs deux parents au moment de l'enquête)") +
  xlab("Année de naissance") +
  ylab("Proportion") +
  scale_color_discrete(name = "Origine") +
  scale_x_continuous(breaks = seq(1991, 2017, 1)) +
  scale_y_continuous(breaks = seq(0, max(évolution_mixité_enfants_immigré$prop_mixte), 0.05), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Enquête emploi en continu de l'INSEE (2005-2017) - Calculs de Philippe Lemoine") +
  ggsave("Enfants ayant au moins un parent de nationalité étrangère à la naissance.png", width = 15, height = 7)

ggplot(évolution_mixité_petit_enfants_immigré, aes(x = anais, y = prop_mixte, group = origine, color = origine)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Proportion d'enfants, parmi ceux dont au moins un parent est né en France de parents immigrés, dont l'autre parent a deux parents qui étaient français à la naissance\n(calcul basé uniquement sur les enfants vivant avec leurs deux parents au moment de l'enquête)") +
  xlab("Année de naissance") +
  ylab("Proportion") +
  scale_color_discrete(name = "Origine") +
  scale_x_continuous(breaks = seq(1991, 2017, 1)) +
  scale_y_continuous(breaks = seq(0, max(évolution_mixité_petit_enfants_immigré$prop_mixte), 0.05), labels = scales::percent_format(accuracy = 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : Enquête emploi en continu de l'INSEE (2005-2017) - Calculs de Philippe Lemoine") +
  ggsave("Enfants dont au moins un parent était de nationalité française à la naissance mais avait lui-même au moins un parent qui était étranger à la naissance.png", width = 15, height = 7)