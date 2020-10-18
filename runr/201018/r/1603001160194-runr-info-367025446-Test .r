# dit eerste stuk as.ab moet eigenlijk ingebouwd in het #1 script. Echt met split en rename en join enzo. Dit doe ik later.

# eerst alle kolommen niet sir weggooien, zodat je alleen de sir columns overhoudt plus het eerste deel
df_alleenSIRs <- select(df, 1:6, ends_with("_SIR"))
str(df_alleenSIRs)

#nu "_SIR" erafstrippen
names(df_alleenSIRs) <- gsub("_SIR", "", names(df_alleenSIRs))

#nu de kolomnamen eruithalen en in een vector stoppen
kolomnamen_oud <- names(df_alleenSIRs)

# van deze string de eerste erafhalen, zijn nml geen antibiotica
kolomnamen_sirsoud <- kolomnamen_oud[7:91]

#de eerste kolommen even pakken
kolomnamen_eerste <- kolomnamen_oud[1:6]

#ombouwen tot ab
kolomnamen_sirsnieuw <- as.ab(kolomnamen_sirsoud)

# #voor duidelijkheid SIR er weer aanhangen, dit hieronder bestaat niet, volgt nog, hoeft nu niet, maar wel als je het in script1 wilt onderbrengen samen met mics enzo
# kolomnamen_sirsnieuw <- gadd("", "_SIR"(kolomnamen_sirsnieuw))

#en ze weer samenvoegen
kolomnamen_nieuw <- c(kolomnamen_eerste, kolomnamen_sirsnieuw)

#en nu moeten deze weer in de dataframe worden gehangen
names(df_alleenSIRs) <- kolomnamen_nieuw

str(df_alleenSIRs)

rm(kolomnamen_oud)
rm(kolomnamen_eerste)
rm(kolomnamen_sirsoud)
rm(kolomnamen_sirsnieuw)
rm(kolomnamen_nieuw)





#----------------------------------------------------------------------------------------------------------



# dan aan de slag nu

head(df_alleenSIRs)

rlang::last_error()

?mo_orde

df_alleenSARs <- df_alleenSIRs %>% mutate(Micro_order = mo_order(Micro_ID))


#deze hieronder doet het.  waarom alleen deze? tot nu toe? en de uitkomst is nu: 
df_alleenSIRs$MicroOrganismeName %>%  mo_order()

#is zelfde:
mo_order(df_alleenSIRs$MicroOrganismeName)

maar bij beide zit het nu nog niet in de dataframe 



mutate(ding = mo_order(Micro_ID))


dit is een functie, die werkt goed:
  df_alleenSIRs <- mo_order(df_alleenSIRs$Mo_ID)

# nb: je hoeft helemaal niet een nieuwe kolom te maken!!! je kunt ook gewoon bij het filteren vd rijen, de Micro_ID gewoon in die functie stoppen en daarop dan filteren!


df_alleenSIRs <- mutate()



df_alleenSIRs <- df_alleenSIRs %>%  mutate(Micro_order = mo_order(Mo_ID))
