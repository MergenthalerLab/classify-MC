
base_path = Sys.getenv("BASEPATH")
input_path = paste0(base_path, "/Data")
#thymus pathology:

thymus_path = Anamnese %>% left_join(mk_info_wa %>% select(Pat_Nr, grp, Vorname, Nachname), by = c("Pat_Nr")) %>% group_by(grp, Pat_Nr, Vorname, Nachname, Thymektomie, Thymom, `Thymus-Hyperplasie`, `Thymus-WHO`, `Thymus-Masaoka`) %>% 
                                       summarize(cnt_Thymektomie =  n())

########################################################################################################################################
#compare the treatment date to the MC date and throw out all dates, that happened half a year after the crisis
########################################################################################################################################
encounters_after_crisis = filter(sap_patients, h_v == "Date") %>% mutate(value = dmy(str_pad(value,8, pad = "0"))) %>%
  left_join(mk_info_wa %>% select(starts_with("mk"), Patient_ID, Geschlecht, Vorname), by = c("Patient_ID")) %>%
  mutate_at(vars(starts_with("mk"), -mk), ~coalesce(ymd(.x),ymd("9999-12-31"))) %>% 
  filter((value >= mk1_date) & (value < mk1_date + months(6)) | (value >= mk2_date) & (value < mk2_date + months(6)) | 
           (value >= mk3_date) & (value < mk3_date + months(6))) %>% distinct(Encounter, Geschlecht, mk)


lab_ids  = group_by(sap_patients, Patient_ID, Encounter) %>% filter(h_v == "LeistID") %>% mutate(lab_ID = paste0(Encounter, "_", row_number()))

sap_patients_wLabID = sap_patients %>% left_join(select(lab_ids, Patient_ID, Encounter, new_headers, lab_ID), by = c("Patient_ID", "Encounter", "new_headers")) %>% 
  fill(lab_ID)


pre_crisis = anti_join(sap_patients_wLabID, encounters_after_crisis, by = c("Encounter")) %>% 
  inner_join(mk_info_wa %>% select(Patient_ID, mk, Gender)) %>% rename(Geschlecht = Gender)

#sanity check
if(FALSE){
  group_by(pre_crisis, Patient_ID) %>% summarize(cnt_E = n_distinct(Encounter)) %>% arrange(desc(cnt_E))
}

####################################################################
#look at Age and Gender
####################################################################

#inferred age, gender for those where it's missing: 
Age_and_Gender_inferred = fread(paste0(input_path, "/raw data/patients_correction_11232021.csv"), 
                            colClasses = c("character", "character", "character", "numeric","character", 
                                           "character", "numeric", "numeric"))

Age_and_Gender = inner_join(mk_info, Onset, by = c("Vorname", "Nachname" = "Name")) %>% select(Vorname, Nachname, Patient_ID, Geschlecht, Geburtsdatum, Diagnose_Jahr) %>% 
  mutate(Age = (floor(as.numeric(ymd(today()) - ymd(Geburtsdatum))/365)/100), Age_of_onset = (as.numeric(Diagnose_Jahr) - as.numeric(year(ymd(Geburtsdatum))))/100) %>%
  mutate(Geschlecht = if_else(grepl("weibl", Geschlecht), 1, 0))

Age_and_Gender = bind_rows(Age_and_Gender, Age_and_Gender_inferred)

#####################
#Create Features
#####################

#Encounters
Encounters_days = group_by(pre_crisis, Patient_ID, Geschlecht, Encounter, lab_ID,  mk) %>% 
  summarize(key = value[h_v == "Kat"], Date = value[h_v == "Date"]) %>% transmute(Patient_ID, Encounter, Date = dmy(str_pad(Date, 8, "left", pad = "0"))) %>% 
             ungroup() %>% distinct(Patient_ID, Date)

nr_Encounter_days = group_by(Encounters_days, Patient_ID) %>% summarize(cnt_Encounters_days = n()/365)

nr_Encounters_days_by_timeframe = Encounters_days %>% mutate(Date = Date, dummy = 1) %>% 
  bind_rows( Encounters_days %>% mutate(Date =  Date + months(6), dummy = -1)) %>% 
  group_by(Patient_ID) %>% arrange(Date) %>% 
                      mutate(cnt_Encounters_days_6months = cumsum(dummy)/365) %>% select(-dummy) %>% 
  group_by(Patient_ID, Date) %>% arrange(cnt_Encounters_days_6months) %>% filter(row_number() == 1)
                      

#Medication
Drugs = pivot_longer(Medikation, cols= c(contains("Drug")), names_to = "Drug") %>% select(Pat_Nr, Datum, Drug, value) %>% filter(!is.na(value) & value != "")
Mclass = pivot_longer(Medikation, cols = c(contains("Klasse")), names_to = "Mclass") %>% select(Pat_Nr, Datum, Mclass, value)%>% filter(!is.na(value) & value != "")
Factor = pivot_longer(Medikation, cols = c(contains("Faktor")), names_to = "Factor") %>% select(Pat_Nr, Datum, Factor, value)%>% filter(!is.na(value) & value != "")
Instructions = pivot_longer(Medikation, cols = c(contains("Anweisung")), names_to = "instructions")%>% select(Pat_Nr, Datum, instructions, value)%>% filter(!is.na(value) & value != "")
Dose = pivot_longer(Medikation, cols = c(contains("Tagesmenge")), names_to = "dose")%>% select(Pat_Nr, Datum, dose, value)%>% filter(!is.na(value) & value != "")
                          
Medikations_distr = Mclass %>% group_by(value) %>% summarize(cnt_patients = n_distinct(Pat_Nr)) %>% 
  mutate(cnt_p = cnt_patients/sum(cnt_patients))

ggplot(data = Medikations_distr, aes(x = value, y = cnt_p, fill = cnt_p)) + geom_col() + theme_minimal() + scale_fill_continuous_tableau(name = "%", label = percent) + 
  xlab(NULL) + scale_y_continuous(name = NULL, label = percent) + 
  ggtitle(paste0("Distribution of Medication Classes,  N = ", (distinct(Medikation, Pat_Nr) %>% summarize(cnt = n()))$cnt))


Medication_clean = list()
for(i in 1:10){
  M = select(Medikation, Pat_Nr, Datum, ends_with(as.character(i)))
  names(M) = gsub("[[:digit:]]+$", "", names(M))
  Medication_clean[[i]]= M
  
}
Medication_clean = bind_rows(Medication_clean)
cehl = filter(Medication_clean, Klasse == "CE_Hemmer") %>% distinct(Drug) %>% filter(Drug != "") %>% pull()
cortl = filter(Medication_clean, Klasse == "Corticoid") %>% distinct(Drug) %>% filter(Drug != "") %>% pull()
isprl = filter(Medication_clean, Klasse == "Immunsupressivum") %>% distinct(Drug) %>% filter(Drug != "") %>% pull()
Medication_clean= mutate(Medication_clean, Klasse = coalesce(Klasse, if_else(Drug %grin% cehl, "CE_Hemmer", 
                                                                             if_else(Drug %grin% cortl, "Corticoid", 
                                                                                if_else(Drug %grin% isprl, "Immunsupressivum", "")))))

#Wirkstoffklassen
Wirkstoff_kombinationen_beforeCrisis = left_join(Medication_clean, select(id_lookup, Pat_Nr, mk1_date, Patient_ID), by = c("Pat_Nr")) %>%
  filter(ymd(Datum) < ymd(mk1_date) | mk1_date == ""| is.na(mk1_date)) %>% 
   filter(!is.na(Patient_ID)) %>% filter(!is.na(Klasse)) %>% 
  group_by(Patient_ID) %>% summarize(Wirkstoffe = paste0(unique(Klasse), collapse = ", "))  %>% 
  #separate(Wirkstoffe, c("CE_Hemmer", "Corticoid", "Immunsuppressivum"), sep = ", ") %>% 
  mutate(CE_Hemmer = if_else(grepl("CE_Hemmer", Wirkstoffe), 1, 0), 
         Immunsuppressivum = if_else(grepl("Immunsuppressivum", Wirkstoffe), 1, 0), 
         Corticoid = if_else(grepl("Corticoid", Wirkstoffe), 1, 0))


#referezwerte
referenz_werte = group_by(pre_crisis, Patient_ID, Geschlecht, Encounter, lab_ID,  mk) %>% 
  summarize(key = value[h_v == "Kat"], Date = value[h_v == "Date"], range_low = value[h_v == "range_low"], range_high = value[h_v == "range_high"], value = value[h_v == "Value"]) %>% 
  mutate(value = if_else(grepl("Titin", key) & value == "negativ", "0", 
                         if_else(grepl("Titin", key) & value == "positiv", "1", 
                                 if_else(grepl("Titin", key) & value == "stark positiv", "2", 
                                         if_else(grepl("Titin" ,key), "", value))))) %>% 
  mutate(range_low =  gsub(",", ".", range_low), range_high =  gsub(",", ".", range_high)) %>% mutate(value = as.numeric(value)) %>% filter(value >= 0) %>% 
  ungroup()%>% mutate(key = gsub(" ", "_", key)) %>% 
  mutate(key = gsub("_.37°.|_absolut", "", key)) %>% mutate(key = gsub("°", "", key)) %>% mutate(key = gsub("ö", "?", key)) 
  


##############################     normalize values ###########################
#determine_scale_for_values = group_by(referenz_werte, key) %>% summarize(cnt = n()) %>% filter(!is.na(key)) %>% select(key)
#determine_scale_for_values = fread("../Desktop/Sivani_Dottore/Blutwerte_clean_V2.csv", header = T, encoding = "UTF-8") %>% select(-Kommentar, -Unit, -Material) #
determine_scale_for_values = fread(paste0(input_path, "/lookup_tables/Blutwerte_clean_V3.csv"), header = T, encoding = "UTF-8") %>% select(-Kommentar, -Unit, -Material)


referenz_werte = ungroup(referenz_werte) %>% inner_join(determine_scale_for_values, by = c("key" = "Key")) %>% 
  mutate(range_low = if_else(`Key Clean` == "Titin_Ak", "0", range_low), range_high = if_else(`Key Clean` == "Titin_Ak", "2", range_high)) %>% 
  mutate(range_low = if_else(range_low == range_high, "", range_low)) %>% 
  mutate(range_low = if_else(range_low == "", NA_character_, range_low), range_high = if_else(range_high == "", NA_character_, range_high)) %>% 
  mutate(range_low = coalesce(range_low, if_else(Geschlecht == "w", as.character(`Erwachsene Frauen Untergrenze`), 
                                                 if_else(Geschlecht == "m", as.character(`Erwachsene M?nner Untergrenze`), as.character(`Kinder Untergrenze`)))), 
         range_high = coalesce(range_high, if_else(Geschlecht == "w", as.character(`Erwachsene Frauen Obergrenze`), 
                                                   if_else(Geschlecht == "m", as.character(`Erwachsene M?nner Obergrenze`), as.character(`Kinder Obergrenze`))))) %>% 
  mutate(value = if_else(`Key Clean` == "Haematokrit" &  as.numeric(range_high) <= 1, value*100, value), 
         range_low = if_else(`Key Clean` == "Haematokrit" &  as.numeric(range_high) <= 1, as.character(as.numeric(range_low)*100), range_low), 
         range_high = if_else(`Key Clean` == "Haematokrit" &  as.numeric(range_high) <= 1, as.character(as.numeric(range_high)*100), range_high)) %>% 
  mutate(range_low = if_else(`Key Clean` == "Procalcitonin", 0.005, as.numeric(range_low)))


##############################    Timelineplots by patient    ######################################

#normalized version
referenz_werte_normalized = mutate(referenz_werte, value = ((value - as.numeric(range_low))/(as.numeric(range_high) - as.numeric(range_low)))) %>% 
  mutate(Wert = `Key Clean`) %>%select( -`Erwachsene Frauen Untergrenze`,  -`Erwachsene Frauen Obergrenze`,-`Erwachsene M?nner Untergrenze`, -`Erwachsene M?nner Obergrenze`,
                                        -`Kinder Untergrenze`,  -`Kinder Obergrenze`) %>% 
  mutate(quarter = as.character(quarter(dmy(str_pad(Date, 8, "left", pad = "0")))))


#plots by patient for timeline of values
if(FALSE){
  for(pid in unique(referenz_werte_normalized$Patient_ID)){
    timeline_view_by_patient_n = mutate(referenz_werte_normalized, value = as.numeric(value), range_low = as.numeric(range_low), range_high = as.numeric(range_high)) %>%
      filter(Patient_ID == pid) %>% filter(!is.na(value)) %>% mutate(Date = dmy(str_pad(Date,8, pad = "0"))) %>%  ungroup() %>%
      inner_join(select(mk_info,Patient_ID, mk1_date, mk2_date, mk3_date), by = c("Patient_ID")) %>% 
      mutate_at(vars(starts_with("mk"), -mk), ymd)
      
    
    ggplot(data = timeline_view_by_patient_n) + 
      geom_ribbon(data = timeline_view_by_patient_n %>% group_by(Patient_ID, Date, Wert) %>% summarize(min_value = min(value, na.rm = T), max_value = max(value, na.rm = T)), 
                  aes(x = Date, ymin = min_value, ymax = max_value), 
                  color = "grey", fill = "lightgrey")+ theme_minimal() + 
      geom_point(aes(x = Date, y = value), color = "black") + 
      geom_rect(data = group_by(timeline_view_by_patient_n, Wert) %>% summarize(ymin = pmin(min(value, na.rm = T),0), 
                                                                                ymax = pmax(max(value, na.rm = T),1), 
                                                                                xmin = first(mk1_date), 
                                                                                xmax = first(mk1_date) + months(6)), 
                aes(xmax = xmax, xmin = xmin, ymin = ymin, ymax = ymax), alpha = 0.4, color = "gray") +
      geom_rect(data = group_by(timeline_view_by_patient_n, Wert) %>% summarize(ymin = pmin(min(value, na.rm = T),0), 
                                                                                ymax = pmax(max(value, na.rm = T),1), 
                                                                                xmin = first(mk2_date), 
                                                                                xmax = first(mk2_date) + months(6)), 
                aes(xmax = xmax, xmin = xmin, ymin = ymin, ymax = ymax), alpha = 0.4, color = "gray") +
      geom_rect(data = group_by(timeline_view_by_patient_n, Wert) %>% summarize(ymin = pmin(min(value, na.rm = T),0), 
                                                                                ymax = pmax(max(value, na.rm = T),1), 
                                                                                xmin = first(mk3_date), 
                                                                                xmax = first(mk3_date) + months(6)), 
                aes(xmax = xmax, xmin = xmin, ymin = ymin, ymax = ymax), alpha = 0.4, color = "gray") +
      facet_wrap(~Wert, scales = "free_y") + ggtitle(paste0("Normalisierte Blutwerte-Zeitreihe ", if_else(first(timeline_view_by_patient_n$mk), "MK ", "NMK "), "Patient: ", first(timeline_view_by_patient_n$Patient_ID))) + 
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      geom_hline(color = "blue", yintercept = 0)+
      geom_hline(color = "red", yintercept = 1) + ylab("") + xlab("")
    
    ggsave(paste0("../Plots/Blutwerte_Timeline_", pid, "_normalisiert.png"))
  }
}

#too few data points, revisit later
if(FALSE){
  AK = select(mk_info, Patient_ID, Vorname, Nachname, grp) %>% 
    left_join(select(Patienten, Pat_Nr, Name, Vorname), by = c("Nachname" = "Name", "Vorname")) %>% 
    left_join(select(Anamnese, Pat_Nr, AK_negativ, AChR, `AChR-value`, MuSK, `MuSK-value`, Titin, `Titin-value`, VGCC), by = c("Pat_Nr")) %>% 
    distinct(Vorname, Nachname, .keep_all = T)
  AK_g = group_by(AK, grp, AChR, MuSK) %>% summarize(cnt = n())
  
  Thymus_Pathologie = select(mk_info, Patient_ID, Vorname, Nachname, grp) %>% 
    left_join(select(Patienten, Pat_Nr, Name, Vorname), by = c("Nachname" = "Name", "Vorname")) %>% 
    left_join(select(Anamnese, Pat_Nr, Thymektomie, `Thymektomie-Jahr`, Thymom, `Thymus-Hyperplasie`)) %>% 
    distinct(Vorname, Nachname, .keep_all = T)
  group_by(Thymus_Pathologie, grp, Thymom, `Thymus-Hyperplasie`) %>% summarize(cnt = n()) %>% View()
  
  
  
  
}

#treatment features: 
#cleaning_table_treatment = filter(treat_proc, h_v == "Descr") %>% group_by(value) %>% summarize(cnt = n())
#fwrite(cleaning_table_treatment, "../Desktop/Sivani_Dottore/cleaning_table_treatment_unclean.csv", sep = "|")
cleaning_table_treatment = fread(paste0(input_path, "/lookup_tables/cleaning_table_treatment_V2.csv"))

treat_proc_clean = left_join(treat_proc, cleaning_table_treatment, by = c("value" = "value")) %>% 
  mutate(treatment = value_clean) %>% filter(h_v == "Descr") %>% 
  select(treatment, Encounter, Patient_ID) %>% mutate(value = 1) %>% 
  pivot_wider(id_cols = c(Encounter, Patient_ID), names_from = treatment, values_from = value, values_fn = list(value = max), values_fill = list(value = 0))

#visitation features: 

hospital_path = filter(encounter_attr, h_v %in% c("oe_admission", "oe_discharge")) %>% pivot_wider(names_from = h_v, values_from = value) %>% 
  group_by(Patient_ID, oe_admission, oe_discharge) %>% summarize(cnt_Encounters = n_distinct(Encounter)) %>% #filter(cnt_Encounters > 10) %>% 
  arrange(-cnt_Encounters) %>% 
  group_by(Patient_ID) %>% mutate(p_Encounters = cnt_Encounters/sum(cnt_Encounters)) %>% mutate(path = paste0(oe_admission, ", ", oe_discharge)) %>% 
  select(Patient_ID, path, p_Encounters) %>% pivot_wider(names_from = path, values_from = p_Encounters, values_fill = list(p_Encounters = 0))


hospital_stay = filter(encounter_attr, h_v %in% c("admission", "discharge", "encounter_type")) %>% pivot_wider(names_from = h_v, values_from = value) %>%
  mutate(admission = dmy(admission), discharge = dmy(discharge)) %>% mutate(dummy = 1) %>% 
  pivot_wider(names_from = encounter_type, values_from = dummy, values_fn = list(dummy = max), values_fill = list(dummy = 0)) %>% filter(!is.na(admission) & !is.na(discharge)) %>% 
  group_by(Patient_ID) %>% 
  arrange(admission) %>% mutate(cnt_stat = cumsum(stat), cnt_amb = cumsum(amb)) %>% 
    left_join(select(mk_info, Patient_ID, mk1_date)) %>% mutate(mk1_date = ymd(mk1_date))


hospital_stay = hospital_stay %>% select(Patient_ID, Encounter, cnt_stat, cnt_amb)

bloodwork_trends = referenz_werte_normalized %>% group_by(Patient_ID, `Key Clean`) %>% mutate(Date = dmy(str_pad(Date, 8, "left", pad = "0"))) %>% 
     arrange(Date) %>% mutate(trend_abs = coalesce(value - lag(value),0), trend = coalesce(trend_abs/as.numeric(Date - lag(Date)),0)) %>% select(Patient_ID, Encounter, Date, `Key Clean`, trend_abs, trend) %>% 
  filter(!is.infinite(trend_abs) & (!is.infinite(trend))) %>% 
  pivot_wider(names_from = `Key Clean`, values_from = c(trend_abs, trend), values_fn = list(trend_abs = mean, trend = mean), values_fill = list(trend_abs = 0, trend = 0))
 
if(FALSE){
#Encounter_join_Table = distinct(sap_patients, Encounter,Patient_ID, h_v, value) %>% filter(h_v == "Date") %>% 
#  mutate(Date = dmy(str_pad(value, 8, pad = "0"))) %>% select(-h_v)

Medication_by_Encounter = select(aktuelle_Medikation, Pat_Nr, Datum, Name) %>% inner_join(distinct(Medication_clean2, Drug_orig, cnt, Wirkstoffname) %>% 
                                                                                            unnest(Drug_orig), by = c("Name" = "Drug_orig")) %>% select(-Name) %>% 
  inner_join(id_lookup %>% select(Pat_Nr, Patient_ID)) %>% 
  inner_join(sap_patients_wd, by = c("Patient_ID")) %>% ungroup() %>% mutate(Date = ymd(Datum), dummy = 1) %>% 
  pivot_wider(names_from =Wirkstoffname, values_from = dummy, values_fn = list(dummy = max), values_fill = list(dummy = 0))

}

#assemple all Features
#not used AK, Thymus-Pathologie, cuz medical matching. but maybe in second run

Features_MK_NMK = mutate(referenz_werte_normalized, Date = dmy(str_pad(Date, 8, "left", pad = "0"))) %>% select(Patient_ID, Encounter, mk, Date, Wert, value) %>% 
  pivot_wider(names_from = Wert, values_from = value, values_fn = list(value = mean)) %>%
  group_by(Patient_ID) %>% arrange(Date) %>% fill(-Patient_ID, -Encounter, -mk, -Date) %>% 
  left_join(select(Age_and_Gender, Patient_ID, Geschlecht, Age, Age_of_onset), by = c("Patient_ID")) %>% 
  #left_join(select(AK, Patient_ID, AChR), by = c("Patient_ID")) %>%
  left_join(nr_Encounter_days, by = c("Patient_ID")) %>% 
  #left_join(select(Thymus_Pathologie, -Name, -Vorname, -Pat_Nr, -`Thymektomie-Jahr`), by = c("Patient_ID")) %>% 
  left_join(treat_proc_clean, by = c("Patient_ID", "Encounter")) %>% 
  left_join(hospital_path, by = c("Patient_ID")) %>% 
  left_join(hospital_stay, by = c("Patient_ID", "Encounter")) %>% 
  left_join(bloodwork_trends, by = c("Patient_ID", "Encounter", "Date")) %>% 
  left_join(nr_Encounters_days_by_timeframe, by = c("Patient_ID", "Date")) %>% 
  #left_join(Medication_history, by = c("Patient_ID", "Encounter", "Date")) %>% 
  left_join(Wirkstoff_kombinationen_beforeCrisis, by = c("Patient_ID")) %>% select(-"Wirkstoffe") %>%
  inner_join(select(mk_info, Patient_ID, mk1_date, mk2_date, mk3_date)) %>% 
  ungroup()

  
#eliminate all data after the crisis - should be more sophisticated in case of many crises
Features_MK_NMK_g = group_by(Features_MK_NMK, Patient_ID, mk, mk1_date, mk2_date, mk3_date) %>% filter(is.na(ymd(mk1_date)) | Date < ymd(mk1_date)) %>% 
  arrange(desc(Date)) %>%
  summarize_at(vars(-Encounter, -Date), 
               list(min = ~min(pmin(0,.x), na.rm = T), max = ~max(pmax(0, .x), na.rm = T), mean =  ~mean(.x, na.rm = T),
                    median = ~median(.x, na.rm = T), lastKnown = ~first(na.omit(.x)), stdev = ~sd(.x, na.rm = T))) %>% 
  mutate_if(is.numeric, ~if_else(is.infinite(.x), NA_real_, as.numeric(.x))) %>% select(Patient_ID, mk, contains("trend"))  

Features_MK_NMK_g2 = group_by(Features_MK_NMK, Patient_ID, mk, mk1_date, mk2_date, mk3_date) %>% filter(is.na(ymd(mk1_date)) | Date < ymd(mk1_date)) %>% arrange(desc(Date)) %>%
  summarize_at(vars(-Encounter, -Date), #-quarter
               list(min = ~min(.x, na.rm = T), max = ~max(.x, na.rm = T), mean =  ~mean(.x, na.rm = T), median = ~median(.x, na.rm = T), lastKnown = ~first(na.omit(.x)), stdev = ~sd(.x, na.rm = T))) %>% 
  mutate_if(is.numeric, ~if_else(is.infinite(.x), NA_real_, as.numeric(.x))) %>% select(-contains("trend"))

Features_MK_NMK_all = inner_join(Features_MK_NMK_g, Features_MK_NMK_g2, by = c("Patient_ID", "mk")) %>% rename(cnt_Encounters_days = cnt_Encounters_days_min) %>% 
  select(-cnt_Encounters_days_max, -cnt_Encounters_days_mean, -cnt_Encounters_days_median, -cnt_Encounters_days_lastKnown, -mk1_date.x, -mk2_date.x, -mk1_date.y, -mk2_date.y) %>% 
  rename(cnt_amb = cnt_amb_min) %>% select(-cnt_amb_max, -cnt_amb_mean, -cnt_amb_median, -cnt_amb_lastKnown) %>% filter(!is.na(mk))
names(Features_MK_NMK_all) = make.names(names(Features_MK_NMK_all))


#0 is the best, 51 means, no data at all
completeness  = ungroup(Features_MK_NMK_all)%>% summarize_all(.funs = ~sum(is.na(.x))) %>% gather() %>% filter(value<10) %>% 
  filter(!(key %in% c("Patient_ID", "Encounter", "mk", "mk1_date", "mk2_date", "mk3_date", "Date")))

ggplot(data = ungroup(Features_MK_NMK_all)%>% summarize_all(.funs = ~sum(is.na(.x))) %>% gather() %>%
         mutate(completeness = if_else(value < 10, "insufficient completeness", "sufficient completeness")) %>% 
         group_by(completeness) %>% summarize(cnt = n())) +
  geom_col(aes(x = completeness, y = cnt)) + theme_minimal() + xlab(NULL) + ylab("number of features")

if(FALSE){
  #first regression trials without imputation
  mya_model_V1 = glm(as.formula(paste("mk ~ ", paste(completeness$key, collapse = " + "))), data = regdff)
  summary(mya_model_V1)
  
  #debug for correlation
  reg_df %>%group_by(mk, cnt_Encounters) %>% summarize(cnt = n()) %>% View()
  model.matrix(as.formula(paste("~ ", paste(completeness$key, collapse = " + "))), data = regdff) 
}

result_df = list()
#Regressions ?ber die Einzelwerte, um Signifikanz zu testen
for(key in completeness$key){
  mya_model_V1 = glm(as.formula(paste("mk ~ ", key)), data = Features_MK_NMK_all, family = "binomial")
  result_df[[key]] = mya_model_V1 %>% tidy %>% filter(row_number() == 2)
}

result_final1 = bind_rows(result_df) %>% arrange(p.value) %>% mutate(stat_sign = p.value < 0.05) %>% 
  separate(term, into = c("I1", "I2", "I3", "I4"), sep =  "_", remove = F) %>% mutate(Interpretation = if_else(I1 == "trend", 
                                                                                                               if_else(I2 == "abs", paste0("The absolute ", I4, " trend of ", I3 , " shows a "),
                                                                                                                       paste0("The time relative ", I3, " trend of ", I2 , " shows a ")), ""), 
                                                                                      Interpretation2 = if_else(estimate >= 0, "positive Estimate. ", "negative Estimate. "), 
                                                                                      Interepretation3 = if_else(Interpretation2 == "negative Estimate. ", "this suggests, the abrupt decrease in comparison to the last value during the entire disease progression",
                                                                                                                 "this suggests, the abrupt rise in comparison to last value during the entire disease progression"), 
                                                                                      Interpretation4 = "is predictive of a myasthenic crisis") %>% 
  mutate(Interpretation_final = if_else(Interpretation != "", paste(Interpretation, Interpretation2, Interepretation3, Interpretation4), "")) %>% select(-Interpretation, -Interpretation2, -Interepretation3, -Interpretation4) %>% 
  mutate(Interpretation = if_else(I1 == "cnt", paste0("The Count of "), ""), Interpretation2 = if_else(I2 == "amb", "ambulant stays shows a", "encounters shows a"),
        Interpretation2 = if_else(estimate >= 0, "positive Estimate.", "negative Estimate.")) %>% 
  mutate(Interepretation3 = if_else(Interpretation2 == "negative Estimate. ", " This suggests anti-correlation with myasthenic crisis", "This suggests correlation with myasthenic crisis.")) %>% 
  mutate(Interpretation_final = if_else(Interpretation_final == "" & I1 == "cnt", paste(Interpretation, I2, "shows a", Interpretation2, Interepretation3), Interpretation_final)) %>% select(-Interpretation, -Interpretation2, -Interepretation3)%>% 
  mutate(Interpretation = if_else(!(I1 %in% c("cnt", "trend")), paste0("The ", I2, " value of ", I1, " shows a"), ""), 
         Interpretation2 = if_else(estimate >= 0, "positive Estimate. ", "negative Estimate. "), 
         Interepretation3 = if_else(Interpretation2 == "negative Estimate. ", "This suggests no correlation with myasthenic crisis","This suggests a correlation with myasthenic crisis")) %>% 
  mutate(Interpretation_final = if_else(Interpretation_final == "", paste(Interpretation,Interpretation2, Interepretation3), Interpretation_final)) %>% select(-Interpretation, -Interpretation2, -Interepretation3) %>% 
  select(-I1, -I2, -I3, -I4, -statistic)# %>% mutate(estimate = as.character(round(estimate,2)), std.error = as.character(round(std.error,2)), 
                                        #            p.value = as.character(round(p.value,2)))

#complex features
result_df2 = list()
cnter = 1
for(key1 in completeness$key){
  cnter = cnter + 1
  print(paste0("Feature: ", cnter, " ,", "key1: " ,key1))
  for(key2 in filter(completeness, value < 7)$key){
    #print(paste0("key2: " ,key2))
    if(key1 >= key2){next()}
     mya_model_V2 = glm(as.formula(paste0("mk ~ ", key1, ":", key2)), data = Features_MK_NMK_all, family = "binomial")
     result_df2[[paste(key1, key2, sep = ":")]] = mya_model_V2 %>% tidy %>% filter(row_number() == 2)
  }
}
result_final2 = bind_rows(result_df2) %>% arrange(p.value) %>% mutate(stat_sign = p.value < 0.05) %>% 
  separate(term, into = c("I1", "I2", "I3", "I4"), sep =  "_", remove = F) %>% mutate(Interpretation = if_else(I1 == "trend", 
                                                                                                               if_else(I2 == "abs", paste0("The absolute ", I4, " trend of ", I3 , " shows a "),
                                                                                                                       paste0("The time relative ", I3, " trend of ", I2 , " shows a ")), ""), 
                                                                                      Interpretation2 = if_else(estimate >= 0, "positive Estimate. ", "negative Estimate. "), 
                                                                                      Interepretation3 = if_else(Interpretation2 == "negative Estimate. ", "this suggests, the abrupt decrease in comparison to the last value during the entire disease progression",
                                                                                                                 "this suggests, the abrupt rise in comparison to last value during the entire disease progression"), 
                                                                                      Interpretation4 = "is predictive of a myasthenic crisis") %>% 
  mutate(Interpretation_final = if_else(Interpretation != "", paste(Interpretation, Interpretation2, Interepretation3, Interpretation4), "")) %>% select(-Interpretation, -Interpretation2, -Interepretation3, -Interpretation4) %>% 
  mutate(Interpretation = if_else(I1 == "cnt", paste0("The Count of "), ""), Interpretation2 = if_else(I2 == "amb", "ambulant stays shows a", "encounters shows a"),
         Interpretation2 = if_else(estimate >= 0, "positive Estimate.", "negative Estimate.")) %>% 
  mutate(Interepretation3 = if_else(Interpretation2 == "negative Estimate. ", " This suggests anti-correlation with myasthenic crisis", "This suggests correlation with myasthenic crisis.")) %>% 
  mutate(Interpretation_final = if_else(Interpretation_final == "" & I1 == "cnt", paste(Interpretation, I2, "shows a", Interpretation2, Interepretation3), Interpretation_final)) %>% select(-Interpretation, -Interpretation2, -Interepretation3)%>% 
  mutate(Interpretation = if_else(!(I1 %in% c("cnt", "trend")), paste0("The ", I2, " value of ", I1, " shows a"), ""), 
         Interpretation2 = if_else(estimate >= 0, "positive Estimate. ", "negative Estimate. "), 
         Interepretation3 = if_else(Interpretation2 == "negative Estimate. ", "This suggests no correlation with myasthenic crisis","This suggests a correlation with myasthenic crisis")) %>% 
  mutate(Interpretation_final = if_else(Interpretation_final == "", paste(Interpretation,Interpretation2, Interepretation3), Interpretation_final)) %>% select(-Interpretation, -Interpretation2, -Interepretation3) %>% 
  select(-I1, -I2, -I3, -I4, -statistic) #%>% mutate(estimate = as.character(round(estimate,2)), std.error = as.character(round(std.error,2)), 
                                        #            p.value = as.character(round(p.value,2)))

full_featurecandidates_list = result_final1 %>%  select(term) 

#write_csv(full_featurecandidates_list, paste0(input_path, "Classifying_Patients_V", V, "/full_featurecandidates_list.csv"))
write_csv(full_featurecandidates_list, paste0(base_path, "/Paper_1_MC_Classification/data/full_featurecandidates_list.csv"))


result_final = bind_rows(result_final1, result_final2) %>% group_by(interaction = grepl(":", term)) %>% arrange(p.value) %>% filter(row_number() < 30)
#result_final_rf = result_final1 

plot_complex = separate(result_final2, term, sep = ":", into = c("key1", "key2"), remove = FALSE) %>% arrange(p.value) %>%
gc()
plots = list()
for(i in 1:20){
 df = data_frame(mk = Features_MK_NMK_all$mk, x = Features_MK_NMK_all[[plot_complex$key1[i]]], y = Features_MK_NMK_all[[plot_complex$key2[i]]])
 plots[[i]] = ggplot(df, aes(x = x, y = y, color = mk)) + geom_point() + ggtitle(plot_complex$term[i]) + theme_minimal() + scale_color_colorblind() + 
   xlab(plot_complex$key1[i]) + ylab(plot_complex$key2[i])
}

fwrite(result_final, paste0(input_path, "/Classifying_Patients_V", V, "/interpretation_simple_and_complex_features.csv"))

##############################    Imputation     ######################################
cols = names(Features_MK_NMK_all)[names(Features_MK_NMK_all) %in% c(semi_join(plot_complex, result_final, by = c("term"))$key1, semi_join(plot_complex, result_final, by = c("term"))$key2, result_final$term, "mk")]
cols = cols[!(grepl("cnt_Encounter_days_m.*", cols)) & !(grepl("Age_m.*", cols)) & !(grepl("MAUFN..M146_m", cols)) & !(grepl("MNEURO..MNE.AMB_m", cols))]

df_clean = Features_MK_NMK_all[,cols]
names(df_clean) = make.names(names(df_clean))

result_final_s = filter(result_final, !(grepl("cnt_Encounters_m.*", term)) & !(grepl("Age_of_onset_m.*", term)) & !(grepl("MAUFN..M146_m", term)) & !(grepl("MNEURO..MNE.AMB_m", term)))
#result_final_rf_s = filter(result_final_rf, !(grepl("cnt_Encounters_m.*", term)) & !(grepl("Age_of_onset_m.*", term)) & !(grepl("MAUFN..M146_m", term)) & !(grepl("MNEURO..MNE.AMB_m", term)))

features_all_runs = list()
features_all_runs_rf = list()
importance_all_rF_runs = list()
all_metrics = list() 
imputed_runs = list()

results_distribution = list()
results_distribution_rF = list()
for(j in 1:100){
  print(j)
  print(glue("{now()}: start mice"))
  #initialize df, set which vars won't be imputed. 
  init = mice(df_clean, maxit = 0) #numeric = "pmm", factor = "logreg", more than two classes = "polyreg", ordered factors = "polr"
  cristel_mef = init$method
  cristel_mef[c("mk")]=""
  
  #choose which vars not to use for imputation
  goldene_prem = init$predictorMatrix
  goldene_prem[, c("mk")]=0
  
  
  #impute
  imputed = mice(df_clean, method=cristel_mef, predictorMatrix=goldene_prem, m=5)
  imputed = complete(imputed)
  
  #emergency imputation
  imputed_s = mutate_if(imputed, is.numeric, ~coalesce(.x, mean(.x, na.rm = T))) 
  print(glue("{now()}: end mice"))
  
  imputed_runs[[j]]= imputed_s
  #fixing imputed_s, because 2 patients are out and have NA values
  imputed_s = filter(imputed_s, !is.na(mk)) #%>% select(-Vitamine_lastKnown)
  
  #build a regression model with the top 30 features
  
  #f = "trend_Natrium_min"
  plot_list= list() 
  
  Features_MK_NMK_all_nona = filter(Features_MK_NMK_all, !is.na(mk))

  res_glmnet = rep(NA, nrow(imputed_s))
  res_randFor = rep(NA, nrow(imputed_s))
  
  for(i in 1:nrow(imputed_s)){
    print(i)
    #make matrix with dummy coded factors
    mya_model_imputed_mat = model.matrix(object = as.formula(paste("mk ~ -1+", paste(filter(result_final_s, !((grepl("Mestinon", term)) | 
                                                                                                             grepl("Vitamine_lastKnown", term)))$term, collapse = "+"))), data = imputed_s)
                                                                                                             #grepl(":", term) | grepl("MNEURO", term)
    
    mya_rF_imputed_mat = model.matrix(object = as.formula(paste("mk ~ -1+", paste(filter(result_final_s, !((grepl("Mestinon", term)) | 
                                                                                                         grepl("Vitamine_lastKnown", term) |
                                                                                                         grepl(":", term)))$term, collapse = "+"))), data = imputed_s)
    
                                                                                                             
    mya_model_LOO = cv.glmnet(x = mya_model_imputed_mat[-i,], y = imputed_s$mk[-i], family = "binomial")
    mya_model_randFor = randomForest(y = as.factor(imputed_s$mk[-i]), x = mya_rF_imputed_mat[-i,], ntree = 100, mtry = 6, sampsize = c(10,10), 
                                     importance = TRUE)
   
    
    
    #coef(mya_model_LOO, s = "lambda.min")
    plot(mya_model_LOO)
    
    features = tidy(coef(mya_model_LOO, s = "lambda.min")) %>% filter(row != "(Intercept)") %>% mutate(run_id = paste0(i,"_", j))
    features_all_runs[[length(features_all_runs) + 1]] = features
    
  
    print(features)
    
    imp = importance(mya_model_randFor, type = 1)
    imp_df = data_frame(value = as.vector(imp), row = rownames(imp))  
    importance_all_rF_runs[[length(importance_all_rF_runs) + 1]] = imp_df %>% filter(row != "(Intercept)") %>% mutate(run_id = paste0(i,"_", j)) 
    varImpPlot(mya_model_randFor, type = 1)
    
    #apply the top 30 features and see how good the fit is: 
    res_glmnet[i] = predict(mya_model_LOO, newx = as(mya_model_imputed_mat, "dgCMatrix"), type = "response", s = "lambda.min")[i]
    res_randFor[i] <- predict(mya_model_randFor, mya_rF_imputed_mat[i,], type = "prob")[2]
    gc()
    Sys.sleep(0.2)
  }
  #results by run
  results_distribution[[j]] = res_glmnet
  results_distribution_rF[[j]] = res_randFor
  
 
  result_glmnet= data_frame(predicted = res_glmnet, mk = imputed_s$mk)
  result_rf= data_frame(predicted = res_randFor, mk = imputed_s$mk)
  
  all_models = list(lasso = result_glmnet, Rforest = result_rf)
  
  
  for(m in names(all_models)){
    result = all_models[[m]]
    measured = group_by(result, predicted = predicted > 0.3, mk) %>% summarize(cnt =n()) %>% ungroup() %>% mutate(cnt_p = cnt/sum(cnt))
    pred <- prediction(result$predicted, result$mk)
    pred
    perf <- ROCR::performance(pred,"tpr","fpr")
    perf
    auc_ROCR <- ROCR::performance(pred, measure = "auc")
    auc_ROCR <- auc_ROCR@y.values[[1]]
    
    #sensitivity = recall = True Positive Rate = TP/P
    filter = dplyr::filter
    recall = sum(filter(measured, predicted, mk)$cnt)/ sum(filter(measured, mk)$cnt)
    #False Positive Rate = FP/(FP+TN)
    fallout = sum(filter(measured, predicted, !mk)$cnt)/sum(filter(measured, !mk)$cnt)
    #specificity = TN/all negatives
    specificity = sum(filter(measured, !predicted, !mk)$cnt)/ sum(filter(measured, !mk)$cnt)
    #precision = TP/PosPred = predictive power --> TP/(TP + FP)
    precision = sum(filter(measured, predicted, mk)$cnt)/ sum(filter(measured, predicted)$cnt)
    #f-measure = (2*precision * recall)/(precision + recall) 1 is good, 0 is bad
    fmeasure = (2*precision*recall)/(precision + recall)
    #cer Type I error / Type II error = 1
    
    all_metrics[[paste0(m,j)]] = data.frame(fmeasure, specificity, precision, fallout, recall, auc_ROCR, model = m)
    gc()
    
  }
}

#all features with their values by run
features_all_runs = bind_rows(features_all_runs)
#features with number of used, avg value und sd 
features_all_runs_g = group_by(features_all_runs, row) %>% summarize(cnt_occ = n(), avg_value = mean(value), sd_value = sd(value))

#comparing specificity, sensitivity etc. 
all_metrics = bind_rows(all_metrics)
results_dist = map_df(results_distribution, ~imputed_s %>% mutate(predicted = .x, Patient_ID = Features_MK_NMK_all$Patient_ID))
results_dist_rF = map_df(results_distribution_rF, ~imputed_s %>% mutate(predicted = .x, Patient_ID = Features_MK_NMK_all$Patient_ID))
#results by patient (truth, avg pred value, sd)
results_dist_g = group_by(results_dist, Patient_ID, mk) %>% summarize(avg_predicted_value = mean(predicted), sd_predicted_value = sd(predicted))
results_dist_g_rF = group_by(results_dist_rF, Patient_ID, mk) %>% summarize(avg_predicted_value = mean(predicted), sd_predicted_value = sd(predicted))
imputed_runs = bind_rows(imputed_runs)
importance_all_rF_runs = bind_rows(importance_all_rF_runs)

#sanity check
if(FALSE){
  ggplot(data = results_dist_g, aes(x = avg_predicted_value, y = sd_predicted_value)) + facet_wrap(~mk) + geom_point()
  arrange(results_dist_g, desc(sd_predicted_value)) %>% head()
}

features_commonly_used = features_all_runs_g %>% separate_rows(row, sep = ":") %>% select(row)
inner_circle = c(features_commonly_used$row, names(imputed_s), names(Features_MK_NMK_all)) %>% setdiff(c("mk", "Patient_ID")) %>% unique()
for(f in inner_circle){
  print(f)
  f_clean = gsub("_min|_max|_mean|_median|_stdev|_LastKnown", "", f)
  print(f_clean)
  
  f_clean_helper = filter(distinct(referenz_werte, `Key Clean` ,`Erwachsene Frauen Untergrenze`, `Erwachsene Frauen Obergrenze`, 
           `Erwachsene M?nner Untergrenze`, `Erwachsene M?nner Obergrenze`), `Key Clean` == f_clean)
  
  plot_data= tibble(f = as_tibble(select(Features_MK_NMK_all_nona, -mk3_date))[[f]], mk = Features_MK_NMK_all_nona$mk, f_clean) 
  if(length(table(plot_data$f)) <= 1 || nrow(plot_data %>% filter(!is.na(f)) %>% group_by(f = quantile_cut(f)) %>% summarize(mk_p = sum(mk)/n())) == 1){next()}
  
  if(nrow(f_clean_helper) == 0){
    plot_list[[f]] =  ggplot(data = plot_data %>% as_tibble() %>% 
                               group_by(fq = quantile_cut(f, n = 4), f_clean) %>% summarize(mk_p = sum(mk)/n()), aes(x = fq, y = mk_p)) + 
      geom_col(position = "stack", alpha = if_else(f %in% features_commonly_used$row, 1, 
                                                   if_else(f %in% names(imputed_s), 0.5, 0.2))) + 
      theme_minimal() + ggtitle(f) + xlab("quantiles") + ylab("Probability of crisis")
  }else{
    plot_list[[f]] =  ggplot(data = plot_data %>% as_tibble() %>% inner_join(f_clean_helper, by = c("f_clean" = "Key Clean")) %>%
                             group_by(fq = quantile_cut(f, n = 4), f_clean) %>% summarize(mk_p = sum(mk)/n(), 
                              in_range_p = sum(f >= as.numeric(`Erwachsene Frauen Untergrenze`) & f <= as.numeric(`Erwachsene M?nner Obergrenze`))/n()), aes(x = fq, y = mk_p, fill = in_range_p)) + 
      geom_col(position = "stack", alpha = if_else(f %in% features_commonly_used$row, 1, 
                                                  if_else(f %in% names(imputed_s), 0.5, 0.2))) + 
      scale_fill_continuous(name = "% of quantile in\nnormal range") + theme_minimal() + ggtitle(f) + xlab("quantiles") + ylab("Probability of crisis")
    }
}


#metric plots
specificity_plot = ggplot(data = all_metrics, aes(x = specificity))+ geom_histogram() + facet_wrap(~model) + theme_minimal()



purrr::map_df(c(0.1, 0.3, 0.9), ~ data_frame(threshold = .,
                                             specificity = nrow(filter(results_dist, predicted <= ., !mk))/ nrow(filter(results_dist, !mk)),
                                             sensitivity = nrow(filter(results_dist, predicted > ., mk))/ nrow(filter(results_dist, mk)), 
                                             cnt_high_risk = nrow(filter(results_dist, predicted >= .))/100
                                            
                                             )
)
             
purrr::map_df(c(0.1, 0.3, 0.9), ~ data_frame(threshold = .,
                                             specificity = nrow(filter(results_dist_rF, predicted <= ., !mk))/ nrow(filter(results_dist_rF, !mk)),
                                             sensitivity = nrow(filter(results_dist_rF, predicted > ., mk))/ nrow(filter(results_dist_rF, mk)), 
                                             cnt_high_risk = nrow(filter(results_dist_rF, predicted >= .))/100
                                             )
)

#paper mentioned metrics: 
group_by(all_metrics, model) %>% summarize(cnt = n(), avg_AUC= mean(auc_ROCR), sd_AUC = sd(auc_ROCR))


report_items = list(ReportName = "how features relate to MK", plot_list = plot_list[1:20])

save.image(paste0(input_path, "/Classifying_Patients_V", V, "/before_report_", sql_dump_date, ".RData"))

render(paste0(base_path, "/mya_models/Feature_Group_Relation.RMD"), params = list(report_items = report_items),
       output_file = paste9(base_path, "/results/Feature_Group_Relation.html"))
       #output_file = "C:/Users/bershans/Documents/Feature_Group_Relation.html")
browseURL(paste0(base_path, "/results/Feature_Group_Relation.html"))
#browseURL("C:/Users/bershans/Documents/Feature_Group_Relation.html")


anonymize_ids = . %>% group_by(model) %>% arrange(Patient_ID) %>% mutate(Patient_ID_anonymous = paste0("Patient #", row_number(), " ", mk, ", " ,model))

results_dist_g_both = bind_rows(results_dist_g %>% mutate(model ="glm"), results_dist_g_rF %>% mutate(model = "rF"))
  
ggplot(data = results_dist_g_both %>% anonymize_ids %>%
         mutate(Patient_ID_anonymous = fct_reorder(as.factor(gsub( "Patient ", "", Patient_ID_anonymous)), -avg_predicted_value)), 
       aes(x = Patient_ID_anonymous, y = avg_predicted_value)) + geom_col() + theme_minimal(base_size = 15) + 
  geom_errorbar(aes(ymin = avg_predicted_value - sd_predicted_value, ymax = avg_predicted_value + sd_predicted_value), width = 0.2, size = 1,
                color = "orange") + theme(axis.text.x = element_text(angle = 90)) + 
  xlab("") + ggtitle("Mean predicted value by patient", subtitle = "A: Lasso regression, B: Random forest") + 
  scale_fill_continuous(name = "predicted value", label = percent) + scale_y_continuous(name = "mean predicted value by patient", label = percent, limits = c(-0.1,1.1))# + 

ggsave(paste0(base_path, "/Paper_1_MC_Classification/plots/Mean_predicted_value_100runs.svg"), width = 50, height = 20, unit = "cm")

 
 ggplot(data = results_dist_g_rF %>% mutate( model = "rF") %>%anonymize_ids %>% 
          mutate(Patient_ID_anonymous = fct_reorder(as.factor(Patient_ID_anonymous), -avg_predicted_value)), 
        aes(x = Patient_ID_anonymous, y = avg_predicted_value)) + geom_col() + theme_minimal() + 
   geom_errorbar(aes(ymin = avg_predicted_value - sd_predicted_value, ymax = avg_predicted_value + sd_predicted_value), width = 0.2, size = 1,
                 color = "orange") + theme(axis.text.x = element_text(face = "bold", angle = 90)) + 
   xlab("") + ggtitle("Mean predicted value by patient in 100 runs\n(sd in orange)") + 
   scale_fill_continuous(name = "predicted value", label = percent) + scale_y_continuous(name = "mean predicted value by patient", label = percent, limits = c(-0.1,1.1))# + 
 ggsave(paste0(base_path, "/results/Mean_predicted_value_100runs_rF.png"))
 
 
 colors = rep(c("#89CFF0", "#7393B3", "#088F8F", "#5F9EA0","#6495ED", "#6F8FAF", "#6082B6", "#A7C7E7", "#96DED1", "#87CEEB", 
                "#116530", "#21B6A8", "#A3EBB1","#18A558"),4)
 #making a combined plot: 
 ggplot(data = bind_rows(results_dist_g %>% mutate(model = "glm"), results_dist_g_rF %>% mutate(model = "rF")) %>% 
          anonymize_ids %>% mutate(Patient_ID_anonymous = fct_reorder(as.factor(Patient_ID_anonymous), -avg_predicted_value)), 
        aes(x = Patient_ID_anonymous, y = avg_predicted_value, fill = Patient_ID)) + geom_col() + theme_minimal() + 
   facet_wrap(~model, scales = "free") + 
   geom_errorbar(aes(ymin = avg_predicted_value - sd_predicted_value, ymax = avg_predicted_value + sd_predicted_value), width = 0.2, size = 1,
                 color = "orange") + theme(axis.text.x = element_text(face = "bold", angle = 90)) + 
   xlab(NULL) + ggtitle("Mean predicted value by patient righ") + 
   scale_fill_manual(guide = F, values = colors) + 
   scale_y_continuous(name = "mean predicted value by patient", label = percent, limits = c(-0.1,1.1))# + 
 
#install.packages("ggallin")
library(ggallin)
 
fi_lasso = ggplot(data = features_all_runs_g %>% mutate(row = fct_reorder(as.factor(row), -avg_value)) %>%
          filter(cnt_occ > 3), aes(x = avg_value, y = row, alpha = cnt_occ)) +
   geom_col() +
  theme_minimal(base_size = 14) + 
  theme(axis.text.x = element_text(face = "bold")) + scale_alpha_continuous(name = "Occurrences")+ 
  xlab("average coeffcient") + ylab("") + #geom_text(aes(x = 150, y = row, label = cnt_occ), color = "orange", fontface = "bold") +
   scale_x_continuous(trans = pseudolog10_trans, breaks = c(-10, -1, 1, 10, 100)) + 
   ggtitle("Panel A: Feature Importance\nLasso regression", subtitle = "occurence by alpha")
 
#ggsave(paste0("S:/AG/AG-PM/DB07/results/average_coeffcient.png"))



fi_rF = ggplot(data = importance_all_rF_runs %>% group_by(row) %>% summarize(cnt_occ = n(), avg_value = mean(value), sd_value = sd(value)) %>% 
         mutate(row = fct_reorder(as.factor(row), -avg_value)), aes(x = avg_value, y = row)) +
  geom_col() +
  theme_minimal(base_size = 14) + 
  theme(axis.text.x = element_text(face = "bold")) + 
  xlab("average coeffcient") + ylab("") +
 ggtitle("Panel B: Feature Importance\nrandom Forest", subtitle ="similar occurence by feature")

fi_lasso_rF = fi_lasso + fi_rF
ggsave(paste0(base_path, "/results/fi_both.svg"), width = 50, height = 20, unit = "cm")
ggsave(paste0(base_path, "/Paper_1_MC_Classification/plots/fi_both.png"), width = 50, height = 20, unit = "cm", dpi = 1200)



#scrambling DV: 
if(FALSE){
  imputed_scrambled = imputed_s
  
  
  res_glmnet_s = rep(NA, nrow(imputed_scrambled))
  features_all_runs_s = list()
  all_metrics_s = list() 
  
  results_distribution_s = list()
  for(j in 1:100){
    imputed_scrambled$mk = imputed_scrambled$mk[sample(1:length(imputed_scrambled$mk))]
    print(paste0("j = ", j))
    for(i in 1:nrow(imputed_scrambled)){
      print(i)
      #make matrix with dummy coded factors
      mya_model_imputed_mats = model.matrix(object = as.formula(paste("mk ~ ", paste(filter(result_final_s, !((grepl("Mestinon", term)) | 
                                                                                                                grepl("Vitamine_lastKnown", term) ))$term, 
                                                                                     collapse = "+"))), data = imputed_scrambled)
                                                                                                                #grepl(":", term) |  #grepl("MNEURO", term)
                                                                                                               
      mya_model_LOO_s = cv.glmnet(x = mya_model_imputed_mats[-i,], y = imputed_scrambled$mk[-i], family = "binomial")
      
      #coef(mya_model_LOO, s = "lambda.min")
      plot(mya_model_LOO_s)
      
      features_s = tidy(coef(mya_model_LOO_s, s = "lambda.min")) %>% filter(row != "(Intercept)") %>% mutate(run_id = i)
      features_all_runs_s[[i]] = features_s
      print(features_s)
      #print(as.formula(paste("mk ~ ", paste(features$row, collapse = "+"))))
      #mya_model_V5 = glm(formula = as.formula(paste("mk ~ ", paste(features$row, collapse = "+"))), data = imputed_s[-i,], family = "binomial", maxit = 100)
      
      
      res_glmnet_s[i] = predict(mya_model_LOO_s, newx = as(mya_model_imputed_mats, "dgCMatrix"), type = "response", s = "lambda.min")[i]
      gc()
      Sys.sleep(0.2)
    }
    results_distribution_s[[j]] = res_glmnet_s
    
    result_glmnet_s= data_frame(predicted = res_glmnet_s, mk = imputed_scrambled$mk)
    measured_glmnet_s = group_by(result_glmnet_s, predicted = predicted > 0.3, mk) %>% summarize(cnt =n()) %>% ungroup() %>% mutate(cnt_p = cnt/sum(cnt))
    
    spred <- prediction(result_glmnet_s$predicted, result_glmnet_s$mk)
    spred
    sperf <- ROCR::performance(spred,"tpr","fpr")
    sperf
    plot(sperf, colorize = T)
    sauc_ROCR <- ROCR::performance(spred, measure = "auc")
    sauc_ROCR <- sauc_ROCR@y.values[[1]]
    sperf_pr <- ROCR::performance(spred, "prec", "rec")
    plot(sperf_pr)
    
    #sensitivity = recall = True Positive Rate = TP/P
    srecall = sum(filter(measured_glmnet_s, predicted, mk)$cnt)/ sum(filter(measured_glmnet_s, mk)$cnt)
    #False Positive Rate = FP/(FP+TN)
    sfallout = sum(filter(measured_glmnet_s, predicted, !mk)$cnt)/sum(filter(measured_glmnet_s, !mk)$cnt)
    #specificity = TN/all negatives
    sspecificity = sum(filter(measured_glmnet_s, !predicted, !mk)$cnt)/ sum(filter(measured_glmnet_s, !mk)$cnt)
    #precision = TP/posPred = predictive power
    sprecision = sum(filter(measured_glmnet_s, predicted, mk)$cnt)/ sum(filter(measured_glmnet_s, predicted)$cnt)
    #f-measure = (2*precision * recall)/(precision + recall) 1 is good, 0 is bad
    sfmeasure = (2*sprecision*srecall)/(sprecision + srecall)
    
    all_metrics_s[[j]] = data.frame(sfmeasure, sspecificity, sprecision, sfallout, srecall, sauc_ROCR)
  }
  
  all_metrics_s = bind_rows(all_metrics_s)
  hist(as.numeric(all_metrics_s$sspecificity))
  ggsave(paste0(base_path, "/results/specificity_s.png"))
  hist(as.numeric(all_metrics_s$sprecision))
  ggsave(paste0(base_path, "/results/precision_s.png"))
  hist(all_metrics_s$srecall)
  ggsave(paste0(base_path, "/results/recall_s.png"))
  hist(all_metrics_s$sauc_ROCR)
  ggsave(paste0(base_path, "/results/auc_s.png"))
  
  features_all_runs_s = bind_rows(features_all_runs_s)
  features_all_runs_s_g = group_by(features_all_runs_s, row) %>% summarize(cnt_occ = n(), avg_value = mean(value), sd_value = sd(value))
  
  
  results_dist_s = map_df(results_distribution_s, ~imputed_scrambled %>% mutate(predicted = .x, Patient_ID = Features_MK_NMK_all$Patient_ID))
  results_dist_s_g = group_by(results_dist_s, Patient_ID, mk) %>% summarize(avg_predicted_value = mean(predicted, na.rm = T), sd_predicted_value = sd(predicted, na.rm = T))
  
 
  ggplot(data = results_dist_s_g %>% ungroup() %>% mutate(Patient_ID_anonymous = paste0("Patient #", row_number(), " ", mk)) %>% 
           mutate(Patient_ID_anonymous = fct_reorder(as.factor(Patient_ID_anonymous), -avg_predicted_value)), 
         aes(x = Patient_ID_anonymous, y = avg_predicted_value)) + geom_col() + theme_minimal() + 
    geom_errorbar(aes(ymin = avg_predicted_value - sd_predicted_value, ymax = avg_predicted_value + sd_predicted_value), width = 0.2, size = 1,
                  color = "orange") + theme(axis.text.x = element_text(face = "bold", angle = 90)) + 
    xlab("") + ggtitle("Mean predicted value by patient in 100 runs\n(sd in orange)") + 
    scale_fill_continuous(name = "predicted value", label = percent) + scale_y_continuous(name = "mean predicted value by patient", label = percent, limits = c(-0.1,1.1)) #+ 
    #geom_hline(aes(yintercept = 0.06), size = 1, color = "red") + 
    #geom_hline(aes(yintercept = 0.25), size = 1, color = "orange") + 
    #geom_hline(aes(yintercept = 0.35), size = 1, color = "yellow") 
  
  ggsave(paste0(base_path, "/results/Mean_predicted_value_100_runs_s.png"))
  
  ggplot(data = features_all_runs_s_g %>% mutate(row = fct_reorder(row, -avg_value)) %>% filter(cnt_occ > 3), aes(x = row, y = avg_value)) + geom_col() + theme(axis.text.x = element_text(face = "bold", angle = 90)) + 
    xlab("") + ylab("average coeffcient") + geom_text(aes(x = row, y = avg_value/2, label = cnt_occ), color = "white")
  
  ggsave(paste0(base_path, "/results/used_features_s.png"))
}


confusion <- function(prediction, actual, sel_percent = 0.05) {
  prediction = prediction + runif(length(prediction))* 1e-5
  t <- quantile(prediction, 1 - sel_percent)
  list(
    confusion_matrix = table(
      predicted = prediction >= t,
      actual = actual
    ),
    threshold = t,
    recall = sum(actual[prediction >= t], na.rm = T) / sum(actual, na.rm = T), #sum of those that are in range and are also predicted in range divided by those that are in range
    precision = sum(actual[prediction >= t], na.rm = T) / sum(prediction >= t, na.rm = T), #sum of those that are in range and are predicted in range divided by those that are predicted in range
    lift = sum(actual[prediction >= t], na.rm = T) / sum(actual, na.rm = T) / sel_percent, #recall divided by selection percent
    specificity = sum(!actual[prediction < t], na.rm = T) / sum(!actual, na.rm = T) #specificity = TN/all negatives
    #percent_acc_loc = sum(actual[prediction > t], na.rm = T) / sum(prediction > t, na.rm = T) #same as precision
  )
}

sweep_metrics <- function(prediction, actual, min_sel = 0.05, max_sel = 1, steps = 200, required_selections = c()) {
  selections <- unique(c(
    seq(min_sel, max_sel, length.out = steps),
    required_selections
  ))
  purrr::map_df(selections, function(x) {
    res <- confusion(prediction, actual, x)
    data_frame(bid_on = x, Recall = res$recall, Precision = res$precision, Lift = res$lift, Specificity = res$specificity, threshold = res$threshold)
  })
}
plot_metrics_sweep <- function(predictions, actuals, min_sel = 0.05, max_sel = 1, steps = 50, mark = c(0.05, 0.3, 0.5, 0.7, 0.95), metrics_shown = c("Precision", "Recall", "Specificity", "threshold"), source_order=NULL) {
  all_metrics <- map_df(
    names(predictions),
    ~ sweep_metrics(predictions[[.x]], actuals[[.x]], min_sel = min_sel, max_sel = max_sel, required_selections = mark) %>% mutate(source = .x)
  )
  
  all_metrics %<>% gather(metric, value, -bid_on, -source) %>% filter(metric %in% metrics_shown) %>% 
    mutate(source = factor(source, levels =source_order))
  #if(FALSE){
  ggplot(all_metrics, aes(x = bid_on, color = source, y = value)) + geom_line() +
    facet_wrap(~metric, ncol = 1) + scale_x_continuous(name = "predicted with crisis", label = percent, breaks = mark) +
    scale_y_continuous(name = NULL, label = percent) + geom_vline(data = data_frame(x = mark), aes(xintercept = x), linetype = "dashed") +
    geom_label_repel(data = filter(all_metrics, bid_on %in% mark), aes(x = bid_on, color = source, y = value, label = paste0(round(value,2)*100, "%"))) +
    geom_point(data = filter(all_metrics, bid_on %in% mark), aes(x = bid_on, color = source, y = value), size = 2) +
    scale_color_colorblind() + theme_minimal() + 
    theme(legend.position = "bottom")
  #}
}
if(FALSE) {
  #try out a ci for AUC
  mlr_data = results_dist_forplot[[1]] %>% mutate(mk = as_factor(mk), prob.TRUE = predicted, prob.FALSE = 1-predicted)
  mlr_desc = makeClassifTaskDesc("x", mlr_data,
                                 "mk", 1, NULL, "TRUE", NULL)
  mlr_pred = makePrediction(mlr_desc,
                            1:nrow((mlr_data)),mlr_data$Patient_ID, mlr_data$mk, 
                            predict.type = "prob", predict.threshold = 0.5, y = select(mlr_data, `TRUE`=prob.TRUE, `FALSE`=prob.FALSE), time = 0)
  
  mlr_obj <- generateThreshVsPerfData(mlr_pred, list(fpr, tpr))
  mlr_roc = roc(mlr_pred$data$truth, mlr_pred$data$prob.TRUE, ci=TRUE, plot=FALSE)
  ciobj <- ci.se(mlr_roc, specificities=seq(0, 1, l=25))
  dat.ci <- data.frame(x = as.numeric(rownames(ciobj)),
                       lower = ciobj[, 1],
                       upper = ciobj[, 3])
  
  ggroc(mlr_roc) +
    theme_minimal() +
    geom_abline(
      slope = 1,
      intercept = 1,
      linetype = "dashed",
      alpha = 0.7,
      color = "grey"
    ) + coord_equal() +
    geom_ribbon(
      data = dat.ci,
      aes(x = x, ymin = lower, ymax = upper),
      fill = "steelblue",
      alpha = 0.2
    ) + ggtitle(capture.output(mlr_roc$ci))

}


results_dist_forplot = map(results_distribution, ~imputed_s %>% mutate(predicted = .x, Patient_ID = Features_MK_NMK_all$Patient_ID) %>% 
  select(Patient_ID, mk, predicted))

results_dist_forplot_s = map(results_distribution_s, ~imputed_s %>% mutate(predicted = .x, Patient_ID = Features_MK_NMK_all$Patient_ID) %>% 
                             select(Patient_ID, mk, predicted))

plot_auc = function(.x){
  plot_pred <- prediction(.x$predicted, .x$mk)
  #plot_perf <- ROCR::performance(plot_pred,"tpr","fpr")
  plot_auc_ROCR <- ROCR::performance(plot_pred, measure = "auc")
  data_frame(AUC =  plot_auc_ROCR@y.values[[1]])
}
results_distAUC_forplot = map_df(results_dist_forplot, plot_auc) %>% mutate(run = row_number()) %>% arrange(AUC) %>% 
  filter(row_number() %in% c(5, 50, 95))

results_distAUC_forplot_s = map_df(results_dist_forplot_s, plot_auc) %>% mutate(run = row_number()) %>% arrange(AUC) %>% 
  filter(row_number() %in% c(5, 50, 95))

rocr_pred_all = prediction(map(results_dist_forplot, ~ pull(., predicted)),
                           map(results_dist_forplot, ~ pull(., mk))) 

rocr_pred_all_s = prediction(map(results_dist_forplot_s, ~ pull(., predicted)),
                           map(results_dist_forplot_s, ~ pull(., mk))) 


rocr_perf_all = ROCR::performance(rocr_pred_all,measure="tpr", x.measure="fpr")
plot(rocr_perf_all, avg="vertical", spread.estimate = "stddev", spread.scale = 2)

rocr_perf_all_s = ROCR::performance(rocr_pred_all_s,measure="tpr", x.measure="fpr")
plot(rocr_perf_all_s, avg="vertical", spread.estimate = "stddev", spread.scale = 2)

## remove samples with x or y not finite
for (i in 1:length(rocr_perf_all@x.values)) {
  ind.bool <- (is.finite(rocr_perf_all@x.values[[i]]) &
                 is.finite(rocr_perf_all@y.values[[i]]))
  
  if (length(rocr_perf_all@alpha.values)>0)
    rocr_perf_all@alpha.values[[i]] <- rocr_perf_all@alpha.values[[i]][ind.bool]
  
  rocr_perf_all@x.values[[i]] <- rocr_perf_all@x.values[[i]][ind.bool]
  rocr_perf_all@y.values[[i]] <- rocr_perf_all@y.values[[i]][ind.bool]
}


## remove samples with x or y not finite
for (i in 1:length(rocr_perf_all_s@x.values)) {
  ind.bool_s <- (is.finite(rocr_perf_all_s@x.values[[i]]) &
                 is.finite(rocr_perf_all_s@y.values[[i]]))
  
  if (length(rocr_perf_all_s@alpha.values)>0)
    rocr_perf_all_s@alpha.values[[i]] <- rocr_perf_all_s@alpha.values[[i]][ind.bool_s]
  
  rocr_perf_all_s@x.values[[i]] <- rocr_perf_all_s@x.values[[i]][ind.bool_s]
  rocr_perf_all_s@y.values[[i]] <- rocr_perf_all_s@y.values[[i]][ind.bool_s]
}

perf.avg <- rocr_perf_all
perf.avg_s = rocr_perf_all_s

x.values <- seq(min(unlist(rocr_perf_all@x.values)), max(unlist(rocr_perf_all@x.values)),
                length=max( sapply(rocr_perf_all@x.values, length)))

x.values_s <- seq(min(unlist(rocr_perf_all_s@x.values)), max(unlist(rocr_perf_all_s@x.values)),
                length=max( sapply(rocr_perf_all_s@x.values, length)))



for (i in 1:length(rocr_perf_all@y.values)) {
  print(i)
  perf.avg@y.values[[i]] <-
    stats::approxfun(rocr_perf_all@x.values[[i]], rocr_perf_all@y.values[[i]],
                     ties=mean, rule=2)(x.values)
  perf.avg@alpha.values[[i]] <-
    stats::approxfun(rocr_perf_all@x.values[[i]], rocr_perf_all@alpha.values[[i]],
                     ties=mean, rule=2)(x.values)
}
perf.avg@x.values <- list(x.values)

perf.avg@y.values <- list(rowMeans( data.frame( perf.avg@y.values )))

perf.avg@alpha.values <- list(rowMeans( data.frame( perf.avg@alpha.values )))


for (i in 1:length(rocr_perf_all_s@y.values)) {
  print(i)
  perf.avg_s@y.values[[i]] <-
    stats::approxfun(rocr_perf_all_s@x.values[[i]], rocr_perf_all_s@y.values[[i]],
                     ties=mean, rule=2)(x.values)
  perf.avg_s@alpha.values[[i]] <-
    stats::approxfun(rocr_perf_all_s@x.values[[i]], rocr_perf_all_s@alpha.values[[i]],
                     ties=mean, rule=2)(x.values)
}
perf.avg_s@x.values <- list(x.values)

perf.avg_s@y.values <- list(rowMeans( data.frame( perf.avg_s@y.values )))

perf.avg_s@alpha.values <- list(rowMeans( data.frame( perf.avg_s@alpha.values )))





## y.values at show.spread.at (midpoint of error bars )
show.spread.at= (seq(min(unlist(rocr_perf_all@x.values)),
                     max(unlist(rocr_perf_all@x.values)),
                     length=40))
show.spread.at.y.values <-
  lapply(as.list(1:length(rocr_perf_all@x.values)),
         function(i) {
           stats::approxfun(rocr_perf_all@x.values[[i]], rocr_perf_all@y.values[[i]],
                            rule=2,
                            ties=mean)( show.spread.at)
         })

show.spread.at.y.values <- as.matrix(data.frame(show.spread.at.y.values ))
colnames(show.spread.at.y.values) <- c()
bar.width.y <- apply(show.spread.at.y.values, 1, stats::sd)



show.spread.at_s= (seq(min(unlist(rocr_perf_all_s@x.values)),
                     max(unlist(rocr_perf_all_s@x.values)),
                     length=40))
show.spread.at.y.values_s <-
  lapply(as.list(1:length(rocr_perf_all_s@x.values)),
         function(i) {
           stats::approxfun(rocr_perf_all_s@x.values[[i]], rocr_perf_all_s@y.values[[i]],
                            rule=2,
                            ties=mean)( show.spread.at)
         })

show.spread.at.y.values_s <- as.matrix(data.frame(show.spread.at.y.values_s))
colnames(show.spread.at.y.values_s) <- c()
bar.width.y_s <- apply(show.spread.at.y.values_s, 1, stats::sd)


show.spread.at.alpha.values <-
  lapply(as.list(1:length(rocr_perf_all@x.values)),
         function(i) {
           stats::approxfun(rocr_perf_all@x.values[[i]], rocr_perf_all@alpha.values[[i]],
                            rule=2,
                            ties=mean)( show.spread.at)
         })

show.spread.at.alpha.values <- as.matrix(data.frame(show.spread.at.alpha.values ))
colnames(show.spread.at.alpha.values) <- c()
## now, show.spread.at.y.values[i,] contains the curve y values at the
## sampling x value .garg(arglist,'show.spread.at')[i]
alpha95l <- apply(show.spread.at.alpha.values, 1, function(.) quantile(., 0.025, na.rm = T))
alpha95u <- apply(show.spread.at.alpha.values, 1, function(.) quantile(., 0.975, na.rm = T))


show.spread.at.alpha.values_s <-
  lapply(as.list(1:length(rocr_perf_all_s@x.values)),
         function(i) {
           stats::approxfun(rocr_perf_all_s@x.values[[i]], rocr_perf_all_s@alpha.values[[i]],
                            rule=2,
                            ties=mean)( show.spread.at)
         })

show.spread.at.alpha.values_s <- as.matrix(data.frame(show.spread.at.alpha.values_s))
colnames(show.spread.at.alpha.values_s) <- c()
## now, show.spread.at.y.values[i,] contains the curve y values at the
## sampling x value .garg(arglist,'show.spread.at')[i]
alpha95l_s <- apply(show.spread.at.alpha.values_s, 1, function(.) quantile(., 0.025, na.rm = T))
alpha95u_s <- apply(show.spread.at.alpha.values_s, 1, function(.) quantile(., 0.975, na.rm = T))


line_roc_data = data_frame(fpr = unlist(perf.avg@x.values)[], tpr = unlist(perf.avg@y.values))
line_roc_data_s = data_frame(fpr = unlist(perf.avg_s@x.values)[], tpr = unlist(perf.avg_s@y.values))
line_roc_data_random = data_frame(fpr = unlist(perf.avg_s@x.values)[], tpr = unlist(perf.avg_s@y.values))

ribbon_sd1_data = data_frame(tpr = rowMeans(show.spread.at.y.values), fpr= show.spread.at,
                             tprl1 = tpr -     bar.width.y, tpru1 = tpr +    bar.width.y,
                             tprl2 = tpr - 2 * bar.width.y, tpru2 = tpr - 2* bar.width.y)

ribbon_sd2_data = data_frame(tpr = rowMeans(show.spread.at.y.values), fpr= show.spread.at,
                             tprl1 = tpr -     bar.width.y, tpru1 = tpr +    bar.width.y,
                             tprl2 = tpr - 2 * bar.width.y, tpru2 = tpr + 2* bar.width.y)

label_cutoff_data = data_frame(tpr = rowMeans(show.spread.at.y.values), fpr = show.spread.at,
                               cutoff = round(rowMeans(show.spread.at.alpha.values) * 20) / 20,
                               label = paste0("TH:", cutoff, "\nCI:[", round(alpha95l, 2), ",", round(alpha95u, 2), "]")) %>%
  filter(cutoff != 0, !is.na(cutoff) & !is.infinite(cutoff)) %>%
  distinct(cutoff, .keep_all = T)

roc_lasso = ggplot() +
  geom_line(data = line_roc_data, aes(x=fpr, y=tpr), size = 1) +
  geom_line(data = line_roc_data_s, aes(x=fpr, y=tpr), color = "purple", size = 1) +
  geom_ribbon(data = ribbon_sd1_data, aes(x=fpr, y=tpr,ymin=tprl1, ymax=tpru1), alpha= 0.4, fill="blue")+
  geom_ribbon(data = ribbon_sd2_data, aes(x=fpr, y=tpr,ymin=tprl2, ymax=tpru2), alpha= 0.2, fill="blue") +
  geom_label_repel(data = label_cutoff_data, aes(x=fpr, y=tpr, label = label), alpha= 0.8, size = 3.5, min.segment.length = 1) +
  geom_label(aes(x=0.6, y=0.45, label = "randomized target model"), color = "purple", alpha= 0.8, min.segment.length = 1) +
  geom_point(data = label_cutoff_data, aes(x=fpr, y=tpr)) +
  ggtitle("Panel A: Receiver-Operator-Curve lasso regression") + 
  theme_minimal(base_size = 15) +
  scale_x_continuous("False Positive Rate", label = percent) +
  scale_y_continuous("Average True Positive Rate", label = percent) +
  geom_segment(x=0,y=0,xend=1,yend=1, linetype=2) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = F)

#ggsave(paste0("S:/AG/AG-PM/DB07/results/ROC_curve.png"))

plot_metrics_sweep(results_dist_forplot[results_distAUC_forplot$run] %>% map("predicted") %>% set_names(c("5th percentile", "50th percentile", "95th percentile")), 
                   results_dist_forplot[results_distAUC_forplot$run] %>% map("mk")%>% set_names(c("5th percentile", "50th percentile", "95th percentile")),
                   source_order = c("5th percentile", "50th percentile", "95th percentile"))

ggsave(paste0(base_path, "/results/Prec_Rec_Spec_thresh.png"))



#same plot for rF
results_dist_forplot_rF = map(results_distribution_rF, ~imputed_s %>% mutate(predicted = .x, Patient_ID = Features_MK_NMK_all$Patient_ID) %>% 
                             select(Patient_ID, mk, predicted))

results_distAUC_forplot_rF = map_df(results_dist_forplot_rF, plot_auc) %>% mutate(run = row_number()) %>% arrange(AUC) %>% 
  filter(row_number() %in% c(5, 50, 95))

rocr_pred_all_rF = prediction(map(results_dist_forplot_rF, ~ pull(., predicted)),
                           map(results_dist_forplot_rF, ~ pull(., mk))) 

rocr_perf_all_rF = ROCR::performance(rocr_pred_all_rF,measure="tpr", x.measure="fpr")
plot(rocr_perf_all_rF, avg="vertical", spread.estimate = "stddev", spread.scale = 2)

## remove samples with x or y not finite
for (i in 1:length(rocr_perf_all_rF@x.values)) {
  ind.bool <- (is.finite(rocr_perf_all_rF@x.values[[i]]) &
                 is.finite(rocr_perf_all_rF@y.values[[i]]))
  
  if (length(rocr_perf_all_rF@alpha.values)>0)
    rocr_perf_all_rF@alpha.values[[i]] <- rocr_perf_all_rF@alpha.values[[i]][ind.bool]
  
  rocr_perf_all_rF@x.values[[i]] <- rocr_perf_all_rF@x.values[[i]][ind.bool]
  rocr_perf_all_rF@y.values[[i]] <- rocr_perf_all_rF@y.values[[i]][ind.bool]
}

perf.avg_rF <- rocr_perf_all_rF

x.values_rF <- seq(min(unlist(rocr_perf_all_rF@x.values)), max(unlist(rocr_perf_all_rF@x.values)),
                length=max(sapply(rocr_perf_all_rF@x.values, length)))

for (i in 1:length(rocr_perf_all_rF@y.values)) {
  print(i)
  perf.avg_rF@y.values[[i]] <-
    stats::approxfun(rocr_perf_all_rF@x.values[[i]], rocr_perf_all_rF@y.values[[i]],
                     ties=mean, rule=2)(x.values_rF)
  perf.avg_rF@alpha.values[[i]] <-
    stats::approxfun(rocr_perf_all_rF@x.values[[i]], rocr_perf_all_rF@alpha.values[[i]],
                     ties=mean, rule=2)(x.values_rF)
}
perf.avg_rF@x.values <- list(x.values_rF)

perf.avg_rF@y.values <- list(rowMeans( data.frame( perf.avg_rF@y.values )))

perf.avg_rF@alpha.values <- list(rowMeans( data.frame( perf.avg_rF@alpha.values )))


## y.values at show.spread.at (midpoint of error bars )
show.spread.at_rF= (seq(min(unlist(rocr_perf_all_rF@x.values)),
                     max(unlist(rocr_perf_all_rF@x.values)),
                     length=40))
show.spread.at.y.values_rF <-
  lapply(as.list(1:length(rocr_perf_all_rF@x.values)),
         function(i) {
           stats::approxfun(rocr_perf_all_rF@x.values[[i]], rocr_perf_all_rF@y.values[[i]],
                            rule=2,
                            ties=mean)( show.spread.at)
         })

show.spread.at.y.values_rF <- as.matrix(data.frame(show.spread.at.y.values_rF ))
colnames(show.spread.at.y.values_rF) <- c()
bar.width.y_rF <- apply(show.spread.at.y.values_rF, 1, stats::sd)

show.spread.at.alpha.values_rF <-
  lapply(as.list(1:length(rocr_perf_all_rF@x.values)),
         function(i) {
           stats::approxfun(rocr_perf_all_rF@x.values[[i]], rocr_perf_all_rF@alpha.values[[i]],
                            rule=2,
                            ties=mean)( show.spread.at)
         })

show.spread.at.alpha.values_rF <- as.matrix(data.frame(show.spread.at.alpha.values_rF ))
colnames(show.spread.at.alpha.values_rF) <- c()
alpha95l_rF <- apply(show.spread.at.alpha.values_rF, 1, function(.) quantile(., 0.025, na.rm = T))
alpha95u_rF <- apply(show.spread.at.alpha.values_rF, 1, function(.) quantile(., 0.975, na.rm = T))


line_roc_data_rF = data_frame(fpr = unlist(perf.avg_rF@x.values)[], tpr = unlist(perf.avg_rF@y.values))

ribbon_sd1_data_rF = data_frame(tpr = rowMeans(show.spread.at.y.values_rF), fpr= show.spread.at_rF,
                             tprl1 = tpr -     bar.width.y, tpru1 = tpr +    bar.width.y,
                             tprl2 = tpr - 2 * bar.width.y, tpru2 = tpr - 2* bar.width.y)

ribbon_sd2_data_rF = data_frame(tpr = rowMeans(show.spread.at.y.values_rF), fpr= show.spread.at_rF,
                             tprl1 = tpr -     bar.width.y, tpru1 = tpr +    bar.width.y,
                             tprl2 = tpr - 2 * bar.width.y, tpru2 = tpr + 2* bar.width.y)

label_cutoff_data_rF = data_frame(tpr = rowMeans(show.spread.at.y.values_rF), fpr = show.spread.at_rF,
                               cutoff = round(rowMeans(show.spread.at.alpha.values) * 20) / 20,
                               label = paste0("TH:", cutoff, "\nCI:[", round(alpha95l_rF, 2), ",", round(alpha95u_rF, 2), "]")) %>%
  filter(cutoff != 0, !is.na(cutoff) & !is.infinite(cutoff)) %>%
  distinct(cutoff, .keep_all = T)


roc_rf = ggplot() +
  geom_line(data = line_roc_data_rF, aes(x=fpr, y=tpr), size = 1) +
  geom_line(data = line_roc_data_s, aes(x=fpr, y=tpr), color = "purple", size = 1) +
  geom_ribbon(data = ribbon_sd1_data_rF, aes(x=fpr, y=tpr,ymin=tprl1, ymax=tpru1), alpha= 0.4, fill="blue")+
  geom_ribbon(data = ribbon_sd2_data_rF, aes(x=fpr, y=tpr,ymin=tprl2, ymax=tpru2), alpha= 0.2, fill="blue") +
  geom_label_repel(data = label_cutoff_data_rF, aes(x=fpr, y=tpr, label = label), alpha= 0.8, size = 3.5, min.segment.length = 1) +
  geom_label(aes(x=0.6, y=0.45, label = "randomized target model"), color = "purple", alpha= 0.8, min.segment.length = 1) +
  geom_point(data = label_cutoff_data_rF, aes(x=fpr, y=tpr)) +
  theme_minimal(base_size = 15) +
  scale_x_continuous("False Positive Rate", label = percent) +
  scale_y_continuous("Average True Positive Rate", label = percent) +
  geom_segment(x=0,y=0,xend=1,yend=1, linetype=2) +
  coord_cartesian(xlim = c(0,1), ylim = c(0,1), expand = F) + 
  ggtitle("Panel B: Receiver-Operator-Curve random forest")

#ggsave(paste0("S:/AG/AG-PM/DB07/results/ROC_curve_rF.png"))

roc_lasso_rf = roc_lasso + roc_rf
print(roc_lasso_rf)
ggsave(paste0(base_path, "/results/ROC_curve_both.svg"), width = 40, height = 20, units = "cm")
ggsave(paste0(base_path, "/Paper_1_MC_Classification/plots/ROC_curve_both_dpi.png"), width = 40, height = 20, units = "cm", dpi = 1200)


#format result table
if(result_table){
  library(gt)
  # Create a gt table based on preprocessed
  tdf = result_final1 %>% filter(grepl("Basophile|Lymphozyten|Thrombozyten|Neutrophile|Haemoglobin|Kalium|MCHC|Monozyten|Granulozyten|Phosphatase|LDH|GPT|Thrombozyten|
                                       GOT|Procalcitonin|GPT|Eosinophile|Natrium|Magnesium|Haematokrit|Leukozyten|Age|Encounters", term)) %>% 
    mutate(Group1 = if_else(grepl("trend_abs", term), "Trend", 
                     if_else(grepl("trend", term), "Change Rate", ""))) %>% 
    mutate(term = gsub("trend_|trend_abs_", "", term)) %>%
    mutate(Group2 = gsub(".*_(mean|median|max|min|lastKnown|stdev)$", "\\1", term)) %>% 
    mutate(term = gsub("(.*)_mean|_median|_max|_min|_lastKnown|_stdev$", "\\1", term)) %>% 
    mutate(Group2 = if_else(grepl("cnt_Encounters$", term) | grepl("Age", term) | grepl("MNEURO", term), "", Group2)) %>%
    #mutate(Group1 = if_else(grepl("cnt_Encounters$", term) | grepl("Age", term) | grepl("MNEURO", term), term, Group1)) %>%
    #mutate(term = if_else(grepl("cnt_Encounter$", term) | grepl("Age", term) | grepl("MNEURO", term), "", term)) %>% 
    mutate(Value = glue("{Group2} {Group1}")) %>% group_by(term) %>% arrange(nchar(Value)) %>% 
    transmute(Value, pvalue = round(p.value,2), estimate = round(estimate,2)) %>% distinct() 
  
  
  tdf %>% 
    filter(pvalue <= 0.05) %>%
    gt() %>%
    tab_header(
      title = "Table I: Results from univariate logistic regression for feature selection"
    )
  
  
}

#debug used features
if(FALSE){
  ggplot(data = imputed_s, aes(x = Basophile_stdev, fill = mk)) + geom_histogram() + scale_fill_economist() + theme_minimal()
  ggplot(data = imputed_s, aes(x = cnt_Encounters_6months_median, fill = mk)) + geom_histogram() + scale_fill_economist() + theme_minimal()
  
  ggplot(data = imputed_s, aes(x = trend_abs_Basophile_min, fill = mk)) + geom_histogram() + scale_fill_economist() + theme_minimal()
  ggplot(data = imputed_s, aes(x = unreife_Granulozyten_lastKnown, fill = mk)) + geom_histogram() + scale_fill_economist() + theme_minimal()
  
  ggplot(data = imputed_s, aes(x =  trend_abs_Kalium_lastKnown, fill = mk)) + geom_histogram() + scale_fill_economist() + theme_minimal()
  ggplot(data = imputed_s, aes(x =  Age_of_onset_lastKnown, fill = mk)) + geom_histogram() + scale_fill_economist() + theme_minimal()
  
  
  ggplot(data = group_by(regdff, mk) %>% summarize(c_mean = Corticoid_mean, na.rm = T), aes(x =  mk, y = c_mean, fill = mk)) + geom_col() + scale_fill_economist() + theme_minimal()
  
  ggplot(data = group_by(regdff, mk) %>% summarize(c_mean = Corticoid_mean, na.rm = T), aes(x =  c_mean, fill = mk)) + geom_histogram() + scale_fill_economist() + theme_minimal()
 
    
  
  
}

#how many patients are stable, how many switch groups: 

result_stability_prep = bind_rows(group_by(results_dist, mk, Patient_ID) %>% summarize(cnt_above_th = sum(predicted >= 0.096), cnt_below_th = sum(predicted < 0.096), mean_predicted_value = mean(predicted)) %>% 
                                    mutate(model = "glm"), 
                                  group_by(results_dist_rF, mk, Patient_ID) %>% 
                                    summarize(cnt_above_th = sum(predicted >= 0.096), cnt_below_th = sum(predicted < 0.096), mean_predicted_value = mean(predicted)) %>% 
                                    mutate(model = "rF"))%>% anonymize_ids %>% mutate(Patient_ID_anonymous = fct_reorder(Patient_ID_anonymous, cnt_above_th))
  
  
result_stability =  result_stability_prep  %>% 
  mutate(mk = if_else(mk, "above treshold with crisis", "above threshold no crisis")) %>%
  mutate(mk = fct_relevel(as.factor(mk), "above treshold with crisis", "above threshold no crisis"))


ggplot(data = result_stability %>% bind_rows(result_stability %>% mutate(cnt_above_th = 100-cnt_above_th, mk = "below the treshold")) %>% 
         mutate(Patient_ID_anonymous = gsub("Patient ", "", Patient_ID_anonymous))) + 
  geom_col(aes(x = Patient_ID_anonymous, y =cnt_above_th, fill = mk), position = "stack") + theme_minimal(base_size = 15) +
  geom_text(aes(x = Patient_ID_anonymous, y =50, label = round(mean_predicted_value,2))) + 
  scale_fill_manual(values = c("below the treshold" = "dark gray", "above treshold with crisis" = "light yellow" , "above threshold no crisis" = "light blue")) + 
  coord_flip() + xlab(NULL) + 
  ylab("predictions")+facet_wrap(~model, scales = "free") + 
  ggtitle("Stability of patient classification across runs in the lasso regression and random forest")
ggsave(paste0(base_path, "/Paper_1_MC_Classification/plots/supplementFig1_Stability_across_runs.svg"), width = 40, height = 20, units = "cm")

#stability dependent of imputation? 
imputation_impact = data_frame(Patient_ID = Features_MK_NMK_all$Patient_ID, nas = df_clean %>% apply(FUN = function(x) sum(is.na(x)), MARGIN = 1))

ii = group_by(results_dist, mk, Patient_ID) %>% 
  summarize(cnt_above_th = sum(predicted >= 0.1), cnt_below_th = sum(predicted < 0.1), sd_pred = sd(predicted)) %>% 
  mutate(correct_perc = if_else(mk, cnt_above_th/100, cnt_below_th/100)) %>% mutate(correct_percb = floor(correct_perc*10)/10) %>% inner_join(imputation_impact)


ggplot(data =ii, aes(x = cnt_above_th, y = nas)) + geom_point(color = "#116530", size = 2) + theme_minimal() +
  xlab("runs above the threshold") + ylab("number of imputed values") + 
  theme_minimal(base_size = 15) + ggtitle("Imputed values versus above the threshold")

ggsave(paste0(base_path, "/results/imputed_values.svg"), width = 40, height = 20, units = "cm")

#Einzelpatientenbetrachtung: 
for(pat_id in c("xyz")){ 
  View(filter(mk_info, Patient_ID == pat_id), title = paste0("mk_info_", pat_id))
  View(filter(Encounters_days, Patient_ID == pat_id), title = paste0("encounters_", pat_id))
  View(filter(referenz_werte, Patient_ID == pat_id), title = paste0("ref_werte_", pat_id))
  View(filter(referenz_werte_normalized, Patient_ID == pat_id), title = paste0("ref_werte_norm", pat_id))
  View(filter(Features_MK_NMK, Patient_ID == pat_id), title = paste0("features_mk_nmk_", pat_id))
  View(filter(results_dist, Patient_ID == pat_id), title = paste0("prediction_results_", pat_id))
  
  ggplot(data = filter(results_dist, Patient_ID == pat_id),aes(x = predicted)) + geom_histogram() + theme_minimal() + 
  ggtitle("Verteilung Prediction Scores von Patientin mit AcHR und MusK")
  ggplot(data = filter(results_dist, Patient_ID == pat_id) %>% mutate(predicted = if_else(predicted > 0.5,1,0)),aes(x = predicted)) + geom_histogram() + theme_minimal() + 
    ggtitle("Verteilung Prediction Scores von Patientin mit AcHR und MusK")
  
  View(mutate(results_dist_g, thresholds_01 = if_else(avg_predicted_value >= 0.1, 1, 0)))
 
  thresholds = mutate(results_dist_g, 
                         thresholds_01 = if_else(avg_predicted_value >= 0.1, T, F), 
                         thresholds_03 = if_else(avg_predicted_value >= 0.3, T, F), 
                         thresholds_08 = if_else(avg_predicted_value >= 0.8, T, F)) 
  
  thresholds_01 = group_by(thresholds, thresholds_01, mk) %>% summarize(cnt = n())
  thresholds_03 = group_by(thresholds, thresholds_03, mk) %>% summarize(cnt = n())
  thresholds_09 = group_by(thresholds, thresholds_08, mk) %>% summarize(cnt = n())
  confmatrglm = ggplot(data = thresholds_01, aes(x = thresholds_01, y = mk, fill = cnt)) + geom_tile() + 
    geom_label(aes(x = thresholds_01, y = mk, label = cnt), fill = "white") + ggtitle("Confusion Matrix of threshold 0.1")
  confmatrglm2 = ggplot(data = thresholds_03, aes(x = thresholds_03, y = mk, fill = cnt)) + geom_tile() + 
    geom_label(aes(x = thresholds_03, y = mk, label = cnt), fill = "white")+ ggtitle("Confusion Matrix of threshold 0.3")
  confmatrglm3 = ggplot(data = thresholds_09, aes(x = thresholds_08, y = mk, fill = cnt)) + geom_tile() + 
    geom_label(aes(x = thresholds_08, y = mk, label = cnt), fill = "white")+ ggtitle("Confusion Matrix of threshold 0.8")
  
  confmatrglm  + confmatrglm2+ confmatrglm3
  ggsave(paste0(base_path, "/Paper_1_MC_Classification/plots/confusion_matrix_glm.svg"), width = 40, height = 20, units = "cm")
  ggsave(paste0(base_path, "/Paper_1_MC_Classification/plots/confusion_matrix_glm.png"), width = 40, height = 20, units = "cm", dpi = 1200)
  
  
  
  thresholds_rF = mutate(results_dist_g_rF, 
                      thresholds_01 = if_else(avg_predicted_value >= 0.1, T, F), 
                      thresholds_03 = if_else(avg_predicted_value >= 0.3, T, F), 
                      thresholds_08 = if_else(avg_predicted_value >= 0.8, T, F)) 
  
  thresholds_rF_01 = group_by(thresholds_rF, thresholds_01, mk) %>% summarize(cnt = n())
  thresholds_rF_03 = group_by(thresholds_rF, thresholds_03, mk) %>% summarize(cnt = n())
  thresholds_rF_09 = group_by(thresholds_rF, thresholds_08, mk) %>% summarize(cnt = n())
  confmatrrf = ggplot(data = thresholds_rF_01, aes(x = thresholds_01, y = mk, fill = cnt)) + geom_tile() + 
    geom_label(aes(x = thresholds_01, y = mk, label = cnt), fill = "white") + ggtitle("Confusion Matrix of threshold 0.1")
  confmatrrf2 = ggplot(data = thresholds_rF_03, aes(x = thresholds_03, y = mk, fill = cnt)) + geom_tile() + 
    geom_label(aes(x = thresholds_03, y = mk, label = cnt), fill = "white")+ ggtitle("Confusion Matrix of threshold 0.3")
  confmatrrf3 = ggplot(data = thresholds_rF_09, aes(x = thresholds_08, y = mk, fill = cnt)) + geom_tile() + 
    geom_label(aes(x = thresholds_08, y = mk, label = cnt), fill = "white")+ ggtitle("Confusion Matrix of threshold 0.8")
  
  confmatrrf + confmatrrf2 + confmatrrf3
  ggsave(paste0(base_path, "/Paper_1_MC_Classification/plots/confusion_matrix_rf.svg"), width = 40, height = 20, units = "cm")
  ggsave(paste0(base_path, "/Paper_1_MC_Classification/plots/confusion_matrix_rf.png"), width = 40, height = 20, units = "cm", dpi = 1200)
  
}

#completeness vs imputation
#unimputed_df = Features_MK_NMK_all
#imputed_df = imputed_s
alr_gs = separate_rows(features_all_runs, row, sep = ":") %>% 
  group_by(row) %>% summarize(nr_runs = n_distinct(run_id))

use_vs_na = select(alr_gs, row, nr_runs) %>% inner_join(completeness, by= c("row" = "key")) %>% rename(feature = row, nr_nas = value)
ggplot(use_vs_na, aes(x=nr_runs, y=nr_nas, label = feature)) + geom_point() + geom_label_repel() + theme_minimal() + scale_y_continuous()

#check, which patients are finally in the used df: 
patients_finall_used = inner_join(mk_info_wa, results_dist, by= c("Patient_ID", "mk")) %>% distinct(Vorname, Nachname, Patient_ID, year, month, mk, Gender, grp)
group_by(patients_finall_used, mk) %>% summarize(cnt = n())
#imputed ages are missing: 

#mutate(patients_finall_used, age = if(as.numeric(month) < 10, year(today()) - as.numeric(year)) %>% group_by(mk) %>% summarize(median_age = median(age, na.rm = T), mean_age = mean(age, na.rm = T))
group_by(patients_finall_used, mk, Gender) %>% summarize(cnt = n())
inner_join(select(AK, Patient_ID, AK_negativ, AChR, MuSK), patients_finall_used, by = c("Patient_ID")) %>% group_by(grp, AChR, MuSK) %>% summarize(cnt =n())
inner_join(select(Thymus_Pathologie, Patient_ID, Thymektomie, Thymom, `Thymus-Hyperplasie`), patients_finall_used, by= c("Patient_ID")) %>% 
  group_by(grp, Thymektomie, Thymom, `Thymus-Hyperplasie`) %>% summarize(cnt =n())

nr_of_mks = inner_join(mk_info_wa, results_dist, by= c("Patient_ID", "mk")) %>% 
  distinct(Patient_ID, .keep_all = T) %>% mutate(nr_mks = if_else(mk3_date != "", 3,
                                                                   if_else(mk2_date != "", 2, 1))) %>% 
  group_by(nr_mks, grp) %>% 
  summarize(cnt = n())



save.image(paste0(input_path, "/Classifying_Patients_V", V, "/final_WS", sql_dump_date, ".RData"))



