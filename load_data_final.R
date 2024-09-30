#config: 
sql_pull = F
neuros_pull = F
base_path = Sys.getenv("BASEPATH")
input_path = paste0(base_path, "/Data")
V = 500
sql_dump_date = "11232023"



##################################
#sql db connector
##################################
if(sql_pull){
  con = DBI::dbConnect(RMySQL::MySQL(),
                       dbname = Sys.getenv("DBNAME"), #
                       host = Sys.getenv("HOSTNAME"), 
                       port = 3306,
                       user = Sys.getenv("USERNAME"),
                       password = Sys.getenv("PASSWORD")
  
  dbFetch(dbSendQuery(con, "show databases"))
  
  
  #list all tables
  table_names = dbListTables(con)
  

  all_filled = list()
  for(f in 1:length(table_names)){
    t=tbl(con, table_names[f]) %>% collect()
    if(nrow(t) >0){
      assign(table_names[f],t)
      
      head_t = head(t)
      assign(paste0(table_names[f], '_head'),t)
      
      names_t = names(t)
      assign(paste0(table_names[f], '_cols'),t)
      
      filled = list()
      unfilled = list()
      for(n in names_t){
        filled_cells = sum(!is.na(t[n]) & t[n] != '')
        unfilled_cells = sum(is.na(t[n]) | t[n] == '')
        filled[[n]] = filled_cells
        unfilled[[n]] = unfilled_cells
      } 
      
      filled_df = ldply (filled, data.frame)
      unfilled_df = ldply (unfilled, data.frame)
      names(unfilled_df) = c('Column', 'unfilled_values') 
      names(filled_df) = c('Column', 'filled_values') 
      filled_df = inner_join(filled_df, unfilled_df, by = c("Column"))
      
      filled_df = mutate(filled_df, table_name = table_names[f], filled_percent = round(filled_values/(filled_values + unfilled_values),2))
      all_filled[[n]] = filled_df
      if(FALSE){
        if(table_names[f] == "HADS_Score"){
          t = mutate(t, Pat_Nr = as.integer(Pat_Nr), Datum = as.character(Datum), lfd_Nr = as.integer(lfd_Nr))
        }
        if(table_names[f] == "ISI_Score"){
          t = mutate(t, Schweregrad = as.character(Schweregrad))
        }
        if(is.null(final)){
          final = t
        }
        else{
          keys = intersect(names(final), names(t))
          final = left_join(final,t, by = c(keys))
          print(paste0("table in bearbeitung:", table_names[f]))
          print(paste0("Join an:", keys))
        }
      }
    }
  }

  
  Anamnese = filter(Anamnese,!is.na(Pat_Nr))
  Patienten = mutate(Patienten, Name = trimws(Name), Vorname = trimws(Vorname))
  
  all_filled = bind_rows(all_filled) 
  dir.create(paste0(input_path, "Classifying_Patients_V", V))
  #system(paste0("mkdir ", input_path, "Classifying_Patients_V", V))
  save.image(file = paste0(input_path, "Classifying_Patients_V", V, "/sql_dump_", sql_dump_date, ".RData"))
  fwrite(all_filled, paste0(input_path, "Tables_Attribute_Status.csv"))
}else{load(paste0(input_path, "Classifying_Patients_V", V, "/sql_dump_", sql_dump_date, ".RData"))}


##################################
#load SAP extracts
##################################
#the exports from SAP have fixed width format. Thus fread or read.csv do not work. read_fwf from readr table 

if(neuros_pull){
  raw_MNEURO = read_fwf(paste0(input_path, "raw data/G70-alle faelle_01012010-08082019_MNEURO-MNEUROS.csv"), 
                        fwf_empty(paste0(input_path, "raw data/G70-alle faelle_01012010-08082019_MNEURO-MNEUROS.csv"),
                                  col_names = c("ICD", "Fallnr", "Patnr", "Nachname", "Vorname", "Gebdat"), skip= 3), skip = 3, 
                        locale(encoding = "ISO-8859-1"), col_types = NULL) %>% mutate(Fallnr = str_pad(as.character(Fallnr), 10, "left", "0"), Patnr = str_pad(as.character(Patnr), 10, "left", "0"))
  
  
  raw_SNEURO = read_fwf(paste0(input_path, "raw data/G70-alle faelle_01012010-08082019_SNRO.csv"), 
                        fwf_empty(paste0(input_path, "raw data/G70-alle faelle_01012010-08082019_SNRO.csv"),
                                  
                                  col_names = c("ICD", "Fallnr", "Patnr", "Nachname", "Vorname", "Gebdat"), skip= 3),skip = 3, 
                        locale(encoding = "ISO-8859-1"), col_types = NULL) %>% mutate(Fallnr = str_pad(as.character(Fallnr), 10, "left", "0"), Patnr = str_pad(as.character(Patnr), 10, "left", "0"))
  
  
  raw_WNEURO = read_fwf(paste0(input_path, "raw data/G70-alle faelle_01012010-08082019_WNEURO.csv"), 
                        fwf_empty(paste0(input_path, "raw data/G70-alle faelle_01012010-08082019_WNEURO.csv"),
                                  col_names = c("ICD", "Fallnr", "Patnr", "Nachname", "Vorname", "Gebdat"), skip= 3),skip = 3, 
                        locale(encoding = "ISO-8859-1"), col_types = NULL) %>% mutate(Fallnr = str_pad(as.character(Fallnr), 10, "left", "0"), Patnr = str_pad(as.character(Patnr), 10, "left", "0"))
  
  neuros = bind_rows(raw_MNEURO, raw_SNEURO, raw_WNEURO)
  
  lab_proc = list()
  treat_proc = list()
  encounter_attr = list()
  
  filelist = data.frame(fname = list.files(paste0(base_path, 'HDP-MG Projekt/Ergebnisse/by_patient'), pattern=NULL, all.files=FALSE, 
                                           full.names=FALSE))
  
  for(i in 1:length(filelist$fname)){
    patient = fread(paste0(base_path,'HDP-MG Projekt/Ergebnisse/by_patient/', filelist$fname[i]), 
                    colClasses=c(V1="character",V2="character",V3="character", V4 = "character", V5 = "character"),
                    header = F, skip = 1, sep = ',', fill = T) %>% gather(key, value, -V1, -V2)  %>%
      rename(Patient_ID = V1, Encounter_ID = V2) %>%  as_data_frame()
    
    
    for(Encounter in unique(patient$Encounter_ID)){
      print(Encounter)
      dfe = filter(patient, Encounter_ID == Encounter) 
      i = 1
      df = data_frame(h_v = c("admission", "discharge", "oe_admission", "oe_discharge", "encounter_type", "DRG_Code_Aufn"), 
                      #value = dfe$value[3:9],
                      value = dfe$value[1:6],
                      Encounter = Encounter, 
                      Patient_ID = dfe$Patient_ID[[1]])
      encounter_attr[[length(encounter_attr) + 1]] = df
      
      #DRG codes fehlen noch
      
      while(i < nrow(dfe)){
        if(i+8 < nrow(dfe) && (!is.na(as.numeric(dfe$value[[i+5]])) && as.numeric(dfe$value[[i+5]]) == 0) && (grepl("._", dfe$value[[i]]) | grepl("MIN|IM|M[[:digit:]]|WAU|WGA", dfe$value[[i]]))){
          df = data_frame(h_v = c("LeistID", "Kat", "Value", "Unit", "Date", "Time", "LOINC", "range_low", "range_high"), 
                          value = c(dfe$value[i:(i+8)]), 
                          Encounter = Encounter, 
                          Patient_ID = dfe$Patient_ID[[1]])
          lab_proc[[length(lab_proc) + 1]] = df
          print("lab_proc")
          print(df)
          i = i+9
        }else if(i+4 < nrow(dfe) && (!is.na(as.numeric(dfe$value[[i+4]])) && as.numeric(dfe$value[[i+4]]) == 0) && nchar(dfe$value[[i+2]]) >5 && is.na(as.numeric(dfe$value[[i+2]]))) {
          df = data_frame(h_v = c("Type", "Code", "Descr", "Date", "Time"), 
                          value = c(dfe$value[i:(i+4)]), 
                          Encounter = Encounter, 
                          Patient_ID = dfe$Patient_ID[[1]])
          treat_proc[[length(treat_proc) + 1]] = df
          print("treat_proc")
          print(df) 
          i =  i+5
        }else{
          i = i+1
        }
      }
    }
    
  }
  
  lab_proc = bind_rows(lab_proc)
  treat_proc = bind_rows(treat_proc)
  encounter_attr = bind_rows(encounter_attr)
  save.image(paste0(input_path, "Classifying_Patients_V", V, "/labs_treatment_encounters_", sql_dump_date, ".RData"))
}else{
  load(paste0(input_path, "Classifying_Patients_V", V, "/labs_treatment_encounters_", sql_dump_date, ".RData"))
}



##################################
#prep SAP extract
##################################
sap_patients = mutate(lab_proc, new_headers = paste0(row_number(), '_', h_v), 
                  year = if_else(grepl("Date", h_v), gsub('.*(....)$', '\\1', value), ""), 
                  daymonth = if_else(year != "", str_pad(gsub('(.*)....$', '\\1', value), 4, "left", pad = 0), ""), 
                  month = substr(daymonth, 3,4), 
                  day = substr(daymonth, 1,2)) %>% select(-daymonth)


#join lab and treatment data on Patientdata making sure none of the patientdata is lost, even if there is no lab procedures (doesn't currently occur, just a precaution)
sap_and_sql = right_join(distinct(sap_patients, Patient_ID, Encounter), neuros,  by = c("Patient_ID" = "Patnr", "Encounter" = "Fallnr")) 


##################################
#load cohort info
##################################

mk_info = tibble(fread(paste0(input_path, "raw data/patients_11232021.csv"), colClasses = c("character", "character", "logical", "character","character", 
                                                                                     "character", "character", "character", "character", "character")))
####################################################################
#join all data, clean, create lookup tables, and plot overview
####################################################################
Patienten_m = mutate(Patienten, Name = gsub("Dr. (.*)", "\\1", Name))
Patienten_m = date_manip(Patienten_m)

mk_info_wa = left_join(mk_info, Patienten_m, by = c("Vorname", "Nachname" = "Name")) %>% 
  mutate(decade = paste0(substr(Geburtsdatum, 1,3), "0"))
  
mk_info_wa_g = mk_info_wa %>% 
  group_by(decade, Gender, grp) %>% dplyr::summarize(cnt = n()) %>% dplyr::group_by(decade) %>% dplyr::mutate(cnt_p = cnt/sum(cnt), s = sum(cnt)) 


sap_and_sql = full_join(sap_and_sql, select(mk_info_wa, Patient_ID, mk, Control_for, grp), by = c("Patient_ID")) %>% filter(!is.na(ICD))%>% 
  distinct(Encounter, .keep_all = T) 

sap_and_sql = sap_and_sql %>% mutate(Nachname = gsub("\u0081", "�", Nachname)) %>% mutate(Nachname = gsub("\u0084", "�", Nachname))

id_lookup = select(mk_info_wa, Vorname, Nachname, grp, Patient_ID, mk1_date) %>% left_join(select(Patienten, Name, Vorname, Pat_Nr), by = c("Vorname",  "Nachname" = "Name"))


sap_and_sql_g = sap_and_sql %>% mutate(grp = if_else(grp == ""| is.na(grp), "pop", grp))
sap_and_sql_g_agi = left_join(sap_and_sql_g, select(Patienten_m, Pat_Nr, Name, Vorname, Geburtsdatum, Geschlecht), by = c("Vorname", "Nachname" = "Name"))




#final plot for age and gender
ggplot(data = mk_info_wa_g %>% ungroup(), aes(x = as.numeric(decade), y = cnt_p, fill = Gender)) + geom_col() + 
  theme_minimal() + scale_fill_manual(name = NULL, breaks = c("m", "w", "test", "control"), values = c(control = "#efe8d1", test = "#acc8d4", 
                                                                                                       m = "#d4dddd", w = "#4a4a4a")) + xlab(NULL) + ylab(NULL) + 
  geom_label(aes(x = as.numeric(decade), y = if_else(Gender == "w" & grp == "control", 0.2, 
                                                     if_else(Gender == "m" & grp == "control", 0.8,
                                                             if_else(Gender == "w" & grp == "test", 0.1, 
                                                                     if_else(Gender == "m" & grp == "test", 0.7, 0.5)))), label = cnt, fill = grp))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15), axis.text.y = element_text(size = 15)) + ggtitle(paste0("Birth Decade and Gender of Sample, N = ", sum(mk_info_wa_g$cnt), "")) + 
  scale_y_continuous(label = percent)
ggsave(filename = paste0(input_path, "Classifying_Patients_V", V, "sample_age_and_gender.png"))



#final plot for AOO and gender: 
Onset = Patienten_m %>% inner_join(select(Anamnese, Pat_Nr, Diagnose_Jahr)) %>% filter(!is.na(Diagnose_Jahr)) %>% 
  mutate(onset_age = as.numeric(Diagnose_Jahr) - as.numeric(year)) %>% 
  inner_join(select(mk_info, Vorname, Nachname, mk, grp, Gender, mk1_date), by = c("Vorname", "Name" = "Nachname"))

paper_table = group_by(Onset, grp, Gender) %>% summarize(Median_Onset_age = median(onset_age), 
                                                         Minimum_Onset_age = min(onset_age), 
                                                         Maximum_Onset_age = max(onset_age))

from_Diag_to_MC = group_by(Onset, grp, Gender) %>% summarize(median(coalesce(as.numeric(substr(mk1_date, 1,4), na.rm = T), 2021) - as.numeric(Diagnose_Jahr, na.rm = T)), 
                                                             min(coalesce(as.numeric(substr(mk1_date, 1,4), na.rm = T), 2021) - as.numeric(Diagnose_Jahr, na.rm = T)), 
                                                             max(coalesce(as.numeric(substr(mk1_date, 1,4), na.rm = T), 2021) - as.numeric(Diagnose_Jahr, na.rm = T)))

ggplot(data = group_by(Onset, Late_Onset = onset_age >= 50, mk) %>% summarize(cnt = n()), aes(x = Late_Onset, y = cnt)) + geom_col() + facet_wrap(~Gender) 


mk_info_waoo = Onset %>%
  mutate(age_of_onset_decade = floor(onset_age/10)*10)%>% 
  group_by(age_of_onset_decade, Gender, grp) %>% 
  summarize(cnt = n()) %>% group_by(age_of_onset_decade) %>% mutate(cnt_p = cnt / sum(cnt))


ggplot(data = mk_info_waoo, aes(x = age_of_onset_decade, y = cnt_p, fill = Gender)) + geom_col() + 
  theme_minimal() + scale_fill_manual(name = NULL, breaks = c("m", "w", "test", "control"), 
                                      values = c(control = "#efe8d1", test = "#acc8d4", m = "#d4dddd", w = "#4a4a4a")) + xlab(NULL) + ylab(NULL) +
  geom_label(aes(x = age_of_onset_decade, y = if_else(Gender == "w" & grp == "control", 0.12, 
                                                     if_else(Gender == "m" & grp == "control", 0.8,
                                                             if_else(Gender == "w" & grp == "test", 0.05, 
                                                                     if_else(Gender == "m" & grp == "test", 0.9, 0.5)))), label = cnt, fill = grp))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 15), axis.text.y = element_text(size = 15)) + 
  scale_x_continuous(labels = c(0, 10,20,30,40,50,60,70,80), breaks = c(0, 10,20,30,40,50,60,70, 80)) + 
  ggtitle(paste0("Onset decade, N = ", sum(mk_info_waoo$cnt), " (+4 unknown)")) + scale_y_continuous(label = percent)


  
ggsave(filename = paste0(input_path, "Classifying_Patients_V", V, "/Alter_bei_Erstsymptomen.png"))


save.image(paste0(input_path, "/MG_load", today(), ".RData"))





