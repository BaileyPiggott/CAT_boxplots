# subset engineering data by discipline

source("setup_CAT.R")


# ALL ENGINEERING ---------------------------------------------------------

eng <- apsc %>% mutate(Subject = "Z_APSC")# subject starts with Z so it will be on the right of the disciplines

# all engineering sample sizes:
n_eng_1 <-  sum(with(eng, year == 1 & CAT.Score > 1), na.rm = TRUE)  
n_eng_2 <-  sum(with(eng, year == 2 & CAT.Score > 1), na.rm = TRUE) 
n_eng_3 <-  sum(with(eng, year == 3 & CAT.Score > 1), na.rm = TRUE) 
n_eng_4 <-  sum(with(eng, year == 4 & CAT.Score > 1), na.rm = TRUE) 


# CONSENTING FIRST AND SECOND YEARS ---------------------------------------
#apsc_100 is all consenting eng first years
#apsc_200 is all consenting eng second years

all_eng_cat <- read.csv("ENG CAT 1-2.csv") %>% select(-year) #consenting CAT scores with disciplines

eng_1 <- all_eng_cat %>% 
  filter(course == 101) %>%
  select(studentid, discipline, score) %>%
  mutate(year = 1)

eng_2 <- all_eng_cat %>% 
  filter(course == 200) %>%
  select(studentid, discipline, score) %>%
  mutate(year = 2)

all_eng <- bind_rows(eng_1, eng_2)

# discipline data frames  -------------------------------------------


mech <- all_eng %>% filter(discipline == "MECH-M-BSE") 

elec <- all_eng %>% filter(discipline == "ELEC-M-BSE") 

cmpe <- all_eng %>% filter(discipline == "CMPE-M-BSE") 

civl <- all_eng %>% filter(discipline == "CIVL-M-BSE") 

chee <- all_eng %>% filter(discipline == "CHEE-M-BSE") 

ench <- all_eng %>% filter(discipline == "ENCH-M-BSE") 

mine <- all_eng %>% filter(discipline == "MINE-M-BSE") 

geoe <- all_eng %>% filter(discipline == "GEOE-M-BSE") 

enph <- all_eng %>% filter(discipline == "ENPH-M-BSE") 

mthe <- all_eng %>% filter(discipline == "MTHE-M-BSE") 

all_eng <- all_eng %>% mutate(discipline = "Z_ENG") #start with Z so all eng is right of discipline

# need null data for 3rd year to plot properly:
fix <- data.frame(c(NA,NA,NA,NA), c(NA,NA,NA,NA), c(NA,NA,NA,NA),c(1,2,3,4))
colnames(fix) <- colnames(all_eng)
