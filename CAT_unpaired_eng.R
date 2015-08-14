# unpaired engineering CAT data

# load data ----------------------------------
all_eng_cat <- read.csv("LOAC CAT Unpaired.csv") %>% filter(subject == "APSC")#consenting CAT scores with disciplines

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


# ALL ENGINEERING ---------------------------------------------------------

all_eng <- all_eng %>% mutate(discipline = "Z_ENG") #start with Z so all eng is right of discipline

# all engineering sample sizes:
n_eng_1 <-  sum(with(all_eng, year == 1 & score > 1), na.rm = TRUE)  
n_eng_2 <-  sum(with(all_eng, year == 2 & score > 1), na.rm = TRUE) 
n_eng_3 <-  sum(with(all_eng, year == 3 & score > 1), na.rm = TRUE) 
n_eng_4 <-  sum(with(all_eng, year == 4 & score > 1), na.rm = TRUE) 

# need null data for 3rd year to plot properly:
fix <- data.frame(c(NA,NA,NA,NA), c(NA,NA,NA,NA), c(NA,NA,NA,NA),c(1,2,3,4))
colnames(fix) <- colnames(all_eng)


# CHOOSE DISCIPLINE -------------------------------------------------------

# choose what to plot
df <- mech
prog_name <- "Mechanical Engineering"

df <- elec
prog_name <- "Electrical Engineering"

df <- cmpe
prog_name <- "Computer Engineering"

df <- civl
prog_name <- "Civil Engineering"

df <- chee
prog_name <- "Chemical Engineering"

df <- ench
prog_name <- "Engineering Chemistry"

df <- mine
prog_name <- "Mining Engineering"

df <- geoe
prog_name <- "Geological Engineering"

df <- enph
prog_name <- "Engineering Physics"

df <- mthe
prog_name <- "Math and Engineering"




# RUN FROM HERE--------------------------------------------------------------------------

#calculate sample sizes:
n_1 <-  sum(with(df, year == 1 & score > 1), na.rm = TRUE)     
year1 <- paste0("First Year\nn = ", n_1, "   n = ", n_eng_1) #text for xlabel &sample size
n_2 <-  sum(with(df, year ==2 & score > 1), na.rm = TRUE)     
year2 <- paste0("Second Year\nn = ", n_2, "   n = ", n_eng_2) #text string for xlabel
n_3 <-  sum(with(df, year == 3 & score > 1), na.rm = TRUE)     
year3 <- paste0("Third Year\nn = ", n_3, "   n = ", n_eng_3) #text string for xlabel
n_4 <-  sum(with(df, year == 4 & score > 1), na.rm = TRUE)     
year4 <- paste0("Fourth Year\nn = ", n_4, "   n = ", n_eng_4) #text string for xlabel

# set up data frame and title
data <- bind_rows(df, all_eng, fix) # combine with all queens data
graph_title <- paste0(prog_name,  " CAT Scores")

## plot description
ggplot(
  data = data, 
  aes(x = as.factor(year), y = score, fill = discipline)
) +
  geom_boxplot(
    width = 0.5
  ) +     
  coord_cartesian(xlim = c(0.5,4.5),ylim = c(0, 40)) +
  scale_x_discrete(labels = c(year1, year2, year3, year4)) + 
  labs(title = graph_title, x = "Year", y = "CAT Score")  +
  scale_fill_manual(
    values =  c("olivedrab3","coral4"),
    labels = c(prog_name, "All Engineering")
  )+
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.grid.major.y = element_line("grey"), #change horizonatal line colour (from white)
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(),
    legend.title = element_blank(), #remove legend title
    legend.key = element_blank(), #remove grey background from legend items
    plot.title = element_text(size = 15),
    axis.title.x = element_blank(), # remove x axis title
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12) #size of x axis labels
  ) 

