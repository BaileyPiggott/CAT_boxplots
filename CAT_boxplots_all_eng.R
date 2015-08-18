# CAT engineering boxplots, disciplines compared

source("setup_CAT_eng_disciplines.R")

#SET UP DATA FRAME FOR PLOTTING ----------------------
eng_disciplines = c("CHEE", "CIVL", "CMPE", "ELEC", "ENCH", "ENPH", "GEOE", "MECH", "MINE", "MTHE")

all_disp <- bind_rows(chee, civl, cmpe, elec, ench, enph, geoe, mech, mine, mthe)

# change year column to better titles for plotting
all_disp$year <- as.character(all_disp$year)

all_disp$year[all_disp$year== "1"] <- "Year 1"
all_disp$year[all_disp$year== "2"] <- "Year 2"

# AVERAGED YEARS ----------------------------
ggplot(data = all_disp, aes(x = discipline, y = score)) +
  geom_boxplot(width = 0.4, fill = "tomato") +     
  coord_cartesian(xlim = c(0.5,10.5),ylim = c(0, 40)) +
  scale_x_discrete(labels = eng_disciplines) +
  labs(title = "Engineering CAT Scores", x = "Engineering Discipline", y = "CAT Score") +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.grid.major.y = element_line("grey"), #change horizonatal line colour (from white)
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(),    
    legend.title = element_blank(), #remove legend title
    legend.key = element_blank(), #remove grey background from legend items
    plot.title = element_text(size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12) #size of x axis labels
  ) 

# DODGED YEARS ----------------------------

ggplot(data = all_disp, aes(x = discipline, y = score, fill = factor(year))) +
  geom_boxplot(width = 0.5) +     
  coord_cartesian(xlim = c(0.5,10.5),ylim = c(0, 40)) +
  scale_x_discrete(labels = eng_disciplines) +
  scale_fill_manual(
    values = c("tomato", "tomato4")
    ) +
  labs(title = "Engineering CAT Scores", x = "Engineering Discipline", y = "CAT Score") +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.grid.major.y = element_line("grey"), #change horizonatal line colour (from white)
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(),    
    legend.title = element_blank(), #remove legend title
    legend.key = element_blank(), #remove grey background from legend items
    plot.title = element_text(size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12) #size of x axis labels
  ) 

# FACET PLOT BY YEAR-----------------------

ggplot(data = all_disp, aes(x = discipline, y = score, fill = factor(year))) +
  geom_boxplot(width = 0.5) +  
  facet_grid(year~.) +
  coord_cartesian(xlim = c(0.5,10.5),ylim = c(0, 36)) +
  scale_x_discrete(labels = eng_disciplines) +
  scale_fill_manual(
    values = c("tomato", "tomato4")
    ) +
  labs(title = "Engineering CAT Scores", x = "Engineering Discipline", y = "CAT Score") +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.grid.major.y = element_line("grey"), #change horizonatal line colour (from white)
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(),    
    legend.title = element_blank(), #remove legend title
    legend.key = element_blank(), #remove grey background from legend items
    plot.title = element_text(size = 15),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12), #size of x axis labels
    strip.text = element_text(size = 12)
  ) 
