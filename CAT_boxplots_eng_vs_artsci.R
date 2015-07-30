# CAT engineering vs artsci
source("setup_CAT.R")

# SET UP DATA FRAMES ------------------------------------------------------

# separate into faculty
eng <- cat %>% filter(Subject == 'APSC') %>% mutate(Subject = "Eng")
artsci <- cat %>% filter(Subject != 'APSC') %>% mutate(Subject = "ArtSci") %>% unique()

# need null data for 3rd year to plot properly:
fix <- data.frame(c(NA,NA,NA,NA),c(1,2,3,4), c(NA,NA,NA,NA), c(NA,NA,NA,NA))
colnames(fix) <- colnames(cat)

# sample sizes
n_eng_1 <-  sum(with(eng, year == 1 & CAT.Score > 1), na.rm = TRUE)  
n_eng_2 <-  sum(with(eng, year == 2 & CAT.Score > 1), na.rm = TRUE) 
n_eng_3 <-  sum(with(eng, year == 3 & CAT.Score > 1), na.rm = TRUE) 
n_eng_4 <-  sum(with(eng, year == 4 & CAT.Score > 1), na.rm = TRUE) 

n_artsci_1 <-  sum(with(artsci, year == 1 & CAT.Score > 1), na.rm = TRUE)  
n_artsci_2 <-  sum(with(artsci, year == 2 & CAT.Score > 1), na.rm = TRUE) 
n_artsci_3 <-  sum(with(artsci, year == 3 & CAT.Score > 1), na.rm = TRUE) 
n_artsci_4 <-  sum(with(artsci, year == 4 & CAT.Score > 1), na.rm = TRUE) 

#x labels
year1 <- paste0("First Year\nn = ", n_artsci_1, "   n = ", n_eng_1) #text string for xlabel including sample size   
year2 <- paste0("Second Year\nn = ", n_artsci_2, "   n = ", n_eng_2) #text string for xlabel   
year3 <- paste0("Third Year\nn = ", n_artsci_3, "   n = ", n_eng_3) #text string for xlabel  
year4 <- paste0("Fourth Year\nn = ", n_artsci_4, "   n = ", n_eng_4) #text string for xlabel

# fix box width for 4th year
dummy_4 <- data.frame(NA, 4, "Eng", 60) # fix box width in 4th year
colnames(dummy_4) <- colnames(eng)

# combine into one big data frame
all_cat <- bind_rows(eng, artsci, dummy_4, fix)

# CREATE PLOT -------------------------------------------------------------

ggplot(
  data = all_cat, 
  aes(x = as.factor(year), y = CAT.Score, fill = Subject)
  ) +
  geom_boxplot(
    width = 0.5
  ) + # geom_boxplot must be after stat_boxplot    
  coord_cartesian(xlim = c(0.5,4.5),ylim = c(0, 40)) +
  scale_x_discrete(labels = c(year1, year2, year3, year4)) + #text strings from above with sample sizes
  labs(title = "CAT Scores", x = "Year", y = "CAT Score")  +
  scale_fill_manual(
    values = c("firebrick2", "darkgoldenrod1")
  ) +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.grid.major.y = element_line("grey"), #change horizonatal line colour (from white)
    panel.background = element_rect("white"), #change background colour
    panel.grid.major.x = element_blank(),
    legend.title = element_blank(), #remove legend title
    plot.title = element_text(size = 15),
    axis.title.x = element_blank(), # remove x axis title
    axis.title.y = element_text(size = 14),
    axis.text = element_text(size = 12) #size of x axis labels
  ) 

