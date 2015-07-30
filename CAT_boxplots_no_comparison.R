# CAT boxplot total


source("CAT_boxplots_setup.R")

#calculate sample sizes:    
year1 <- paste0("First Year\nn = ", n_q_1) #text string for xlabel including sample size   
year2 <- paste0("Second Year\nn = ", n_q_2) #text string for xlabel    
year3 <- paste0("Third Year\nn = ", n_q_3) #text string for xlabel 
year4 <- paste0("Fourth Year\nn = ", n_q_4, "\n(DRAM400)") #text string for xlabel

## plot description
ggplot(
  data = queens, 
  aes(x = as.factor(year), y = CAT.Score)
) +
  stat_boxplot(geom = "errorbar", stat_params = list(width = 0.3)) +
  geom_boxplot(
    width = 0.5,
    fill = "steelblue3") + # geom_boxplot must be after stat_boxplot    
  coord_cartesian(xlim = c(0.5,4.5),ylim = c(0, 40)) +
  scale_x_discrete(labels = c(year1, year2, year3, year4)) + #text strings from above with sample sizes
  labs(title = "CAT Scores", x = "Year", y = "CAT Score")  +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.grid.major.y = element_line("grey"), #change horizonatal line colour (from white)
    panel.background = element_rect("white"), #change background colour
    axis.title.x = element_blank(), # remove x axis title
    axis.text.x = element_text(size = 12) #size of x axis labels
  )
