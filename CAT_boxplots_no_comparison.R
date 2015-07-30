# CAT boxplot total


source("CAT_boxplots_setup.R")

# choose what to plot
df <- dram
prog_name <- "Drama"

df <- psyc
prog_name <- "Psychology"

df <- phys
prog_name <- "Physics"

df <- apsc
prog_name <- "Engineering"

df <- queens # all scores
prog_name <- "Queen's"


#calculate sample sizes:    
n_1 <-  sum(with(df, year == 1 & CAT.Score > 1), na.rm = TRUE)     
year1 <- paste0("First Year\nn = ", n_1) #text string for xlabel including sample size
n_2 <-  sum(with(df, year ==2 & CAT.Score > 1), na.rm = TRUE)     
year2 <- paste0("Second Year\nn = ", n_2) #text string for xlabel
n_3 <-  sum(with(df, year == 3 & CAT.Score > 1), na.rm = TRUE)     
year3 <- paste0("Third Year\nn = ", n_3) #text string for xlabel
n_4 <-  sum(with(df, year == 4 & CAT.Score > 1), na.rm = TRUE)     
year4 <- paste0("Fourth Year\nn = ", n_4) #text string for xlabel

graph_title <- paste0(prog_name, " CAT Scores")

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
  labs(title = graph_title, x = "Year", y = "CAT Score")  +
  theme(
    panel.border = element_rect(colour = "grey", fill = NA), #add border around graph
    panel.grid.major.y = element_line("grey"), #change horizonatal line colour (from white)
    panel.background = element_rect("white"), #change background colour
    axis.title.x = element_blank(), # remove x axis title
    axis.text.x = element_text(size = 12) #size of x axis labels
  )
