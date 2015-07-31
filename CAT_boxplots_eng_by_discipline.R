# engineering CAT boxplots by discipline

source("setup_CAT_eng_disciplines.R")


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
year1 <- paste0("First Year\nn = ", n_1, "   n = ", n_eng_1) #text string for xlabel including sample size
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
  scale_x_discrete(labels = c(year1, year2, year3, year4)) + #text strings from above with sample sizes
  labs(title = graph_title, x = "Year", y = "CAT Score")  +
  scale_fill_manual(
    values =  c("darkgoldenrod1", "steelblue3"),
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
