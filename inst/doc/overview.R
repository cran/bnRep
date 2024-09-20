## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(bnRep)

## ----eval = FALSE-------------------------------------------------------------
#  # Install stable version from CRAN:
#  install.packages("bnRep")
#  
#  # Or the development version from GitHub:
#  remotes::install_github("manueleleonelli/bnRep")

## ----message=F, out.width="50%"-----------------------------------------------
library(bnRep)
library(bnlearn)
data("lawschool")
qgraph::qgraph(bn.net(lawschool))

## ----echo=F-------------------------------------------------------------------
colnames(bnRep_summary)

## ----echo=F,message= F, out.width="50%"---------------------------------------
library(ggplot2)
library(dplyr)
library(stringr)
library(scales)
library(RColorBrewer)

# Assuming bnRep_summary is your data frame and Type is a factor column
# Creating the barplot with percentages on the Y-axis and labels on the bars
bnRep_summary %>%
  count(Type) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Type, y = perc, fill = Type)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(perc, 1), "%")), vjust = -0.5, size = 5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +
  labs(title = "Bayesian networks by type",
       x = "Type",
       y = "Percentage") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Pastel1")

## ----echo=F,message= F, out.width="50%"---------------------------------------
bnRep_summary %>%
  count(Structure) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Structure, y = perc, fill = Structure)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(perc, 1), "%")), vjust = -0.5, size = 5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 50)) +
  labs(title = "Bayesian networks by structure definition",
       x = "Type",
       y = "Percentage") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Pastel1")


## ----echo=F,message= F, out.width="50%"---------------------------------------
bnRep_summary %>%
  count(Probabilities) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Probabilities, y = perc, fill = Probabilities)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(perc, 1), "%")), vjust = -0.5, size = 5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 60)) +
  labs(title = "Bayesian networks by probabilities definition",
       x = "Type",
       y = "Percentage") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Pastel1")


## ----echo=F,message= F, out.width="50%"---------------------------------------
bnRep_summary %>%
  count(Graph) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Graph, y = perc, fill = Graph)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(perc, 1), "%")), vjust = -0.5, size = 5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 70)) +
  labs(title = "Bayesian networks by graph type",
       x = "Type",
       y = "Percentage") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Pastel1")


## ----echo=F,message= F, out.width="50%"---------------------------------------
custom_colors <- colorRampPalette(RColorBrewer::brewer.pal(9, "Pastel1"))(length(unique(bnRep_summary$Area)))


bnRep_summary %>%
  count(Area) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Area, y = perc, fill = Area)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(perc, 1), "%")), vjust = -0.5, size = 3.5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 25)) +
  labs(title = "Bayesian networks by academic area",
       x = "Type",
       y = "Percentage") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 60, hjust = 1),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_manual(values = custom_colors) 


## ----echo=F,message= F, out.width="50%"---------------------------------------
bnRep_summar <- bnRep_summary
bnRep_summar <- bnRep_summar %>%
  mutate(Year = ifelse(Year <= 2019, "2019 and earlier", as.character(Year)))

# Convert Year into a factor for plotting
bnRep_summar$Year <- factor(bnRep_summar$Year, levels = c("2019 and earlier", sort(unique(bnRep_summar$Year[bnRep_summar$Year != "2019 and earlier"]))))


bnRep_summar %>%
  count(Year) %>%
  mutate(perc = n / sum(n) * 100) %>%
  ggplot(aes(x = Year, y = perc, fill = Year)) +
  geom_bar(stat = "identity", width = 0.7, color = "black", show.legend = FALSE) +
  geom_text(aes(label = paste0(round(perc, 1), "%")), vjust = -0.5, size = 5, color = "black") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 35)) +
  labs(title = "Bayesian networks by year",
       x = "Type",
       y = "Percentage") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Pastel1")


## ----echo=F,message= F, out.width="50%"---------------------------------------
unique_journals_df <- bnRep_summary %>%
  distinct(Reference, .keep_all = TRUE) %>%
  group_by(Journal) %>%
  filter(n() >= 3) %>%
  ungroup()

unique_journals_df <- unique_journals_df %>%
  mutate(Journal = stringr::str_replace_all(Journal, "\\s", "\n")) 

# Create a barplot with counts instead of percentages
unique_journals_df %>%
  count(Journal) %>%
  ggplot(aes(x = Journal, y = n, fill = Journal)) +  # y = n for counts
  geom_bar(stat = "identity", width = 0.7, color = "black", show.legend = FALSE) +
  geom_text(aes(label = n), vjust = -0.5, size = 5, color = "black") +  # Show counts on top of bars
  labs(title = "Bayesian Networks by Journal (having at least 3)",
       x = "Journal",
       y = "Count") +
  theme_minimal(base_size = 15) +
theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # No rotation, centered
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Pastel1")+
  scale_y_continuous(limits = c(0, 7))



## ----echo = F, out.width="50%"------------------------------------------------


# Create the histogram with log10 scale on the x-axis
ggplot(bnRep_summary, aes(x = Nodes)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +  # Customize the appearance
  scale_x_log10() +  # Set x-axis to log10 scale
  labs(title = "Distribution of Nodes (Log10 Scale)",
       x = "Number of Nodes (log10 scale)",
       y = "Count") +
  theme_minimal(base_size = 15) +  # Use a clean theme with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center the title
    axis.title.x = element_text(face = "bold"),  # Make x-axis title bold
    axis.title.y = element_text(face = "bold"),  # Make y-axis title bold
    panel.grid.major = element_line(color = "gray80"),  # Customize grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines for a cleaner look
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle the x-axis labels for better readability
  ) +
  scale_fill_brewer(palette = "Set2")  # Use a custom color palette



## ----echo =F, out.width="50%"-------------------------------------------------
ggplot(bnRep_summary, aes(x = Arcs)) +
  geom_histogram(binwidth = 0.1, fill = "skyblue", color = "black", alpha = 0.7) +  # Customize the appearance
  scale_x_log10() +  # Set x-axis to log10 scale
  labs(title = "Distribution of Arcs (Log10 Scale)",
       x = "Number of Arcs (log10 scale)",
       y = "Count") +
  theme_minimal(base_size = 15) +  # Use a clean theme with larger base text size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center the title
    axis.title.x = element_text(face = "bold"),  # Make x-axis title bold
    axis.title.y = element_text(face = "bold"),  # Make y-axis title bold
    panel.grid.major = element_line(color = "gray80"),  # Customize grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines for a cleaner look
    axis.text.x = element_text(angle = 45, hjust = 1)  # Angle the x-axis labels for better readability
  ) +
  scale_fill_brewer(palette = "Set2") 

