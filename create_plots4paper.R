# --------------------------------------------------------------------------------------

# INPUT: 


# OUTPUT:

# Author: Lisa Schneider
# Date:
# --------------------------------------------------------------------------------------

rm(list=ls())
library(hrbrthemes)
library(tidyverse)
library(viridis)
library(skimr)
library(readxl)
library("cowplot") 
library(DiagrammeR)
library(DiagrammeRsvg)

# plotPath <- "../../20.Data/project4_dataPaperTrophy/figures"

# to match data against WoRMS ---------------------------------------------

# library(taxize)
# 
# result <- classification(speciesList$AphiaID, db = "worms", return_id = F)
# 
# taxonHierarchy <- result %>% 
#   map(spread, rank, name) %>%
#   bind_rows(.id = "AphiaID")

# load data ---------------------------------------------------------------

load("../../20.Data/project4_dataPaperTrophy/Schneider2020_trophicModesData.Rdata")

# rename to plotdata  -----------------------------------------------------

dataLevels <- c("book", "scientific cruise", "review", "paper", "routineMonitoring")

plotData <- p24List %>% 
  mutate(dataType = factor(dataType, levels = dataLevels))

# plot figure 1  ----------------------------------------------------------
brks <- c(0, 0.25, 0.5, 0.75, 1)
cexV <-  20

# 1a
p1a <- ggplot(plotData, aes(dataType)) +
  geom_bar(mapping = aes(y = ..prop.., group = 1), fill = "black") +
  scale_y_continuous(limits = c(0, 1), breaks = brks, labels = scales::percent(brks)) +
  ylab("of total dataset") +
  xlab("data origin") +
  scale_fill_viridis_d(direction = -1) +
  theme_minimal() + 
  coord_flip() +
  theme(axis.title.x = element_text(size = cexV),
        axis.title.y = element_text(size = cexV),
        axis.text = element_text(size = cexV)) 

# 1b
p1b <- plotData %>% 
  mutate(dataType = as.factor(dataType),
         Trophy = as.factor(Trophy)) %>% 
  ggplot(aes(x = dataType, fill = Trophy)) +
  geom_bar(position="fill") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  ylab("of each data origin") +
  xlab("data origin") +
  scale_fill_viridis_d(direction = -1) +
  theme_minimal() + 
  coord_flip() + 
  theme(axis.title.x = element_text(size = cexV),
        axis.title.y = element_text(size = cexV),
        axis.text = element_text(size = cexV),
        legend.text = element_text(size = cexV),
        legend.title = element_text(size = cexV),
        legend.key.height = unit(0.5, "cm"), 
        legend.key.width = unit(3, "cm"),
        legend.margin = margin(0.7, 0.2, -0.8, 0.2, "cm")) 

p <- plot_grid(p1a, p1b, ncol = 2, labels = c("a)", "b)"))

p

# ggsave("figure1.png", p, path = plotPath,
#        width = 600, height = 250, units = "mm")

# Figure 3 ----------------------------------------------------------------

workflow <- "digraph {

graph [layout = dot, rankdir = TB]

# define the global styles of the nodes. We can override these in box if we wish
node [shape = rectangle, style = filled, fillcolor = Linen]

data1 [label = 'Dataset 1', shape = folder, fillcolor = Beige]
data2 [label = '...', shape = folder, fillcolor = Beige]
data3 [label = 'Dataset N', shape = folder, fillcolor = Beige]
worms [label =  'Match \n against WoRMS']
dataAp1 [label = 'Dataset 1 \n with AphiaID', shape = folder, fillcolor = Beige]
dataAp2 [label = '...', shape = folder, fillcolor = Beige]
dataAp3 [label = 'Dataset N \n with AphiaID', shape = folder, fillcolor = Beige]
join [label = 'Join datasets \n via AphiaID']
trophy [label= 'assign trophic mode']
option1 [label= 'datasets diagree \n on trophic mode']
option1_1 [label= 'phytoplankton \n vs. \n mixoplankton']
option1_1a [label= 'assign mixoplankton']
option1_2 [label= 'protozooplankton \n vs. \n mixoplankton']
option1_2a [label= 'assign mixoplankton']
option1_3 [label= 'phytoplankton \n vs. \n protozooplankton']
option1_3a [label= 'consult experts']
option2 [label= 'datasets agree \n on trophic mode']
option2_1 [label= 'assign trophic mode']
dataTot [label = 'Joined dataset \n with AphiaIDs \n and trophic mode', shape = folder, fillcolor = Beige]

# edge definitions with the node IDs
{data1 data2 data3} -> worms -> {dataAp1 dataAp2 dataAp3} -> join -> trophy -> {option1 option2}
option1 -> option1_1 -> option1_1a
option1 -> option1_2 -> option1_2a
option1 -> option1_3 -> option1_3a
option2 -> option2_1
{option1_1a option1_2a option1_3a option2_1} -> dataTot

}"

grViz(workflow) %>%
  export_svg %>% charToRaw %>% rsvg_png(paste0(plotPath, "/figure3.png"))


# Figure 2 ----------------------------------------------------------------

# 2
p2 <- ggplot(plotData, aes(Trophy)) +
  geom_bar(mapping = aes(y = ..prop.., group = 1), fill = "black") +
  scale_y_continuous(limits = c(0, 1), breaks = brks, labels = scales::percent(brks)) +
  ylab("of total dataset") +
  scale_fill_viridis_d(direction = -1) +
  theme_minimal()+
  theme(axis.title.x = element_text(size = cexV),
        axis.title.y = element_text(size = cexV),
        axis.text = element_text(size = cexV)) 

p2

ggsave("figure2.png", p4a, path = plotPath,
       width = 600, height = 250, units = "mm")

# Figure 5 ----------------------------------------------------------------
brks2 <- c(0, 0.1, 0.2, 0.3, 0.4)

# 5a
fig5dataRaw <-plotData %>% 
  filter(Class != "NA") 

# select only Class that contribute to the 98th percentile
fig5adata <- fig5dataRaw %>% 
  count(Class) %>% 
  mutate(perc = n / nrow(fig5dataRaw)) %>% 
  arrange(desc(perc)) %>% 
  mutate(cumsum = round(cumsum(perc), digits = 2)) %>% 
  filter(cumsum <= 0.90) %>%
  arrange((perc))

phylumOrder <- as.factor(fig5adata$Class)

p5a <- ggplot(fig5adata, aes(x = reorder(Class, perc), y = perc)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = n, hjust = -0.3), size = 8) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 0.4), breaks = brks2, labels = scales::percent(brks2)) +
  ylab("of total datatset") +
  xlab("Class") +
  scale_fill_viridis_d(direction = -1) +
  theme_minimal()+
  theme(axis.title.x = element_text(size = cexV),
        axis.title.y = element_text(size = cexV),
        axis.text = element_text(size = cexV)) 

p5a

# 5b
fig5bData <- fig5dataRaw %>% 
  filter(Class %in% phylumOrder) %>% 
  mutate(Class = factor(Class, levels = phylumOrder)) 

p5b <- ggplot(fig5bData, aes(x = Class, fill = Trophy)) + 
  geom_bar(position="fill") +
  scale_y_continuous(breaks = brks, labels = scales::percent(brks)) +
  ylab("of each class") +
  scale_fill_viridis_d(direction = -1) +
  theme_minimal() + 
  coord_flip()  + 
  theme(axis.title.x = element_text(size = cexV),
        axis.title.y = element_text(size = cexV),
        axis.text = element_text(size = cexV),
        legend.text = element_text(size = cexV),
        legend.title = element_text(size = cexV),
        legend.key.height = unit(0.5, "cm"), 
        legend.key.width = unit(3, "cm"),
        legend.margin = margin(0.7, 0.2, -0.8, 0.2, "cm")) 

p <- plot_grid(p5a, p5b, ncol = 2, labels = c("a)", "b)"))

p

ggsave("figure5.png", p, path = plotPath,
       width = 600, height = 250, units = "mm")
