# this file just provide global variables about nodes information or network level information
library(igraph)
require(tidyverse)
require(igraph)
require(visNetwork)
require(LagSeq)
require(dplyr)
require(ggplot2)
require(tidytext)
sna_data <- read.csv('all_code.csv',stringsAsFactors = FALSE)
sna_graph_10 <- graph_from_data_frame(sna_data %>% filter(week == 'Week 10') %>% select(c('vert1_id','vert2_id')))
deg_10 <- degree(sna_graph_10)

sna_graph_11 <- graph_from_data_frame(sna_data %>% filter(week == 'Week 11') %>% select(c('vert1_id','vert2_id')))
deg_11 <- degree(sna_graph_11)

sna_graph_12 <- graph_from_data_frame(sna_data %>% filter(week == 'Week 12') %>% select(c('vert1_id','vert2_id')))
deg_12 <- degree(sna_graph_12)

#hub
hub_10 <- hub_score(sna_graph_10)$vector
hub_11 <- hub_score(sna_graph_11)$vector
hub_12 <- hub_score(sna_graph_12)$vector

# sequential analysis
sa_data <- read.csv('sa_data.csv')
sa_data <- as.vector(sa_data$code)
#transfer character code to numberic code
sa_code_data <- case_when(sa_data == 'SKI'~1,sa_data=='MKI'~2,sa_data=='DKI'~3,sa_data=='SKC'~4,sa_data=='MKC'~5,sa_data=='DKC'~6)
sa_freq <- as.matrix(LagSeq(sa_code_data)$freq)
#build graph data 
sa_graph_freq_data <- graph_from_adjacency_matrix(sa_freq,weighted = TRUE)

deg_sa <- degree(sa_graph_freq_data)
