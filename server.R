
sna_data <- read.csv('all_code.csv',stringsAsFactors = FALSE)
# generate vertex color
generateVcolor2 <- function(vertexObjects){
    colordataset <- c()
    for (i in vertexObjects) {
        if(i<=12)
            red <- max(255-i*20,0)
        else
            red <- i
        cr <- rgb(red=red,green=125,blue=125,max=255)
        colordataset <- append(colordataset,cr)
    }
    return(colordataset)
}

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    # load required packages
    require(tidyverse)
    require(igraph)
    require(visNetwork)
    require(LagSeq)
    require(dplyr)
    require(ggplot2)
    require(tidytext)
    
    # decide which interactive type should be displayed.
    output$sna <- renderVisNetwork({
        if (input$week == 1) {
            sna_da <- sna_data %>% filter(week == 'Week 10') %>% select(c('vert1_id','vert2_id'))
            graph_san_da <- graph_from_data_frame(sna_da)
            vis_sna_da <- toVisNetworkData(graph_san_da)
            sna_degree <- degree(graph_san_da)
            vis_sna_da$nodes$color <- generateVcolor2(as.integer(sna_degree))
            vis_sna_da$nodes$shape <- 'circle'
            visNetwork(vis_sna_da$nodes,vis_sna_da$edges) %>%
                visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 1, type = 'arrow'))) %>%
                visOptions(highlightNearest = list(degree=1,hover=T),nodesIdSelection = TRUE)
        }
        else if (input$week == 2) {
            sna_da <- sna_data %>% filter(week == 'Week 11') %>% select(c('vert1_id','vert2_id'))
            graph_san_da <- graph_from_data_frame(sna_da)
            vis_sna_da <- toVisNetworkData(graph_san_da)
            sna_degree <- degree(graph_san_da)
            vis_sna_da$nodes$color <- generateVcolor2(as.integer(sna_degree))
            vis_sna_da$nodes$shape <- 'circle'
            visNetwork(vis_sna_da$nodes,vis_sna_da$edges) %>%
                visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 1, type = 'arrow'))) %>%
                visOptions(highlightNearest = list(degree=1,hover=T),nodesIdSelection = TRUE)
        }
        else {
            sna_da <- sna_data %>% filter(week == 'Week 12') %>% select(c('vert1_id','vert2_id'))
            graph_san_da <- graph_from_data_frame(sna_da)
            vis_sna_da <- toVisNetworkData(graph_san_da)
            sna_degree <- degree(graph_san_da)
            vis_sna_da$nodes$color <- generateVcolor2(as.integer(sna_degree))
            vis_sna_da$nodes$shape <- 'circle'
            visNetwork(vis_sna_da$nodes,vis_sna_da$edges) %>%
                visEdges(arrows = list(to = list(enabled = TRUE, scaleFactor = 1, type = 'arrow'))) %>%
                visOptions(highlightNearest = list(degree=1,hover=T),nodesIdSelection = TRUE)
        }

        
    })
    output$sa <- renderVisNetwork({

        
        sa_data <- read.csv('sa_data.csv')
        sa_data <- as.vector(sa_data$code)
        #transfer character code to numberic code
        sa_code_data <- case_when(sa_data == 'SKI'~1,sa_data=='MKI'~2,sa_data=='DKI'~3,sa_data=='SKC'~4,sa_data=='MKC'~5,sa_data=='DKC'~6)
        sa_freq <- as.matrix(LagSeq(sa_code_data)$freq)
        #build graph data 
        sa_graph_freq_data <- graph_from_adjacency_matrix(sa_freq,weighted = TRUE)
        #the following edges' width be larger than we expected.
        E(sa_graph_freq_data)$width<-c(6,3,3,2,42,83,8,21,3,58,125,14,55,1,1,11,5,20,45,4,3,35,35,41,100,25,2,4,5,4,15,7)
        # we change the width of edges.
        E(sa_graph_freq_data)$width <- E(sa_graph_freq_data)$width/20
        size=c(12,158,255,87,239,36)
        plot(sa_graph_freq_data, edge.arrow.size=0,edge.curved=.5,
             layout=layout_in_circle,
             vertex.label=c("SKI","MKI","DKI","SKC","MKC","DKC"),
             vertex.frame.color="grey38",vertex.label.color="black",vertex.color="slateblue2",
             vertex.size=size^0.5/0.7)
        #the igraph data structure can't be used by visNetwork directly.
        #transform igraph data to visNetwork data
        sa_vis_graph_data <- toVisNetworkData(sa_graph_freq_data)
        sa_vis_graph_data$nodes$shape <- 'circle'
        
        if(input$plotType==11){
            sa_vis_graph_data$nodes$color <- generateVcolor2(as.integer(degree(sa_graph_freq_data)))
            visNetwork(sa_vis_graph_data$nodes,sa_vis_graph_data$edges,smooth=TRUE) %>%
                visEdges(arrows = list(to=list(enable=TRUE,type='arrow')))
        }
        else {
            visNetwork(sa_vis_graph_data$nodes,sa_vis_graph_data$edges,smooth=TRUE) 
        }
    })
    
    output$ca <- renderPlot({
        #load dataset for time analysis
        contra_data <- read.csv('df8.csv')
        #指定时间的先后时序
        contra_data$sortedid <- 1:nrow(contra_data)
        #transform the code column to factor type
        contra_data$code <- factor(contra_data$code,
                                   levels = c("IEx", "IEl", "IQE", "IQR", "IPA","GEx", "GEl-1","GEl-2","GQE", "GQR", "GPA","IML", "GML","ISP", "GSI-1","GSI-2"),
                                   labels = c("IEx", "IEl", "IQE", "IQR", "IPA","GEx", "GEl-1","GEl-2","GQE", "GQR", "GPA","IML", "GML","ISP", "GSI-1","GSI-2"))
        contra_data_levels <- factor(contra_data$code)
        ##plot with ggplot2##
        ggplot(contra_data)+
            geom_point(aes(contra_data$sortedid,contra_data$code,shape=contra_data$code,color=contra_data$student))+
            scale_shape_manual(values = 1:length(contra_data_levels))+
            xlab('Discussion threads')+
            ylab('Coding categories')
    })
    
    output$term <- renderVisNetwork({
        text_message_person <- as_tibble(sna_data[1:3])
        text_person_words <- text_message_person[c('vert1_id','message_text')]
        names(text_person_words) <- c('learners','messages')

        text_person_words <- text_person_words %>%
            unnest_tokens(bigram,messages,token = 'ngrams',n=2)

        text_person_words <- text_person_words %>%
            separate(bigram,c('wd1','wd2'),sep=' ') %>%
            count(learners,wd1,wd2,sort = TRUE)

        text_person_words <- text_person_words %>%
            filter(!wd1 %in% stop_words$word)%>%
            filter(!wd2 %in% stop_words$word)

        text_person_words_count2 <- text_person_words %>%
            filter(n>=3)

        text_person_words_unite <- text_person_words_count2 %>%
            unite('words',wd1:wd2,sep=' ')

        graph_person_words <- graph_from_data_frame(text_person_words_unite)

        graph_person_words_vis <- toVisNetworkData(graph_person_words)
        #color
        graph_person_words_color <- generateVcolor2(
            as.integer(degree(graph_person_words))
        )
        graph_person_words_vis$nodes$color <- graph_person_words_color
        graph_person_words_vis$nodes$shape <- 'circle'
        visNetwork(graph_person_words_vis$nodes,graph_person_words_vis$edges) %>%
            visEdges(arrows = list(to = list(enabled = TRUE, type = 'arrow'))) %>%
            visOptions(highlightNearest = list(degree=1,hover=T),nodesIdSelection = TRUE)
    })
    
    output$text <- reactive({
        if (input$tabs == 'social network analysis') {
            print("this section introduce a interactive relationship named 'sna',which is social network analysis")
        }
        else if(input$tabs == 'sequential analysis'){
            print("this section denote sequential analysis among learning process.")
        }
        else if(input$tabs == 'time-series analysis'){
            print("this part show what something occurs in learning community among all learners,as time goes on.")
        }
        else {
            print("this part show some meaningful things behind the learning context contents.")
        }
    })
    
    output$ceshi <- renderTable({
        if (input$week == 1) {
            if (input$info_level==1) {
                as.table(deg_10)
            } else {
                as.table(hub_10)
            }
            
        }
        else if (input$week == 2) {
            if (input$info_level==1) {
                as.table(deg_11)
            } else {
                as.table(hub_11)
            }
        }
        else{
            if (input$info_level==1) {
                as.table(deg_12)
            } else {
                as.table(hub_12)
            }
        }
        
    })
    
    output$d_sa <- renderTable({
        as.table(deg_sa)
    })

})
