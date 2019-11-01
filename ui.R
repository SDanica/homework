library(visNetwork)
library(shiny)
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("《学习分析课程》期末汇报"),
    sidebarLayout(
        sidebarPanel(
            textOutput('text'),
            
            conditionalPanel(
                condition = "input.tabs == 'social network analysis'",
                selectInput('week', 'choose week your like to show:', 
                            c("week01" =1,
                              'week02' =2,
                              'week03' =3)),
                selectInput('info_level', 'choose levels your like to show:', 
                            c("node_lev" =1,
                              'net_lev' =2)),
                tableOutput('ceshi')
            ),
            conditionalPanel(
                condition = "input.tabs == 'sequential analysis'",
                selectInput('plotType', 'choose your like to with:',
                            c("static" =11,
                              'dynamic' =22)),
                tableOutput('d_sa')
                #put a tableoutput
                
            ),
            ## new codes##
            conditionalPanel(
                condition = "input.tabs == 'time-series analysis'",
                br(),
                p('IEI refers to individual liberation, a detailed explanation at the individual level.'),
                p("GEL refers to group elaboration, a detailed explanation at the group level.")
            
                
        )

    ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(id='tabs',
                tabPanel("social network analysis",id ='sna',visNetworkOutput("sna",height = '700px')),
                tabPanel("sequential analysis",id='sa',visNetworkOutput("sa",height = '700px')),
                tabPanel("time-series analysis",id='ca',plotOutput("ca",height = '700px')),
                tabPanel("terms analysis",id='term',visNetworkOutput("term",height = '700px'))

            )
            )
        )
    )
)


