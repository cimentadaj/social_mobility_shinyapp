library(shiny)
library(tidyverse)
library(GGally)
library(gridExtra)
library(artyfarty)

all_data <- read_csv("./dataset.csv", col_names = T)

server <- # Define server logic required to draw a histogram
    shinyServer(function(input, output, session) {
        
        filtered <- reactive({
            if (is.null(input$country)) {
                return(NULL)
            }
            
            all_data1 <- 
                all_data %>%
                filter(country == input$country)
            
            all_data1
        })
        
        ineq_graph <- reactive({
            
            if (is.null(filtered())) {
                return(NULL)
            }
            
            dist_pred <-
                filtered() %>%
                ggplot(aes(x = pred, fill = isced)) + 
                geom_density(alpha = .5) +
                geom_rug(data = filter(filtered(), is.na(cogn_label_fac)), aes(x = pred), color = "black", inherit.aes = FALSE) +
                scale_x_continuous(name = NULL, breaks = seq(0, 100, 10), labels = paste0(seq(0, 100, 10), "%"),
                                   lim = c(0, 100)) +
                scale_y_continuous(name = NULL, labels = NULL) +
                scale_fill_manual(name = "", values = c("red", "blue")) +
                ggtitle("Probability of achieving service class") +
                coord_cartesian(expand = FALSE) +
                artyfarty::theme_scientific() +
                theme(legend.position = c(0.95, 0.95))
            
            graph_m <-
                filtered() %>%
                ggparcoord(columns = c(6, 2),
                           groupColumn = 4,
                           alphaLines = 0.2,
                           scale = "globalminmax")
            
            labels <- c("Bottom cognitive \n Top non cognitive",
                        "Top cognitive \n Bottom non cognitive",
                        "Bottom cognitive \n Top non cognitive",
                        "Top cognitive \n Bottom non cognitive")
            
            cogn_pred <-
                graph_m +
                scale_x_discrete(name = NULL, labels = NULL) +
                scale_y_continuous(name = NULL,
                                   limits = c(0, 100),
                                   breaks = c(10, 30, 70, 90),
                                   labels = labels) +
                scale_colour_manual(guide = F, values = c("red", "blue")) +
                theme(panel.background = element_rect(fill = "white"),
                      axis.ticks = element_blank(),
                      axis.line.x = element_line(colour = "black")) +
                coord_flip(expand = FALSE) +
                artyfarty::theme_scientific() +
                theme(axis.text.x = element_text(size = 13))
            
            grid.arrange(dist_pred, cogn_pred, ncol=1, nrow=2, widths = 10, heights = c(2, 4))
        })
        
        output$graph <- renderPlot({
            ineq_graph()
        })
    })

ui <- bootstrapPage(
    selectInput('country', 'Choose country:', unique(all_data$country)),
    plotOutput('graph', width = 950, height = 700)
)


shinyApp(ui = ui, server = server)