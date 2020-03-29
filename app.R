library(shiny)
library(tidyverse)
library(grid)
library(egg)
library(RColorBrewer)
library(ggplotify)

source("covid.R")
maxNStates <- 9
thm <- theme(panel.background = element_rect(color='grey', fill=NA),
             panel.grid=element_line(linetype='dotted', color='lightgrey'),
             strip.background = element_rect(fill=NA,color='grey'),
             strip.text = element_text(size=14,color='black'),
             axis.text = element_text(size=11, color='black'),
             axis.title.y = element_text(angle=0, size=12, color='black',
                                         vjust=0.66),
             plot.title = element_text(size=16, color='black'),
             legend.position=c(0,1),
             legend.background = element_rect(fill=NA),
             legend.justification = c(-0.1,0.8),
             legend.direction='vertical',
             legend.text = element_text(size=11,color='black'),
             legend.key = element_rect(fill=NA))
logBreaks <- function(x) {
  if (x[2] < 10){
    return(seq(trunc(x[1]), round(x[2],2),length.out=4))
  } else{
    return(seq(trunc(x[1]), trunc(x[2])+1)) 
  }
}
logLabels <- function(x) {
  if (max(x, na.rm=T) < 1){
    return(formatC(10^x, digits=3, format='f'))
  } else {
    return(c(prettyNum(
      ifelse(x < 1, 10^x, round(10^x)), digits=3, big.mark=','))) 
  }
}
rawBreaks <- function(x) return(seq(0, x[2] %/% (10^trunc(log10(x[2]))) * (10^trunc(log10(x[2]))),
                                    by=10^(trunc(log10(x[2])))/2))
rawLabels <- function(x) return(prettyNum(x,big.mark=','))


# Define UI for application that draws a histogram
ui <- # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    titlePanel("Covid-19 Cases by County"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("response", "Plot:", 
                    choices = respOptions),
        
        hr(),
        radioButtons("yscale", "Scale",
                     c("Linear", "Logarithmic"), inline = TRUE),
        hr(),
        dateRangeInput('dateRange',
                       label = 'Date range:',
                       start = min(dc$date) , 
                       end = max(dc$date)
        ),
        hr(),
        selectizeInput("state", "State:", 
                    choices = sort(statenames),
                    multiple = TRUE, selected=NULL,
                    options=list(maxItems=maxNStates)),
        hr(),
        helpText('Covid-19 data from',
                 a("The New York Times",
                href="https://www.nytimes.com/interactive/2020/us/coronavirus-us-cases.html",
                target="_blank"),
                "based on reports from state and local health agencies.",
                "County and state populations from the U.S. Census Bureau, American Community Survey.",
                "Visualized by ",
                a("Brook Luers", href="http://www.brookluers.com/", target="_blank"),
                " using ",
                a("Shiny.", href="https://shiny.rstudio.com/", target="_blank"))
      ),
      
      # Create a spot for the barplot
      mainPanel(
        plotOutput("thePlot", height='750px')
        )
      
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Fill in the spot we created for a plot
  output$thePlot <- renderPlot({
    xscale <- scale_x_date(date_labels = "%b %d",
                           date_minor_breaks="1 day",
                           limits=c(input$dateRange[1], input$dateRange[2]))
    stateColScale <- scale_color_manual(values = 
                                          c(setNames(brewer.pal(maxNStates, 
                                                       'Set1'), 
                                                     c(input$state,rep('',maxNStates-length(input$state)))),
                                                 'other'='grey'),
                                        breaks=input$state,
                                        name='') 
    plds <- ds %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2]) %>%
      mutate(stateCut = ifelse(state %in% input$state, state, 'other'))
    pldc <- dc %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    if (!is.null(input$state)){
      pldc <- filter(pldc, state %in% input$state)
    } 
    if (input$yscale == 'Logarithmic'){
      ps <- 
        plds %>%
        ggplot(aes_string('date', 
                          lrespVarMap[input$response],
                          group = 'state')) + 
        scale_y_continuous(breaks=logBreaks, labels=logLabels)
      pc <- pldc %>%
        ggplot(aes_string('date', 
                          lrespVarMap[input$response], 
                          group = 'countystate')) +
        scale_y_continuous(breaks=logBreaks, labels=logLabels)
    } else {
      ps <- 
        plds %>%
        ggplot(aes_string('date',
                          respVarMap[input$response],
                          group = 'state')) +
        scale_y_continuous(breaks=rawBreaks, labels=rawLabels)
      pc <- 
        pldc %>%
        ggplot(aes_string('date', respVarMap[input$response], 
                          group = 'countystate')) + 
        scale_y_continuous(breaks=rawBreaks,
                           labels=rawLabels)
    } 
    if (is.null(input$state)) {
      pc <- pc + geom_line()
      ps <- ps + geom_line()
    } else {
      pc <- pc +
        geom_line(aes(color=state)) +
        stateColScale + 
        guides(list(color='none'))
      ps <- ps +
        geom_line(aes(color = stateCut, alpha=(stateCut=='other'),
                      size = (stateCut == 'other'))) +
        stateColScale + scale_alpha_manual(breaks=c('TRUE','FALSE'),
                                            values=c(1, 1/8)) +
        scale_size_manual(breaks=c('TRUE','FALSE'),
                            values=c(1.5, 0.8)) +
        guides(list(color=guide_legend(override.aes=list(size=3),
                                       nrow=3,ncol=3,byrow=T),
                    alpha='none',size='none'))
    }
    pc <- pc +
      ggtitle(ifelse(is.null(input$state), "All US Counties", 
                     "Counties in...")) +
      thm + xscale + xlab("") + ylab(paste(respLabels[input$response], ' ',sep='')) 
    if (!is.null(input$state)) {
      pc <- pc + facet_wrap(~state, scales='fixed', nrow=3)
    }
    ps <- ps + ggtitle("States") + thm + xscale + xlab("") +
      ylab(paste(respLabels[input$response], ' ', sep=''))
    as.ggplot(ggarrange(ps,pc,
                        nrow=2, heights=c(1,2)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

