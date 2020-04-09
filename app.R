library(shiny)
library(tidyverse)
library(grid)
library(egg)
library(RColorBrewer)
library(ggplotify)

source("covid.R")
maxNStates <- 6
thm <- theme(panel.background = element_rect(color='grey', fill=NA),
             panel.grid=element_line(linetype='dotted', color='lightgrey'),
             strip.background = element_rect(fill=NA,color='grey'),
             strip.text = element_text(size=14, color='black'),
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
  xs <- seq(trunc(x[1]), x[2], length.out=8)
  xs10 <- 10^xs
  return(unique(log10(ifelse(xs10 < 1, signif(xs10,1),round(xs10)))))
}
logLabels <- function(x) {
  if (max(x, na.rm=T) < 1){
    return(
      format(signif(10^x, 3), format='f', scientific=FALSE, drop0trailing = TRUE)
    )
  } else {
    return(c(prettyNum(
      ifelse(x < 1, 10^x, round(10^x)), digits=3, big.mark=','))) 
  }
}
rawBreaks <- function(x){
  nz <- trunc(log10(x[2]))
  if (nz <1 ){
    ret <- signif(seq(0, signif(x[2],1), length.out=6), 2)
  } else {
    maxSeq <- x[2] %/% (10^nz) * (10^nz)
    incr <- 10^ nz / 2
    ret <- c(seq(0, maxSeq, by=incr),
             signif(x[2], trunc(log10(x[2]))))
  }
  if (length(ret) > 8) {
    ret <- ret[unique(seq(1,length(ret),2),length(ret))]
  }
  return(ret)
} 
rawLabels <- function(x) return(prettyNum(x, big.mark=','))
fadd <- rep(c(1,1.5,2),each=3)

# Define UI for application that draws a histogram
ui <- # Use a fluid Bootstrap layout
  fluidPage(    
    
    # Give the page a title
    #titlePanel("Covid-19 Cases by County"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
      
      # Define the sidebar with one input
      sidebarPanel(
        selectInput("response", "Plot:", 
                    choices = respOptions,
                    selected='New cases per 1,000 people'),
        
        hr(),
        radioButtons("yscale", "Scale",
                     c("Linear", "Logarithmic"), inline = TRUE),
        hr(),
        dateRangeInput('dateRange',
                       label = 'Date range:',
                       start = Sys.Date() - 30,
                       min = min(dc$date) , 
                       end = max(dc$date),
                       max = max(dc$date)
        ),
        hr(),
        selectizeInput("state", "Select states:", 
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
                "Visualized with",
                a("Shiny.", href="https://shiny.rstudio.com/", target="_blank"))
      ),
      
      # Create a spot for the barplot
      mainPanel(
        plotOutput("thePlot", height='750px', width='800px')
        )
      
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Fill in the spot we created for a plot
  output$thePlot <- renderPlot({
    ndays <- input$dateRange[2] - input$dateRange[1]
    xscale <- scale_x_date(date_labels = "%b %d",
                           date_minor_breaks="1 day",
                           breaks = seq(input$dateRange[1], input$dateRange[2],
                                        length.out=ifelse(ndays < 3, 2,
                                                          ifelse(ndays < 8, 3, 5))),
                           limits= c(input$dateRange[1], input$dateRange[2] +
                                       ifelse(ndays < 5, 2,
                                              ifelse(ndays < 9, 3,
                                                     ifelse(ndays < 20, 3, 6)))))
    nstates <- length(input$state)
    
    xscale_facet <- scale_x_date(date_labels = "%b %d",
                                           date_minor_breaks="2 days",
                                           breaks = seq(input$dateRange[1], input$dateRange[2],
                                                        length.out=ifelse(ndays < 3, 2, 3)),
                                           limits= c(input$dateRange[1], input$dateRange[2] +
                                                       ifelse(ndays < 5, 2,
                                                              ifelse(ndays < 9, 5,
                                                                     ifelse(ndays < 20, 5, 7)))*fadd[nstates]))
    stateColScale <- scale_color_manual(values = 
                                          c(setNames(brewer.pal(maxNStates, 
                                                       'Set1'), 
                                                     c(input$state,rep('',maxNStates-nstates))),
                                                 'other'='grey'),
                                        breaks=input$state,
                                        name='') 
    plds <- ds %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2]) %>%
      mutate(stateCut = ifelse(state %in% input$state, state, 'other'))
    pldsText <-
      plds %>% group_by(state) %>% filter(date==max(date)) %>% ungroup
    pldc <- dc %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
    if (!is.null(input$state)){
      pldc <- filter(pldc, state %in% input$state)
    } 
    pldcText <-
      pldc %>% group_by(countystate) %>% 
      filter(date==max(date)) %>% ungroup
      
    if (input$yscale == 'Logarithmic'){
      respvar <- lrespVarMap[input$response]
      yscale <- scale_y_continuous(breaks=logBreaks, labels=logLabels)
      pldcText$labstate <-
        paste(str_replace(as.character(pldcText$countystate), ', ', ',\n'),
              ": ", prettyNum(10^pldcText[[respvar]],
                              digits=2, big.mark=','), sep='')
      pldcText$lab_nostate <-
        paste(pldcText$county, ": ", prettyNum(10^pldcText[[respvar]],
                              digits=2, big.mark=','), sep='')
      pldsText$lab <-
        paste(pldsText$state, ': ',
              prettyNum(10^pldsText[[respvar]], digits=2, big.mark=','),
              sep='')
    } else {
      respvar <- respVarMap[input$response]
      yscale <- scale_y_continuous(breaks=rawBreaks, labels=rawLabels)
      pldcText$labstate <-
        paste(str_replace(as.character(pldcText$countystate), ', ', ',\n'),
              ": ", prettyNum(pldcText[[respvar]],
                              digits=2, big.mark=','), sep='')
      pldcText$lab_nostate <-
        paste(pldcText$county, ": ", prettyNum(pldcText[[respvar]],
                              digits=2, big.mark=','), sep='')
      pldsText$lab <-
        paste(pldsText$state, ': ',
              prettyNum(pldsText[[respvar]], digits=2, big.mark=','),
              sep='')
    } 
    
    ps <- 
      plds %>%
      ggplot(aes_string('date', respvar,
                        group = 'state')) + yscale
    pc <- 
      pldc %>%
      ggplot(aes_string('date', respvar, 
                        group = 'countystate')) + yscale
    if (is.null(input$state)) {
      pc <- pc + geom_line() +
        geom_text(aes_string('date', respvar,
                             label='labstate'),
                  hjust=0,vjust=0.5,size=3,
                  check_overlap = TRUE,
                  data=pldcText %>% top_n(2, !!sym(respvar)))
      ps <- ps + geom_line() +
        geom_text(aes_string('date',respvar,
                             label='lab'),
                  check_overlap = TRUE,
                  hjust=0, vjust=0.5, size=3.5,
                  data = top_n(pldsText, 4, !!sym(respvar)))
    } else {
      pc <- pc +
        geom_line(aes(color=state)) +
        geom_text(aes_string('date',respvar,
                             label='lab_nostate'),
                  hjust=0, vjust = 0.5, size=3.1,
                  check_overlap = TRUE,
                  data=pldcText %>% group_by(state) %>% top_n(2, !!sym(respvar))) +
        stateColScale + 
        guides(list(color='none'))
      ps <- ps +
        geom_line(aes(color = stateCut, alpha=(stateCut=='other'),
                      size = (stateCut == 'other'))) +
        geom_text(aes_string('date',respvar,
                             label='lab', color='stateCut'),
                  hjust=0,vjust=0.5,size=3.5,check_overlap = TRUE,
                  data = filter(pldsText, state %in% input$state) %>%
                    top_n(4, !!sym(respvar))) +
        stateColScale + scale_alpha_manual(breaks=c('TRUE','FALSE'),
                                            values=c(1, 1/8)) +
        scale_size_manual(breaks=c('TRUE','FALSE'),
                            values=c(1.5, 0.8)) +
        guides(list(color=guide_legend(override.aes=list(size=3),
                                       nrow=3, ncol=3, sbyrow=T),
                    alpha='none',size='none'))
    }
    pc <- pc +
      ggtitle(ifelse(is.null(input$state), "All US Counties", 
                     "Counties in...")) +
      thm + xlab("") + ylab(paste(respLabels[input$response], ' ',sep='')) 
    if (!is.null(input$state)) {
      pc <- pc + facet_wrap(~state, scales='fixed', nrow=3) + xscale_facet
    } else { 
      pc <- pc + xscale
    }
    ps <- ps + ggtitle("States") + thm + xscale + xlab("") +
      ylab(paste(respLabels[input$response], ' ', sep=''))
    as.ggplot(ggarrange(ps,pc,
                        nrow=2, heights=c(1,2)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

