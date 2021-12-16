library(shiny)
library(ggplot2)
library(GGIR)

rm(list = ls())
source("plot_act_test.R")
source("plot5_test.R")
source("CMI_plot5_test.R")

if(!"timeDate" %in% installed.packages()){
  install.packages("timeDate")
}

# ------------------------------------------------------------------------------
# Define UI for data upload

ui <- fluidPage(
  
  # Application title
  titlePanel(title = h4("CMI Actigraphy Visualization Tool",align="center")),
  
  sidebarLayout(
    sidebarPanel(
      # Input: Select file
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values, text/plain",
                           ".csv")),
      actionButton("save", "Save")
    ),
    
    mainPanel(
      verbatimTextOutput("info")
    )
  ),
  fluidRow(
  plotOutput('MyPlot', brush="plot_brush")
  )
)

# ------------------------------------------------------------------------------
# Define server logic

server <- function(input, output, session) {
  
  # Option to increase the maximum upload size to 100MB
  options(shiny.maxRequestSize=100*1024^2)
  
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath)
  })
  
  output$MyPlot <- renderPlot({
    #if(is.null(input$file1)){return()}
    #req(input$file1)

    load("/Users/nathalia.esper/Documents/Actigraph/output_csv/meta/basic/meta_NDARAA948VFH.csv.RData")
    
    # M = metadata from output_dir/meta/basic/subj.csv.RData
    ws3 = M$windowsizes[1]
    metric = "ENMO"
    desiredtz = "America/New_York"

    # Get variables - activity
    ACC = as.numeric(as.matrix(M$metashort[,metric])) * 1000
    time =  as.character(M$metashort[,1])
    nw_time = as.character(M$metalong[,1])
    if (length(unlist(strsplit(time[1],"T"))) > 1) { # ISO timestamp format
      time = as.character(iso8601chartime2POSIX(time,desiredtz))
      nw_time = as.character(iso8601chartime2POSIX(nw_time,desiredtz))
    }
    time_unclassed = unclass(as.POSIXlt(time,desiredtz))
    sec = time_unclassed$sec
    min_vec = time_unclassed$min
    hour = time_unclassed$hour
    
    # Get variables - sleep
    angle = as.matrix(M$metashort[,which(colnames(M$metashort) == "anglez")])
    
    nightsi = which(sec == 0 & min_vec == 0 & hour == 12)
    xaxislabels = c("noon","2pm","4pm","6pm","8pm","10pm","midnight",
                    "2am","4am","6am","8am","10am","noon")
    
    if (length(nightsi) > 0) {
      nplots = length(nightsi)+1
      
      npointsperday = (60/ws3)*1440
      x = 1:npointsperday
      
      daycount = 1
      
      for (g in 2:2) {
        
        skip = FALSE
        if (g == 1) {
          t0 = 1
          t1 = nightsi[g]-1
        } else if (g > 1 & g < nplots){
          t0 = nightsi[g-1]
          t1 = nightsi[g]-1
        } else if (g == nplots){
          t0 = nightsi[g-1]
          t1 = length(time)
        }
        
        # day with 25 hours, just pretend that 25th hour did not happen
        if (((t1-t0) + 1) / (60*60/ws3) == 25) {
          t1 = t1 - (60*60/ws3)
        }
        
        # day with 23 hours, just extend timeline with 1 hour
        if (((t1-t0) + 1) / (60*60/ws3) == 23) {
          t1 = t1 + (60*60/ws3)
        }
        
        # Initialize daily "what we think you did" vectors
        acc = abs(ACC[t0:t1])
        ang = angle[t0:t1]
        
        YXLIM = c(-230,300)
        
        # Plot accelerometer data
        plot(x, acc, type = "l", lwd = 0.2, bty = "l", axes = FALSE, ylim = YXLIM,
             xlab = "", ylab = "", main = "", cex.main = 0.9, lend = 2)
        lines(x, ang, type = "l", lwd = 0.2, bty = "l", xlab = "", ylab = "",
              cex = 0.3, lend = 2)
        
        axis(side=1,at=seq(1,(((60/ws3)*60*24)+1),by=(2*(60/ws3)*60)),labels=xaxislabels,cex.axis=0.7)
        plot_loc = -length(x)*0.05
        text(x=plot_loc,y=-120,labels="Arm movement:",pos=4,font=1.8,cex=0.9)
        text(x=plot_loc,y=100,labels="Angle of sensor's z-axis relative to horizontal plane:",pos=4,font=1.8,cex=0.9)
        
        
        
      }
    }

  })

  output$info <- renderText({
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("sleep = ", round(e$xmin, 1)/60, "\nwake = ", round(e$xmax, 1)/60)
    }
    
    paste0(
      "Sleep diary:\n", xy_range_str(input$plot_brush)
    )
  })
  
  # Code to use the reset button.
  # Reset button is not correctly working
  #observeEvent(input$reset,{
  #  output$MyPlot <- NULL
  #})
  
  # Code to show datatable contents
  #output$contents <- renderTable({
  #  req(input$file1)
  #  df <- read.csv(input$file1$datapath)
  #})
  
}


# ------------------------------------------------------------------------------
# Create Shiny app
shinyApp(ui, server)