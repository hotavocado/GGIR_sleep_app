library(shinyBS)
library(shinyjs)
library(shiny)
library(shinyFiles)
library(ggplot2)
library(GGIR)
library(shinydashboard)

rm(list = ls())

if(!"timeDate" %in% installed.packages()){
  install.packages("timeDate")
}


# ------------------------------------------------------------------------------
# Define UI for data upload

ui <- fluidPage(
  useShinyjs(),
  # Application title
  titlePanel(title = h4("CMI Actigraphy Visualization Tool",align="center")),
  
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("dir", "Select directory", "Upload"),
      verbatimTextOutput("dir", placeholder = TRUE),
      textInput("subj_ID", label = h3("Enter subject ID")),
      actionButton("load_data", "Load subject"),
      actionButton("save_data", "Save diary")
    ),
    
    mainPanel(
      verbatimTextOutput("info"),
      bsAlert('upload_complete')
    )
  ),
  column(width=6,
         box(style='width:800px;overflow-x: scroll;height:2000px;overflow-y: scroll;',
             plotOutput("MyPlot", brush = brushOpts(id = "plot_brush",clip=TRUE,
                                                    direction = c("x")))
         ))
)

# ------------------------------------------------------------------------------
# Define server logic

server <- function(input, output, session) {
  
  # --- Option to increase the maximum upload size to 100MB
  options(shiny.maxRequestSize=100*1024^2)
  
  # --- Choose directory
  shinyDirChoose(
    input,
    'dir',
    roots = c(home = '~'),
    filetypes = c('', 'txt', 'bigWig', "tsv", "csv", "bw")
  )
  
  global <- reactiveValues(datapath = getwd())
  
  dir <- reactive(input$dir)
  
  output$dir <- renderText({
    global$datapath
  })
  
  observeEvent(ignoreNULL = TRUE,
               eventExpr = {
                 input$dir
               },
               handlerExpr = {
                 if (!"path" %in% names(dir())) return()
                 home <- normalizePath("~")
                 global$datapath <-
                   file.path(home, paste(unlist(dir()$path[-1]), collapse = .Platform$file.sep))
               })
  # --- End choose directory
  
  # --- Read RData file
  observeEvent(input$load_data, {
    
    output$plot <- renderUI({
      plotOutput("MyPlot")
    })
  })
  
  
  output$MyPlot <- renderPlot({
    
    if(is.null(input$load_data)){return()}
    req(input$load_data)
    
    #subj = 'NDARAA948VFH'
    
    load(paste0(global$datapath, '/meta/basic/meta_', input$subj_ID, '.csv.RData'))
    load(paste0(global$datapath, '/meta/ms4.out/', input$subj_ID, '.csv.RData'))
    
    createAlert(session, 'upload_complete', title = 'Data Import Complete',
                content = 'Wait until graphs are generated',
                alertId = 'alert_delete', append = FALSE)
    
    # M = metadata from output_dir/meta/basic/subj.csv.RData
    ws3 = M$windowsizes[1]
    metric = "ENMO"
    desiredtz = "America/New_York"
    viewingwindow = 2
    summarysleep_tmp = nightsummary
    
    # create list of day names
    wdaynames = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
    
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
      
      NGPP = 10 #number of graphs per page
      par(mfcol=c(NGPP,1),mar=c(2,0.5,1,0.5)+0.1,omi=c(0,0,0.1,0),mgp=c(2,0.8,0))
      
      daycount = 1
      #g = daycount
      #if (g == 0){g = 1}
      
      for (g in 1:2) {
        
        skip = FALSE
        if (daycount == 1) {
          t0 = 1
          t1 = nightsi[daycount]-1
        } else if (daycount > 1 & daycount < nplots){
          t0 = nightsi[daycount-1]
          t1 = nightsi[daycount]-1
        } else if (daycount == nplots){
          t0 = nightsi[daycount-1]
          t1 = length(time)
        }
        
        curr_date = as.Date(substr(time[t0],start=1,stop=10),format = '%Y-%m-%d', origin="1970-1-1")  # what day is it?
        
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
        extension = rep(NA,(npointsperday-((t1-t0)+1)))
        
        sleep_dates = as.Date(summarysleep_tmp$calendar_date,format='%d/%m/%Y', origin="1970-1-1")
        
        # check to see if there are any sleep onset or wake annotations on this day 
        sleeponset_loc = 0
        wake_loc = 0
        if (viewingwindow == 1) {  # use different search coefficients for noon or midnight centered plots
          sw_coefs = c(0,24)  
        } else if (viewingwindow == 2) {
          sw_coefs = c(12,36)  
        }
        # check for sleeponset & wake time that is logged on this day before midnight
        curr_date = as.Date(substr(time[t0],start=1,stop=10),format = '%Y-%m-%d', origin="1970-1-1")  # what day is it?
        if (viewingwindow == 2) {
          # check to see if it is the first day that has less than 24 and starts after midnight
          if ((t1 - t0) < ((60*60*12)/ws3)) { # if there is less than half a days worth of data
            curr_date = curr_date - 1
          }
        }
        check_date = match(curr_date,sleep_dates)
        if (is.na(check_date) == FALSE) {
          sleeponset_time = summarysleep_tmp$sleeponset[check_date]  # get the time of sleep_onset
          if (sleeponset_time >= sw_coefs[1] & sleeponset_time < sw_coefs[2]) {
            sleeponset_hour = trunc(sleeponset_time)
            if (sleeponset_hour == 24) sleeponset_hour = 0
            if (sleeponset_hour > 24) sleeponset_hour = sleeponset_hour - 24 # only with viewingwindow==2
            sleeponset_min = round((sleeponset_time - trunc(sleeponset_time)) * 60)
            if (sleeponset_min == 60) sleeponset_min = 0
            sleeponset_locations = which(hour[t0:t1] == sleeponset_hour & min_vec[t0:t1] == sleeponset_min)
            if (!is.na(sleeponset_locations[1])) { 
              sleeponset_loc = sleeponset_locations[1]
            }
          }
          wake_time = summarysleep_tmp$wakeup[check_date]
          if (wake_time >= sw_coefs[1] & wake_time < sw_coefs[2]) {
            wake_hour = trunc(wake_time)
            if (wake_hour == 24) wake_hour = 0
            if (wake_hour > 24) {
              wake_hour = wake_hour - 24
            }
            wake_min = round((wake_time - trunc(wake_time)) * 60)
            if (wake_min == 60) wake_min = 0
            wake_locations = which(hour[t0:t1] == wake_hour & min_vec[t0:t1] == wake_min)
            if (!is.na(wake_locations[1])) {
              wake_loc = wake_locations[1]
            }
          }
        }
        
        # add extensions if <24hr of data
        first_day_adjust = 0 # hold adjustment amounts on first and last day plots
        last_day_adjust = 0
        if (((t1-t0)+1) != npointsperday & t0 == 1) {
          extension = rep(NA,(npointsperday-((t1-t0)+1)))
          first_day_adjust = length(extension)
          acc = c(extension,acc)
          ang = c(extension,ang)
          t1 = length(acc) # this was missing and causing x.y coords errors in some tests
          if (length(acc) == (length(x)+1)) {
            extension = extension[2:(length(extension))]
            acc = acc[2:(length(acc))]
            ang = ang[2:(length(ang))]
          }
          extension_mat = matrix(NA,nrow=length(extension),ncol=6)
          # adjust any sleeponset / wake annotations if they exist:
          if (sleeponset_loc[1] != 0) {
            for (i in 1:length(sleeponset_loc)) {
              sleeponset_loc[i] = sleeponset_loc[i] + length(extension)
            }
          }
          if (wake_loc[1] != 0) {
            for (i in 1:length(wake_loc)) {
              wake_loc[i] = wake_loc[i] + length(extension)
            }
          }
        } else if (((t1-t0)+1) != npointsperday & t1 == length(time)) {
          extension = rep(NA,(npointsperday-((t1-t0)+1)))
          last_day_adjust = length(acc)
          acc = c(acc,extension)
          ang = c(ang,extension)
          if (length(acc) == (length(x)+1)) {
            extension = extension[1:(length(extension)-1)]
            acc = acc[1:(length(acc)-1)]
            ang = ang[1:(length(ang)-1)]
          }
          extension_mat = matrix(NA,nrow=length(extension),ncol=6)
        }
        acc = as.numeric(acc)
        acc[which(acc >= 900)] = 900
        acc = (acc/9) - 210
        
        YXLIM = c(-230,300)
        
        closeAlert(session, 'alert_delete')
        
        # Plot accelerometer data
        plot(x, acc, type = "l", lwd = 0.2, bty = "l", axes = FALSE, ylim = YXLIM,
             xlab = "", ylab = "", main = "", cex.main = 0.9, lend = 2, col = "blue")
        lines(x, ang, type = "l", lwd = 0.4, bty = "l", xlab = "", ylab = "",
              cex = 0.3, lend = 2)
        
        curr_date_unclassed = unclass(as.POSIXlt(curr_date,desiredtz))
        title = paste("Day ",daycount,": ",
                      wdaynames[curr_date_unclassed$wday+1],
                      " | ",
                      curr_date_unclassed$mday, " ",
                      month.abb[curr_date_unclassed$mon+1], " ",
                      curr_date_unclassed$year+1900, sep="",
                      " - ",
                      "Day ",daycount+1, ": ",
                      wdaynames[curr_date_unclassed$wday+2],
                      " | ",
                      curr_date_unclassed$mday+1, " ",
                      month.abb[curr_date_unclassed$mon+1], " ",
                      curr_date_unclassed$year+1900)
        
        axis(side=1,at=seq(1,(((60/ws3)*60*24)+1),by=(2*(60/ws3)*60)),labels=xaxislabels,cex.axis=0.7)
        plot_loc = -length(x)*0.05
        text(x=plot_loc,y=-120,labels="Arm movement:",pos=4,font=1.8,cex=0.9)
        text(x=plot_loc,y=100,labels="Angle of sensor's z-axis relative to horizontal plane:",pos=4,font=1.8,cex=0.9)
        text(x=plot_loc,y=285,labels=title,pos=4,font=2,cex=1)
        
        # add sleeponset time annotation to plot:
        arrow_line_length = length(x) * 0.01736 # make arrow line length adaptable to different short epochs
        if (sleeponset_loc[1] != 0){
          for (i in sleeponset_loc) { # allow for multiple sleeponset_loc
            set_pos = 4 # position of text in relation to arrow
            ar_start_idx <- i
            ar_end_idx <- i + arrow_line_length # make arrow go to the right of the annotation line
            if (i > (0.8 * length(x))) {  # check to see if text should be placed on the left side of the line
              set_pos = 2
              ar_end_idx <- i - arrow_line_length
            }
            # draw sleeponset annotation:
            segments(i,-230,i,210,col='black',lwd=1.5)
            arrows(ar_start_idx,205,ar_end_idx,205,length=0.05,angle = 20,code=1,lwd=0.5)
            segments(ar_start_idx,205,ar_end_idx,205,col="black",lwd=0.5)
            text(ar_end_idx,205,labels="Sleep-onset",pos=set_pos,font=1.8,cex=0.8,col="darkgrey")
          }
        }
        
        # add wake time annotation to plot:
        if (wake_loc[1] != 0) {
          for (i in wake_loc) {
            set_pos <- 4
            ar_start_idx <- i
            ar_end_idx <- i + arrow_line_length
            if (i > (0.8 * length(x))) {  # check to see if text should be placed on the left side of the line
              set_pos = 2
              ar_end_idx <- i - arrow_line_length
            }
            # draw wake annotation:
            segments(i,-230,i,210,col='black',lwd=1.5)
            arrows(ar_start_idx,160,ar_end_idx,160,length=0.05,angle = 20,code=1,lwd=0.5)
            segments(ar_start_idx,160,ar_end_idx,160,col="black",lwd=0.5)
            text(ar_end_idx,160,labels="Wake",pos=set_pos,font=1.8,cex=0.8,col="darkgrey")
          }
        }
        
        daycount = daycount + 1
      }
      
    }
    
    
  }, height = 2000, width = 800)
  
  
  output$info <- renderText({
    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0("sleep = ", round(e$xmin), "\nwake = ", round(e$xmax))
    }
    
    paste0(
      "Sleep diary:\n", xy_range_str(input$plot_brush)
    )
  })
  
  
  
}


# ------------------------------------------------------------------------------
# Create Shiny app
shinyApp(ui, server)
