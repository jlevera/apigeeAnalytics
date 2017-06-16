library(RJSONIO)
library(RCurl)
library(httr)
library(ggplot2)
library(anytime)
library(shiny)
library(shinythemes)
library(markdown)
library(data.table)
library(gridExtra)
library(scales)

# A read-only data set that will load once, when Shiny starts, and will be
# available to each user session
metricList <-  c("sum(message_count)",
                 "sum(cache_hit)",
                 "avg(target_response_time)",
                 "max(target_response_time)",
                 "avg(total_response_time)",
                 "max(total_response_time)",
                 "avg(response_processing_latency)",
                 "avg(request_processing_latency)",
                 "sum(is_error)",
                  "tps")
  
names(metricList) <-  c("sum_message_count",
                        "sum_cache_hit",
                        "avg_target_response_time",
                        "max_target_response_time",
                        "avg_total_response_time",
                        "max_total_response_time",
                        "avg_response_processing_latency",
                        "avg_request_processing_latency",
                        "sum_is_error",
                        "tps")

apiStatistics <- NULL
apiList4checkbox <- NULL

# A non-reactive function that will be available to each user session
apiProxiesTable <- function(fromDate, toDate, user, password) {
  
  timeRange <- paste("timeRange=",fromDate,"%2000:00~",toDate,"%2023:59", sep = "")
  dimension <- "/apis" #paste("/", dimension, sep="")   #/apiproducts #/apps #/devs
  metric <- "sum_message_count"   #for more metrics check http://docs.apigee.com/analytics-services/reference/analytics-reference
  filter <- "" # "&filter=(apiproxy in 'tigo_mobile_py_fulfillment_v1')" #filter=(apiproxy in 'tigo_mobile_py_fulfillment_v1')"
  timeUnit <-"&timeUnit=day"
  
  functionToExecute <- paste("select=", as.character(metricList[metric]),  sep="")
 # browser()

  if(is.null(apiList4checkbox))
  {
  
  req <- GET(paste("https://api.enterprise.apigee.com/v1/organizations/millicom/environments/prod/stats/", dimension, "/?", functionToExecute ,"&", timeRange, timeUnit, filter , sep = ""),
             authenticate(user, password), 
             body = '{ "query": [], "response": { "format": "json" } }')
  
  
  
  json <- content(req,as = "text", encoding = "UTF-8")
  
  apiList4checkbox <-fromJSON(json)
  
  }
  
 
  df <- rbindlist( apiList4checkbox$environments[[1]]$dimensions[] )

  
  
  return(sort(df$name) )
  
}

# A non-reactive function that will be available to each user session
apigee <- function(fromDate, toDate, metric, dimension, selectedApiProxies, timeunit, user, password) {
 
   timeRange <- paste("timeRange=",fromDate,"%2000:00~",toDate,"%2023:59", sep = "")
  # dimension <- paste("/", dimension, sep="")   #/apiproducts #/apps #/devs
   #metric <- "message_count"   #for more metrics check http://docs.apigee.com/analytics-services/reference/analytics-reference
   #my_timeUnit <- paste0("timeUnit=", timeunit)
   plotOfApiProxies <- NULL
   filter<-""
   
   # if(is.null(selectedApiProxies))
   # {
   #   return(plot.new())
   # }
   
   
   if(length(selectedApiProxies)>0)  ## add filter string
   {
     filter <-  "&filter=("  
     for(var in selectedApiProxies)
     {
       filter <-  paste0(filter, "apiproxy eq '", selectedApiProxies, "' or")
     }
     
     filter <-substr(filter, 1, nchar(filter)-2)
    filter <-  paste0(filter, ")")       # "&filter=(apiproxy in 'tigo_mobile_py_fulfillment_v1')" #filter=(apiproxy in 'tigo_mobile_py_fulfillment_v1')"
   }
   

   functionToExecute <- paste("select=", as.character(metricList[metric]),  sep="")
   
   #browser()
   if(is.null(apiStatistics))
   {
     
   req <- GET(paste("https://api.enterprise.apigee.com/v1/organizations/millicom/environments/prod/stats/", dimension, "/?", functionToExecute ,"&", timeRange, "&timeUnit=",timeunit, filter , sep = ""),
              authenticate(user, password),
             body = '{ "query": [], "response": { "format": "json" } }')
   
   

   json <- content(req,as = "text", encoding = "UTF-8")

   apiStatistics <-fromJSON(json)
   }
   
   
  
   df <- data.frame("Timestamp"= numeric(), "Value"=double(), stringsAsFactors=FALSE)
   
   for(apiproxy in selectedApiProxies)
   {
  
    if(metric=="avg_target_response_time" || metric=="avg_request_processing_latency")
   {
     
     for(var in apiStatistics$environments[[1]]$dimensions[[1]]$metrics[[2]]$values[] )
     {
       #print(c(as.numeric(var$timestamp), as.double(var$value)))
       
       df[nrow(df)+1,] <-c(as.numeric(var$timestamp), as.double(var$value))
     }
  
   }
   else
   {
     for(var in apiStatistics$environments[[1]]$dimensions[[1]]$metrics[[1]]$values[] )
     {
       #print(c(as.numeric(var$timestamp), as.double(var$value)))
       
       df[nrow(df)+1,] <-c(as.numeric(var$timestamp), as.double(var$value))
     }
     
   }
     #browser()
     
     if(!is.null(selectedApiProxies))
     {
       #plotOfApiProxies <-  ggplot(df,aes(x=anytime(Timestamp/1000), y=Value, color=apiproxyname)) + geom_line() + xlab("Date") + ylab(metric) + theme(legend.position="bottom")
       plotOfApiProxies <- ggplot(df) + scale_x_datetime( minor_breaks=date_breaks("1 hour")) #breaks = date_breaks("1 hour"),
       plotOfApiProxies <-  plotOfApiProxies + geom_line(aes(x=anytime(Timestamp/1000, tz="US/Pacific"), y=Value, color=selectedApiProxies)) + xlab("Date") + ylab(metric) + theme(legend.position="bottom", axis.text.x = element_text(angle = 60, hjust = 1))
     }
     
     
   }

 
  
   
  return(plotOfApiProxies) 
}
################################################################################  
#FUNcTION: gridPlotApiProxies
#return value: returns a grid with plots of apiproxies selected in checkbox of UI
###################################################################################
gridPlotApiProxies <- function(fromDate, toDate, metric, dimension, selectedApiProxies, timeunit, user, password) {
 
  if(is.null(selectedApiProxies))
  {
    return(plot.new())
  }
  else {
    
    plotList <- list()
    for(var in selectedApiProxies)
    {
      plotList[[length(plotList)+1]] <- apigee(fromDate, toDate, metric, dimension,var, timeunit, user, password)
    }
    return( grid.arrange(grobs=plotList, ncol=1 ,nrow=length(plotList)))
  }
  
 
  }

#SERVER function
server <- function(input, output, session) {

  #browser()
  reactive(input$btSave, {
    updateTabsetPanel(session, "uiTabSetPanel",
                      selected = "Plots")
  })
     
  dataInput <- reactive({
    apiProxiesTable(strsplit(format(input$dateRange,"%m/%d/%C%y")," ")[1],strsplit(format(input$dateRange,"%m/%d/%C%y")," ")[2], input$user, input$password  )
  })
   
   output$tableApis <- renderTable({
     withProgress(message = 'Retriving data', min=0, max=10,value = 0, {
       incProgress(5)
            dataInput()
       
            }
           
        )
     
   })
   
   output$checkboxControls <- renderUI({
     withProgress(message = 'Loading...', min=0, max=10,value = 0, {
       incProgress(6)
     checkboxGroupInput("selectedApiProxies", "Choose ApiProxy:",  apiProxiesTable(strsplit(format(input$dateRange,"%m/%d/%C%y")," ")[1],strsplit(format(input$dateRange,"%m/%d/%C%y")," ")[2], input$user, input$password ), input$selectedApiProxies)
             })
   })
   

    output$plot <- renderPlot({
      withProgress(message = 'Loading...', min=0, max=10,value = 0, {
        incProgress(6)
        gridPlotApiProxies(strsplit(format(input$dateRange,"%m/%d/%C%y")," ")[1],strsplit(format(input$dateRange,"%m/%d/%C%y")," ")[2], input$metric, input$dimension,input$selectedApiProxies, input$timeunit, input$user, input$password)
      
        })
    })
    

  
 
} 
################################################################################
## User Interface
###############################################################################
ui <-  navbarPage("Apigee Analitycs",
                  tabsetPanel(
                  tabPanel("Plot",
                           sidebarLayout(
                             sidebarPanel(
                               dateRangeInput('dateRange',
                                              label = 'Date range input: mm/dd/yyyy',
                                              format = "mm/dd/yyyy",
                                              start = Sys.Date() , end = Sys.Date()
                               ),
                               selectInput("metric", "Metric:",
                                           c("sum(message_count)" = "sum_message_count",
                                             "sum(cache_hit)" = "sum_cache_hit",
                                             "avg(target_response_time)" = "avg_target_response_time",
                                             "max(target_response_time)" = "max_target_response_time",
                                             "avg(total_response_time)" = "avg_total_response_time",
                                             "max(total_response_time)" = "max_total_response_time",
                                             "avg(response_processing_latency)" = "avg_response_processing_latency",
                                             "avg(request_processing_latency)" = "avg_request_processing_latency",
                                              "sum(is_error)" = "sum_is_error",
                                             "tps" = "tps")
                                              ),
                               selectInput("dimension", "Dimension:",
                                                          c("Apis" = "apis",
                                                            "ApiProducts" = "apiproducts",
                                                            "Apps" = "apps",
                                                            "Devs" = "devs")),
                               selectInput("timeunit", "Time Unit:",
                                           c("minute" = "minute",
                                             "hour" = "hour",
                                             "day" = "day"), selected ="hour"),
                               submitButton("Update View"),
                               uiOutput("checkboxControls")
                               
                               #actionButton(inputId = 'buttonAction',label ="Update View")
                
                               
                             ),
                             mainPanel(
                             plotOutput("plot")
                             #uiOutput("plot",inline = TRUE)
                              
                             )
                           )
                  ),
                  tabPanel("Credentials",
                           HTML("Credentials from Apigee"),
                             textInput("user", "User:"),
                             passwordInput("password", "Password:"),
                             actionButton("btSave","Save"),
                    
                             verbatimTextOutput("errorMessage")
                  ),
            
                 tabPanel("Help",
                             HTML("<p>This shiny app consumes Apigee Analytics Api in order to plot traffic and other measures of each Api Proxy listed in the associated account.
                                  </p>
                                <p>In order to use this site, you will first need an account in Apigee</p>
<p> For more information about Apigee Analytics, please follow the link http://docs.apigee.com/analytics-services/content/use-analytics-api-measure-api-program-performance
   </p>                               
                                  ")
                        
                                       
                                      )
                    ,id="uiTabSetPanel" , selected="Credentials")
                  )
                  

shinyApp(ui = ui, server = server)

