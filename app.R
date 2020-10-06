##### Audit ##

#iprospect-manchester-analytics@appspot.gserviceaccount.com

############# Libraries #########################
library(googleAuthR) # Authenticate Google
library(googleAnalyticsR) # GA
library(googleTagManageR) # GTM
#library(bigrquery) #Use bigquery 
#library(bigQueryR)
library(DBI)

##Models
#library(ChannelAttribution) #generating markov models
#library(prophet)

#Website crawling
#library(RCurl)
library(urltools)
library(xml2)
library(rvest)

##Shiny
library(DT)
library(rsconnect)
library(shiny)
library(shinydashboard)

##Visualisation tools
library(ggplot2)
library(plotly)
library(purrrlyr)
library(visNetwork)
library(igraph)

##General
library(devtools)
library(sqldf) #SQL on data frames
library(Rcpp)
library(rlang)
library(tidyr)
library(tidyselect)
library(dplyr)
library(reshape)
library(stringr)
library(data.table)
library(scales)
library(progress)
library(rlang)  
library(tibble)
library(hms)
library(pillar) 
library(ellipsis)
library(stringr)
library(readr)
library(lubridate)
library(naniar)

### Functions ####
source("functions.R")

## Authentication #####

#viewId <- "89127389"
authEmail = "analytics-support-emea@dentsuaegis.com"
#Authentication service Account <- iprospect-manchester-analytics@appspot.gserviceaccount.com
set_client_token <- file.path("client_secret_531001441888-0d4d16g02790c9smgdqjcptcq4f86pdr.apps.googleusercontent.com.json")
token_path_ga <- file.path("iprospect-manchester-analytics-f0717bbd89b6.json")

#options(googleAuthR.verbose = 1,  gargle_quiet = FALSE)
#gar_auth_service( json_file = token_path_ga)

#googleAnalyticsR::ga_auth(email = "analytics-support-emea@dentsuaegis.com")

#Instructions for resetting client ID
#options(googleAuthR.client_id = client_id)
#options(googleAuthR.client_secret = client_secret)
#devtools::reload(pkg = devtools::inst("googleAnalyticsR"))
#ga_auth()

scopes <- c("https://www.googleapis.com/auth/analytics.readonly",
            "https://www.googleapis.com/auth/analytics.edit",
            "https://www.googleapis.com/auth/analytics.manage.users",
            "https://www.googleapis.com/auth/analytics.manage.users.readonly",
            "https://www.googleapis.com/auth/tagmanager.edit.containers"
)

googleAuthR::gar_set_client(json = set_client_token, scopes = scopes)

#googleAnalyticsR::ga_auth(email = "analytics-support-emea@dentsuaegis.com")

gar_auth_service(json_file = token_path_ga)

###### GLOBAL VARS ###########

pok <- T #KEEP T
content_grouping_message <- "Content Grouping is not available via GA API at the manegement level so we use the reporting API and assume that '(not set)' values over a period of time indicates the tracking code is not enabled."
sampling_logic <- TRUE # MAKE TRUE WHEN READY

###### UI #########

ui <- shinyUI(
  fluidPage(
    fluidPage(
      #uiOutput("in.pss"),
      uiOutput("in.filter.logic"),
      fluidRow(
        column(
          width = 3,
          uiOutput("in.filter.dimension")
        ),
        column(
          width = 3,
          uiOutput("in.filter.operator")
        ),
        column(
          width = 3,
          uiOutput("in.filter.expression")
        )
      ),
      tabsetPanel(
        tabPanel( #### UI GA DATA HYGIENE ####
                  "Data Hygiene", 
                  fluidRow(
                    column(width = 2,
                           uiOutput("in.ga.hygiene.account") 
                    ),
                    column(width = 2,
                           uiOutput("in.ga.hygiene.property")
                    ),
                    column(width = 2,
                           uiOutput("in.ga.hygiene.view")
                    ),
                    column(width = 2, offset = 1,
                           uiOutput("in.ga.hygiene.dateRange")
                    ),
                    column(width = 1, offset = 2,
                           imageOutput("ga.hygiene.image.powertool", width = 100, height = 100, inline = TRUE)
                    )
                  ),
                  fluidRow(
                    column(
                      width = 2,
                      uiOutput("in.ga.hygiene.websiteUrl.input")
                    ),
                    column(
                      width = 2, style = "margin-top: 25px;",
                      uiOutput("in.ga.hygiene.websiteUrl.go")
                    ),
                    column(
                      width = 2, style = "margin-top: 25px;",
                      uiOutput("in.ga.hygiene.websiteUrl.go.change")
                    )
                  ),
                  br(),
                  fluidRow(
                    column(
                      width = 2,
                      uiOutput("in.ga.hygiene.go.report")
                    ),
                    column(
                      width = 2,
                      uiOutput("in.ga.hygiene.go.change")
                    ),
                    column(
                      width = 2, offset = 6,
                      uiOutput("in.ga.hygiene.go.reverse")
                      
                    )
                  ),
                  br(),
                  fluidRow(
                    column(width = 6, style = "background-color:#7FFFD4;",
                           uiOutput("in.hygiene.total.spam")  
                    ),
                    column(width = 6, style = "background-color:#7FFFD4;",
                           uiOutput("in.hygiene.total.filter_map")  
                    )
                  ),
                  br(),
                  #tags$header("Sessions"),
                  h1("Account Structure and Tracking"),
                  
                  h2("Hostnames"), #### UI GA DATA HYGIENE HOSTNAMES ####
                  uiOutput("in.ga.hygiene.hostnames.description"),
                  uiOutput("in.ga.hygiene.hostnames.table"),
                  uiOutput("in.ga.hygiene.hostnames.filter"),
                  uiOutput("in.ga.hygiene.hostnames.filter.suggestion"),
                  uiOutput("in.ga.hygiene.hostnames.filter.change"),
                  br(),
                  h2("Referrals"), #### UI GA DATA HYGIENE REFERRALS ####
                  fluidRow(
                    column(
                      width = 4,
                      h3("Self Referrals"), 
                      #h3(""),
                      uiOutput("in.ga.hygiene.selfReferral.table"),
                    ),
                    column(
                      width = 4,
                      h3("Payment Gateway Referrals"),
                      uiOutput("in.ga.hygiene.paymentReferral.table")
                    ),
                    column(
                      width = 4,
                      h3("Spam Referrals"),
                      uiOutput("in.ga.hygiene.spamReferral.table")
                    )
                  ),
                  uiOutput("in.ga.hygiene.referrals.filter"),
                  uiOutput("in.ga.hygiene.referrals.suggestion_text"),
                  uiOutput("in.ga.hygiene.referrals.filter_create_limit"),
                  uiOutput("in.ga.hygiene.referrals.suggestion_remove"),
                  uiOutput("in.ga.hygiene.referrals.suggestion_create"),
                  uiOutput("in.ga.hygiene.referrals.change"),
                  br(),
                  h2("Browser Version Spam"), #### UI GA DATA HYGIENE SPAM ####
                  h4("Report is compiled by comparing potential spam browser versions to browser versions which are not spam. Then we can remove browser versions with ONLY high bounce rate, low time and page and session duration. These will most likely be outated browser versions where spam likes to hide. This is usually enough, but as an extra layer of confidence we also filter out any browser versions where a transaction happened, which is rarely the case."),
                  uiOutput("in.ga.hygiene.spam.table"),
                  uiOutput("in.ga.hygiene.browser_spam.filter"),
                  uiOutput("in.ga.hygiene.browser_spam.suggestion_text"),
                  uiOutput("in.ga.hygiene.browser_spam.filter_create_limit"),
                  uiOutput("in.ga.hygiene.browser_spam.suggestion_remove"),
                  uiOutput("in.ga.hygiene.browser_spam.suggestion_create"),
                  uiOutput("in.ga.hygiene.browser_spam.change"),
                  br(),
                  h2("Personally Identifiable Information (PII)"), #### UI GA DATA HYGIENE PII ####
                  h4("PII is any personal data - emails, names, phone numbers, passwords etc. It absolutely should not appear anywhere in GA."),
                  fluidRow(
                    column(
                      width = 6,
                      h3("Page Path Data- Query Parameters"),
                      uiOutput("in.ga.hygiene.pii.table.page"),
                      uiOutput("in.ga.hygiene.pii.table.page.suggestion")
                    ),
                    column(
                      width = 6,
                      h3("Event Data"),
                      uiOutput("in.ga.hygiene.pii.table.event"),
                      uiOutput("in.ga.hygiene.pii.table.event.suggestion")
                    )
                  ),
                  h2("Pages (not set)"), #### UI GA DATA HYGIENE PAGES NOT SET ####
                  fluidPage(
                    column(
                      width = 6,
                      uiOutput("in.ga.hygiene.notSet.page")
                    ),
                    column(
                      width = 6,
                      uiOutput("in.ga.hygiene.notSet.landingPage")
                    )
                  )
        ),
        tabPanel( ###### UI GA ######
                  title = "GA",
                  #div(img(
                  #  scr = "ga.png",
                  # height = 100,
                  #  width = 100,
                  #style = "margin:10px 10px"
                  #)),
                  fluidRow(
                    column(
                      width = 7,
                      uiOutput("in.ga.account")    
                    ),
                    column(            
                      width = 3,
                      uiOutput("in.ga.content.date")
                    ),
                    column(
                      width = 2,
                      imageOutput("ga.image.powertool", width = 100, height = 100, inline = TRUE)  
                    )
                  ),
                  tabsetPanel(
                    type = "tabs",
                    tabPanel( #### UI GA ACCOUNT STRUCTURE ####
                              "Account Structure", 
                              uiOutput("in.ga.account.metadata"),
                              uiOutput("in.ga.account.structure.export"),
                              uiOutput("in.ga.account.structure")
                    ),
                    tabPanel( #### UI GA USER MANAGEMENT ####
                              "User Management", 
                              uiOutput("in.ga.users.export"),
                              uiOutput("in.ga.users")
                    ),
                    navbarMenu( #### UI GA CUSTOM VARIABLES #####
                                "Custom Variables", 
                                tabPanel(
                                  "Account",
                                  uiOutput("in.ga.customVar.account.go"),
                                  uiOutput("in.ga.customVar.account.table.export"),
                                  uiOutput("in.ga.customVar.account.table")
                                ),
                                tabPanel(
                                  "Property",
                                  uiOutput("in.ga.customVar.property"),
                                  uiOutput("in.ga.customVar.property.table.export"),
                                  uiOutput("in.ga.customVar.table")
                                )
                                
                    ),
                    navbarMenu( #### UI GA FILTERS ####
                                "Filters", 
                                tabPanel(
                                  "Account",
                                  uiOutput("in.ga.filters.account.go"),
                                  
                                  uiOutput("in.ga.filters.account.export"),
                                  uiOutput("in.ga.filters.account.table"),
                                  
                                  
                                  uiOutput("in.ga.filters.account.account.export"),
                                  uiOutput("in.ga.filters.account.account.table")
                                ),
                                tabPanel(
                                  "Property",
                                  fluidRow(
                                    column(width = 2,
                                           uiOutput("in.ga.filters.property.input")    
                                    ),
                                    column(width = 2,
                                           uiOutput("in.ga.filters.property.go")    
                                    )
                                  ),
                                  uiOutput("in.ga.filters.property.table.export"),
                                  uiOutput("in.ga.filters.property.table"),
                                  uiOutput("in.ga.filters.property.structure")
                                ),
                                tabPanel(
                                  "View",
                                  fluidRow(
                                    column(width = 2,
                                           uiOutput("in.ga.filters.view.property.input")    
                                    ),
                                    column(width = 2,
                                           uiOutput("in.ga.filters.view.view.input")    
                                    )
                                  ),
                                  uiOutput("in.ga.filters.view.structure"),
                                  uiOutput("in.ga.filters.view.table.export"),
                                  uiOutput("in.ga.filters.view.table")
                                )
                    ),
                    navbarMenu( #### UI GA GOALS ####
                                "Goals",
                                tabPanel(
                                  "Account",
                                  uiOutput("in.ga.goals.account.go"),
                                  uiOutput("in.ga.goals.account.table.export"),
                                  uiOutput("in.ga.goals.account.table")
                                ),
                                tabPanel(
                                  "Property",
                                  fluidRow(
                                    column(width = 2,
                                           uiOutput("in.ga.goals.property.input")    
                                    ),
                                    column(width = 2,
                                           uiOutput("in.ga.goals.property.go")    
                                    )
                                  ),
                                  uiOutput("in.ga.goals.property.table.export"),
                                  uiOutput("in.ga.goals.property.table"),
                                  uiOutput("in.ga.goals.property.structure")
                                ),
                                tabPanel(
                                  "View", 
                                  fluidRow(
                                    column(width = 2,
                                           uiOutput("in.ga.goals.view.property.input")    
                                    ),
                                    column(width = 2,
                                           uiOutput("in.ga.goals.view.view.input")    
                                    )
                                  ),
                                  uiOutput("in.ga.goals.view.structure"),
                                  fluidRow(
                                    column(
                                      width = 2,
                                      uiOutput("in.ga.goals.view.table.export")
                                    ),
                                    column(
                                      width = 2,
                                      uiOutput("in.ga.goals.view.copy.logic")
                                    )
                                  ),
                                  fluidRow(
                                    column(
                                      width = 2,
                                      uiOutput("in.ga.goals.view.property.copy")    
                                    ),
                                    column(
                                      width = 2,
                                      uiOutput("in.ga.goals.view.view.copy")    
                                    ),
                                    column(
                                      width = 2,
                                      uiOutput("in.ga.view.view.copy.regexp")
                                    )
                                  ),
                                  uiOutput("in.ga.goals.view.copy.go"),
                                  br(),
                                  uiOutput("in.ga.goals.view.table")
                                )
                    ),
                    tabPanel( #### UI GA CHANNEL GROUPING ####
                              "Channel Grouping", 
                              h4("There is currently no way of accessing Channel Grouping definitions at the API level. However, we are going to see if it's possible to systematically recreate the definitions by comparing source medium etc dimensions from the reporting API with raw and main views.")
                    ),
                    navbarMenu( #### UI GA CONTENT GROUPING ####
                                "Content Grouping", 
                                tabPanel(
                                  "Account",
                                  h4(content_grouping_message),
                                  h2("work in progress...")
                                ),
                                tabPanel(
                                  "Property",
                                  h4(content_grouping_message),
                                  fluidRow(
                                    column(width = 2,
                                           uiOutput("in.ga.content.property.property.input")    
                                    ),
                                    column(width = 2, style = "margin-top: 25px;",
                                           uiOutput("in.ga.content.property.property.go")    
                                    )
                                  ),
                                  uiOutput("in.ga.content.property.meta.export"),
                                  uiOutput("in.ga.content.property.meta"),
                                  uiOutput("in.ga.content.property.table"),
                                  uiOutput("in.ga.content.property.structure")
                                ),
                                tabPanel(
                                  "View",
                                  h4(content_grouping_message),
                                  fluidRow(
                                    column(width = 2,
                                           uiOutput("in.ga.content.view.property.input")    
                                    ),
                                    column(width = 2, 
                                           uiOutput("in.ga.content.view.view.input")    
                                    ),
                                    column(width = 2, style = "margin-top: 25px;" , 
                                           uiOutput("in.ga.content.view.view.go")    
                                    )
                                  ),
                                  uiOutput("in.ga.content.view.meta.export"),
                                  uiOutput("in.ga.content.view.meta"),
                                  uiOutput("in.ga.content.view.table"),
                                  uiOutput("in.ga.content.view.structure")
                                )
                    )
                  )
        ),
        tabPanel( ##### UI GTM ######
                  "GTM",
                  fluidRow(
                    column(width = 10,
                           uiOutput("in.gtm.account")    
                    ),
                    column(
                      width = 2,
                      imageOutput("gtm.image.powertool", width = 100, height = 100, inline = TRUE),
                    )
                  ),
                  #uiOutput("in.gtm.containers"),
                  tabsetPanel(
                    type = "tabs",
                    tabPanel(
                      "Containers",
                      uiOutput("in.gtm.containers.table.export"),
                      uiOutput("in.gtm.containers.table")
                    ),
                    tabPanel( #### UI GTM TAGS ####
                              "Tags",
                              fluidRow(
                                column(width = 2,
                                       uiOutput("in.gtm.tags.input")
                                ),
                                column(width = 2,
                                       uiOutput("in.gtm.tags.go")    
                                )
                              ),
                              uiOutput("in.gtm.tags.table.export"),
                              uiOutput("in.gtm.tags.table"),
                              br(),
                              uiOutput("in.gtm.tags.container")
                    ),
                    tabPanel( #### UI GTM TRIGGERS ####
                              "Triggers",
                              fluidRow(
                                column(width = 2,
                                       uiOutput("in.gtm.triggers.input")
                                ),
                                column(width = 2,
                                       uiOutput("in.gtm.triggers.go")    
                                )
                              ),
                              
                              uiOutput("in.gtm.triggers.table.export"),
                              uiOutput("in.gtm.triggers.table"),
                              br(),
                              uiOutput("in.gtm.triggers.container")
                    ),
                    tabPanel( #### UI GTM VARIABLES ####
                              "Variables",
                              fluidRow(
                                column(width = 2,
                                       uiOutput("in.gtm.variables.input")
                                ),
                                column(width = 2,
                                       uiOutput("in.gtm.variables.go")    
                                )
                              ),
                              uiOutput("in.gtm.variables.table.export"),
                              uiOutput("in.gtm.variables.table"),
                              br(),
                              uiOutput("in.gtm.variables.container")
                    )
                  )
        )
      )
    )
  ))

##### Begin Sever#####
server <- function(input, output) {
  
  ### Server UI ####
  
  #legacy password
  output$in.pss <- renderUI({
    input$pss; if (pok) return(NULL) else return(textInput("pss", "Enter Password:",""))
  })
  
  output$in.filter.logic <- renderUI({
    checkboxInput(
      "filter.logic",
      "Advanced Table Filter",
      value = FALSE
    )
  })
  output$in.filter.dimension <- renderUI({
    if (isFALSE(input$filter.logic)) {NULL} else {
      textInput(
        "filter.dimension",
        label = "Dimension"
      )
    }
  })
  output$in.filter.operator <- renderUI({
    if (isFALSE(input$filter.logic)) {NULL} else {
      selectInput(
        "filter.operator",
        label = "Operator",
        choices = c("EXACT", "REGEXP")
      )
    }
  })
  output$in.filter.expression <- renderUI({
    if (isFALSE(input$filter.logic)) {NULL} else {
      textInput(
        "filter.expression",
        label = "Expression"
      )
    }
  })
  
  ####### SERVER UI GA HYGIENE ####
  
  output$in.ga.hygiene.account <- renderUI({
    input$pss; if (pok) return(
      selectizeInput(
        "ga.hygiene.account",
        label = "Account",
        choices = sort(ga_accounts()$name)
      )
    )
  })
  
  hygiene_property_choices <- eventReactive(input$ga.hygiene.account, {
    unique(filter(ga_account_list(), accountName == input$ga.hygiene.account)$webPropertyName)
  })
  output$in.ga.hygiene.property <- renderUI({
    if (pok) return(
      selectizeInput(
        "ga.hygiene.property",
        label = "Property",
        choices = hygiene_property_choices()
      )
    )
  })
  output$in.ga.hygiene.view <- renderUI({
    return(
      selectizeInput(
        "ga.hygiene.view",
        label = "View",
        choices = hygiene_view()$viewName
      )
    )
  })
  output$in.ga.hygiene.dateRange <- renderUI({
    shiny::dateRangeInput(
      "ga.hygiene.dateRange",
      "Date Range",
      start = floor_date(floor_date(today(), "month")-1, "month"),
      end = floor_date(today(), "month")-1,
      max = today()
    )
  })
  output$ga.hygiene.image.powertool <- renderImage({
    list(
      src = "images/powerTool.png",
      contentType = "image/png",
      width = 85,
      height = 85,
      alt = "powertool"
    )
  }, deleteFile = FALSE)
  
  property_website_url <- eventReactive(input$ga.hygiene.property, {
    unique(filter(hygiene_account_structure(), webPropertyName == input$ga.hygiene.property)$websiteUrl)
  })
  output$in.ga.hygiene.websiteUrl.input <- renderUI({
    textInput(
      "ga.hygiene.websiteUrl.input",
      label = "Property Website URL",
      value = property_website_url()
    )
  })
  
  output$in.ga.hygiene.websiteUrl.go <- renderUI({
    actionButton(
      "ga.hygiene.websiteUrl.go",
      "Change Website URL",
      class = "btn-warning"
    )
  })
  
  output$in.ga.hygiene.go.report <- renderUI({
    actionButton(
      "ga.hygiene.go.report",
      "Run GA Hygiene Report",
      class = "btn-success",
      #icon = icon("google", lib = "font-awesome")
    )
  })
  output$in.ga.hygiene.go.change <- renderUI({
    actionButton(
      "ga.hygiene.go.change",
      "Make Changes to GA",
      class = "btn-warning",
      icon = icon("google", lib = "font-awesome")
    )
  })
  output$in.ga.hygiene.go.reverse <- renderUI({
    actionButton(
      "ga.hygiene.go.reverse",
      "Remove all PowerTool Filters",
      class = "btn-danger",
      icon = icon("google", lib = "font-awesome")
    )
  })
  remove_all_power_tool_filters_from_view_er <- eventReactive(input$ga.hygiene.go.reverse, {
    remove_all_power_tool_filters_from_view(
      accountId = ga_account_id(input$ga.hygiene.account),
      propertyId = ga_property_id(input$ga.hygiene.account, input$ga.hygiene.property),
      viewId = ga_view_id(input$ga.hygiene.account, input$ga.hygiene.property, input$ga.hygiene.view)$viewId
    )
  })
  
  
  #### SERVER UI GA DATA HYGIENE HOSTNAMES ####
  
  output$in.ga.hygiene.hostnames.description <- renderUI({
    hostnames <- paste(hostname(), collapse = ", ")
    return(
      HTML(paste("<h4>Potential Hostnames: <b>",hostnames,".</b> Gathered from <i>href 'a'</i> and <i>'link'</i>  nodes (css) of source HTML. </h4>"), sep = "")
    )
  })
  output$in.ga.hygiene.hostnames.table <- renderUI({
    return(DT::dataTableOutput("ga.hygiene.hostnames.table"))
  })
  output$in.ga.hygiene.hostnames.filter <- renderUI({
    tableOutput("ga.hygiene.hostnames.filter")
  })
  
  #### SERVER UI GA DATA HYGIENE REFERRAL ####
  
  #### SELF
  output$in.ga.hygiene.selfReferral.table <- renderUI({
    DT::dataTableOutput("ga.hygiene.selfReferral.table")
  })
  
  #### PATMENT GATEWAY
  output$in.ga.hygiene.paymentReferral.table <- renderUI({
    DT::dataTableOutput("ga.hygiene.paymentReferral.table")
  })
  
  #### SPAM
  output$in.ga.hygiene.spamReferral.table <- renderUI({
    DT::dataTableOutput("ga.hygiene.spamReferral.table")
  })
  
  ## FILTER
  output$in.ga.hygiene.referrals.filter <- renderUI({
    tableOutput("ga.hygiene.referrals.filter")
  })
  
  output$in.ga.hygiene.referrals.filter_create_limit <- renderUI({
    referral_exclusion_list <- c(self_refferal()$source, payment_gateway()$source, spam_referral()$source)
    number_of_filters <- get_maximum_filter_number(referral_filter(), referral_exclusion_list, referral_filter_out())
    return(
      numericInput(
        "ga.hygiene.referrals.filter_create_limit",
        label = "Number of filters to create:",
        value = number_of_filters,
        min = 1,
        max = number_of_filters
      )
    )
  })
  
  #### SERVER UI GA DATA HYGIENE SPAM ####
  
  output$in.ga.hygiene.spam.table <- renderUI({
    DT::dataTableOutput("ga.hygiene.spam.table")
  })
  
  output$in.ga.hygiene.browser_spam.filter <- renderUI({
    tableOutput("ga.hygiene.browser_spam.filter")
  })
  
  output$in.ga.hygiene.browser_spam.filter_create_limit <- renderUI({
    number_of_filters <- get_maximum_filter_number(browser_version_filter(), browser_version_spam()$browserVersion, browser_version_filter_out())
    return(
      numericInput(
        "ga.hygiene.browser_spam.filter_create_limit",
        label = "Number of filters to create:",
        value = number_of_filters,
        min = 1,
        max = number_of_filters
      )
    )
  })
  
  #### SERVER UI GA DATA HYGIENE PII ####
  
  output$in.ga.hygiene.pii.table.page <- renderUI({
    DT::dataTableOutput("ga.hygiene.pii.table.page")
  })
  
  output$in.ga.hygiene.pii.table.event <- renderUI({
    DT::dataTableOutput("ga.hygiene.pii.table.event")
  })
  
  #### SERVER UI GA DATA HYGIENE PAGES NOT SET ####
  
  output$in.ga.hygiene.notSet.page <- renderUI({
    DT::dataTableOutput("ga.hygiene.notSet.page")
  })
  
  output$in.ga.hygiene.notSet.landingPage <- renderUI({
    DT::dataTableOutput("ga.hygiene.notSet.landingPage")
  })
  
  
  ##### SERVER UI GA #####
  
  output$in.ga.account <- renderUI({
    input$pss; if (pok) return(
      selectizeInput(
        "ga.account",
        label = "Account",
        choices = sort(ga_accounts()$name),
        width = "1000px"
      )
    )
  })
  
  output$ga.image.powertool <- renderImage({
    list(
      src = "images/DDLPowertool.png",
      contentType = "image/png",
      width = 85,
      height = 85,
      alt = "powertool"
    )
  }, deleteFile = FALSE)
  
  
  #### SERVER UI GA ACCOUT STRUCTURE ####
  output$in.ga.account.metadata <- renderUI({
    if (pok) return(tableOutput("ga.account.metadata"))
  })
  output$in.ga.account.structure <- renderUI({
    if (pok) return(DT::dataTableOutput("ga.account.structure"))
  })
  output$in.ga.account.structure.export <- renderUI({
    input$pss; if (pok) return(downloadButton("ga.account.structure.export", "Export Data"))
  })
  
  ## USERS
  output$in.ga.users <- renderUI({
    if (pok) return(DT::dataTableOutput("ga.users"))
  })
  output$in.ga.users.export <- renderUI({
    input$pss; if (pok) return(downloadButton("ga.users.export", "Export Data"))
  })
  
  #### SERVER UI GA CUSTOM VARIABLES ####
  
  #### ACCOUNT 
  output$in.ga.customVar.account.go <- renderUI({
    actionButton(
      "ga.customVar.account.go",
      "Get all custom dimensions from account for each property."
    )
  })
  output$in.ga.customVar.account.table <- renderUI({
    DT::dataTableOutput("ga.customVar.account.table")
  })
  output$in.ga.customVar.account.table.export <- renderUI({
    return(downloadButton("ga.customVar.account.table.export", "Export Data"))
  })
  
  #### PROPERTY
  output$in.ga.customVar.property <- renderUI({
    if (pok) return(
      selectizeInput(
        "ga.customVar.property",
        label = "Property",
        choices = unique(filter(ga_account_list(), accountName == input$ga.account)$webPropertyName),
        multiple = TRUE
      )
    )
  })
  output$in.ga.customVar.table <- renderUI({
    DT::dataTableOutput("ga.customVar.table")
  })
  output$in.ga.customVar.property.table.export <- renderUI({
    return(downloadButton("ga.customVar.property.table.export", "Export Data"))
  })
  
  #### SERVER UI GA FILTERS ####
  
  ##### ACCOUNT 
  output$in.ga.filters.account.account.table <- renderUI({
    if (pok) return(DT::dataTableOutput("ga.filters.account.account.table"))
  })
  output$in.ga.filters.account.account.export <- renderUI({
    input$pss; if (pok) return(downloadButton("ga.filters.account.account.export", "Export Data"))
  })
  
  
  output$in.ga.filters.account.go <- renderUI({
    if (pok) return(
      actionButton(
        "ga.filters.account.go",
        label = "Get individual filter set-ups for every view in account"
      )
    )
  })
  output$in.ga.filters.account.export <- renderUI({
    return(downloadButton("ga.filters.account.export", "Export Data"))
  })
  output$in.ga.filters.account.table <- renderUI({
    return(DT::dataTableOutput("ga.filters.account.table"))
  })
  
  ##### PROPERTY 
  output$in.ga.filters.property.input <- renderUI({
    if (pok) return(
      selectInput(
        "ga.filters.property.input",
        label = NULL,
        choices = unique(filter(ga_account_list(), accountName == input$ga.account)$webPropertyName),
        multiple = TRUE
      )
    )
  })
  output$in.ga.filters.property.go <- renderUI({
    if (pok) return(
      actionButton(
        "ga.filters.property.go",
        label = "Go"
      )
    )
  })
  output$in.ga.filters.property.structure <- renderUI({
    if (pok) return(tableOutput("ga.filters.property.structure"))
  })
  output$in.ga.filters.property.table <- renderUI({
    if (pok) return(DT::dataTableOutput("ga.filters.property.table"))
  })
  output$in.ga.filters.property.table.export <- renderUI({
    input$pss; if (pok) return(downloadButton("ga.filters.property.table.export", "Export Data"))
  })
  
  ##### VIEW
  output$in.ga.filters.view.structure <- renderUI({
    if (pok) return(tableOutput("ga.filters.view.structure"))
  })
  output$in.ga.filters.view.property.input <- renderUI({
    if (pok) return(
      selectizeInput(
        "ga.filters.view.property.input",
        label = NULL,
        choices = unique(filter(ga_account_list(), accountName == input$ga.account)$webPropertyName)
      )
    )
  })
  output$in.ga.filters.view.view.input <- renderUI({
    if (pok) {
      return(
        selectizeInput(
          "ga.filters.view.view.input",
          label = NULL,
          choices = filters_view_property()$viewName
        )
      )
    }
  })
  output$in.ga.filters.view.table <- renderUI({
    if (pok) return(DT::dataTableOutput("ga.filters.view.table"))
  })
  output$in.ga.filters.view.table.export <- renderUI({
    input$pss; if (pok) return(downloadButton("ga.filters.view.table.export", "Export Data"))
  })
  
  #### SERVER UI GA GOALS ####
  
  #### ACCOUNT
  output$in.ga.goals.account.go <- renderUI({
    if (pok) return(
      actionButton(
        "ga.goals.account.go",
        label = "Get goals for every view in account"
      )
    )
  })
  output$in.ga.goals.account.table.export <- renderUI({
    return(downloadButton("ga.goals.account.table.export", "Export Data"))
  })
  output$in.ga.goals.account.table <- renderUI({
    return(DT::dataTableOutput("ga.goals.account.table"))
  })
  
  ### PROPERTY ###
  
  output$in.ga.goals.property.input <- renderUI({
    if (pok) return(
      selectInput(
        "ga.goals.property.input",
        label = NULL,
        choices = unique(filter(ga_account_list(), accountName == input$ga.account)$webPropertyName),
        multiple = TRUE
      )
    )
  })
  
  output$in.ga.goals.property.go <- renderUI({
    if (pok) return(
      actionButton(
        "ga.goals.property.go",
        label = "Go"
      )
    )
  })
  output$in.ga.goals.property.structure <- renderUI({
    if (pok) return(tableOutput("ga.goals.property.structure"))
  })
  
  output$in.ga.goals.property.table <- renderUI({
    if (pok) return(DT::dataTableOutput("ga.goals.property.table"))
  })
  output$in.ga.goals.property.table.export <- renderUI({
    input$pss; if (pok) return(downloadButton("ga.goals.property.table.export", "Export Data"))
  })
  
  #### --VIEW ######
  output$in.ga.goals.view.property.input <- renderUI({
    if (pok) return(
      selectizeInput(
        "ga.goals.view.property.input",
        label = NULL,
        choices = unique(filter(ga_account_list(), accountName == input$ga.account)$webPropertyName)
      )
    )
  })
  output$in.ga.goals.view.view.input <- renderUI({
    if (pok) {
      return(
        selectizeInput(
          "ga.goals.view.view.input",
          label = NULL,
          choices = goals_view_property()$viewName
        )
      )
    }
  })
  output$in.ga.goals.view.structure <- renderUI({
    if (pok) return(tableOutput("ga.goals.view.structure"))
  })
  output$in.ga.goals.view.table <- renderUI({
    if (pok) return(DT::dataTableOutput("ga.goals.view.table"))
  })
  output$in.ga.goals.view.table.export <- renderUI({
    input$pss; if (pok) return(downloadButton("ga.goals.view.table.export", "Export Data"))
  })
  output$in.ga.goals.view.copy.logic <- renderUI({
    checkboxInput(
      "ga.goals.view.copy.logic",
      "Copy Goal Setup"
    )
  })
  
  output$in.ga.goals.view.property.copy <- renderUI({
    if (isTRUE(input$ga.goals.view.copy.logic)) return(
      selectizeInput(
        "ga.goals.view.property.copy",
        label = "Property",
        multiple = TRUE,
        choices = unique(filter(ga_account_list(), accountName == input$ga.account)$webPropertyName)
      )
    )
  })
  output$in.ga.goals.view.view.copy <- renderUI({
    if (isTRUE(input$ga.goals.view.copy.logic)) {
      return(
        selectizeInput(
          "ga.goals.view.view.copy",
          label = "View Setup",
          multiple = TRUE,
          choices = c("MAIN", "TEST", "REGEXP")
        )
      )
    }
  })
  
  regexp_r <- eventReactive(input$ga.goals.view.view.copy, {
    if(input$ga.goals.view.view.copy == "REGEXP") {
      textInput(
        "ga.view.view.copy.regexp",
        label = "View Name Regex"
      ) 
    } else {NULL}
    
  })
  output$in.ga.view.view.copy.regexp <- renderUI({
    return(regexp_r())
  })
  copy_go <- eventReactive(input$ga.goals.view.property.copy,{
    actionButton(
      "ga.goals.view.copy.go",
      "Change GA Goals",
      class = "btn-warning",
      #icon = icon("google", lib = "font-awesome")
    )
  })
  output$in.ga.goals.view.copy.go <- renderUI({
    copy_go()
  })
  
  
  #### SERVER UI GA CHANNEL GROUPING ####
  
  #### SERVER UI GA CONTENT GROUPING #### 
  
  output$in.ga.content.date <- renderUI({
    dateInput(
      "ga.content.date",
      label = "date from",
      value = today() -1,
      max = today()
    )
  })
  
  #### ACCOUNT
  
  #### PROPERTY
  
  content_property_property_choices <- eventReactive(input$ga.account, {
    unique(filter(ga_account_list(), accountName == input$ga.account)$webPropertyName)
  })
  output$in.ga.content.property.property.input <- renderUI({
    if (pok) return(
      selectizeInput(
        "ga.content.property.property.input",
        label = "Properties",
        choices = content_property_property_choices(),
        multiple = TRUE
      )
    )
  })
  output$in.ga.content.property.property.go <- renderUI({
    actionButton(
      "ga.content.property.property.go",
      "Go"
    )
  })
  output$in.ga.content.property.meta.export <- renderUI({
    return(downloadButton("ga.content.property.meta.export", "Export Data"))
  })
  output$in.ga.content.property.meta <- renderUI({
    return(DT::dataTableOutput("ga.content.property.meta"))
  })
  output$in.ga.content.property.table <- renderUI({
    return(DT::dataTableOutput("ga.content.property.table"))
  })
  output$in.ga.content.property.structure <- renderUI({
    return(tableOutput("ga.content.property.structure"))
  })
  
  
  #### VIEW
  
  content_view_property_choices <- eventReactive(input$ga.account, {
    unique(filter(ga_account_list(), accountName == input$ga.account)$webPropertyName)
  })
  output$in.ga.content.view.property.input <- renderUI({
    if (pok) return(
      selectizeInput(
        "ga.content.view.property.input",
        label = "Property",
        choices = content_view_property_choices()
      )
    )
  })
  content_view_view_choices <- eventReactive(input$ga.content.view.property.input, {
    ga_property(input$ga.account, input$ga.content.view.property.input)$viewName
  })
  output$in.ga.content.view.view.input <- renderUI({
    return(
      selectizeInput(
        "ga.content.view.view.input",
        label = "Views",
        choices = content_view_view_choices(),
        multiple = TRUE
      )
    )
  })
  output$in.ga.content.view.view.go <- renderUI({
    actionButton(
      "ga.content.view.view.go",
      "Go"
    )
  })
  output$in.ga.content.view.meta.export <- renderUI({
    return(downloadButton("ga.content.view.meta.export", "Export Data"))
  })
  output$in.ga.content.view.meta <- renderUI({
    return(DT::dataTableOutput("ga.content.view.meta"))
  })
  output$in.ga.content.view.table <- renderUI({
    return(DT::dataTableOutput("ga.content.view.table"))
  })
  output$in.ga.content.view.structure <- renderUI({
    return(tableOutput("ga.content.view.structure"))
  })
  
  
  #### SERVER UI GTM ####
  
  output$gtm.image.powertool <- renderImage({
    list(
      src = "images/DDLPowertool.png",
      contentType = "image/png",
      width = 85,
      height = 85,
      alt = "powertool"
    )
  }, deleteFile = FALSE)
  
  output$in.gtm.account <- renderUI({
    if (pok) return(
      selectInput(
        "gtm.account",
        label = "Account",
        choices = gtm_accounts_list()$name,
        width = "1000px"
      )
    )
  })
  
  #### SERVER UI GTM CONTAINERS ######
  output$in.gtm.containers.table <- renderUI({
    if (pok) return(DT::dataTableOutput("gtm.containers.table"))
  })
  output$in.gtm.containers.table.export <- renderUI({
    input$pss; if (pok) return(downloadButton("gtm.containers.table.export", "Export Data"))
  })
  
  ##### SERVER UI GTM TAGS ####
  
  output$in.gtm.tags.input <- renderUI({
    if (pok) return(
      selectInput(
        "gtm.tags.input",
        label = "Containers",
        choices = gtm_containers_list(gtm_account_id(input$gtm.account))$containers$name,
        multiple = TRUE
      )
    )
  })
  output$in.gtm.tags.go <- renderUI({
    if (pok) return(
      actionButton(
        "gtm.tags.go",
        label = "Go"
      )
    )
  })
  output$in.gtm.tags.container <- renderUI({
    if (pok) return(DT::dataTableOutput("gtm.tags.container"))
  })
  
  output$in.gtm.tags.table <- renderUI({
    if (pok) return(DT::dataTableOutput("gtm.tags.table"))
  })
  output$in.gtm.tags.table.export <- renderUI({
    input$pss; if (pok) return(downloadButton("gtm.tags.table.export", "Export Data"))
  })
  
  #### SERVER UI GTM TRIGGERS ####
  output$in.gtm.triggers.input <- renderUI({
    if (pok) return(
      selectInput(
        "gtm.triggers.input",
        label = "Containers",
        choices = gtm_containers_list(gtm_account_id(input$gtm.account))$containers$name,
        multiple = TRUE
      )
    )
  })
  output$in.gtm.triggers.go <- renderUI({
    if (pok) return(
      actionButton(
        "gtm.triggers.go",
        label = "Go"
      )
    )
  })
  output$in.gtm.triggers.container <- renderUI({
    if (pok) return(DT::dataTableOutput("gtm.triggers.container"))
  })
  output$in.gtm.triggers.table <- renderUI({
    if (pok) return(DT::dataTableOutput("gtm.triggers.table"))
  })
  output$in.gtm.triggers.table.export <- renderUI({
    input$pss; if (pok) return(downloadButton("gtm.triggers.table.export", "Export Data"))
  })
  
  #### SERVER UI GTM VARIABLES ####
  output$in.gtm.variables.input <- renderUI({
    if (pok) return(
      selectInput(
        "gtm.variables.input",
        label = "Containers",
        choices = gtm_containers_list(gtm_account_id(input$gtm.account))$containers$name,
        multiple = TRUE
      )
    )
  })
  output$in.gtm.variables.go <- renderUI({
    if (pok) return(
      actionButton(
        "gtm.variables.go",
        label = "Go"
      )
    )
  })
  output$in.gtm.varibales.container <- renderUI({
    if (pok) return(DT::dataTableOutput("gtm.variables.container"))
  })
  output$in.gtm.variables.table <- renderUI({
    if (pok) return(DT::dataTableOutput("gtm.variables.table"))
  })
  output$in.gtm.variables.table.export <- renderUI({
    input$pss; if (pok) return(downloadButton("gtm.variables.table.export", "Export Data"))
  })
  
  
  ##### Server #####
  
  ## Password {Observe()} ####
  
  observe({
    if (!pok) {
      password <- input$pss
      if (!is.null(password) && password == 'q3947-t(*&G[2938rJurysInnHull01O20(%b') {
        pok <<- TRUE
      }
    }
  })
  
  
  ##### GTM #####
  
  #### GTM CONTAINERS ####
  containers <- reactive({
    containers <- gtm_containers_list(gtm_account_id(input$gtm.account))$containers
    
    #apply filter
    return(apply_filter_logic(containers, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$gtm.containers.table <- DT::renderDataTable({
    containers()
  })
  output$gtm.containers.table.export <- downloadHandler(
    filename = function() {
      "containers.csv"
    }, 
    content = function(file) {
      containers <- containers()
      containers$enabledBuiltInVariable <- vapply(containers$enabledBuiltInVariable, paste, collapse = ", ", character(1L))
      containers$domainName <- vapply(containers$domainName, paste, collapse = ", ", character(1L))
      data <- containers
      names(data) <- tidy_coloumn_names(data)
      fwrite(data, file, row.names = FALSE)
    }
  )
  #### GTM TAGS ####
  tags_container <- reactive({
    containers <- gtm_containers_list(gtm_account_id(input$gtm.account))$containers %>% select(-notes) 
    return(get_containers(containers, input$gtm.tags.input))
  }) 
  output$gtm.tags.container <- DT::renderDataTable({
    return(tags_container())
  })
  tags <- eventReactive(input$gtm.tags.go, {
    data <- gtm_pull_and_bind(tags_container(), unnest_type = "tag")
    
    #apply filter
    return(apply_filter_logic(data, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$gtm.tags.table <- DT::renderDataTable({
    validate(need(input$gtm.tags.input, "Enter Container(s)"))
    tags()
    # TODO: THINK OF A WAY TO BEST DISPLAY NESTED PARAMETERS ON TAGS IN DATATABLE 
    ## ALSO CONSIDER HOW WIRTH IT THIS IS AFTER VIEWING CSV FILES
  })
  output$gtm.tags.table.export <- downloadHandler(
    filename = function() {
      "tags.csv"
    }, 
    content = function(file) {
      data <- tags()
      names(data) <- tidy_coloumn_names(data)
      #write.csv(data, file, row.names = FALSE) 
      write.csv(data, file, row.names = FALSE)
      
    }
  )
  
  #### GTM TRIGGERS ####
  triggers_container <- reactive({
    containers <- containers()
    return(get_containers(containers, input$gtm.triggers.input))
  }) 
  output$gtm.triggers.container <- DT::renderDataTable({
    return(triggers_container())
  })
  triggers <- eventReactive(input$gtm.triggers.go,{
    data <- gtm_pull_and_bind(triggers_container(), unnest_type = "trigger")
    
    #apply filter
    return(apply_filter_logic(data, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$gtm.triggers.table <- DT::renderDataTable({
    validate(need(input$gtm.triggers.input, "Enter Container(s)"))
    triggers()
    # TODO: THINK OF A WAY TO BEST DISPLAY NESTED PARAMETERS ON TAGS IN DATATABLE 
    ## ALSO CONSIDER HOW WIRTH IT THIS IS AFTER VIEWING CSV FILES
  })
  output$gtm.triggers.table.export <- downloadHandler(
    filename = function() {
      "tiggers.csv"
    }, 
    content = function(file) {
      data <- triggers()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  #### GTM VARIABLES ####
  
  variables_container <- reactive({
    containers <- gtm_containers_list(gtm_account_id(input$gtm.account))$containers
    return(get_containers(containers, input$gtm.variables.input))
  }) 
  output$gtm.variables.container <- DT::renderDataTable({
    return(variables_container())
  })
  variables <- eventReactive(input$gtm.variables.go, {
    data <- gtm_pull_and_bind(variables_container(), unnest_type = "variable")
    
    #apply filter
    return(apply_filter_logic(data, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$gtm.variables.table <- DT::renderDataTable({
    validate(need(input$gtm.variables.input, "Enter Container(s)"))
    variables()
    # TODO: THINK OF A WAY TO BEST DISPLAY NESTED PARAMETERS ON TAGS IN DATATABLE 
    ## ALSO CONSIDER HOW WIRTH IT THIS IS AFTER VIEWING CSV FILES
  })
  output$gtm.variables.table.export <- downloadHandler(
    filename = function() {
      "variables.csv"
    }, 
    content = function(file) {
      data <- variables()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  
  ###### GA ######
  
  #### GA DATA HYGIENE NOTES #### 
  
  ## NOTES
  # Points from data studio hygiene audit 
  # able to automate:
  # ...
  
  
  #### GA DATA HYGIENE PROPERTY WEBSITE URL ####
  ga_hygiene_property_website_url_change <-  eventReactive(input$ga.hygiene.websiteUrl.go, {
    ga_property_update(
      property_update = list(
        websiteUrl = input$ga.hygiene.websiteUrl.input
      ),
      accountId = ga_account_id(input$ga.hygiene.account), 
      propertyId = ga_property_id(input$ga.hygiene.account, input$ga.hygiene.property) 
    )
  })
  output$in.ga.hygiene.websiteUrl.go.change <- renderUI({
    ga_hygiene_property_website_url_change()
    HTML('<p STYLE = "color:green"> Website URL changed in GA property settings. </p>')
  })
  
  
  #### HA DATA HYGIENE GENERAL ####
  
  hygiene_account_structure <- eventReactive(input$ga.hygiene.account, {
    filter(ga_account_list(), accountName == input$ga.hygiene.account)
  })
  
  hygiene_view <- eventReactive(input$ga.hygiene.property,{
    ga_property(input$ga.hygiene.account, input$ga.hygiene.property)
  })
  
  hygiene_total_spam <- eventReactive(input$ga.hygiene.go.report, {
    return(sum(spam_referral()$sessions) + sum(browser_version_spam()$sessions))
  })
  output$in.hygiene.total.spam <- renderUI({
    HTML(paste("<h4 style='text-align:center;'> Total Spam: ", hygiene_total_spam(), " sessions</h4>"))
  })
  hygiene_total_filter_map <- reactive({
    #req(referral_exclusion_list())
    #from
    from = nrow(hygiene_ga_filters())
    
    #to
    to = from + length(referral_exclusion_list()) + length(browser_version_exclusion_list()) - (length(filter_remove()$name) + length(browser_version_filter_remove()$name))
    
    return(list(
      from = from,
      to = to
    ))
    
  })
  output$in.hygiene.total.filter_map <- renderUI({
    filterMap <- hygiene_total_filter_map()
    HTML(paste("<h4 style='text-align:center;'> Totals Filter Map: from", filterMap$from, "to", filterMap$to, "</h4>"))
  })
  
  hygiene_ga_pull_traffic <- eventReactive(input$ga.hygiene.go.report,{
    viewId <- ga_view_id(input$ga.hygiene.account, input$ga.hygiene.property, input$ga.hygiene.view)$viewId 
    google_analytics(
      viewId,
      date_range = input$ga.hygiene.dateRange,
      metrics = c("sessions","avgTimeOnPage","bounceRate"),
      dimensions = c("hostname", "source", "channelGrouping"),
      anti_sample = sampling_logic
    )
  })
  hygiene_ga_pull_browser_version <- eventReactive(input$ga.hygiene.go.report,{
    viewId <- ga_view_id(input$ga.hygiene.account, input$ga.hygiene.property, input$ga.hygiene.view)$viewId 
    google_analytics(
      viewId,
      date_range = input$ga.hygiene.dateRange,
      metrics = c("sessions","bounceRate", "avgTimeOnPage","avgSessionDuration", "transactions"),
      dim_filters = filter_clause_ga4(list(dim_filter(dimension = "fullReferrer", operator = "EXACT", expressions = "(direct)"))),
      dimensions = c("browser", "browserVersion"),
      anti_sample = sampling_logic
    )
  })
  hygiene_ga_pull_page <- eventReactive(input$ga.hygiene.go.report,{
    viewId <- ga_view_id(input$ga.hygiene.account, input$ga.hygiene.property, input$ga.hygiene.view)$viewId 
    google_analytics(
      viewId,
      date_range = input$ga.hygiene.dateRange,
      metrics = c("pageviews", "entrances"),
      dimensions = c("pagePath", "landingPagePath"),
      anti_sample = sampling_logic
    )
  })
  hygiene_ga_pull_event <- eventReactive(input$ga.hygiene.go.report,{
    viewId <- ga_view_id(input$ga.hygiene.account, input$ga.hygiene.property, input$ga.hygiene.view)$viewId 
    google_analytics(
      viewId,
      date_range = input$ga.hygiene.dateRange,
      metrics = "totalEvents",
      dimensions = c("eventCategory", "eventAction", "eventLabel"),
      anti_sample = sampling_logic
    )
  })
  hostname <- eventReactive(input$ga.hygiene.go.report,{
    url <- filter(hygiene_account_structure(), viewName == input$ga.hygiene.view)$websiteUrl
    return(get_hostnames_from_url(url))
  })
  hygiene_ga_filters <- eventReactive(input$ga.hygiene.go.report, {
    accountFilters <- ga_filter_list(ga_account_id(input$ga.hygiene.account))
    viewFilters <- ga_filter_view_list(ga_account_id(input$ga.hygiene.account),
                                       ga_property_id(input$ga.hygiene.account, input$ga.hygiene.property), 
                                       ga_view_id(input$ga.hygiene.account, input$ga.hygiene.property, input$ga.hygiene.view)$viewId 
    )
    viewFilters["id"] <- str_replace(viewFilters[["id"]], "^.*:", "" )  
    viewFilters <- merge(accountFilters,viewFilters, by.x = "id", by.y = "id")
  })
  
  
  #### GA DATA HYGIENE HOSTNAME #### 
  
  hygiene_hostnames <- reactive({
    hygiene_ga_pull_traffic() %>%
      select(hostname, sessions) %>%
      group_by(hostname) %>% 
      summarise_if(is.numeric, sum) %>%
      dplyr::arrange(desc(sessions))
  })
  output$ga.hygiene.hostnames.table <- DT::renderDataTable({
    hygiene_hostnames()
  })
  hostname_hygiene_filter <- reactive({
    viewFilters <- hygiene_ga_filters()
    hostnameFilter <- filter(viewFilters, includeDetails.field == "PAGE_HOSTNAME") %>%
      dplyr::select_at(c("id", "accountId", "rank","name", "type", "includeDetails.field", "includeDetails.matchType", "includeDetails.expressionValue", "includeDetails.caseSensitive"))
    return(hostnameFilter)
  })
  output$ga.hygiene.hostnames.filter <- renderTable({
    hostname_hygiene_filter()  
  })
  output$in.ga.hygiene.hostnames.filter.suggestion <- renderUI({
    hostname_hygiene_filter <- hostname_hygiene_filter() 
    
    if (nrow(hostname_hygiene_filter) != 0) {
      from <- hostname_hygiene_filter$includeDetails.expressionValue 
      to <- paste("^(www\\.)?(", paste(str_replace_all(hostname(),"\\.","\\\\." ) , collapse = "|"),")$", sep = "")
      rank <- hostname_hygiene_filter$rank
      if (identical(from, to) && rank == "1") {
        HTML("<h4><b> No Suggested Change </b>.</h4>")
      } else if (!(identical(from, to)) && rank == "1") {
        HTML(paste("<h4><b> Suggested Change: </b> from <b>",from,"</b> to <b>",to,"</b>.</h4>"))  
      } else if ((identical(from, to)) && rank != "1") {
        HTML(paste("<h4><b> Suggested Change: </b> Move rank to 1. </h4>"))  
      } else {
        HTML(paste("<h4><b> Suggested Change: </b> from <b>",from,"</b> to <b>",to,"</b>. In addition move rank to 1. </h4>"))  
      }
      
    } else {
      to <- paste("^(www\\.)?(", paste(str_replace_all(hostname(),"\\.","\\\\." ) , collapse = "|"),")$", sep = "")
      HTML(paste("<h4><b> Suggested Change:</b> Add new filter,<b> ",to,"</b>, and ensure rank is 1.</h4>"))
    }
  })
  ga_hygiene_hostname_change <- eventReactive(input$ga.hygiene.go.change, {
    
    accountId <- ga_account_id(input$ga.hygiene.account)
    webPropertyId <- ga_property_id(input$ga.hygiene.account, input$ga.hygiene.property)
    viewId <- ga_view_id(input$ga.hygiene.account, input$ga.hygiene.property, input$ga.hygiene.view)$viewId
    
    
    hostname_hygiene_filter <- hostname_hygiene_filter() 
    to <- paste("^(www\\.)?(", paste(str_replace_all(hostname(),"\\.","\\\\." ) , collapse = "|"),")$", sep = "")
    
    if (nrow(hostname_hygiene_filter) != 0) {
      from <- hostname_hygiene_filter$includeDetails.expressionValue
      if (identical(from, to)) {
        print("HOSTNAME: No Change")
        response <- NULL
      } else {
        print("HOSTNAME: Change")
        if(is.data.frame(hostname_hygiene_filter) && nrow(hostname_hygiene_filter == 0)) { #if there is already a hostname filter
          print("there is already a hostname filter")
          if(hostname_hygiene_filter[["rank"]] == 1) { #if rank is 1
            print("Rank is 1")
            filter <- list(includeDetails = list(expressionValue = to))
            response <- ga_filter_update(filter,accountId , hostname_hygiene_filter$id, method = "PATCH" )
          } else {
            print("rank is not 1")
            filter <- list(includeDetails = list(expressionValue = to))
            response <- ga_filter_update(
              filter, 
              ga_account_id(input$ga.hygiene.account), 
              hostname_hygiene_filter$id, method = "PATCH"
            )
            print(response)
            response <- ga_filter_update_filter_link(list(rank = 1), accountId, webPropertyId , viewId, linkId = response$id)
          }
        } else {
          repsonse <- NULL
        }
      }
    } else {
      print("HOSTNAME: There is no hostname filter")
      filter <- list(
        name = "INCLUDE: Only Hostname - DDL PowerTool",
        type = "INCLUDE",
        includeDetails = list(
          field = "PAGE_HOSTNAME",
          matchType = "MATCHES",
          expressionValue = to,
          caseSensative = "FALSE"
        )
      )
      response <- ga_filter_add(filter, accountId, webPropertyId, viewId, linkFilter = TRUE)
      print(response)
      response <- ga_filter_update_filter_link(list(rank = 1), accountId, webPropertyId , viewId, linkId = response$id)
    }
    print(response)
    return(response)
  })
  output$in.ga.hygiene.hostnames.filter.change <- renderUI({
    if (!is.null(ga_hygiene_hostname_change())) {
      ga_hygiene_hostname_change()
      HTML('<p STYLE = "color:green"> HOSTNAME: Change Made </p>')
    } else {
      ga_hygiene_hostname_change()
      HTML('<p STYLE = "color:green"> HOSTNAME: No change Required </p>')
    }
  })
  
  #### GA DATA HYGIENE REFERRALS #### 
  
  #### SELF
  self_refferal <- reactive({
    hostname_regex <- paste("^(www\\.)?(", paste(str_replace_all(hostname(),"\\.","\\\\." ) , collapse = "|"),")$", sep = "")
    return(
      hygiene_ga_pull_traffic() %>%
        dplyr::filter(
          grepl("(R|r)eferral(s)?", channelGrouping),
          grepl(hostname_regex, source)
        ) %>%
        select(source, sessions) %>%
        group_by(source) %>%
        summarise_if(is.numeric, sum) %>%
        dplyr::arrange(desc(sessions))   
    )
  })
  output$ga.hygiene.selfReferral.table <- DT::renderDataTable({
    self_refferal() 
  })
  
  #### PAYMENT GATEWAY
  payment_gateway <- reactive({
    payment_gateway_regex <- payment_gateway_regex()
    hygiene_ga_pull_traffic() %>%
      filter(grepl(payment_gateway_regex, source)) %>%
      select(source, sessions) %>%
      group_by(source) %>%
      summarise_if(is.numeric, sum) %>%
      dplyr::arrange(desc(sessions))
  })
  output$ga.hygiene.paymentReferral.table <- DT::renderDataTable({
    payment_gateway()
  })
  
  #### SPAM 
  spam_referral <- reactive({
    #WRITE THE SQL FOR THIS
    hygiene_ga_pull_traffic() %>%
      filter(
        avgTimeOnPage < 1,
        bounceRate > 99
      ) %>%
      select(source, sessions, avgTimeOnPage, bounceRate) %>%
      #group_by(source) %>%
      #summarise_if(is.numeric, sum) %>%
      dplyr::arrange(desc(sessions))
  })
  output$ga.hygiene.spamReferral.table <- DT::renderDataTable({
    spam_referral()
  })
  
  #### REFERRAL 
  referral_filter <- reactive({
    referral_filter <- hygiene_ga_filters() %>% filter(excludeDetails.field == "REFERRAL") %>%
      dplyr::select_at(c("id", "accountId", "rank","name", "type", 
                         "excludeDetails.field", 
                         "excludeDetails.matchType",
                         "excludeDetails.expressionValue",
                         "excludeDetails.caseSensitive"
      )) %>%
      arrange(rank)
    if (nrow(referral_filter) != 0) {
      return(referral_filter)
    } else {
      return(NULL)
    }
  })
  output$ga.hygiene.referrals.filter <- renderTable({
    referral_filter() 
  })
  #This extracts the regex expression from the filters we want to remove because they are not maximimising filter character space in GA -->
  referral_filter_out <- reactive({
    return(filter_out(referral_filter()))
  })
  #Gets all the sources from self , payment gateway and spam referrals and puts into a list of regex (ensureing list doesnt exceed 255 limit) -->
  referral_exclusion_list <- reactive({
    req(input$ga.hygiene.referrals.filter_create_limit)
    referral_exclusion_list <- c(self_refferal()$source, payment_gateway()$source, spam_referral()$source)
    return(
      prepare_list_for_filters(
        ga_filter_table = referral_filter(),
        filter_in = referral_exclusion_list, 
        filter_out = referral_filter_out(), 
        limit_filter_input = input$ga.hygiene.referrals.filter_create_limit 
      )
    )
  })
  
  referral_text <- eventReactive(input$ga.hygiene.go.report,{
    HTML("<h4><b> Suggested change: </b></h4>")
  })
  output$in.ga.hygiene.referrals.suggestion_text <- renderUI({
    referral_text()  
  })
  filter_remove  <- reactive({
    
    
    if (is.null(referral_filter())) {return(NULL)} 
    else {
      referral_filter_out <- referral_filter_out()
      filter_remove <- referral_filter() %>%
        filter(excludeDetails.expressionValue %in% referral_filter_out)
      if(nrow(filter_remove) == 0) {
        return(NULL) 
      } else {
        return(filter_remove)
      }
      
    }
  })
  output$in.ga.hygiene.referrals.suggestion_remove <- renderUI({
    if (is.null(referral_filter()) || is.null(filter_remove())) {return(NULL)} 
    else {
      remove <- paste(filter_remove()$name, collapse = ", ")
      return(
        HTML(paste("<h4> Replace following filters, <b>",remove, "</b>, into more efficient version</h4>"))
      )
    }
  })
  output$in.ga.hygiene.referrals.suggestion_create <- renderUI({
    referral_exclusion_list <- referral_exclusion_list()
    if(is.null(referral_exclusion_list)) {
      HTML("<h4><b> No Suggested Change </b>,</h4>")
    } else {
      HTML(paste("<h4> Create filter: ",referral_exclusion_list, "</h4>"))  
    }
  })
  
  ga_hygiene_referral_change <- eventReactive(input$ga.hygiene.go.change, {
    
    accountId <- ga_account_id(input$ga.hygiene.account)
    webPropertyId <- ga_property_id(input$ga.hygiene.account, input$ga.hygiene.property)
    viewId <- ga_view_id(input$ga.hygiene.account, input$ga.hygiene.property, input$ga.hygiene.view)$viewId
    filter_create_limit <- input$ga.hygiene.referrals.filter_create_limit
    filter <- referral_filter()
    print("ga_filter_table:")
    print(filter)
    hygiene_report <- c(self_refferal()$source, payment_gateway()$source, spam_referral()$source)
    print("filter_in:")
    print(hygiene_report)
    remove <- filter_remove()
    print("filter_out:")
    print(remove)
    referral_exclusion_list <- referral_exclusion_list() 
    
    return(
      ga_hygiene_filter_update(
        accountId,
        webPropertyId,
        viewId,
        limit_filter_input = filter_create_limit,
        hygiene_ga_filters = hygiene_ga_filters(),
        ga_filter_table = filter,
        filter_in = hygiene_report,
        filter_out = remove,
        regex_filter_list = referral_exclusion_list,
        filter_field = "REFERRAL"
      )
    )
    
  })
  output$in.ga.hygiene.referrals.change <- renderUI({
    response <- ga_hygiene_referral_change()
    if (!is.null(response)) {
      response
      HTML('<p STYLE = "color:green"> REFERRAL: Change Made </p>')
    } else {
      response
      HTML('<p STYLE = "color:green"> REFERRAL: No change Required </p>')
    }
  })
  
  #### GA DATA HYGIENE BROWSER SPAM #### 
  
  browser_version_spam <- reactive({
    browser_version_not_spam <- hygiene_ga_pull_browser_version() %>%
      filter(
        avgTimeOnPage > 1,
        avgSessionDuration > 1,
        bounceRate < 99
      ) 
    browser_version_potential_spam <- hygiene_ga_pull_browser_version() %>%
      filter(
        avgTimeOnPage < 1,
        avgSessionDuration < 1,
        bounceRate > 99
      )
    
    #create a list of browser versions to remove from potential spam
    #If browser version of potential spam is inside browser version not spam then remove else keep 
    potential_spam <- unique(browser_version_potential_spam$browserVersion)
    not_spam <- browser_version_not_spam$browserVersion
    
    for (browserVersion in 1:length(potential_spam)) {
      if (potential_spam[browserVersion] %in% not_spam) {
        potential_spam <- potential_spam[!potential_spam %in% potential_spam[browserVersion]]
      } else {
        next
      }
    }
    spam <- potential_spam
    spam <- browser_version_potential_spam %>%
      filter(
        browserVersion %in% spam,
        transactions == 0 
      ) %>%
      arrange(desc(sessions))
    return(spam)
    
    #maximum_characters_in_ga_filter <- 251
    #number_of_filters <- ceiling(nchar(paste(spam, collapse = "|"))/maximum_characters_in_ga_filter)
    #spam_filters <- prepare_list_for_filters(spam, number_of_filters)
    #return(spam_filters)
  }) 
  output$ga.hygiene.spam.table <- DT::renderDataTable({
    browser_version_spam() 
  })
  
  browser_version_filter <- reactive({
    browser_version_filter <- hygiene_ga_filters() %>% filter(excludeDetails.field == "BROWSER_VERSION") %>%
      dplyr::select_at(c("id", "accountId", "rank","name", "type", 
                         "excludeDetails.field", 
                         "excludeDetails.matchType",
                         "excludeDetails.expressionValue",
                         "excludeDetails.caseSensitive"
      )) %>%
      arrange(rank)
    if (nrow(browser_version_filter) != 0) {
      return(browser_version_filter)
    } else {
      return(NULL)
    }
  })
  output$ga.hygiene.browser_spam.filter <- renderTable({
    browser_version_filter() 
  })
  
  browser_version_filter_out <- reactive({
    return(filter_out(browser_version_filter()))
  })
  browser_version_exclusion_list <- reactive({
    req(input$ga.hygiene.browser_spam.filter_create_limit)
    return(
      prepare_list_for_filters(
        ga_filter_table = browser_version_filter(),
        filter_in = browser_version_spam()$browserVersion, 
        filter_out = browser_version_filter_out(), 
        limit_filter_input = input$ga.hygiene.browser_spam.filter_create_limit
      )
    )
  })
  
  browser_version_text <- eventReactive(input$ga.hygiene.go.report,{
    HTML("<h4><b> Suggested change: </b></h4>")
  })
  output$in.ga.hygiene.browser_spam.suggestion_text <- renderUI({
    browser_version_text()  
  })
  browser_version_filter_remove  <- reactive({
    if (is.null(browser_version_filter())) {return(NULL)} 
    else {
      browser_version_filter_out <- browser_version_filter_out()
      return(
        browser_version_filter() %>%
          filter(excludeDetails.expressionValue %in% browser_version_filter_out)
      )
    }
  })
  output$in.ga.hygiene.browser_spam.suggestion_remove <- renderUI({
    if (is.null(browser_version_filter()) || is.null(browser_version_filter_remove())) {return(NULL)} 
    else {
      remove <- paste(browser_version_filter_remove()$name, collapse = ", ")
      return(
        HTML(paste("<h4> Replace following filters, <b>",remove, "</b>, into more efficient version</h4>"))
      )
    }
  })
  output$in.ga.hygiene.browser_spam.suggestion_create <- renderUI({
    browser_version_exclusion_list <- browser_version_exclusion_list()
    if(is.null(browser_version_exclusion_list)) {
      HTML("<h4><b> No Suggested Change </b>,</h4>")
    } else {
      HTML(paste("<h4> Create filter: ",browser_version_exclusion_list, "</h4>"))  
    }
  })
  
  ga_hygiene_browser_version_change <- eventReactive(input$ga.hygiene.go.change, {
    
    accountId <- ga_account_id(input$ga.hygiene.account)
    webPropertyId <- ga_property_id(input$ga.hygiene.account, input$ga.hygiene.property)
    viewId <- ga_view_id(input$ga.hygiene.account, input$ga.hygiene.property, input$ga.hygiene.view)$viewId
    filter_create_limit <- input$ga.hygiene.browser_spam.filter_create_limit    
    filter <- browser_version_filter()
    print(filter)
    hygiene_report <- browser_version_spam()$browserVersion 
    print(hygiene_report)
    remove <- filter_remove()
    print(remove)
    browser_version_exclusion_list <- browser_version_exclusion_list() 
    
    return(
      ga_hygiene_filter_update(
        accountId,
        webPropertyId,
        viewId,
        limit_filter_input = filter_create_limit,
        hygiene_ga_filters = hygiene_ga_filters(),
        ga_filter_table = filter,
        filter_in = hygiene_report,
        filter_out = remove,
        regex_filter_list = browser_version_exclusion_list,
        filter_field = "BROWSER_VERSION"
      )
    )
  })
  output$in.ga.hygiene.browser_spam.change <- renderUI({
    response <- ga_hygiene_browser_version_change()
    if (!is.null(response)) {
      response
      HTML('<p STYLE = "color:green"> Browser version: Change Made </p>')
    } else {
      response
      HTML('<p STYLE = "color:green"> Browser version: No change Required </p>')
    }
  })
  
  #### GA DATA HYGIENE PII #### 
  
  hygiene_page_pii <- reactive({
    hygiene_ga_pull_page() %>%
      filter(grepl(pii_regex(), pagePath)) %>%
      select(pagePath, pageviews) %>%
      group_by(pagePath) %>%
      arrange(desc(pageviews))
  })
  output$ga.hygiene.pii.table.page <- DT::renderDataTable({
    hygiene_page_pii()
  })
  hygiene_pii_query_parameters <- reactive({
    pii <- hygiene_page_pii()$pagePath
    #pii <- c("cheese.com/mapple?email=lewis@google.com&postcode=m36JG&cheese=wensleydale", "bloop/beep?phone=07568706194")
    qp <- unique(extract_query_parameters_from_url(pii))
    return(qp)
  })
  output$in.ga.hygiene.pii.table.page.suggestion <- renderUI({
    pagePath <- hygiene_page_pii()
    if (nrow(pagePath) == 0) {
      HTML("<h4><b> No Suggested Change </b></h4>")
    } else {
      HTML(paste("<h4><b> Suggested Change:</b> Add ",hygiene_pii_query_parameters()," to query parameters </h4>"), sep = "")
    }
  })
  
  hygiene_event_pii <- reactive({
    piiRegex <- pii_regex()
    hygiene_ga_pull_event() %>%
      filter(
        grepl(piiRegex, eventCategory),
        grepl(piiRegex, eventAction),
        grepl(piiRegex, eventLabel)
      ) %>%
      select(eventCategory, eventAction, eventLabel, totalEvents) %>%
      group_by(eventCategory, eventAction, eventLabel) %>%
      arrange(desc(totalEvents))
  })
  output$ga.hygiene.pii.table.event <- DT::renderDataTable({
    hygiene_event_pii()
  })
  
  output$in.ga.hygiene.pii.table.event.suggestion <- renderUI({
    eventData <- hygiene_event_pii()
    if (nrow(eventData) == 0) {
      HTML("<h4><b> No Suggested Change </b></h4>")
    } else {
      HTML(paste("<h4><b> Suggested Change:</b> Add ",NULL," to query parameters </h4>"), sep = "")
    }
  })
  
  
  #### GA DATA HYGIENE PAGES NOT SET #### 
  
  not_set_pages <- reactive({
    hygiene_ga_pull_page() %>%
      select(pagePath, pageviews) %>%
      filter(pagePath == "(not set)") %>%
      group_by(pagePath) %>%
      arrange(desc(pageviews))
  })
  output$ga.hygiene.notSet.page <- DT::renderDataTable({
    not_set_pages() 
  })
  
  not_set_landing_pages <- reactive({
    hygiene_ga_pull_page() %>%
      select(landingPagePath, entrances) %>%
      filter(landingPagePath == "(not set)") %>%
      group_by(landingPagePath) %>%
      arrange(desc(entrances))
  }) 
  output$ga.hygiene.notSet.landingPage <- DT::renderDataTable({
    not_set_landing_pages()
  })
  
  ## GA ACCOUNT STRUCTURE ####
  metadata <- reactive({
    
    accounts <- filter(ga_accounts(), name == input$ga.account)
    accounts <- accounts[3:5]
    
  })
  output$ga.account.metadata <- renderTable({
    metadata()
  })
  
  account_structure <- reactive({
    
    data <- filter(ga_account_list(), accountName == input$ga.account)
    
    #apply filter logic
    return(apply_filter_logic(data, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$ga.account.structure <- DT::renderDataTable({
    account_structure()
  })
  output$ga.account.structure.export <- downloadHandler(
    filename = function() {
      "account_structure.csv"
    }, 
    content = function(file) {
      data <- account_structure()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  #### GA USER MANAGEMENT ####
  users <- reactive({
    #users <- ga_users_list(ga_account_id(input$ga.account), "~all", "~all")
    
    account <- ga_account(input$ga.account)
    
    accountId <- unique(account$accountId)
    propertyIds <- account$webPropertyId
    viewIds <- unique(account$viewId)
    
    userView <- NULL
    for (id in 1:length(viewIds)) {
      if (is.null(userView)) {
        userView <- ga_users_list(accountId, webPropertyId = propertyIds[id], viewId = viewIds[id])
      } else {
        userView <- rbind(userView, ga_users_list(accountId, webPropertyId = propertyIds[id], viewId = viewIds[id]))
      }
    }
    
    
    #apply filter logic
    return(apply_filter_logic(userView, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$ga.users <- DT::renderDataTable({
    users()
  })
  output$ga.users.export <- downloadHandler(
    filename = function() {
      "users.csv"
    }, 
    content = function(file) {
      data <- users()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  #### GA CUSTOM VARIABLES ####
  
  ####ACCOUNT
  ga_custom_vars_table_account <- eventReactive(input$ga.customVar.account.go,{
    accountName <- input$ga.account
    account <- ga_account(accountName)
    accountId <- unique(account$accountId) 
    properties <- unique(account$webPropertyName)
    
    customVars <- NULL
    for (property in 1:length(properties)) {
      if (is.null(customVars)) {
        customVars <- ga_custom_vars_list(
          accountId,
          webPropertyId = ga_property_id(accountName, properties[property]),
          type = c("customDimensions", "customMetrics") 
        )
      } else {
        customVars <- rbind(customVars, ga_custom_vars_list(
          accountId,
          webPropertyId = ga_property_id(accountName, properties[property]),
          type = c("customDimensions", "customMetrics") 
        ))
      }
    }
    
    #apply filter logic
    return(apply_filter_logic(customVars, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$ga.customVar.account.table <- DT::renderDataTable({
    ga_custom_vars_table_account()
  })
  output$ga.customVar.account.table.export <- downloadHandler(
    filename = function() {
      "customVar_account.csv"
    }, 
    content = function(file) {
      data <- ga_custom_vars_table_account()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  #### PROPERTY
  ga_custom_vars_table_property <- eventReactive(input$ga.customVar.property,{
    accountName <- input$ga.account
    accountId <- ga_account_id(input$ga.account)
    properties <- input$ga.customVar.property
    
    customVars <- NULL
    for (property in 1:length(properties)) {
      if (is.null(customVars)) {
        customVars <- ga_custom_vars_list(
          accountId,
          webPropertyId = ga_property_id(accountName, properties[property]),
          type = c("customDimensions", "customMetrics") 
        )
      } else {
        customVars <- rbind(customVars, ga_custom_vars_list(
          accountId,
          webPropertyId = ga_property_id(accountName, properties[property]),
          type = c("customDimensions", "customMetrics") 
        ))
      }
    }
    return(apply_filter_logic(customVars, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$ga.customVar.table <- DT::renderDataTable({
    ga_custom_vars_table_property()
  })
  output$ga.customVar.property.table.export <- downloadHandler(
    filename = function() {
      "customVar_property.csv"
    }, 
    content = function(file) {
      data <- ga_custom_vars_table_property()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  #### GA FILTERS ####
  
  ### ACCOUNT
  
  filters_account_account <- reactive({
    data <- ga_filter_list(ga_account_id(input$ga.account))
    #apply filter
    return(apply_filter_logic(data, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  }) 
  output$ga.filters.account.account.table <- DT::renderDataTable({
    filters_account_account()
  })
  output$ga.filters.account.account.export <- downloadHandler(
    filename = function() {
      "filters_account_account.csv"
    }, 
    content = function(file) {
      data <- filters_account_account()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  filters_account <- eventReactive(input$ga.filters.account.go, {
    account <- ga_account(input$ga.account)
    accountId <- unique(account$accountId)
    propertyIds <- account$webPropertyId
    viewIds <- account$viewId
    
    filters <- NULL
    for (propertyId in 1:length(propertyIds)) {
      if (is.null(filters)) {
        filters <- ga_filter_view_list(accountId, propertyIds[propertyId], viewIds[propertyId])
      } else {
        filters <- rbind(filters, ga_filter_view_list(accountId, propertyIds[propertyId], viewIds[propertyId]))
      }
    }
    #Apply Filter Logic
    data <- filters
    return(apply_filter_logic(data, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$ga.filters.account.table <- DT::renderDataTable({
    filters_account()
  })
  output$ga.filters.account.export <- downloadHandler(
    filename = function() {
      "filters_account.csv"
    }, 
    content = function(file) {
      data <- filters_account()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  #### PROPERTY
  
  filters_property_structure <- reactive({
    property <- input$ga.filters.property.input
    ga_property(input$ga.account, property)
  })
  output$ga.filters.property.structure <- renderTable({
    filters_property_structure()
  })
  filters_property <- eventReactive(input$ga.filters.property.go, {
    account <- filters_property_structure()
    
    accountId <- account$accountId[[1]]
    propertyIds <- account$webPropertyId
    viewIds <- account$viewId
    
    filters <- NULL
    
    for (propertyId in 1:length(propertyIds)) {
      if (is.null(filters)) {
        filters <- ga_filter_view_list(accountId, propertyIds[propertyId], viewIds[propertyId])
      } else {
        filters <- rbind(filters, ga_filter_view_list(accountId, propertyIds[propertyId], viewIds[propertyId]))
      }
    }
    
    #Apply Filter Logic
    data <- filters
    return(apply_filter_logic(data, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
    
  })
  output$ga.filters.property.table <- DT::renderDataTable({
    filters_property()
  })
  output$ga.filters.property.table.export <- downloadHandler(
    filename = function() {
      "filters_property.csv"
    }, 
    content = function(file) {
      data <- filters_property()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  #### VIEW
  
  filters_view_property <- reactive({
    ga_property(input$ga.account, input$ga.filters.view.property.input)
  })
  filters_view_structure <- reactive({
    ga_view_id(input$ga.account, input$ga.filters.view.property.input, input$ga.filters.view.view.input)
  })
  filters_view <- reactive({
    data <- filters_view_structure()
    data <-ga_filter_view_list(data$accountId, data$webPropertyId, data$viewId)
    #Apply Filter Logic
    return(apply_filter_logic(data, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$ga.filters.view.structure <- renderTable({
    filters_view_structure()
  })
  output$ga.filters.view.table <- DT::renderDataTable({
    filters_view()
  })
  output$ga.filters.view.table.export <- downloadHandler(
    filename = function() {
      "filters_view.csv"
    }, 
    content = function(file) {
      data <- filters_view()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE
      )
    }
  )
  
  #### GA GOALS ####
  
  #### ACCOUNT
  goals_account <- eventReactive(input$ga.goals.account.go,{
    
    account <- ga_account(input$ga.account)
    
    #account <- ga_account("All Value Retail Websites (Roll Up)")
    #properties <- c("UA-50487017-1", "UA-50487017-4", "UA-50487017-2")
    #accountStructure <- ga_property("All Value Retail Websites (Roll Up)", properties, "id")
    
    properties <- unique(account$webPropertyId)
    accountStructure <- ga_property(input$ga.account, properties,"id")
    
    
    goals <- goal_loop(accountStructure)
    
    #apply filter
    return(apply_filter_logic(goals, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
    
  })
  output$ga.goals.account.table <- DT::renderDataTable({
    goals_account()
  })
  output$ga.goals.account.table.export <- downloadHandler(
    filename = function() {
      "goals_account.csv"
    }, 
    content = function(file) {
      data <- goals_account()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  #### PROPERTY 
  goals_property_structure <- reactive({
    property <- input$ga.goals.property.input
    ga_property(input$ga.account, property)
  })
  output$ga.goals.property.structure <- renderTable({
    validate(need(input$ga.goals.property.input, "Enter properties to see account structure and 'Go' for Goals"))
    goals_property_structure()
  })
  goals_property <- eventReactive(input$ga.goals.property.go, {
    account <- goals_property_structure()
    goals <- goal_loop(account)
    
    #apply filter
    return(apply_filter_logic(goals, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$ga.goals.property.table <- DT::renderDataTable({
    goals_property()
  })
  output$ga.goals.property.table.export <- downloadHandler(
    filename = function() {
      "goals_property.csv"
    }, 
    content = function(file) {
      data <- goals_property()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  #### VIEW
  goals_view_property <- reactive({
    ga_property(input$ga.account, input$ga.goals.view.property.input)
  })
  goals_view_view <- reactive({
    ga_view_id(input$ga.account, input$ga.goals.view.property.input, input$ga.goals.view.view.input)
  })
  output$ga.goals.view.structure <- renderTable({
    goals_view_view()
  })
  goals_view <- reactive({
    account <- goals_view_view()
    accountId <- account$accountId
    propertyId <- account$webPropertyId
    viewId <- account$viewId
    goals <- ga_goal_list(accountId = accountId, webPropertyId = propertyId, profileId = viewId)
    
    #apply filter
    return(apply_filter_logic(goals, input$filter.logic, input$filter.dimension, input$filter.operator, input$filter.expression))
  })
  output$ga.goals.view.table <- DT::renderDataTable({
    goals_view()
  })
  output$ga.goals.view.table.export <- downloadHandler(
    filename = function() {
      "goals_view.csv"
    }, 
    content = function(file) {
      data <- goals_view()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE
      )
    }
  )
  
  #### GA CHANNEL GROUPING ####
  
  #### GA CONTENT GROUPING ####
  
  #### ACCOUNT
  
  #### PROPERTY
  
  ga_content_grouping_property_structure <- eventReactive(input$ga.content.property.property.input, {
    ga_property(input$ga.account, input$ga.content.property.property.input)
  })
  output$ga.content.property.structure <- renderTable({
    ga_content_grouping_property_structure()
  })
  
  ga_content_grouping_property <- eventReactive(input$ga.content.property.property.go, {
    accountName <- input$ga.account
    propertyNames <- input$ga.content.property.property.input
    properties <- ga_property(accountName, propertyNames)
    accountId <- unique(properties$accountId)
    propertyIds <- unique(properties$webPropertyId)
    #print(propertyIds)
    content <- NULL
    for (property in 1:length(propertyIds)) { #for each property
      propertyId <- propertyIds[property]
      viewIds <- ga_property(accountName, propertyId, type = "id")$viewId
      #print(viewIds)
      for (view in 1:length(viewIds)) { #for each view within property
        if (is.null(content)) {
          content <- google_analytics(
            viewId = viewIds[view],
            date_range = c(input$ga.content.date, today()),
            dimensions = c("contentGroup1", "contentGroup2", "contentGroup3", "contentGroup4", "contentGroup5"),
            metrics = "sessions",
            anti_sample = TRUE
          )
          if (!is.null(content)) {content <- content} else {
            content <- data.frame(
              contentGroup1 = "(not set)", contentGroup2 = "(not set)", contentGroup3 = "(not set)", contentGroup4 = "(not set)", contentGroup5 = "(not set)",
              sessions = NA
            )
          }
          content["viewId"] <- viewIds[view]
          content["propertyId"] <- propertyId
        } else {
          contentIn <- google_analytics(
            viewId = viewIds[view],
            date_range = c(input$ga.content.date, today()),
            dimensions = c("contentGroup1", "contentGroup2", "contentGroup3", "contentGroup4", "contentGroup5"),
            metrics = "sessions",
            anti_sample = TRUE
          )
          if (!is.null(contentIn)) {contentIn <- contentIn} else {
            contentIn <- data.frame(
              contentGroup1 = "(not set)", contentGroup2 = "(not set)", contentGroup3 = "(not set)", contentGroup4 = "(not set)", contentGroup5 = "(not set)",
              sessions = NA
            )
          }
          contentIn["viewId"] <- viewIds[view] 
          contentIn["propertyId"] <- propertyId
          
          content <- rbind(content, contentIn)
        }
      }
    }
    return(content)
    
  })
  ga_content_grouping_property_meta <- eventReactive(input$ga.content.view.view.go, {
    accountName <- input$ga.account
    propertyNames <- input$ga.content.view.property.input
    #viewNames <- input$ga.content.view.view.input
    if (length(propertyNames) > 0) {
      return(content_grouping_loop(ga_content_grouping_property(), accountName, propertyNames))  
    } else {NULL}
    
  })
  output$ga.content.property.meta <- DT::renderDataTable({
    ga_content_grouping_property_meta()  
  })
  output$ga.content.property.meta.export <- downloadHandler(
    filename = function() {
      "content_grouping_property.csv"
    }, 
    content = function(file) {
      data <- ga_content_grouping_property_meta()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  output$ga.content.property.table <- DT::renderDataTable({
    ga_content_grouping_property()
  })
  
  #### VIEW 
  
  ga_content_grouping_view_structure <- eventReactive(input$ga.content.view.view.input, {
    ga_view_id(input$ga.account, input$ga.content.view.property.input, input$ga.content.view.view.input)
  })
  output$ga.content.view.structure <- renderTable({
    ga_content_grouping_view_structure()
  })
  
  ga_content_grouping_view <- eventReactive(input$ga.content.view.view.go, {
    
    viewIds <- ga_view_id(input$ga.account, input$ga.content.view.property.input, input$ga.content.view.view.input)$viewId
    
    if (!is.na(viewIds)) {
      content <- NULL
      for (view in 1:length(viewIds)) {
        if (is.null(content)) {
          content <- google_analytics(
            viewId = viewIds[view],
            date_range = c(input$ga.content.date, today()),
            dimensions = c("contentGroup1", "contentGroup2", "contentGroup3", "contentGroup4", "contentGroup5"),
            metrics = "sessions",
            anti_sample = TRUE
          )
          if (!is.null(content)) {content <- content} else {
            content <- data.frame(
              contentGroup1 = "(not set)", contentGroup2 = "(not set)", contentGroup3 = "(not set)", contentGroup4 = "(not set)", contentGroup5 = "(not set)",
              sessions = NA
            )
          }
          content["viewId"] <- viewIds[view] 
        } else {
          contentIn <- google_analytics(
            viewId = viewIds[view],
            date_range = c(input$ga.content.date, today()),
            dimensions = c("contentGroup1", "contentGroup2", "contentGroup3", "contentGroup4", "contentGroup5"),
            metrics = "sessions",
            anti_sample = TRUE
          )
          if (!is.null(contentIn)) {contentIn <- contentIn} else {
            contentIn <- data.frame(
              contentGroup1 = "(not set)", contentGroup2 = "(not set)", contentGroup3 = "(not set)", contentGroup4 = "(not set)", contentGroup5 = "(not set)",
              sessions = NA
            )
          }
          contentIn["viewId"] <- viewIds[view] 
          
          content <- rbind(content, contentIn)
        }
      }
      return(content)  
    } else {NULL}
    
  })
  ga_content_grouping_view_meta <- eventReactive(input$ga.content.view.view.go, {
    accountName <- input$ga.account
    propertyName <- input$ga.content.view.property.input
    viewNames <- input$ga.content.view.view.input
    if (length(viewNames) > 0) {
      return(content_grouping_loop(ga_content_grouping_view(), accountName, propertyName, viewNames))  
    } else {NULL}
    
    
  })
  
  output$ga.content.view.meta <- DT::renderDataTable({
    ga_content_grouping_view_meta()  
  })
  output$ga.content.view.meta.export <- downloadHandler(
    filename = function() {
      "content_grouping_view.csv"
    }, 
    content = function(file) {
      data <- ga_content_grouping_view_meta()
      names(data) <- tidy_coloumn_names(data)
      write.csv(data, file, row.names = FALSE)
    }
  )
  output$ga.content.view.table <- DT::renderDataTable({
    ga_content_grouping_view()
  })
  
}

##### Run App ####
shinyApp(ui, server)







