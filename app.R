library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(DT)
library(httr)
library(jsonlite)
library(leaflet)
library(googlePolylines)
library(bsicons)

# Strava API credentials
app_name <- Sys.getenv("STRAVA_APP_NAME")
client_id <- Sys.getenv("STRAVA_CLIENT_ID")
client_secret <- Sys.getenv("STRAVA_CLIENT_SECRET")

# Modified OAuth function for Posit Connect
strava_oauth <- function(app_name, client_id, client_secret, session) {
  host_url <- session$clientData$url_hostname
  path <- session$clientData$url_pathname
  redirect_uri <- sprintf("https://%s%s", host_url, path)
  
  auth_url <- sprintf(
    "https://www.strava.com/oauth/authorize?client_id=%s&response_type=code&redirect_uri=%s&scope=activity:read_all,read,profile:read_all",
    client_id,
    utils::URLencode(redirect_uri, reserved = TRUE)
  )
  
  list(
    auth_url = auth_url,
    redirect_uri = redirect_uri
  )
}

# Function to exchange code for token
get_token <- function(code, client_id, client_secret, redirect_uri) {
  response <- POST(
    "https://www.strava.com/oauth/token",
    body = list(
      client_id = client_id,
      client_secret = client_secret,
      code = code,
      grant_type = "authorization_code",
      redirect_uri = redirect_uri
    ),
    encode = "form"
  )
  
  if (status_code(response) != 200) {
    stop("Failed to obtain token")
  }
  
  fromJSON(rawToChar(response$content))
}

# Function to fetch activity details
fetch_activity_details <- function(access_token, activity_id) {
  url <- sprintf("https://www.strava.com/api/v3/activities/%s", activity_id)
  
  response <- GET(url, 
                 add_headers(Authorization = paste("Bearer", access_token)))
  
  if (status_code(response) != 200) {
    return(NULL)
  }
  
  fromJSON(rawToChar(response$content))
}

fetch_strava_activities <- function(access_token, start_date = NULL, end_date = NULL) {
  url <- "https://www.strava.com/api/v3/athlete/activities"
  
  query_params <- list(per_page = 200)
  if (!is.null(start_date)) {
    query_params$after <- as.numeric(as.POSIXct(start_date))
  }
  if (!is.null(end_date)) {
    query_params$before <- as.numeric(as.POSIXct(end_date)) + 86400
  }
  
  response <- GET(url, 
                 add_headers(Authorization = paste("Bearer", access_token)),
                 query = query_params)
  
  if (status_code(response) != 200) {
    stop("Failed to fetch activities from Strava")
  }
  
  activities <- fromJSON(rawToChar(response$content))
  
  if (length(activities) == 0 || nrow(activities) == 0) {
    return(data.frame(
      id = numeric(),
      date = as.Date(character()),
      type = character(),
      country = character(),
      distance = numeric(),
      duration = numeric(),
      elevation = numeric(),
      avg_speed = numeric(),
      avg_watts = numeric(),
      max_watts = numeric(),
      avg_heartrate = numeric(),
      max_heartrate = numeric(),
      calories = numeric()
    ))
  }
  
  df <- data.frame(
    id = activities$id,
    date = as.Date(activities$start_date),
    type = activities$type,
    country = ifelse(is.null(activities$location_country) | is.na(activities$location_country) | activities$location_country == "", 
                    "Unknown", 
                    activities$location_country),
    distance = activities$distance / 1000,
    duration = activities$moving_time / 60,
    elevation = activities$total_elevation_gain,
    avg_speed = activities$average_speed * 3.6,
    avg_watts = activities$average_watts,
    max_watts = activities$max_watts,
    avg_heartrate = activities$average_heartrate,
    max_heartrate = activities$max_heartrate,
    calories = activities$kilojoules
  )
  
  df %>%
    filter(!type %in% c("Walk", "WeightTraining", "Swim")) %>%
    arrange(desc(date))
}

ui <- page_sidebar(
  title = "My Strava Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    verbatimTextOutput("auth_status"),
    uiOutput("auth_link"),
    selectInput("activity_type", "Activity Type",
                choices = c("All", "Run", "Ride", "VirtualRide")),
    dateRangeInput("date_range", "Date Range",
                   start = Sys.Date() - 180,
                   end = Sys.Date()),
    actionButton("search", "Search Activities", class = "btn-primary", 
                style = "margin-top: 10px; width: 100%;")
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(
      title = "Total Distance",
      value = textOutput("total_distance"),
      showcase = bsicons::bs_icon("map"),
      theme = "primary"
    ),
    value_box(
      title = "Total Activities",
      value = textOutput("total_activities"),
      showcase = bsicons::bs_icon("activity"),
      theme = "secondary"
    ),
    value_box(
      title = "Total Elevation",
      value = textOutput("total_elevation"),
      showcase = bsicons::bs_icon("graph-up"),
      theme = "success"
    )
  ),
  
  layout_columns(
    card(
      card_header("Activity Route"),
      leafletOutput("activity_map", height = "400px")
    ),
    card(
      card_header("Activity Details"),
      uiOutput("activity_details")
    )
  ),
  
  card(
    card_header("Recent Activities"),
    DTOutput("activity_table")
  )
)

server <- function(input, output, session) {
  access_token <- reactiveVal(NULL)
  activity_data <- reactiveVal(NULL)
  auth_status <- reactiveVal("")
  activity_details <- reactiveVal(NULL)
  
  observe({
    auth_status("Initiating Strava authentication...")
    oauth_config <- strava_oauth(app_name, client_id, client_secret, session)
    
    output$auth_link <- renderUI({
      tags$div(
        style = "margin-top: 10px;",
        tags$a(
          href = oauth_config$auth_url,
          #target = "_blank",
          class = "btn btn-success btn-sm",
          "Click here to authorize with Strava"
        )
      )
    })
    
    auth_status("Please click the authorization link above.")
  })
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code)) {
      tryCatch({
        auth_status("Processing authentication...")
        
        host_url <- session$clientData$url_hostname
        path <- session$clientData$url_pathname
        redirect_uri <- sprintf("https://%s%s", host_url, path)
        
        token_data <- get_token(query$code, client_id, client_secret, redirect_uri)
        access_token(token_data$access_token)
        
        activities <- fetch_strava_activities(
          token_data$access_token,
          input$date_range[1],
          input$date_range[2]
        )
        activity_data(activities)
        
        auth_status("Successfully connected to Strava!")
        
      }, error = function(e) {
        auth_status(paste("Authentication error:", conditionMessage(e)))
      })
    }
  })
  
  observeEvent(input$search, {
    req(access_token())
    
    tryCatch({
      auth_status("Fetching activities...")
      
      activities <- fetch_strava_activities(
        access_token(),
        input$date_range[1],
        input$date_range[2]
      )
      activity_data(activities)
      
      auth_status("Activities updated!")
      
    }, error = function(e) {
      auth_status(paste("Error fetching activities:", conditionMessage(e)))
    })
  })
  
  activities <- reactive({
    req(activity_data())
    
    data <- activity_data()
    
    if (input$activity_type != "All") {
      data <- data %>% filter(type == input$activity_type)
    }
    
    data
  })
  
  observeEvent(input$activity_table_rows_selected, {
    req(access_token(), activities(), input$activity_table_rows_selected)
    
    selected_activity <- activities()[input$activity_table_rows_selected, ]
    details <- fetch_activity_details(access_token(), selected_activity$id)
    activity_details(details)
  })
  
  output$activity_map <- renderLeaflet({
    req(activity_details())
    details <- activity_details()
    
    if (is.null(details$map$summary_polyline) || details$map$summary_polyline == "") {
      return(leaflet() %>%
        addTiles() %>%
        setView(lng = 0, lat = 0, zoom = 2) %>%
        addMarkers(lng = 0, lat = 0, popup = "No route data available"))
    }
    
    # Decode the polyline using googlePolylines
    coords <- decode(details$map$summary_polyline)[[1]]
    
    # Create the map
    leaflet() %>%
      addTiles() %>%
      addPolylines(
        lng = coords$lon,
        lat = coords$lat,
        color = "red",
        weight = 3,
        opacity = 0.8
      ) %>%
      fitBounds(
        lng1 = min(coords$lon),
        lat1 = min(coords$lat),
        lng2 = max(coords$lon),
        lat2 = max(coords$lat)
      )
  })

    output$activity_table <- renderDT({
    req(activities())
    activities() %>%
      arrange(desc(date)) %>%
      select(date, type, country, distance, duration, elevation, 
             avg_speed, avg_watts, max_watts, 
             avg_heartrate, max_heartrate, calories) %>%
      mutate(
        across(where(is.numeric), ~ifelse(is.na(.), NA, round(., 1)))
      ) %>%
      datatable(
        selection = 'single',
        options = list(
          pageLength = 5,
          columnDefs = list(
            list(
              targets = c(7:12),
              render = JS(
                "function(data, type, row, meta) {
                return data === null ? 'N/A' : data;
              }"
              )
            )
          )
        )
      )
  })
  
  output$activity_details <- renderUI({
    req(activities(), input$activity_table_rows_selected)
    
    selected_activity <- activities()[input$activity_table_rows_selected, ]
    
    div(
      layout_columns(
        value_box(
          title = "Distance",
          value = paste(round(selected_activity$distance, 1), "km"),
          showcase = bsicons::bs_icon("map")
        ),
        value_box(
          title = "Duration",
          value = paste(round(selected_activity$duration, 0), "min"),
          showcase = bsicons::bs_icon("clock")
        )
      ),
      layout_columns(
        value_box(
          title = "Elevation",
          value = paste(round(selected_activity$elevation, 0), "m"),
          showcase = bsicons::bs_icon("graph-up")
        ),
        value_box(
          title = "Avg Speed",
          value = paste(round(selected_activity$avg_speed, 1), "km/h"),
          showcase = bsicons::bs_icon("speedometer")
        )
      ),
      layout_columns(
        value_box(
          title = "Avg Watts",
          value = if(is.na(selected_activity$avg_watts)) "N/A" 
                  else paste(round(selected_activity$avg_watts, 0), "W"),
          showcase = bsicons::bs_icon("lightning")
        ),
        value_box(
          title = "Max Watts",
          value = if(is.na(selected_activity$max_watts)) "N/A" 
                  else paste(round(selected_activity$max_watts, 0), "W"),
          showcase = bsicons::bs_icon("lightning-charge")
        )
      ),
      layout_columns(
        value_box(
          title = "Avg Heart Rate",
          value = if(is.na(selected_activity$avg_heartrate)) "N/A" 
                  else paste(round(selected_activity$avg_heartrate, 0), "bpm"),
          showcase = bsicons::bs_icon("heart-pulse")
        ),
        value_box(
          title = "Max Heart Rate",
          value = if(is.na(selected_activity$max_heartrate)) "N/A" 
                  else paste(round(selected_activity$max_heartrate, 0), "bpm"),
          showcase = bsicons::bs_icon("heart")
        )
      )
    )
  })
  
  output$auth_status <- renderText({
    auth_status()
  })
  
  output$total_distance <- renderText({
    req(activities())
    paste(round(sum(activities()$distance), 1), "km")
  })
  
  output$total_activities <- renderText({
    req(activities())
    nrow(activities())
  })
  
  output$total_elevation <- renderText({
    req(activities())
    paste(round(sum(activities()$elevation), 0), "m")
  })
}

shinyApp(ui, server)
