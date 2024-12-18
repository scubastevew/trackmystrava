library(shiny)
library(dplyr)
library(ggplot2)
library(bslib)
library(DT)
library(httr)
library(jsonlite)

# Strava API credentials
app_name <- Sys.getenv("STRAVA_APP_NAME")
client_id <- Sys.getenv("STRAVA_CLIENT_ID")
client_secret <- Sys.getenv("STRAVA_CLIENT_SECRET")

# Modified OAuth function for Posit Connect
strava_oauth <- function(app_name, client_id, client_secret, session) {
  # Get the full URL of the app
  host_url <- session$clientData$url_hostname
  path <- session$clientData$url_pathname
  
  # Construct the redirect URI
  redirect_uri <- sprintf("https://%s%s", host_url, path)
  
  # Create the authorization URL
  auth_url <- sprintf(
    "https://www.strava.com/oauth/authorize?client_id=%s&response_type=code&redirect_uri=%s&scope=activity:read_all,read,profile:read_all",
    client_id,
    utils::URLencode(redirect_uri, reserved = TRUE)
  )
  
  # Return the authorization URL and other info
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

fetch_strava_activities <- function(access_token) {
  url <- "https://www.strava.com/api/v3/athlete/activities"
  response <- GET(url, 
                 add_headers(Authorization = paste("Bearer", access_token)),
                 query = list(per_page = 200))
  
  if (status_code(response) != 200) {
    stop("Failed to fetch activities from Strava")
  }
  
  activities <- fromJSON(rawToChar(response$content))
  
  df <- data.frame(
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
    # Add UI element for the authorization link
    uiOutput("auth_link"),
    selectInput("activity_type", "Activity Type",
                choices = c("All", "Run", "Ride", "VirtualRide")),
    dateRangeInput("date_range", "Date Range",
                   start = Sys.Date() - 180,
                   end = Sys.Date())
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
      card_header("Activities Over Time"),
      plotOutput("time_series")
    ),
    card(
      card_header("Distance by Activity Type"),
      plotOutput("activity_dist")
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
  
  # Initialize authentication on app load
  observe({
    auth_status("Initiating Strava authentication...")
    
    # Get OAuth config
    oauth_config <- strava_oauth(app_name, client_id, client_secret, session)
    
    # Create clickable link
    output$auth_link <- renderUI({
      tags$div(
        style = "margin-top: 10px;",
        tags$a(
          href = oauth_config$auth_url,
          target = "_blank",
          class = "btn btn-success btn-sm",
          "Click here to authorize with Strava"
        )
      )
    })
    
    auth_status("Please click the authorization link above.")
  })
  
  # Handle OAuth callback
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$code)) {
      tryCatch({
        auth_status("Processing authentication...")
        
        # Get the redirect URI
        host_url <- session$clientData$url_hostname
        path <- session$clientData$url_pathname
        redirect_uri <- sprintf("https://%s%s", host_url, path)
        
        # Exchange code for token
        token_data <- get_token(query$code, client_id, client_secret, redirect_uri)
        access_token(token_data$access_token)
        
        # Fetch activities
        activities <- fetch_strava_activities(token_data$access_token)
        activity_data(activities)
        
        auth_status("Successfully connected to Strava!")
        
      }, error = function(e) {
        auth_status(paste("Authentication error:", conditionMessage(e)))
      })
    }
  })
  
  output$auth_status <- renderText({
    auth_status()
  })
  
  activities <- reactive({
    req(activity_data())
    
    data <- activity_data() %>%
      filter(date >= input$date_range[1],
             date <= input$date_range[2])
    
    if (input$activity_type != "All") {
      data <- data %>% filter(type == input$activity_type)
    }
    
    data
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
  
  output$time_series <- renderPlot({
    req(activities())
    ggplot(activities(), aes(x = date, y = distance, color = type)) +
      geom_point() +
      geom_smooth(method = "loess", se = FALSE) +
      theme_minimal() +
      labs(x = "Date", y = "Distance (km)", color = "Activity Type") +
      theme(legend.position = "bottom")
  })
  
  output$activity_dist <- renderPlot({
    req(activities())
    activities() %>%
      group_by(type) %>%
      summarise(total_distance = sum(distance)) %>%
      ggplot(aes(x = reorder(type, -total_distance), y = total_distance, fill = type)) +
      geom_col() +
      theme_minimal() +
      labs(x = "Activity Type", y = "Total Distance (km)") +
      theme(legend.position = "none")
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
}

shinyApp(ui, server)
