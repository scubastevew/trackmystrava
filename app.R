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

# Modified OAuth function for deployed environment
strava_oauth <- function(session) {
  # First, try to get token directly using client credentials
  token_url <- "https://www.strava.com/oauth/token"
  
  response <- POST(
    url = token_url,
    body = list(
      client_id = client_id,
      client_secret = client_secret,
      grant_type = "client_credentials"
    ),
    encode = "json"
  )
  
  if (status_code(response) == 200) {
    token_data <- fromJSON(rawToChar(response$content))
    token <- Token2.0$new(
      app = oauth_app(
        appname = app_name,
        key = client_id,
        secret = client_secret
      ),
      endpoint = oauth_endpoint(
        authorize = "https://www.strava.com/oauth/authorize",
        access = "https://www.strava.com/oauth/token"
      ),
      credentials = list(
        access_token = token_data$access_token,
        token_type = "Bearer"
      )
    )
    return(token)
  } else {
    # If client credentials flow fails, try alternative authentication
    token <- oauth2.0_token(
      endpoint = oauth_endpoint(
        authorize = "https://www.strava.com/oauth/authorize",
        access = "https://www.strava.com/oauth/token"
      ),
      app = oauth_app(
        appname = app_name,
        key = client_id,
        secret = client_secret,
        redirect_uri = "https://connect.posit.cloud"
      ),
      scope = "activity:read_all,read,profile:read_all",
      cache = FALSE,
      credentials = list(
        grant_type = "authorization_code"
      )
    )
    return(token)
  }
}

# Enhanced fetch_strava_activities function with better error handling
fetch_strava_activities <- function(token) {
  url <- "https://www.strava.com/api/v3/athlete/activities"
  response <- GET(url, config(token = token), query = list(per_page = 200))
  
  # More detailed error handling
  if (status_code(response) != 200) {
    error_content <- tryCatch(
      fromJSON(rawToChar(response$content)),
      error = function(e) list(message = "Unknown error")
    )
    stop(paste("Failed to fetch activities from Strava:",
               status_code(response),
               error_content$message))
  }
  
  # Add error handling for JSON parsing
  activities <- tryCatch(
    fromJSON(rawToChar(response$content)),
    error = function(e) {
      stop("Failed to parse Strava response")
    }
  )
  
  # Check if activities is empty
  if (length(activities) == 0) {
    stop("No activities found in Strava response")
  }
  
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

# UI remains exactly the same as your original code
ui <- page_sidebar(
  title = "My Strava Dashboard",
  theme = bs_theme(bootswatch = "flatly"),
  
  sidebar = sidebar(
    actionButton("auth", "Connect to Strava", class = "btn-primary"),
    verbatimTextOutput("auth_status"),
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
  token <- reactiveVal(NULL)
  activity_data <- reactiveVal(NULL)
  auth_status <- reactiveVal("")
  
  # Modified authentication handling
  observeEvent(input$auth, {
    tryCatch({
      auth_status("Authenticating with Strava...")
      token_obj <- strava_oauth(session)  # Pass session to the OAuth function
      
      if (is.null(token_obj) || !inherits(token_obj, "Token2.0")) {
        auth_status("Authentication failed: Invalid token received")
        return()
      }
      
      token(token_obj)
      activities <- fetch_strava_activities(token_obj)
      activity_data(activities)
      auth_status("Successfully connected to Strava!")
      
    }, error = function(e) {
      auth_status(paste("Authentication error:", conditionMessage(e)))
    })
  })
  
  # Rest of your server code remains the same
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
