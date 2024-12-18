# Modify the fetch_strava_activities function to accept date parameters
fetch_strava_activities <- function(access_token, start_date = NULL, end_date = NULL) {
  url <- "https://www.strava.com/api/v3/athlete/activities"
  
  # Convert dates to Unix timestamps if provided
  query_params <- list(per_page = 200)
  if (!is.null(start_date)) {
    query_params$after <- as.numeric(as.POSIXct(start_date))
  }
  if (!is.null(end_date)) {
    query_params$before <- as.numeric(as.POSIXct(end_date)) + 86400  # Add one day to include end date
  }
  
  response <- GET(url, 
                 add_headers(Authorization = paste("Bearer", access_token)),
                 query = query_params)
  
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

# In the UI section, modify the sidebar to include the Search button
sidebar = sidebar(
  verbatimTextOutput("auth_status"),
  uiOutput("auth_link"),
  selectInput("activity_type", "Activity Type",
              choices = c("All", "Run", "Ride", "VirtualRide")),
  dateRangeInput("date_range", "Date Range",
                 start = Sys.Date() - 180,
                 end = Sys.Date()),
  actionButton("search", "Search Activities", class = "btn-primary")
),

# In the server function, modify the activity data fetching logic
server <- function(input, output, session) {
  # ... (previous reactive values remain the same)
  
  # New reactive value to store the search trigger
  search_trigger <- reactiveVal(0)
  
  # Observer for the search button
  observeEvent(input$search, {
    search_trigger(search_trigger() + 1)
  })
  
  # Modify the activity data fetching in the OAuth callback
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
        
        # Initial fetch of activities
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
  
  # New observer for the search button
  observeEvent(input$search, {
    req(access_token())
    
    tryCatch({
      auth_status("Fetching activities...")
      
      # Fetch activities with date range
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
  
  # Modify the activities reactive to remove date filtering
  activities <- reactive({
    req(activity_data())
    
    data <- activity_data()
    
    if (input$activity_type != "All") {
      data <- data %>% filter(type == input$activity_type)
    }
    
    data
  })
  
  # ... (rest of the server code remains the same)
}
