 
   # Set working directory
   setwd("C:/Users/bartw/Desktop/KritiR")
 
   # Load data
   rainfall_df <- read.csv("rainfall_data.csv", fileEncoding = "latin1")
   topography_df <- read.csv("topography.csv", fileEncoding = "latin1")
   
     # Clean elevation column (keep only numeric)
     topography_df$ELEVATION <- as.character(topography_df$ELEVATION)
     topography_df$ELEVATION <- gsub("[^0-9.]", "", topography_df$ELEVATION)
     topography_df$ELEVATION <- as.numeric(topography_df$ELEVATION)
     
       # Drop unwanted column if exists
       if ("Unnamed: 5" %in% colnames(topography_df)) {
             topography_df <- topography_df[ , !(names(topography_df) %in% "Unnamed: 5")]
         }
    
       # Merge rainfall and topography on STATE_NAME and DISTRICT
       merged_df <- merge(rainfall_df, topography_df, by = c("STATE_NAME", "DISTRICT"))
       
         library(geosphere)  # for distance calculations
      
         # Function to calculate slope between two points
         calculate_slope <- function(elev1, elev2, lat1, lon1, lat2, lon2) {
               coords1 <- c(lon1, lat1)
               coords2 <- c(lon2, lat2)
               dist <- distGeo(coords1, coords2) / 1000  # distance in km
               elev_diff <- abs(elev1 - elev2)
               slope <- ifelse(dist != 0, elev_diff / dist, 0)
               return(slope)
           }
         
           # Calculate slope for each district compared with next row (district)
           slopes <- c(
                 sapply(1:(nrow(merged_df)-1), function(i) {
                       calculate_slope(merged_df$ELEVATION[i], merged_df$ELEVATION[i+1],
                                        +                         merged_df$LATITUDE[i], merged_df$LONGITUDE[i],
                                        +                         merged_df$LATITUDE[i+1], merged_df$LONGITUDE[i+1])
                   }),
                 NA  # Last district no next neighbor
             )
           
             merged_df$Slope <- as.numeric(slopes)
             
               # Define risk based on slope with 3 categories: Low, Medium, High
               merged_df$Risk <- cut(merged_df$Slope,
                                                           breaks = c(-Inf, 3, 8, Inf),
                                                             labels = c("Low", "Medium", "High"),
                                                             right = FALSE)
               
                 # Color mapping function that handles NA values
                 risk_colors <- function(risk) {
                      if (is.na(risk)) {
                           return("gray")  # color for missing risk values
                         } else if (risk == "High") {
                               return("red")
                           } else if (risk == "Medium") {
                                 return("yellow")
                             } else {
                                   return("green")
                               }
                   }
                
                   # Apply color mapping
                   colors <- sapply(merged_df$Risk, risk_colors)
                   
                     library(leaflet)
                   
                     # Create leaflet map
                     leaflet(data = merged_df) %>%
                         addTiles() %>%
                         setView(lng = 78.9629, lat = 20.5937, zoom = 5) %>%  # Center on India
                         addCircleMarkers(
                               ~LONGITUDE, ~LATITUDE,
                               color = colors,
                               radius = 6,
                               popup = ~paste("District:", DISTRICT,
                                                                       "<br>State:", STATE_NAME,
                                                                       "<br>Slope:", round(Slope, 2),
                                                                       "<br>Risk Level:", Risk)
                           ) %>%
                         addLegend(
                               position = "bottomright",
                               colors = c("red", "yellow", "green", "gray"),
                               labels = c("High Risk (Red)", "Medium Risk (Yellow)", "Low Risk (Green)", "No Data (Gray)"),
                               title = "Landslide Risk Levels",
                               opacity = 1
                           )