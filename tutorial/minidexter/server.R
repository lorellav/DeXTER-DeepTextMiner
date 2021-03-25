
server <- function(input, output, session) {
  
  # place_names_file <- "www/place_names_old.csv"
  # place_names      <- readr::read_csv(place_names_file)
  place_names_file <- "www/place_names.csv"
  place_names      <- readr::read_delim(place_names_file, delim = ";")
  # 
  place_names    %<>% dplyr::select(-date) %>% dplyr::rename(title = Title)
  
  # Every place name was fed into google geo code. The file geo_data contains for every 'location' the current (english)
  # name, the type of location ('city', 'country' or 'administartive area') and the coordinates of its center {lat, lon}
  #
  # geo_data_file   <- "www/geo_data_original.csv"
  # geo_data        <- readr::read_csv(geo_data_file)
  geo_data_file   <- "www/geo_data.csv"
  geo_data        <- readr::read_delim(geo_data_file, delim = ";")
  geo_data$colloquial_area <- as.character(geo_data$colloquial_area)
  
  entity_file   <- "statistics/entities_sentiment.csv"
  entities        <- readr::read_delim(entity_file, delim = ";")
  entities$year <- year(entities$year)
  
  # entities %<>%
  #   group_by(entity, type) %>%
  #   filter(8 < sum(issue.freq))
  # 
  #define color palette for entity statistics plots
  s <-  unique(entities$type)
  colors.entities <- setNames(brewer.pal(length(s), "Dark2"), s)
  
  #read social network
  network.file   <- "sna/network"
  collab.network <- read_graph(network.file, format = "graphml")
  
  E(collab.network)$issue <- paste(E(collab.network)$title, E(collab.network)$issue, sep = " - ")
  

  #define shapes for network nodes
  shapes <- c("dot", "square", "triangle", "star")
  shapes <- shapes[1:length(unique(V(collab.network)$type))]
  names(shapes) <- unique(V(collab.network)$type)
  
  
  
  world1994        <- st_read(dsn= 'data/1994')
  world1920        <- st_read(dsn= 'data/1920')
  world1914        <- st_read(dsn= 'data/1914')
  world1880        <- st_read(dsn= 'data/1880')
  
  
  # currently the app uses the 1994 map from ?? archive as the base map
  #
  
  world         <- world1994
  st_crs(world) <- 4326
  world         %<>% rename(name_long = ABBREVNAME, iso_a2 = FIPS_CODE)
  
  # Add to each name occurrence the geo information (lat, lon)
  # The app treats the type of location differently, so we label these names with location type
  # country, region (administrative & colloquial area's)  and city names (loca. 
  # are supported
  #
  geo_data %<>%
    mutate(location_type = if_else(grepl("(?:^|\\+)locality(?:\\+|$)", location_type), "city", location_type)) %>%
    mutate(location_type = if_else(grepl("(?:^|\\+)country(?:\\+|$)", location_type), "country", location_type)) %>%
    mutate(location_type = if_else(grepl("(?:^|\\+)administrative_area_level_[12](?:\\+|$)", location_type),
                                   str_match(location_type, "(?:^|\\+)administrative_area_(level_[12])(?:\\+|$)")[,2], location_type)) %>%
    mutate(location_type = if_else(grepl("(?:^|\\+)colloquial_area(?:\\+|$)", location_type) &
                                     grepl("(?:^|\\+)political(?:\\+|$)", location_type), "colloquial", location_type)) %>%
    filter(location_type %in% c('country', 'city', 'level_1', 'level_2', 'level_3', 'level_4', 'colloquial'))
  
  # Use the google geocoding names of countries, cities (localities) and regions (administrative levels) to unify all
  # the different spellings of the same location
  #
  geo_data %<>% mutate(name_long = recode(location_type, country =  country,
                                          city =     locality,
                                          level_1 =  admin_1,
                                          level_2 =  admin_2,
                                          level_3 =  admin_2,
                                          level_4 =  admin_2,
                                          colloquial = colloquial_area)) %>%
    
    # No distinction in admin levels and colloquial area anymore. They are all regions
    #
    mutate(location_type = recode(location_type, country =    'country',
                                  city =       'city',
                                  level_1 =    'region',
                                  level_2 =    'region',
                                  level_3 =    'region',
                                  level_4 =    'region',
                                  colloquial = 'region'))
  
  
  # media_location_data <- geo_data %>%
  #   dplyr::inner_join(place_names, by = c("location" = "location")) %>%
  #   dplyr::filter(location_type == 'country' | location_type == 'city' | location_type == 'region') %>%
  #   dplyr::select(location, location_type, title, year, name_long, lat, lon, filename, freq)
  #  
  # 
  # Current world map does not have some small states
  # These nations will be discarded.
  #
  # media_location_data %<>% dplyr::filter(location_type %in% c('city', 'region') | 
  #                                  !location %in% c('vaticano', 'san marino', 'hongkong', 'monaco', 'malta', 'jersey'))
  # 
  
  # The center of the Philipines doesn't lie on it's territory but somewhere in the sea.
  # As a consequence the spatial joint with the world map wil not work for the Philippines.
  # We move the so called center of the Philippines to the center of its capital Manilla
  
  geo_data$lon[geo_data$country == "Philippines" & geo_data$location_type == "country"] <- 120.98422
  geo_data$lat[geo_data$country == "Philippines" & geo_data$location_type == "country"] <- 15.0
  
  # The titles of the media and the time periodes in which articles are published will be used 
  # as selection criteria. The input widgets in the app are dynamically created on the server side
  #
  np_titles <- unique(place_names$title)
  np_period <- c(min(place_names$year), max(place_names$year))
  
  np_issues <- unique(E(collab.network)$issue)
  np_entities <- unique(entities$id)
  
  # Some input values are not set during start up. To distuingish between values not known at start-up or other 
  # problems we need a logical which will be set to FALSE after initialisation stage.
  start_up  <- TRUE
  
  # Restoring from a bookmark doesn't go right automatically. 
  # Explicit restore of workflow is needed. Restore_map is a value to guide the process.
  #
  restore_map <- reactiveVal(value = "START")
  
  # The bounding box of the area that is visible on the map at start-up
  #
  lng1 <- -170.0
  lng2 <- +170.0
  lat1 <- -60.0
  lat2 <- +80.0
  # lng <-   12.56738
  # zoom <-  1
  
  output$selection_period <- renderUI({
    sliderInput(inputId = "years",
                label =    h4("Time Period"),
                min =      np_period[1],
                max =      np_period[2], 
                value =    np_period,
                ticks =    FALSE,
                sep =      "",
                step =     1,
                round =    TRUE)
  })
  
  output$selection_titles <- renderUI({
    checkboxGroupInput(inputId =  "checked_media",
                       label =     h4("Titles"), 
                       choices =   np_titles,
                       selected =  np_titles)
  })
  
  
  cn <- igraph::simplify(collab.network, remove.loops = TRUE, remove.multiple = FALSE)
  cn <- delete.vertices(simplify(cn), degree(cn)==0)
  current.node <- V(cn)$name[1]
  #default entity selected for the network visualization
  updateSelectizeInput(session, 'network_selected_node', 
                       choices = V(cn)$name, 
                       selected = current.node,
                       server = TRUE)
  
  
  
  # default title selected for the network visualization
  # updateSelectizeInput(session, "network_selected_title",
  #                      choices = np_titles,
  #                      selected = np_titles[1],
  #                      server = TRUE)
  
  current.issue <- unlist(unique(E(collab.network)$issue))
  # default title selected for the network visualization
  updateSelectizeInput(session, "network_selected_issue",
                       choices = np_issues,
                       selected = current.issue,
                       server = TRUE)
  
  
# 
#   observeEvent(select_data_network(), {
#     updateSelectizeInput(session, "network_selected_node", 
#                          choices = unique(V(select_data_network())$id), 
#                          selected = unique(V(select_data_network())$id)[1])
#   })
#   
#   #tab/network/network issue
#   observeEvent(select_data_network(), {
#     updateSelectizeInput(session, "network_selected_issue", 
#                       choices = unique(E(select_data_network())$issue), 
#                       selected = unique(E(select_data_network())$issue)[1])
#   })

  current.entity <- unlist(unique(entities$id))
  # default entity selected for the sentiment visualization
  updateSelectizeInput(session, "sentiment_selected_entity",
                       choices = np_entities,
                       selected = current.entity,
                       server = TRUE)
  
  
  select_data <- reactive({
    select_data       <- place_names
    if(!is.null(input$checked_media)) {
      checked_titles <- input$checked_media
    } else {
      if (start_up) {
        start_up <<- FALSE
        checked_titles <- np_titles 
      } else {
        checked_titles <- c("NO TITLES")
      }
    } 
    year_1         <- ifelse(is.null(input$years[1]), np_period[1], input$years[1])
    year_2         <- ifelse(is.null(input$years[2]), np_period[2], input$years[2])
    
    # User has changed his/her selection criteria. New (raw) data, please!
    #
    select_data %<>% filter(title %in% checked_titles)
    select_data %<>% filter(year >= year_1)
    select_data %<>% filter(year <= year_2)
    
    # Summarize data:
    # 1. number of different (front) pages in selection
    # 2. number of occurrences of place names in selection
    # Those numbers are needed when computing percentages
    #
    number_of_pages       <- as.integer(select_data %>% summarise(n_distinct(filename)))
    number_of_occurrences <- as.integer(select_data %>% summarise(sum(freq)))
    warning("TOTALS: ", number_of_pages, number_of_occurrences)
    
    # Joining selected data with geo data to get location type, current english spelling and coordinates (lon/lat)
    #
    select_data <- geo_data %>%
      dplyr::inner_join(select_data, by = c("location" = "location")) %>%
      dplyr::filter(location_type == 'country' | location_type == 'city' | location_type == 'region') %>%
      dplyr::select(location, location_type, title, year, name_long, lat, lon, filename, freq)
    
    # For each place name compute the number of (front) pages it is mentioned at least one time
    # and the total number of occurrences on all those pages
    # Also the number of distinct newspaper titles of all the front pages.
    #
    select_data %<>%
      dplyr::group_by(lon, lat, location_type, name_long) %>%
      dplyr::summarise(location = unique(location),
                       pages =  n_distinct(filename),
                       occurrences = sum(freq),
                       # begin_year = min(year),                  # not used anaymore
                       # end_year =   max(year),
                       n_titles =   n_distinct(title))
    
    # ents <- entities[grepl("GPE", entities$type), ] %>% 
    #   group_by(entity.lower) %>% 
    #   summarize(magnitude = round(mean(as.numeric(magnitude)), 1), score = round(mean(as.numeric(score)), 1))
    # 
    # select_data <- select_data %>% merge(ents, by.x = "location", by.y = "entity.lower", all.x = TRUE) 
    # 
    # normalized data (aka percentages)
    #
    select_data %<>% ungroup() %>% mutate(pages_perc =    100 * pages / number_of_pages,
                                          occurrences_perc = 100 * occurrences / number_of_occurrences,
                                          n_titles_perc =    100 * n_titles / length(checked_titles))
    
    # select_data$magnitude[is.na(select_data$magnitude)] <- 0.0 
    # select_data$score[is.na(select_data$score)] <- 0.0 
    # 
    return(select_data)
  })
  
  # Helper function make_bins_abs creates bins to select fill colors for ranges of values
  #
  make_bins <- function(max_value, normalized = 'no') {
    
    # some constants demanded by color palette (should be parametrized)
    #
    min_bins <- 3
    max_bins <- 9
    
    legend_breaks   <- c(0, 2, 5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000, 20000, 50000, Inf)
    legend_breaks_norm <- c(0, 2, 4, 8, 15, 30, 60, 100)
    
    if (normalized != 'no') legend_breaks <- legend_breaks_norm
    
    upper         <- findInterval(x = max_value, legend_breaks, left.open = TRUE) + 1
    lower         <- ifelse(upper-max_bins+1 < 2, 2, upper-max_bins+1)
    b             <- append(legend_breaks[1], legend_breaks[lower:upper])
    if (length(b) < (min_bins+1)) {b <- legend_breaks[1:4]}
    return(b)
  }
  
  
  # Plots the map
  output$data_map <- renderLeaflet({
    
    # Draw the (base) map of the world
    #
    
    # reactive value restore_map guides the proces of drawing the maps when
    # user has used a bookmark
    #
    if ( restore_map() == "BASE_MAP") {
      restore_map("COUNTRY_MAP")
    }
    
    # layer id's are nec
    #
    layerIds = sprintf("w%g", 1:nrow(world))
    
    leaflet(world)  %>%
      flyToBounds(lng1 = lng1, lat1 = lat1, lng2 = lng2, lat2 = lat2) %>%
      
      # We use panes for maintaining a front to back hierarchy: cities, regions, historical borders, countries and world 
      #
      # World is needed to draw all the countries which aren't mentioned in the corpus
      #
      addMapPane(name = 'world', zIndex = 409) %>%
      addMapPane(name = 'countries', zIndex = 410) %>%
      addMapPane(name = 'hist_countries', zIndex = 411) %>%
      addMapPane(name = 'regions', zIndex = 415) %>%
      addMapPane(name = 'cities', zIndex = 420) %>%
      addPolygons(
        fillColor = "#808080",   # grey value also used for indicating NA value
        fillOpacity = 1,
        layerId =    layerIds,
        color =     "white",
        weight =    1,
        options =   leafletOptions(pane = 'world')
      )
  })
  
  map_bounds <- reactive({
    if (is.null(input$data_map_bounds)) {
      o <- list(west = -170, east = 170, south = -80, north = 80)
    } else {
      o <- list(west =  as.numeric(input$data_map_bounds$west),
                east =  as.numeric(input$data_map_bounds$east),
                south = as.numeric(input$data_map_bounds$south),
                north = as.numeric(input$data_map_bounds$north))
    }
    o
  })
  
  map_data <- reactive({
    
    ll_data    <- select_data()          # select_data is reactive and signals changes in data selection criteria (period and titles)
    normalized <- input$normalized       # signals whether users want to see absolute or normalized values
    
    if(nrow(ll_data) == 0) {
      return(NULL)
    }
    
    # Check if user want to see cities and if so in which percentile range
    #
    if (!as.logical(input$show_cities)) {
      ll_data %<>% filter(location_type != 'city')
    } else {
      above_below_index  <- as.integer(input$perc_cities / 10) + 1
      city_values        <- ll_data %>% filter(location_type == 'city') %>% pull(pages)
      above_below_values <- quantile(city_values, probs = seq(0, 1, 0.1))[above_below_index]
      
      ll_data %<>% filter(location_type %in% c('country', 'region') | (pages >= above_below_values[1] & 
                                                                         pages <= above_below_values[2]))
    }
    
    # idem for regions
    #
    if (!as.logical(input$show_regions)) {
      ll_data %<>% filter(location_type != 'region')
    } else {
      above_below_index  <- as.integer(input$perc_regions / 10) + 1
      region_values      <- ll_data %>% filter(location_type == 'region') %>% pull(pages)
      above_below_values <- quantile(region_values, probs = seq(0, 1, 0.1))[above_below_index]
      
      ll_data %<>% filter(location_type %in% c('country', 'city') | (pages >= above_below_values[1] & 
                                                                       pages <= above_below_values[2]))
    }
    # check if user want to see normalized data
    #
    if (normalized != 'no') {
      ll_data %<>% mutate(map_value = pages_perc)
    } else {
      ll_data %<>% mutate(map_value = pages)
    }
    
    # ll_data %<>% mutate(map_value = score)
    # legend title
    #
    title <- switch(normalized,
                    no =  "Number of pages",
                    yes = "Percentage of pages")
    
    
    # Because we will use polygons for countries and circles for cities we apply color binning to show the value 
    # on the map. Continuous colors is more suitable for rasters.
    #
    bins    <- make_bins(max(ll_data$map_value, na.rm = TRUE), normalized = normalized)   # see make_bins()
    
    # bins <- seq(-1.0, 1.0, by = 0.25)
    
    legends <- sprintf("%g - %g", bins[1:(length(bins)-1)] + 1, bins[2:length(bins)])         # legend label
    if (normalized == "no") {
      legends <- sprintf("%g - %g", bins[1:(length(bins)-1)] + 1, bins[2:length(bins)])
      legends[length(legends)] <- sprintf("%g +", bins[length(bins)-1]+1)
    } else {
      legends <- sprintf("%g - %g", bins[1:(length(bins)-1)], bins[2:length(bins)])
    }
    
    # The value will be mapped to a (fill) color for the polygons or circles. The map_value 
    # must be mapped to the index of its corresponding bin.
    #
    ll_data %<>% mutate(fill_value = ifelse(is.na(findInterval(map_value, bins, left.open = TRUE)),
                                            length(bins),
                                            findInterval(map_value, bins, left.open = TRUE)))
    
    map_colors <- brewer.pal(n = length(bins)-1, name =  "YlOrRd")
    map_colors <- append(map_colors, "#808080")
    legends    <- append(legends, "NA*")
    
    
    # recalculate the hove-over labels
    #
    switch(normalized, 
           yes      = {labels <-  sprintf("<strong>%s</strong><br/>%4.2f%% of pages<br/>%4.2f%% of occurrences</sup>",
                                          ll_data$name_long,
                                          ll_data$pages_perc,
                                          ll_data$occurrences_perc) %>% lapply(htmltools::HTML)},
           category = {labels <-  sprintf("<strong>%s</strong><br/>%4.2f%% of pages</sup>",
                                          ll_data$name_long,
                                          ll_data$pages_perc_category) %>% lapply(htmltools::HTML)},
           no =       {labels <-  sprintf("<strong>%s</strong><br/>%g pages<br/>%g occurrences<br/>%g titles</sup>",  # period deleted <br/>from %g till %g
                                          ll_data$name_long,
                                          ll_data$map_value,
                                          ll_data$occurrences,
                                          ll_data$n_titles            # period deleted ,ll_data$begin_year, ll_data$end_year
           ) %>% lapply(htmltools::HTML)})
    
    ll_data$labels <- labels
    
    r <- list(data =       ll_data,
              normalized = normalized,
              bins =       bins, 
              labels =     labels, title = title, legends = legends, 
              map_colors = map_colors)
  })
  
  country_map_data <- reactive({
    # observe changes in country map data and adjust the map accordingly 
    # Zooming and/or scrolling aren't considered as changes in the map data
    #
    country_map <- map_data()
    if (is.null(country_map)) {
      return(NULL)
    }
    
    country_map$data %<>% filter(location_type == 'country')
    if (nrow(country_map$data) == 0) {
      return(NULL)
    }
    
    # 
    # #Locations and its attributes are linked to geometries (MULTIPOLYGONS) describing the 
    # # the territory of a country on the worldmap.
    # # First transform (lon,lat) to POINT geometries and then do a spatial join with the territories (MULTI Polygons)
    # #
    # 
    # 
    country_map$data %<>% st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
    # 
    country_map$data <- st_join(world[c("iso_a2")],
                                country_map$data[c("pages", "name_long", "n_titles",
                                                   # "begin_year", "end_year",
                                                   "pages_perc", "n_titles_perc", "fill_value",
                                                   "map_value", "labels")],
                                left = FALSE)
    return(country_map)
  })
  
  
  observe({
    new_map <- country_map_data()
    if (restore_map() == "COUNTRY_MAP") {
      restore_map("HIST_MAP")
    }
    
    if (is.null(new_map)) {
      leafletProxy("data_map")  %>%
        clearGroup(group = 'countries') %>%
        removeControl(layerId = 'legend1')
    } else {
      leafletProxy("data_map", data = new_map$data)  %>%
        clearGroup(group = 'countries') %>%
        addPolygons(fillColor =        ~new_map$map_colors[fill_value],
                    #layerId =           polyIds,
                    weight =            1,
                    opacity =           1,
                    color =            "white",
                    fillOpacity =       1,
                    group =            'countries',
                    options =           leafletOptions(pane = 'countries'),
                    highlightOptions  = highlightOptions(weight =       1,
                                                         color =       'black',
                                                         fillOpacity =  0.7,
                                                         bringToFront = TRUE),
                    label =            ~labels,
                    labelOptions =      labelOptions(style =      list("font-weight" = "normal"),
                                                     textsize =  "10px",
                                                     direction = "auto")) %>%
        addLegend(
          colors =    new_map$map_colors,
          layerId =  "legend1",
          group =     'countries',
          values =   ~fill_value,
          labels =    new_map$legends,
          opacity =   0.7,
          title =     new_map$title,
          position = "bottomleft")}
  })
  
  observe({
    #
    # observe if the user wants to see the cities on the map or not
    #
    if (is.null(input$show_cities) || isFALSE(input$show_cities)) {
      leafletProxy("data_map") %>%
        hideGroup(group = 'cities')
    } else {
      leafletProxy("data_map") %>%
        showGroup(group = 'cities')
    }
  })
  
  observe({
    #
    # observe if the user wants to see the regions on the map or not
    #
    if (is.null(input$show_regions) || isFALSE(input$show_regions)) {
      leafletProxy("data_map") %>%
        hideGroup(group = 'regions')
    } else {
      leafletProxy("data_map") %>%
        showGroup(group = 'regions')
    }
  })
  
  #Updates historical map
  observe({
    if (restore_map() == "HIST_MAP") {
      restore_map("REGION_MAP")
    }
    if (input$hist_map == 'none') {
      leafletProxy("data_map") %>% clearGroup(group = "hist_countries")
    } else {
      hist_map <- eval(parse(text = input$hist_map))
      leafletProxy("data_map", data = hist_map)  %>%
        clearGroup(group = 'hist_countries') %>%
        addPolygons(fillColor =        "white",
                    weight =            2,
                    opacity =           0.5,
                    color =            "black",
                    dashArray =        c(3,6),
                    fillOpacity =       0,
                    group =            'hist_countries',
                    options =           leafletOptions(pane = 'hist_countries'),
                    label =            ~ABBREVNAME)}
  })
  
  
  
  
  location_map_data <- reactive({
    # Observe if map data for cities has changed and adjust cities on map accordingly.
    # 
    new_map <- map_data()
    if (is.null(new_map)) {
      return(NULL)
    }
    new_map$data %<>% filter(location_type %in% c('city', 'region'))
    if (nrow(new_map$data) == 0) {
      return(NULL)
    }
    # 
    # Create sf table (geom = POINT) for ggplot and
    # add XandY coordinates for leaflet
    #
    new_map$data %<>% st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")
    new_map$data %<>% cbind(st_coordinates(new_map$data))
    return(new_map)
  })
  
  #Shows regions on the map
  observe({
    new_map        <- location_map_data()
    
    if(restore_map() == "REGION_MAP") {
      restore_map("CITY_MAP")
    }
    
    if (is.null(new_map)) {
      leafletProxy("data_map")  %>%
        clearGroup(group = 'regions')
    } else {
      new_map$data %<>% as_tibble() %>% filter(location_type == 'region')
      if(nrow(new_map$data) == 0) {
        leafletProxy("data_map", data = new_map$data)  %>%
          clearGroup(group = 'regions')
      } else {
        leafletProxy("data_map", data = new_map$data) %>%
          clearGroup(group = 'regions') %>%
          addRectangles(lng1 =    ~(X - fill_value/10),
                        lat1 =    ~(Y - fill_value/15),
                        lng2 =    ~(X + fill_value/10),
                        lat2 =    ~(Y + fill_value/15),
                        #radius = ~5000*fill_value,  # keeps cities with few citations visible
                        color =  '#808080',
                        fillColor =  ~new_map$map_colors[fill_value],
                        fillOpacity = 1,
                        weight =  2,
                        opacity = 1,
                        group =  'regions',                           # group value will be used for clearing and hiding
                        options = leafletOptions(pane = 'regions'),  # panes are used to keep cities on top of countries
                        label =   ~labels,
                        highlightOptions  = highlightOptions(weight =       1,
                                                             color =       'black',
                                                             fillOpacity =  0.8,
                                                             bringToFront = TRUE))
      }
    }
  })
  
  #shows cities on the map
  observe({
    new_map        <- location_map_data()
    #new_map$data %<>% as_tibble()
    
    if(restore_map() == "CITY_MAP") {
      restore_map("RESTORE_END")
    }
    
    if (is.null(new_map)) {
      leafletProxy("data_map")  %>%
        clearGroup(group = 'cities')
    } else {
      new_map$data %<>% as_tibble() %>% filter(location_type == 'city')
      if(nrow(new_map$data) == 0) {
        leafletProxy("data_map", data = new_map$data)  %>%
          clearGroup(group = 'cities')
      } else {
        leafletProxy("data_map", data = new_map$data) %>%
          clearGroup(group = 'cities') %>%
          addCircles(lng =    ~X,
                     lat =    ~Y,
                     radius = ~5000*fill_value,  # keeps cities with few citations visible
                     color =  '#808080',
                     fillColor =  ~new_map$map_colors[fill_value],
                     fillOpacity = 1,
                     weight =  2,
                     opacity = 1,
                     group =  'cities',                           # group value will be used for clearing and hiding
                     options = leafletOptions(pane = 'cities'),  # panes are used to keep cities on top of countries
                     label =   ~labels,
                     highlightOptions  = highlightOptions(weight =       1,
                                                          color =       'black',
                                                          fillOpacity =  0.8,
                                                          bringToFront = TRUE))
      }
    }
  })
  
  #Apparently, this function is 
  # city_map_data <- reactive({
  #   # Observe if map data for cities has changed and adjust cities on map accordingly.
  #   # 
  #   new_map <- map_data()
  #   if (is.null(new_map)) {
  #     return(NULL)
  #   }
  #   new_map$data %<>% filter(location_type == 'city')
  #   if (nrow(new_map$data) == 0) {
  #     return(NULL)
  #   }
  #   
  #   # 
  #   # Create sf table (geom = POINT) for ggplot and
  #   # add XandY coordinates for leaflet
  #   #
  #   new_map$data %<>% st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant")    # sf table
  #   new_map$data %<>% cbind(st_coordinates(new_map$data))                                 # adds X and Y (lon,lat) columns
  #   return(new_map)
  # })
  
  
  select_data_entities <- reactive({
    
    if(!is.null(input$checked_media)) {
      checked_titles <- input$checked_media
    } else {
      if (start_up) {
        start_up <<- FALSE
        checked_titles <- np_titles 
      } else {
        checked_titles <- c("NO TITLES")
      }
    } 
    
    year_1         <- ifelse(is.null(input$years[1]), np_period[1], input$years[1])
    year_2         <- ifelse(is.null(input$years[2]), np_period[2], input$years[2])
    
    # User has changed his/her selection criteria. New (raw) data, please!
    #
    entities %<>% filter(title %in% checked_titles)
    entities %<>% filter(year >= year_1)
    entities %<>% filter(year <= year_2)
    
    # entities %<>% filter(type %in% input$entity_type)
    
    return(entities)
  })
  
  select_data_entities_statistics <- reactive({
    
    entities <- select_data_entities()
    
    entities %<>% filter(type %in% input$entity_type)
    
    return(entities)
  })
  
  
  select_data_network <- reactive({
    
    if(!is.null(input$checked_media)) {
      checked_titles <- input$checked_media
    } else {
      if (start_up) {
        start_up <<- FALSE
        checked_titles <- np_titles 
      } else {
        checked_titles <- c("NO TITLES")
      }
    } 
    
    year_1         <- ifelse(is.null(input$years[1]), np_period[1], input$years[1])
    year_2         <- ifelse(is.null(input$years[2]), np_period[2], input$years[2])
    
    
    edg <- E(collab.network)[title %in% checked_titles]
    
    collab.network <- subgraph.edges(collab.network, edg, delete.vertices = TRUE)
    

    edg <- E(collab.network)[as.Date(ISOdate(year_1, 1, 1)) <= E(collab.network)$year &
                               E(collab.network)$year <= as.Date(ISOdate(year_2, 12, 31))
                             ]
    collab.network <- subgraph.edges(collab.network, edg, delete.vertices = TRUE)
    

   
    return(collab.network)
  })
  
  
  
  select_data_network_all <- reactive({
    
    #tab/network/network issue
    
    collab.network <- select_data_network()
    
    eg <- simplify(collab.network)

    return(eg)
  })
  

  observeEvent(select_data_network(), {
    network <- select_data_network()
    
    cn <- igraph::simplify(collab.network, remove.loops = TRUE, remove.multiple = FALSE)
    cn <- delete.vertices(cn, degree(cn)==0)
    
    updateSelectizeInput(session, "network_selected_node",
                         choices = unique(V(cn)$name),
                         selected = ifelse(current.node %in% unique(V(cn)$name), 
                                           current.node, 
                                           unique(V(cn)$name)[1])
    )
  })
  # first.node <- V(collab.network)[id == "adriatico - LOC"]
  select_data_network_ego <- reactive({
    
    collab.network <- select_data_network()
    
    first.node <- V(collab.network)[name == input$network_selected_node]
    
    current.node <<- input$network_selected_node
    
    if(input$network_ego_check){
      collab.network <- subgraph.edges(collab.network, incident(collab.network, first.node))
    }else{
      neighs <- unique(neighbors(collab.network, first.node))
      collab.network <- subgraph(collab.network, neighs)
    }
    
    collab.network <- igraph::simplify(collab.network, remove.loops = TRUE, remove.multiple = FALSE)
    # collab.network <- delete.vertices(collab.network, degree(collab.network)==0)

    
    network.df <- igraph::as_data_frame(collab.network, what = "both")
    
    gdf <- network.df$edges
    
    gdf <- gdf %>%
      filter(from != to)
    
    gdf <- gdf %>%
      group_by(from, to) %>%
      summarize(title = paste(unique(title), collapse = ", "), issue = paste(unique(issue), collapse = ", "),
                #year = min(year), 
                sentence.freq = sum(sentence.freq), 
                magnitude = round(mean(as.numeric(magnitude)), 2), score = round(mean(as.numeric(score)), 2)
      )  
    
    vdf <- data.frame(name = V(collab.network)$name,
                      id = V(collab.network)$id,
                      entity = V(collab.network)$entity,
                      type = V(collab.network)$type
                      # score = V(collab.network)$score,
                      # magnitude = V(collab.network)$magnitude,
                      # unique.sentence = V(collab.network)$unique.sentence
    )
    

    
    
    eg <- igraph::graph_from_data_frame(gdf, directed = FALSE, vertices = vdf)
    
    
    
    network.df <- igraph::as_data_frame(eg, what = "both")
    
    vdf <- network.df$vertices
    gdf <- network.df$edges
  

    n.gdf <- rbind.data.frame(gdf, gdf %>% rename(from = to, to = from))
    
    vdf <- merge(vdf, n.gdf, by.x = "id", by.y = "from", all.y = FALSE) %>%
      group_by(id) %>%
      # filter(id != to) %>%
      summarize(name = unique(name), type = unique(type),
                score = round(mean(as.numeric(score)), 2), magnitude = round(mean(as.numeric(magnitude)), 2),
                sentence.freq = sum(sentence.freq)
      )
    
    eg <- igraph::graph_from_data_frame(gdf, directed = FALSE, vertices = vdf)
    
    updateSliderInput(session, "link_width", 
                      min = min(E(eg)$sentence.freq), max = max(E(eg)$sentence.freq),
                      # value = c(min(E(eg)$sentence.freq), max(E(eg)$sentence.freq)),
                      step = 1)

    edg <- E(eg)[input$link_width[1] <= sentence.freq & sentence.freq <= input$link_width[2]]
    eg <- subgraph.edges(eg, edg, delete.vertices = TRUE)
    
    updateSliderInput(session, "link_sentiment", 
                      min = min(E(eg)$score), max = max(E(eg)$score),
                      # value = c(min(E(eg)$score), max(E(eg)$score)),
                      step = 0.1)
    
    edg <- E(eg)[input$link_sentiment[1] <= score & score <= input$link_sentiment[2]]
    eg <- subgraph.edges(eg, edg, delete.vertices = TRUE)
  
    
    return(eg)
  })
  
  
  observeEvent(select_data_network(), {
    network <- select_data_network()
    
    issues <- unlist(intersect(current.issue, unique(E(network)$issue)))
    
    if(0 < length(issues)){
      current.issue <<- issues
    }else{
      current.issue <<- unique(E(network)$issue)[1]
    }
    updateSelectizeInput(session, "network_selected_issue",
                         choices = unique(E(network)$issue),
                         selected = current.issue,
                         )
  })
  
  select_data_network_issues <- reactive({
    
    #tab/network/network issue
    
    collab.network <- select_data_network()
    
    edg <- E(collab.network)[issue %in% input$network_selected_issue]
    
    current.issue <<- unlist(input$network_selected_issue)
    
    collab.network <- subgraph.edges(collab.network, edg, delete.vertices = TRUE)
    
    
    network.df <- igraph::as_data_frame(collab.network, what = "both")
    
    gdf <- network.df$edges
    
    gdf <- gdf %>%
      filter(from != to)
    
    gdf <- gdf %>%
      group_by(from, to) %>%
      summarize(title = paste(unique(title), collapse = ", "), issue = paste(unique(issue), collapse = ", "),
                #year = min(year), 
                sentence.freq = sum(sentence.freq), 
                magnitude = round(mean(as.numeric(magnitude)), 2), score = round(mean(as.numeric(score)), 2)
      )  
    
    vdf <- data.frame(name = V(collab.network)$name,
                      id = V(collab.network)$id,
                      entity = V(collab.network)$entity,
                      type = V(collab.network)$type
                      # score = V(collab.network)$score,
                      # magnitude = V(collab.network)$magnitude,
                      # unique.sentence = V(collab.network)$unique.sentence
    )
    
    
    
    
    eg <- igraph::graph_from_data_frame(gdf, directed = FALSE, vertices = vdf)
    
    network.df <- igraph::as_data_frame(eg, what = "both")
    
    vdf <- network.df$vertices
    gdf <- network.df$edges
    
    
    n.gdf <- rbind.data.frame(gdf, gdf %>% rename(from = to, to = from))
    
    vdf <- merge(vdf, n.gdf, by.x = "id", by.y = "from", all.y = FALSE) %>%
      group_by(id) %>%
      # filter(id != to) %>%
      summarize(name = unique(name), type = unique(type),
                score = round(mean(as.numeric(score)), 2), magnitude = round(mean(as.numeric(magnitude)), 2),
                sentence.freq = sum(sentence.freq)
      )
    
    eg <- igraph::graph_from_data_frame(gdf, directed = FALSE, vertices = vdf)
    
    updateSliderInput(session, "link_width", 
                      min = min(E(eg)$sentence.freq), max = max(E(eg)$sentence.freq),
                      # value = c(min(E(eg)$sentence.freq), max(E(eg)$sentence.freq)),
                      step = 1)
    
    edg <- E(eg)[input$link_width[1] <= sentence.freq & sentence.freq <= input$link_width[2]]
    eg <- subgraph.edges(eg, edg, delete.vertices = TRUE)
    
    updateSliderInput(session, "link_sentiment", 
                      min = min(E(eg)$score), max = max(E(eg)$score),
                      # value = c(min(E(eg)$score), max(E(eg)$score)),
                      step = 0.1)
    
    edg <- E(eg)[input$link_sentiment[1] <= score & score <= input$link_sentiment[2]]
    eg <- subgraph.edges(eg, edg, delete.vertices = TRUE)
    
    
    return(eg)
  })

  
  
  select_data_entities_sentiment <- reactive({
    
    entities <- select_data_entities()
    
    entities %<>% filter(type %in% input$sentiment_entity_type)
    
    return(entities)
  })
  
  
  observeEvent(select_data_entities_sentiment(), {
    entities <- select_data_entities_sentiment()

    ents <- unlist(intersect(current.entity, unique(entities$id)))

    if(0 < length(ents)){
      current.entity <<- ents
    }else{
      current.entity <<- unique(entities$id)[1]
    }
    updateSelectizeInput(session, "sentiment_selected_entity",
                         choices = unique(entities$id),
                         selected = current.entity,
    )
  })

  select_data_entities_sentiment_entityovertime <- reactive({

    entities <- select_data_entities_sentiment()

    entities %<>% filter(id %in% input$sentiment_selected_entity)
    
    current.entity <<- unlist(input$sentiment_selected_entity)

    return(entities)
  })
  

  plotstatisticsplot <- function(save.into.file){
    entities <- select_data_entities_statistics()
    
    if(input$normalized_entities == "yes"){
      entities <- entities %>%
        group_by(title) %>%
        mutate(issue.freq = issue.freq/sum(issue.freq))
    }
    
    statistics <- entities %>%
      group_by(title, type) %>%
      summarise(freq = sum(issue.freq))
    
    
    
    cols <- colors.entities[unique(statistics$type)]
    
    title.size <- ifelse(save.into.file, 8, 18)
    
    plot <- statistics %>%
      ggplot(aes(fill=type, y = freq, x = type)) +
      geom_bar(position="dodge", stat="identity") +
      geom_hline(yintercept=0) +
      facet_wrap(~title) +
      #geom_text(aes(label = paste0(round(ab*100, 0),"%")), position = position_stack(vjust = 0.5), size = 5) +
      xlab("Entities") +   ylab("Percentage") +
      # coord_flip() +
      # facet_grid(. ~ title, scale = "free") +
      scale_fill_manual(values = cols) +
      scale_y_continuous(labels = scales::comma) +
      theme_classic() +
      theme(
        plot.title = element_text(size=18, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold", hjust = 0.5, angle = 90),
        axis.text.x = element_blank(), #element_text(size=rel(1.5), angle=0),
        axis.text.y = element_text(size=rel(1.5), angle=0),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.text = element_text(size=rel(1.0), angle=0),
        strip.text = element_text(size = title.size)
      ) 
    plot
    
  }
  output$statisticsplot <- renderPlot({
    
      plotstatisticsplot(save.into.file = FALSE)
    })
  
  if(FALSE){
  output$network_all <- renderVisNetwork({
    
    eg <- select_data_network_ego()
    
    data <- toVisNetworkData(eg)
    
    v.nodes <- data$nodes
    v.links <- data$edges
    

    
    v.nodes$shape  <- shapes[V(eg)$type]
    v.nodes$shadow <- TRUE # Nodes will drop shadow
    v.nodes$label  <- v.nodes$entity # Node label
    v.nodes$size   <- 10.0#100*vis.nodes$sentiment.score # Node size
    v.nodes$borderWidth <- 2 # Node border width
    
    v.links$type <- rep("sentence", nrow(v.links))

    v.nodes$color.border <- "black"
    v.nodes$color.highlight.background <- "orange"
    v.nodes$color.highlight.border <- "darkred"
    v.nodes$font.color <- "black"
    
    

    output$n_nodes_all <- renderText({ 
      paste("The network currently has", vcount(eg), "nodes and", ecount(eg), "edges.")
    })
    
    
    lnodes <- data.frame(shape = shapes, label = names(shapes),
                         font.color = "black", color = list(background = "white", border = "gray"))
    
    
    visNetwork(v.nodes, v.links, background = "white") %>% 
      visIgraphLayout(layout = "layout_with_fr") %>%
      # visNodes(color= list()) %>%
      visEdges(arrows = NULL, shadow = TRUE) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T)) %>%
      visLegend(addNodes = lnodes, useGroups = FALSE)
    
  })
  }
  
  plotnetwork_ego <- function(){
    eg <- select_data_network_ego()
    
    data <- toVisNetworkData(eg)
    
    v.nodes <- data$nodes
    v.links <- data$edges
    
    cols <- colors.entities[unique(V(eg)$type)]
    
    cols <- c("blue", "white", "red")
    names(cols) <- c("negative", "neutral", "positive")
    f <- colorRamp(cols)
    
    
    v.nodes$shape  <- shapes[V(eg)$type]
    v.nodes$shadow <- TRUE # Nodes will drop shadow
    v.nodes$title  <- sapply(V(eg)$sentence.freq, function(v){paste(v)})#v.nodes$titles # Text on click
    v.nodes$label  <- v.nodes$entity # Node label
    v.nodes$size   <- 10.0#100*vis.nodes$sentiment.score # Node size
    v.nodes$borderWidth <- 2 # Node border width
    
    v.links$value <- rescale(v.links$sentence.freq, to = c(0.0, 1.0))#rep(20, nrow(v.links))
    # v.links$title  <- sapply(E(eg)$sentence.freq, function(v){paste(v)})#v.nodes$titles # Text on click
    v.links$title  <- paste(E(eg)$title, E(eg)$sentence.freq, sep = " - ")
    
    v.nodes$color.background <- (colors <- rgb(f((v.nodes$score + 1)/2)/255))
    v.nodes$color.border <- "black"
    v.nodes$color.highlight.background <- "orange"
    v.nodes$color.highlight.border <- "darkred"
    v.nodes$font.color <- "black"
    
    
    
    v.links$color <- (colors <- rgb(f((v.links$score + 1)/2)/255))
    
    output$n_nodes_entities <- renderText({ 
      paste("The network currently has", vcount(eg), "nodes and", ecount(eg), "edges.")
    })
    
    
    lnodes <- data.frame(shape = shapes, label = names(shapes),
                         font.color = "black", color = list(background = "white", border = "gray"))
    
    ledges <- data.frame(color = cols, label = names(cols))
    
    
    visNetwork(v.nodes, v.links, background = "white") %>% 
      visIgraphLayout(layout = "layout_with_fr") %>%
      visEdges(arrows = NULL, shadow = TRUE) %>%
      visNodes(color= list(hover = 'orange')) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T)) %>%
      visLegend(addNodes = lnodes, addEdges = ledges, useGroups = FALSE)
    
    
  }
  output$network_ego <- renderVisNetwork({

   plotnetwork_ego()
    
  })

  plotnetwork_issue <- function(){
    eg <- select_data_network_issues()
    
    data <- toVisNetworkData(eg)
    
    v.nodes <- data$nodes
    v.links <- data$edges
    
    cols <- colors.entities[unique(V(eg)$type)]
    
    cols <- c("blue", "white", "red")
    names(cols) <- c("negative", "neutral", "positive")
    f <- colorRamp(cols)
    
    
    v.nodes$shape  <- shapes[V(eg)$type]
    v.nodes$shadow <- TRUE # Nodes will drop shadow
    v.nodes$title  <- sapply(V(eg)$sentence.freq, function(v){paste(v)})#v.nodes$titles # Text on click
    v.nodes$label  <- v.nodes$entity # Node label
    v.nodes$size   <- 10.0#100*vis.nodes$sentiment.score # Node size
    v.nodes$borderWidth <- 2 # Node border width
    v.links$type <- rep("sentence", nrow(v.links))
    #v.links$weight <- 10*v.links$issue.freq#rep(20, nrow(v.links))
    
    v.links$value <- rescale(v.links$sentence.freq, to = c(0.0, 1.0))#rep(20, nrow(v.links))
    # v.links$title  <- sapply(E(eg)$sentence.freq, function(v){paste(v)})#v.nodes$titles # Text on click
    v.links$title  <- paste(E(eg)$title, E(eg)$sentence.freq, sep = " - ")
    
    
    
    v.nodes$color.background <- (colors <- rgb(f((v.nodes$score + 1)/2)/255))
    v.nodes$color.border <- "black"
    v.nodes$color.highlight.background <- "orange"
    v.nodes$color.highlight.border <- "darkred"
    v.nodes$font.color <- "black"
    
    # #legend attribite
    # v.nodes$group <- v.nodes$shape
    # 
    
    v.links$color <- (colors <- rgb(f((v.links$score + 1)/2)/255))
    
    output$n_nodes_issues <- renderText({ 
      paste("The network currently has", vcount(eg), "nodes and", ecount(eg), "edges.")
    })
    
    lnodes <- data.frame(shape = shapes, label = names(shapes),
                         font.color = "black", color = list(background = "white", border = "gray"))
    
    ledges <- data.frame(color = cols, label = names(cols))# relationship's names
    
    
    visNetwork(v.nodes, v.links, background = "white") %>% 
      visIgraphLayout(layout = "layout_with_fr", smooth = TRUE) %>%
      visEdges(arrows = NULL, shadow = TRUE) %>%
      visNodes(color= list(hover = 'orange')) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T)) %>%
      visLegend(addNodes = lnodes, addEdges = ledges, useGroups = FALSE)
    
  }
  output$network_issues <- renderVisNetwork({
    
   plotnetwork_issue()
    
  })
  
  plotsentiment <- function(save.into.file){
    
    
    entities <- select_data_entities_sentiment()
    
    entities$score <- as.numeric(entities$score)
    entities$magnitude <- as.numeric(entities$magnitude)
    
    entities$sentiment.type <- sapply(entities$score, function(score){
      if(score < 0){
        return("negative")
      }else{
        return("positive")
      }
    })
    
    statistics <- entities %>%
      group_by(title, sentiment.type, year) %>%
      summarise(avg.score = mean(score), avg.magnitude = mean(magnitude))
    
    
    colfunc <- colorRampPalette(c("blue", "red"))
    cols <- colfunc(length(unique(statistics$sentiment.type)))
    
    statistics$avg.magnitude <- statistics$avg.magnitude * sign(statistics$avg.score)
    
    title.size <- ifelse(save.into.file, 6, 18)
    legend.size <- ifelse(save.into.file, 0.7, 1.3)
    
    plot <- statistics %>%
      ggplot(aes(fill=sentiment.type, y = avg.score, x = year)) +
      geom_density(position="dodge", stat="identity", alpha = 0.5, colour = "gray") +
      # geom_density(aes(y = avg.magnitude), position="dodge", stat="identity", alpha = 0.5, color = "gray") +
      geom_hline(yintercept=0) +
      facet_wrap(~title) +
      #geom_text(aes(label = paste0(round(ab*100, 0),"%")), position = position_stack(vjust = 0.5), size = 5) +
      xlab("Year") +   ylab("Sentiment score") +
      # coord_flip() +
      # facet_grid(. ~ title, scale = "free") +
      scale_fill_manual(values = cols) +
      scale_y_continuous(labels = scales::comma) +
      theme_classic() +
      theme(
        plot.title = element_text(size=18, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold", hjust = 0.5, angle = 90),
        axis.text.x = element_blank(), #element_text(size=rel(1.5), angle=0),
        axis.text.y = element_text(size=rel(1.5), angle=0),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size=rel(1.3), angle=0),
        legend.text = element_text(size=rel(1.3), angle=0),
        strip.text = element_text(size = title.size)
      ) + 
      guides(fill=guide_legend(title="Sentiment score"))
    
    plot
    
  }
  output$sentimentplot <- renderPlot({
  
    plotsentiment(save.into.file = FALSE)
    
  })
  
  plotsentimenttopentities <- function(save.into.file){
    
    entities <- select_data_entities_sentiment()
  
    entities$score <- as.numeric(entities$score)
    entities$magnitude <- as.numeric(entities$magnitude)
    
    statistics <- entities %>%
      group_by(id, title) %>%
      summarise(avg.score = mean(score), avg.magnitude = mean(magnitude)) %>%
      arrange(desc(avg.score))
    
    statistics$sentiment.type <- sapply(statistics$avg.score, function(score){
      if(score < -0.2){
        return("negative")
      }else if(0.2 < score){
        return("positive")
      }else{
        return("neutral")
      }
    })
    
    
    colfunc <- colorRampPalette(c("blue", "white", "red"))
    cols <- colfunc(length(unique(statistics$sentiment.type)))
    names(cols) <- sort(unique(statistics$sentiment.type))
    
    statistics.title <- statistics %>% 
      group_by(title) %>%
      group_split()
    
    title.size <- ifelse(save.into.file, 8, 18)
    
    top.n <- input$sentimentnumofentities
    plots <- NULL
    # titles <- unique(entities$title)
    plot.legend <- NULL
    for(d in 1:length(statistics.title)){
      
      df.p <- statistics.title[[d]] %>%
        arrange(desc(avg.score))
      
      df.p <- df.p[1:top.n, ]
      
      df.n <- statistics.title[[d]] %>%
        arrange(avg.score)
      
      df.n <- df.n[1:top.n, ]
      
      df.neut <- statistics.title[[d]] %>%
        mutate(score.neut = 1 - abs(avg.score)) %>%
        arrange(desc(score.neut))
      
      df.neut <- df.neut[1:top.n, ]
      df.neut <- df.neut %>% select(-score.neut)
      
      df <- rbind.data.frame(df.p, df.n, df.neut) %>%
        # mutate(id.sent = paste(id, sentiment.type, sep = " ")) %>%
        arrange(desc(avg.score))
      
      title <- unique(df$title)
      
      
      p <- df %>%
        mutate(id=factor(id, levels=unique(id))) %>%
        ggplot(aes(fill = sentiment.type, y = avg.score, x = id)) +
        geom_col(position="dodge", stat="identity", show.legend = TRUE, alpha = 0.5, colour = "gray") +
        # geom_density(aes(y = avg.magnitude), position="dodge", stat="identity", alpha = 0.7, color = "gray") +
        geom_hline(yintercept=0) +
        # facet_wrap(~sentiment.type) +
        #geom_text(aes(label = paste0(round(ab*100, 0),"%")), position = position_stack(vjust = 0.5), size = 5) +
        xlab("Entities") + ylab("Sentiment score") + ggtitle(title) + 
        ylim(c(0, 1)) +
        # coord_flip() +
        # facet_grid(. ~ sentiment.type, scale = "free") +
        scale_fill_manual(values = cols) +
        scale_y_continuous(labels = scales::comma) +
        theme_classic() + #   theme_dark() + #
        coord_flip() +
        theme(
          plot.title = element_text(size=18, hjust = 0.5),
          axis.title.x = element_text(size=12, face="bold"),
          axis.title.y = element_text(size=12, face="bold", hjust = 0.5, angle = 90),
          axis.text.x = element_blank(), #element_text(size=rel(1.5), angle=0),
          axis.text.y = element_text(size=rel(1.5), angle=0),
          axis.ticks.x = element_blank(),
          legend.title = element_text(size=rel(1.3), angle=0),
          legend.text = element_text(size=rel(1.3), angle=0),
          strip.text = element_text(size = title.size)
        ) + 
        guides(fill=guide_legend(title="Entities"))
      
      if(3 <= length(unique(statistics.title[[d]]$sentiment.type))){
        plot.legend <- p
      }
        
      plots[[length(plots) + 1]] <- p
      
      
    }
    
    legend <-  gtable_filter(ggplot_gtable(ggplot_build(plot.legend)), "guide-box")
    
    plots <- lapply(plots, function(p){p + theme(legend.position="none", strip.text = element_text(size = title.size))})
    
    plots[[length(plots) + 1]] <- legend
    # n.row <- floor(sqrt(length(statistics.title)))
    # n.col <- floor(sqrt(length(statistics.title)))
    grid.arrange(grobs = plots, legend)#, nrow = n.row, ncol = n.col)
    
    # patchwork(sum(unlist(plots)) & theme(legend.position = "bottom"))
  }
  output$sentimentplottoptities <- renderPlot({
    
   plotsentimenttopentities(save.into.file = FALSE)
    
  })
  
  plotsentimentovertime <- function(save.into.file){
    
    entities <- select_data_entities_sentiment_entityovertime()
    
    # if(input$normalized_entities == "yes"){
    #   entities <- entities %>%
    #     group_by(title) %>%
    #     mutate(issue.freq = issue.freq/n())
    # }
    
    
    entities$score <- as.numeric(entities$score)
    entities$magnitude <- as.numeric(entities$magnitude)
    
    entities$sentiment.type <- sapply(entities$score, function(score){
      if(score < 0){
        return("negative")
      }else{
        return("positive")
      }
    })
    
    statistics <- entities %>%
      group_by(id, title, year) %>%
      summarise(avg.score = round(mean(score), 2), avg.magnitude = round(mean(magnitude), 2))
    
    
    # colfunc <- colorRampPalette(c("blue", "red"))
    # cols <- colfunc(length(unique(statistics$sentiment.type)))
    
    
    statistics$avg.magnitude <- statistics$avg.magnitude * sign(statistics$avg.score)
    
    # statistics <- statistics[statistics$id %in% c("italia - GPE", "roma - GPE"), ]
  
    
    n.cols <- length(unique(statistics$id))
    col.rem <- c("light", "red", "tomato", "white", "gray", "grey", "blue", "azure", "coral", "fire", "sky", "see", "turquoise", "marine", "aqua")
    cols <- grDevices::colors()
    cols <- cols[-sort(unique(unlist(lapply(col.rem, function(col){which(grepl(col, cols))}))))]
    col.ind <- seq(from = 1, to = length(cols), by = floor((length(cols) - 1)/n.cols))[1:n.cols]
    cols <- cols[col.ind]
    
    s <-  input$sentiment_selected_entity#unique(unique(statistics$id))
    pal <- brewer.pal(9, "Set1")[-c(1:2)]
    cols <- setNames(pal[1:length(s)], s)

    title.size <- ifelse(save.into.file, 8, 18)
    
    plot <- statistics %>%
      ggplot(aes(fill=id, y = avg.score, x = year, color = id)) +
      geom_density(position="dodge", stat="identity", alpha = 0.5, color = "gray") +
      # geom_density(aes(y = avg.magnitude), position="dodge", stat="identity", alpha = 0.5, color = "gray") +
      geom_point(alpha = 1.0, size = 2, show.legend = FALSE) + 
      geom_hline(yintercept=0) +
      facet_wrap(~title) +
      #geom_text(aes(label = paste0(round(ab*100, 0),"%")), position = position_stack(vjust = 0.5), size = 5) +
      xlab("Year") +   ylab("Sentiment score") +
      # coord_flip() +
      # facet_grid(. ~ title, scale = "free") +
      scale_fill_manual(values = cols) +
      scale_color_manual(values = cols) +
      scale_y_continuous(labels = scales::comma) +
      theme_classic() +
      theme(
        plot.title = element_text(size=18, face="bold", hjust = 0.5),
        axis.title.x = element_text(size=14, face="bold"),
        axis.title.y = element_text(size=14, face="bold", hjust = 0.5, angle = 90),
        axis.text.x = element_blank(), #element_text(size=rel(1.5), angle=0),
        axis.text.y = element_text(size=rel(1.5), angle=0),
        axis.ticks.x = element_blank(),
        legend.position = "bottom",
        legend.title = element_text(size=rel(1.3), angle=0),
        legend.text = element_text(size=rel(1.3), angle=0),
        strip.text = element_text(size = title.size)
      ) + 
      guides(fill=guide_legend(title="Entities"))
    
    plot
  }
  output$sentimentplotovertime <- renderPlot({

    plotsentimentovertime(save.into.file = FALSE)

  })

    

  # Downloadable ggplot map --------
  
  # A ggplot copy of the leaflet map as is displayed ast the moment.
  # A mapshot of Leaflet map doen't work because of leafletProxy
  #
  download_map <- reactive({
    
    # Observe change in bounding box (bb) of map on the screen
    #
    bb <- map_bounds()
    
    # Crop ggplot's version of the world to the boundings of the map on the display
    #
    plot_world   <- sf::st_crop(x = world,
                                xmin = bb$west,  xmax = bb$east,
                                ymin = bb$south, ymax = bb$north)
    plot_world   <- sf::st_transform(plot_world, "+init=epsg:3857")     # 3857 is leaflet's default crs
    
    p <- ggplot2::ggplot() + 
      ggplot2::geom_sf(data =    plot_world,
                       mapping = aes(geometry = geometry), 
                       size =    0.3, 
                       fill =   "#808080", # color for NA value in leaflet map
                       color =  "white")
    
    # Observe change in map values and adjust ggplot map accordingly
    #
    new_country_map <- country_map_data()
    if (!is.null(new_country_map)) {
      plot_data   <- sf::st_crop(x =    new_country_map$data,
                                 xmin = bb$west,  xmax = bb$east,
                                 ymin = bb$south, ymax = bb$north)
      
      # Plot the iso_a2 name of countries in the center of each country
      #
      #plot_labels <- sf::st_centroid(plot_data, of_largest_polygon = TRUE) %>% select(name = iso_a2)
      #plot_labels$name[is.na(plot_labels$name)] <- ""  # of some countries iso_a2 is missing
      
      # reproject the shapes and points with the crs used by leaflet (3857)
      #
      plot_data   <- sf::st_transform(plot_data, "+init=epsg:3857")
      #plot_labels <- sf::st_transform(plot_labels, "+init=epsg:3857")
      
      # Add the countries to the ggplot map
      p <- p +
        ggplot2::geom_sf(data =    plot_data,
                         mapping = aes(geometry = geometry,
                                       fill =     factor(fill_value, levels = 1:length(new_country_map$bins))),
                         color =  'white',
                         size  =   0.3) +
        # ggplot2::geom_sf_text(data =    plot_labels,
        #                       mapping = aes(geometry = geom, label = name),
        #                       size =    2)  +
        ggplot2::scale_fill_manual(values = new_country_map$map_colors,
                                   labels = new_country_map$legends,
                                   name =   new_country_map$title,
                                   drop =   FALSE)                  # show all labels even if not used in the map
    }
    
    if (!is.null(input$hist_map) && input$hist_map != 'none') {
      hist_map <- eval(parse(text = input$hist_map))
      
      st_crs(hist_map) <- 4326
      hist_map   <- sf::st_crop(x =    hist_map,
                                xmin = bb$west,  xmax = bb$east,
                                ymin = bb$south, ymax = bb$north)
      hist_map <- sf::st_transform(hist_map, "+init=epsg:3857")
      p <- p + ggplot2::geom_sf(data =    hist_map,
                                mapping = aes(geometry = geometry),
                                fill =   'white',
                                alpha =   0,
                                color =  'black',
                                size  =   0.3)
    }
    
    # Observe change in city or region map values for and adjust ggplot map accordingly
    #
    new_location_map <- location_map_data() 
    if (!is.null(new_location_map)) {
      
      # crop to the boundings of map on the screen
      #
      plot_data   <- sf::st_crop(x =    new_location_map$data,
                                 xmin = bb$west,  xmax = bb$east,
                                 ymin = bb$south, ymax = bb$north)
      
      # To ensure the printed map has the same appearance as the map on the screen,
      # reproject the shapes to the crs used by leaflet (3857)
      #
      plot_data   <- sf::st_transform(plot_data, "+init=epsg:3857")
      plot_data  %<>% select(-X, -Y) %>% cbind(st_coordinates(plot_data)) # the old X and Y were in the wrong CRS
      
      # Add regions as squares to the map.
      # The fill color indicates the number of articles in which the name of the city occurs. See legend!
      # Also the width/height gives an indication of the number of occurrences
      #
      plot_region <- plot_data %>% filter(location_type == 'region')
      p <- p + geom_tile(data =    plot_region,
                         mapping = aes(x =      X,
                                       y =      Y,
                                       width =  fill_value * 20000,
                                       height = fill_value * 20000,
                                       fill =   factor(fill_value, levels = 1:length(new_location_map$bins))),
                         colour =  "#808080")
      
      # Add cities as circles to the map.
      # The fill color indicates the number of articles in which the name of the city occurs. See legend!
      # Also the radius gives an indication of the number of occurrences
      #
      plot_city <- plot_data %>% filter(location_type == 'city')
      p <- p +
        ggforce::geom_circle(data =    plot_city,
                             mapping = aes(x0 =   X,
                                           y0 =   Y,
                                           r =    fill_value * 5000,
                                           fill = factor(fill_value, levels = 1:length(new_location_map$bins))),
                             colour =  "#808080")
    }
    
    p <- p +
      coord_sf(label_axes = "----") +                 # suppress the graticules (meridians/parallels)
      xlab("") + ylab("") + ggtitle("The GeoNewsMiner (GNM), 1898 - 1920")
    return(p)
  })
  

  output$downloadMap <- downloadHandler(
    filename = function(){paste("map",'.png',sep='')},
    content =  function(file){ggsave(file, plot = download_map())}
  )
  
  # Panel data section -----
  output$table_data <- renderDataTable({
    data <- select_data()
    data %<>% select(-lon, -lat, -n_titles_perc)
    data %<>% rename(
      type =        location_type,
      name =        name_long,
      pages =   pages,
      '% pages' =    pages_perc,
      occurrences =  occurrences,
      '% occs' =    occurrences_perc,
      titles =      n_titles)
    # 'first year' = begin_year,
    # 'last year'=   end_year)
    DT::datatable(data = data,
                  colnames = c('type' =        'type',
                               'name' =        'name',
                               'pages' =       'pages',
                               '% pages' =      '% pages',
                               'occurrences' = 'occurrences',
                               '% occs' =      '% occs',
                               'titles' =      'titles'),
                  # 'first year'=   'first year',
                  # 'last year'=    'last year'),
                  fillContainer = TRUE) %>%
      formatRound(c('% pages', '% occs'), 2)
  })
  
  output$table_data_entities <- renderDataTable({
    #geo_data <- select_data()
    data <- select_data_entities()
    # DT::datatable(data = data,
    #               colnames = c('type' =        'type',
    #                            'name' =        'name',
    #                            'pages' =       'pages',
    #                            '% pages' =      '% pages',
    #                            'occurrences' = 'occurrences',
    #                            '% occs' =      '% occs',
    #                            'titles' =      'titles'),
    #               # 'first year'=   'first year',
    #               # 'last year'=    'last year'),
    #               fillContainer = TRUE) %>%
    #   formatRound(c('% pages', '% occs'), 2)
    
    data <- left_join(data, geo_data %>% mutate(entity = location), by = c('entity')) %>%
    # data <- data %>%
      group_by(entity, type) %>%
      summarize(level = unique(location_type)[1], address = unique(formatted_address)[1], 
                frequency = sum(issue.freq), normalized.freq = round(sum(issue.freq)/n(), 2),
                titles = length(unique(title)), issues = n())
    

    data
  })
  
 
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("place_names", ".csv", sep = "")
    },
    content = function(file) {
      data <- select_data()
      data %<>% select(-lon, -lat, -n_titles_perc)
      data %<>% rename(
        type =         location_type,
        name =         name_long,
        pages =        pages,
        '% pages' =    pages_perc,
        occurrences =  occurrences,
        '% occs' =     occurrences_perc,
        titles =       n_titles)
      # 'first year' = begin_year,
      # 'last year'=   end_year)
      
      write_csv(data, file)
    }
  )
  
  
  output$downloadEntities <- downloadHandler(
    filename = function() {
      paste("Entities distribution", ".png", sep = "")
    },
    content =  function(file){ggsave(file, plot = plotstatisticsplot(save.into.file = TRUE))}
  )
  
  output$downloadEgoNetwork <- downloadHandler(
    filename = function() {
      paste("Ego network", ".html", sep = "")
    },
    content =  function(file){visSave(file, graph = plotnetwork_ego())}
  )
  
  output$downloadIssueNetwork <- downloadHandler(
    filename = function() {
      paste("Issue network", ".html", sep = "")
    },
    content =  function(file){visSave(file, graph = plotnetwork_issue())}
  )
  
  output$downloadsentimentplot <- downloadHandler(
    filename = function() {
      paste("Sentiment over time", ".png", sep = "")
    },
    content =  function(file){ggsave(file, plot = plotsentiment(save.into.file = TRUE))}  )
  
  
  output$downloadsentimenttopentities <- downloadHandler(
    filename = function() {
      paste("Entity sentiment top entities", ".png", sep = "")
    },
    content =  function(file){ggsave(file, plot = plotsentimenttopentities(save.into.file = TRUE), scale = 3)})


  output$downloadsentimentovertime <- downloadHandler(
    filename = function() {
      paste("Entity sentiment over time", ".png", sep = "")
    },
    content =  function(file){ggsave(file, plot = plotsentimentovertime(save.into.file = TRUE))}  )

  
  
  # Bookmark section -----------
  
  # To keep the url short, exclude state variables which will not be used on restore. 
  #
  setBookmarkExclude(c("table_data_rows_current",
                       "table_data_cell_clicked",
                       "table_data_search", 
                       "table_data_rows_selected", 
                       "table_data_rows_all", 
                       "table_data_state",
                       "data_map_center",
                       "data_map_groups",
                       "data_map_zoom",
                       "data_map_shape_mouseover",
                       "data_map_shape_mouseout"))
  
  onBookmark(function(state) {
    
    # Leaflet doesn't restore view point and zoom level from the saved state variables.
    # These values are saved and are explicitely set when restoring the app
    #
    state$values$zoom     <- isolate(input$data_map_zoom)
    state$values$center   <- isolate(input$data_map_center) # {lat, lng}
    state$values$bounds   <- isolate(input$data_map_bounds)
    
    # when restoring program must know which panel was active during bookmarking
    #
    state$values$tabpanel <- isolate(input$Map)             
  }) 
  
  
  onRestore(function(state) {
    
    # 
    #
    if ( restore_map() == "START") {
      
      lat1 <<- state$values$bounds$south
      lat2 <<- state$values$bounds$north
      lng1 <<- state$values$bounds$west
      lng2 <<- state$values$bounds$east
      
      # If a bookmark is made while the data panel is selected, the map will not be
      # restored unless explicitely triggered by reactive value restore_map
      #
      if (state$values$tabpanel == "Data") {
        restore_map("BASE_MAP")   # base map triggers countries and countries triggers regions and so on
      }
    }
  })
  # End of bookmark section -------
  
    
}