

# Definition of the user interface of the GeoNewsMiner app
#
# Developer: Kees van Eijden (k.vaneijden@uu.nl)
#

#MODALS: Functions for showing modal dialogs to users on the same page they are working ----
#
{
modal_data_values <-
  bs_modal(
    id =   "modal_data_values",
    title = h3("Frequencies"),
    body =  includeMarkdown("www/data_values.md"),
    size = "medium")

modal_cities <-
  bs_modal(
    id =   "modal_cities",
    title = h3("Cities"),
    body =  includeMarkdown("www/cities.md"),
    size = "medium")

modal_regions <-
  bs_modal(
    id =   "modal_regions",
    title = h3("Regions"),
    body =  includeMarkdown("www/regions.md"),
    size = "medium")

modal_percentile_cities <-
  bs_modal(
    id =   "modal_percentile_cities",
    title = h3("Show cities based on their frequencies"),
    body =  includeMarkdown("www/percentile_cities.md"),
    size = "medium")

modal_percentile_regions <-
  bs_modal(
    id =   "modal_percentile_regions",
    title = h3("Show Regions based on their frequencies"),
    body =  includeMarkdown("www/percentile_regions.md"),
    size = "medium")

modal_share <-
  bs_modal(
    id =   "modal_share",
    title = h3("Share your results"),
    body =  includeMarkdown("www/share.md"),
    size = "medium")

modal_overlay <- bs_modal(
  id =   "modal_overlay",
  title = h3("Historical Maps"),
  body =  includeMarkdown("www/overlay.md"),
  size = "medium")

modal_ToU <- bs_modal(
  id =     "modal_ToU", 
  title =   h3("Terms of Usage"),
  body =    includeMarkdown(path = "www/ToU.md"),
  footer =  tags$span(bs_modal_closebutton("Understood!")),    # user must accept ToU
  size =   "large")


modal_data_values_entities <-
  bs_modal(
    id =   "modal_data_values_entities",
    title = h3("Frequencies"),
    body =  includeMarkdown("www/data_values_entities.md"),
    size = "medium")

modal_entity_type <-
  bs_modal(
    id =   "modal_entity_type",
    title = h3("Entity type"),
    body =  includeMarkdown("www/modal_entity_type.md"),
    size = "medium")

modal_sentiment_entity_type <-
  bs_modal(
    id =   "modal_sentiment_entity_type",
    title = h3("Entity type"),
    body =  includeMarkdown("www/modal_sentiment_entity_type.md"),
    size = "medium")

modal_network_cooccurrence <-
  bs_modal(
    id =   "modal_network_cooccurrence",
    title = h3("Co-occurrence frequency"),
    body =  includeMarkdown("www/modal_network_cooccurrence.md"),
    size = "medium")

modal_network_sentiment_polarity <-
  bs_modal(
    id =   "modal_network_sentiment_polarity",
    title = h3("Sentiment polarity"),
    body =  includeMarkdown("www/modal_network_sentiment_polarity.md"),
    size = "medium")

modal_network_ego_only <-
  bs_modal(
    id =   "modal_network_ego_only",
    title = h3("Ego only"),
    body =  includeMarkdown("www/modal_network_ego_only.md"),
    size = "medium")
}

# MAIN: User interface has a simple sidebar layout
# The main panel contains a few tabpanels for:
# 1. map to show data in a spatial context,
# 2. datatable of the data,
# 3. explanatory text and 
# 4. references for further reading
#
# The 'function(request)' bit is necessary to allow for sharing bookmarks between users
#
if(FALSE){
tab.map.input <- function(){

  fluidRow(
    radioButtons("normalized",
                 label =        h4("Frequencies"),
                 choiceNames =  c("Absolute", "Percentage"),
                 choiceValues = c('no', 'yes'),
                 selected =     'no',
                 inline =       TRUE) %>%
      shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>% bs_attach_modal(id_modal = "modal_data_values")),

    # show cities on map?
    #
    radioButtons("show_cities",
                 label =        h4("Cities"),
                 choiceNames =  c("Exclude", "Include"),
                 choiceValues = c(FALSE, TRUE),
                 selected =     FALSE,
                 inline =       TRUE) %>%
      shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>%
                               bs_attach_modal(id_modal = "modal_cities")),

    # which cities must be displayed: the most or least referenced?
    #
    # the user only sees this widget when normalized is yes
    #
    conditionalPanel(
      condition = "input.show_cities == 'TRUE'",
      sliderInput(inputId = "perc_cities",
                  label = h4("Percentile Range"),
                  min =   0,                   # only display cities above 10% percentile
                  max =   100,
                  value = c(0, 100),  # By default the TOP 10% is selected
                  step =  10) %>%
        shinyInput_label_embed(shiny_iconlink() %>%
                                 bs_attach_modal(id_modal = "modal_percentile_cities"))),

    # idem regions
    #
    radioButtons("show_regions",
                 label =        h4("Regions"),
                 choiceNames =  c("Exclude", "Include"),
                 choiceValues = c(FALSE, TRUE),
                 selected =     FALSE,
                 inline =       TRUE) %>%
      shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>%
                               bs_attach_modal(id_modal = "modal_regions")),

    conditionalPanel(
      condition = "input.show_regions == 'TRUE'",
      sliderInput(inputId = "perc_regions",
                  label =   h4("Percentile Range"),
                  min =      0,
                  max =      100,
                  value =    c(0, 100),
                  step =     10) %>%
        shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>%
                                 bs_attach_modal(id_modal = "modal_percentile_regions"))),

    # User can opt for an overlay with a historical map (1880, 1914 or 1920)
    #
    radioButtons(inputId =      "hist_map",
                 label =        h4("Historical Maps"),
                 choiceNames =  c("1994", "1880", "1914", "1920"),
                 choiceValues = c("none", "world1880", "world1914", "world1920"),
                 selected =     "none",
                 inline =       TRUE) %>%
      shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>%
                               bs_attach_modal(id_modal = "modal_overlay"))
  )
}
}

tab.map.input <- function(){
  
  fluidRow(

      column(6,
               radioButtons("normalized",
                            label =        h4("Frequencies"),
                            choiceNames =  c("Absolute", "Percentage"),
                            choiceValues = c('no', 'yes'),
                            selected =     'no',
                            inline =       TRUE) %>%
                 shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>% bs_attach_modal(id_modal = "modal_data_values")),
    
      ),
      column(6,
               radioButtons(inputId =      "hist_map",
                            label =        h4("Historical Maps"),
                            choiceNames =  c("1994", "1880", "1914", "1920"),
                            choiceValues = c("none", "world1880", "world1914", "world1920"),
                            selected =     "none",
                            inline =       TRUE) %>%
                 shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>%
                                          bs_attach_modal(id_modal = "modal_overlay"))
             )       
      )

  
}


tab.map.filters <- function(){
  fluidRow(
    column(6,
           radioButtons("show_cities",
                        label =        h4("Cities"),
                        choiceNames =  c("Exclude", "Include"),
                        choiceValues = c(FALSE, TRUE),
                        selected =     FALSE,
                        inline =       TRUE) %>%
             shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>%
                                      bs_attach_modal(id_modal = "modal_cities")),
           
           # which cities must be displayed: the most or least referenced?
           #
           # the user only sees this widget when normalized is yes
           #
           conditionalPanel(
             condition = "input.show_cities == 'TRUE'",
             sliderInput(inputId = "perc_cities",
                         label = h4("Percentile Range"),
                         min =   0,                   # only display cities above 10% percentile
                         max =   100,
                         value = c(0, 100),  # By default the TOP 10% is selected
                         step =  10) %>%
               shinyInput_label_embed(shiny_iconlink() %>%
                                        bs_attach_modal(id_modal = "modal_percentile_cities")))
    ),
    column(6,
           
           # idem regions
           #
           radioButtons("show_regions",
                        label =        h4("Regions"),
                        choiceNames =  c("Exclude", "Include"),
                        choiceValues = c(FALSE, TRUE),
                        selected =     FALSE,
                        inline =       TRUE) %>%
             shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>%
                                      bs_attach_modal(id_modal = "modal_regions")),
           
           conditionalPanel(
             condition = "input.show_regions == 'TRUE'",
             sliderInput(inputId = "perc_regions",
                         label =   h4("Percentile Range"),
                         min =      0,
                         max =      100,
                         value =    c(0, 100),
                         step =     10) %>%
               shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>%
                                        bs_attach_modal(id_modal = "modal_percentile_regions")))
    )
           
  )
}

entityTypeInput <- function(id) {
  checkboxGroupInput(inputId =  NS(id, "entity_type"),
                     label =     h4("Entities"), 
                     choices =   c("PER", "ORG", "GPE", "LOC"),
                     selected =  c("PER", "ORG", "GPE", "LOC"),
                     inline = TRUE, width = "100%"
  )
}


ui <- function(request) {
  
  fluidPage(
    
    # Application title
    #
    titlePanel("DeXTER - Deep teXT minER"),
    
    # Functions displaying modal boxes to show usage info about an input widget
    modal_data_values,
    modal_cities,
    modal_percentile_cities,
    modal_regions,
    modal_percentile_regions,
    modal_share,
    modal_overlay,
    modal_ToU,
    
    modal_data_values_entities,
    modal_entity_type,
    modal_sentiment_entity_type,
    modal_network_cooccurrence,
    modal_network_sentiment_polarity,
    modal_network_ego_only,
    
    # SIDEBAR ----  
    # Sidebar contains widgets for data selection and for display options 
    #
    sidebarLayout(
      sidebarPanel(
        
        # input widgets for selecting data are dynamically created by server, because the choices 
        # depend on data set being used
        #
        uiOutput(outputId = "selection_period"),            # selecting time period
        uiOutput(outputId = "selection_titles"),            # selecting newspaper titles
        
      bookmarkButton(label = "Share the results"),
        shiny_iconlink(name = 'info-circle') %>% 
          bs_attach_modal(id_modal = "modal_share")
      ),
      
      # MAIN ----
      # Contains 4 tabpanels for map, data table, explanation and references
      #
      mainPanel(
        tabsetPanel(
          id = "Map",
          tabPanel("Map",
                   br(),
                   tab.map.input(),
                   br(),
                   leafletOutput("data_map", width = "100%", height = 700),
                   br(),
                   HTML("NA*: occurrences < 6"),
                   br(),
                   tab.map.filters(),
                   br(),
                   downloadButton("downloadMap", "Download Map")),
          tabPanel("Entities",
                   br(),
                   fluidRow(
                     column(6,
                       checkboxGroupInput(inputId =  "entity_type",
                                          label =     h4("Entities"), 
                                          choices =   c("PER", "ORG", "GPE", "LOC"),
                                          selected =  c("PER", "ORG", "GPE", "LOC"),
                                          inline = TRUE, width = "100%"
                                          ) %>%
                         shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>% bs_attach_modal(id_modal = "modal_entity_type"))
                       
                       ),
                       column(6,
                         radioButtons("normalized_entities",
                                      label =        h4("Frequencies"),
                                      choiceNames =  c("Absolute", "Percentage"),
                                      choiceValues = c('no', 'yes'),
                                      selected =     'no',
                                      inline =       TRUE) %>%
                           shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>% bs_attach_modal(id_modal = "modal_data_values_entities"))
                         ),
                     br(),
                     ),
                   br(),
                   plotOutput("statisticsplot", height = "700px"),
                   br(),
                   downloadButton("downloadEntities", "Download Entity plot")
                   
          ),
          tabPanel("Network",
                   br(),
                   fluidRow(
                     column(6,
                            sliderInput(inputId = "link_width",
                                        label = h4("Co-occurence frequency"),
                                        min =   1,                   # only display cities above 10% percentile
                                        max =   10,
                                        value = c(1, 10),  # By default the TOP 10% is selected
                                        step =  1) %>%
                              shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>% bs_attach_modal(id_modal = "modal_network_cooccurrence"))
                     ),
                     column(6, 
                            sliderInput(inputId = "link_sentiment",
                                        label = h4("Sentiment polarity"),
                                        min =   0.0,                   # only display cities above 10% percentile
                                        max =   1.0,
                                        value = c(0.0, 1.0),  # By default the TOP 10% is selected
                                        step =  0.1) %>%
                              shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>% bs_attach_modal(id_modal = "modal_network_sentiment_polarity"))
                            )
                   ),
          tabsetPanel(
              id = "Network",
              # tabPanel("Network",
              #          br(),
              #          textOutput("n_nodes_all"),
              #          br(),
              #          visNetworkOutput("network_all", height = "600px"),
              #          br()),
               tabPanel("Entity-focused network",
                 br(),
                 checkboxInput("network_ego_check", "Select ego only", value = TRUE, width = NULL) %>%
                   shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>% bs_attach_modal(id_modal = "modal_network_ego_only"))
                 ,
                 br(),
                 textOutput("n_nodes_entities"),
                 br(),
                 visNetworkOutput("network_ego", height = "600px"),
                 br(),
                 selectizeInput('network_selected_node', "Select entity", choices = NULL, multiple = FALSE),
                 br(),
                 downloadButton("downloadEgoNetwork", "Download network visualization"),
              br()),
              tabPanel("Issue-focused network",
                 br(),
                 # # sliderInput(inputId = "link_width",
                 # #             label = h4("Link width"),
                 # #             min =   1,                   # only display cities above 10% percentile
                 # #             max =   10,
                 # #             value = c(1, 10),  # By default the TOP 10% is selected
                 # #             step =  1),
                 # br(),
                 textOutput("n_nodes_issues"),
                 br(),
                 visNetworkOutput("network_issues", height = "600px"),
                 br(),
                 selectizeInput('network_selected_issue', "Select one or more issues", choices = NULL, multiple = TRUE),
                 br(),
                 downloadButton("downloadIssueNetwork", "Download network visualization"),
                 br())
            )
          ),
          tabPanel("Sentiment",
                   br(),
                   fluidRow(
                     column(6, 
                       checkboxGroupInput(inputId =  "sentiment_entity_type",
                                          label =     h4("Entities"), 
                                          choices =   c("PER", "ORG", "GPE", "LOC"),
                                          selected =  c("PER", "ORG", "GPE", "LOC"),
                                          inline = TRUE, width = "100%") %>%
                         shinyInput_label_embed(shiny_iconlink(name = 'info-circle') %>% bs_attach_modal(id_modal = "modal_sentiment_entity_type"))
                     )
                   )
                   ,
                   tabsetPanel(
                     id = "Entity sentiment",
                     tabPanel("Time-focused sentiment",
                              br(),
                              plotOutput("sentimentplot", height = "700px"),
                              br(),
                              downloadButton("downloadsentimentplot", "Download sentiment visualization")
                     ),
                     tabPanel("Top entities",
                              br(),
                              plotOutput("sentimentplottoptities", height = "700px"),
                              br(),
                              numericInput("sentimentnumofentities", "Top N entities", 3, min = 1, max = 10),
                              br(),
                              downloadButton("downloadsentimenttopentities", "Download sentiment visualization")
                              ),
                     tabPanel("Entity sentiment over time",
                              br(),
                              plotOutput("sentimentplotovertime", height = "700px"),
                              br(),
                              selectizeInput('sentiment_selected_entity', "Select one or more entities", choices = NULL, multiple = TRUE, options = list(maxItems = 6)),
                              br(),
                              downloadButton("downloadsentimentovertime", "Download sentiment visualization")
                              
                     )
                   )
          ),
          tabPanel("Data", 
                   DT::dataTableOutput("table_data_entities", height = "700px"),
                   br(),
                   downloadButton("downloadData", "Download Data Selection")),
          tabPanel("About ...",
                   includeMarkdown(path = "www/about.md"),
                   
                   # User must accepts the gterms of usage
                   #
                   bs_button("Terms of Usage") %>%
                     bs_attach_modal(id_modal = "modal_ToU")
          ),
          tabPanel("More ...",
                   includeMarkdown(path = "www/more.md")))
      ) # end of main panel ----
    ) # end of sidebarLayout
  ) # end of fluid page
} # end of ui function

