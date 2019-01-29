
###############################################################################
shinyUI(dashboardPage(
  skin = 'green',
###############################################################################
  dashboardHeader(title = 'Shiny Project', titleWidth = 215),
# SIDEBAR #####################################################################
  dashboardSidebar(width = 215,
    sidebarUserPanel(
      name     = 'Sabbir Mohammed',
      image    = 'profile.jpg', 
      subtitle = h6('NYC Data Science Academy')),
    sidebarMenu(
      menuItem(tabName = 'home',        'Home'             , icon = icon('fas fa-home')),
      menuItem(tabName = 'world_map',   'World Map'        , icon = icon('fas fa-globe-americas')),
      menuItem(tabName = 'time_series', 'Global Aggregates', icon = icon('fas fa-chart-line')),
      menuItem(tabName = 'per_country', 'Per Country'      , icon = icon('fas fa-chart-bar')),
      menuItem(tabName = 'top20',       'Top 20 Countries' , icon = icon('fas fa-sort-amount-up')),
      menuItem(tabName = 'about',       'About'            , icon = icon('fas fa-info-circle'))
    )
  ),
###############################################################################
  dashboardBody(
    tabItems(
# HOME ########################################################################
      tabItem(
        tabName = 'home', h1('A Visualization of the Global Carbon Footprint'),
        fluidRow(
          box(
            outputId = 'home1',
            width = 12,
            h4(textOutput('home1'))),
          box(
            outputId = 'home2',
            width = 12,
            h4(textOutput('home2'))),
          box(
            outputId = 'home3',
            width = 12,
            h4(textOutput('home3')))
        ),
        fluidRow(
          img(src = 'homepage.jpg', width = '100%')
        )
      ),
# WORLD MAP ###################################################################
      tabItem(
        tabName = 'world_map', h3('Carbon Emission Levels by Country (1975-2015)'),
        fluidRow( 
          column (4,
          sliderInput(
            inputId = "year", 
            label = h5("Select a Year"), 
            min = 1975, max = 2015, step = 10, sep='', value = 2015, animate = TRUE)
          ), 
          column(8,
          selectizeInput(
            inputId = 'type_map',
            label   = h5('Select a Category'),
            choices = metric)
          )
        ),
        fluidRow( 
          box(
            outputId = 'wmap',
            width    =  12,
            htmlOutput('wmap_gvis') 
          )
        )
      ),
# GLOBAL AGGREGATES ###########################################################
tabItem(
  tabName = 'time_series', h3('Global Carbon Emission Levels (1970 - 2015)'),
  fluidRow(
    box(
      selectizeInput(
        inputId = 'type_total',
        label   = h5('Select a Category'),
        choices = metric
      )
    )
  ),
  fluidRow( 
    box(
      outputId = 'aggregate',
      width    =  12,
      htmlOutput('aggregate_graph')
    )
  )
),
# PER COUNTRY #################################################################
      tabItem(
        tabName = 'per_country', h3('Carbon Emission Levels: Consumption vs Production (1970-2015)'),
        fluidRow(
          box(
            selectizeInput(
              inputId = 'country',
              label   = h5('Select a Country'),
              choices = unique(complete.df$Country)
            )
          )
        ),
        fluidRow( 
          box(
            outputId = 'percountry',
            width    =  12,
            plotOutput('percountry_graph')
          )
        )      
      ), 
# TOP 20 ######################################################################
      tabItem(
        tabName = 'top20', h3('A Closer Look at the Top 20 (2015)'),
        fluidRow(
          box(
            selectizeInput(
              inputId = 'type_20',
              label   = 'Select a Category',
              choices = metric
            )
          )
        ),
        fluidRow( 
          tabBox(
            id    = 't20',
            width =  12,
            tabPanel('Graph', plotOutput('t20_graph')),
            tabPanel('Table', DT::dataTableOutput('t20_table'))
          )
        )
      ),
# ABOUT #######################################################################
      tabItem(
        tabName = 'about', h2('About This Project'),
        fluidRow(
          box(
            title = h4('Myself'),
            outputId = 'about1',
            width = 10,
            h5(textOutput('about1'))
          ),
          box(
            title = h4('Purpose of this App'),
            outputId = 'about2',
            width = 10,
            h5(textOutput('about2'))
          ),
          box(
            title = h4('The Source'),
            outputId = 'about3',
            width = 10,
            h5(textOutput('about3'))
          ),
          box(
            title = h4('Code'),
            outputId = 'about4',
            width = 10,
            h5(textOutput('about4'))
          ),
          box(
            outputId = 'about5',
            width = 10,
            h5(textOutput('about5'))
          )
        )
      )
###############################################################################
    )
  )
###############################################################################
))
###############################################################################