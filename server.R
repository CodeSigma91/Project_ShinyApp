
###############################################################################
shinyServer(function(input, output) {
# HOME ########################################################################  
      output$home1 = renderText(
        'Hello! Welcome to this brief data visualization app!'
      )
      output$home2 = renderText(
        'As the title suggests, this app aims to display global carbon emission 
        data over the years from 1970 till 2015. The data source consists of 190 
        countries, alongside their carbon emissions data through 6 different 
        metrics, categorized over production-based and consumption-based emissions. 
        Also available are the population and GDP records.'
      )
      output$home3 = renderText(
        'Further details regarding the sources and app code are provided in the 
        About page!'
      )
# WORLD MAP ###################################################################  
      output$wmap_gvis = renderGvis({
        gvisGeoChart(
          data = complete.df %>% 
            filter(Record == as.character(input$type_map)) %>%
            select(Country, Value = matches(paste0('X',as.character(input$year)))) %>%
            mutate(Proportion = (Value/sum(Value))),
          locationvar = 'Country',
          colorvar = 'Value',
          hovervar = 'Country',
          options  = list(region = 'world', 
                         displayMode = 'regions',
                         colorAxis = "{colors:['#00853f', 'black', '#e31b23']}",
                         backgroundColor = '#dceef7',
                         datalessRegionColor = 'grey',
                         width = 'auto', height = 'auto',
                         forceFrame = TRUE)
        )
    })
# GLOBAL AGGREGATE GRAPHS #####################################################
    output$aggregate_graph = renderGvis({
      gvisLineChart(
        data = as.data.frame(complete.df %>%
          filter(Record == input$type_total) %>%
          gather(key = 'Year', value = 'Emission', -c(1:2)) %>%
          mutate(Year = as.numeric(substr(Year, 2, 5))) %>%
          group_by(Year) %>%
          summarise(Total.Emission = sum(Emission))),
        
        xvar = 'Year', yvar   = 'Total.Emission',
        options = list(width  = 'auto', height = '400', 
                       legend = "{position:'none'}",
                       hAxis  = "{format: ['####'], title: ['Year']}",
                       series = "{0: {color: '#26B14C'}}",
                       lineWidth = 4)
      )
    })
# PER COUNTRY GRAPHS ##########################################################
    output$percountry_graph = renderPlot(
      complete.df %>%
        filter(Country == input$country, Record %in% c('CBA_GgCO2', 'PBA_GgCO2')) %>%
        gather(key = 'Year', value = 'Emission', -c(1:2)) %>%
        mutate(Year = as.numeric(substr(Year, 2, 5))) %>%
        mutate(Record = ifelse(Record == 'CBA_GgCO2', 'Consumption', 'Production')) %>%
        
        ggplot(aes(x = Year, y = Emission, color = Record)) +
        geom_line(size = 2) +
        ylab(label = 'Emissions (in GgCO2)') +
        scale_color_discrete(name = 'Emission Type') +
        theme   (axis.text.x        = element_text(size = 12),
                 panel.background   = element_rect(fill='white'),
                 panel.grid.major.y = element_line(color = 'grey'),
                 panel.grid.minor.y = element_line(color = 'grey'),
                 panel.grid.major.x = element_line(color = 'grey90'))
    )
# TOP 20 GRAPHS ###############################################################
    output$t20_graph = renderPlot(
      complete.df %>%
        select  (Country, Record , X2015) %>%
        filter  (Record == as.character(input$type_20)) %>%
        arrange (desc(X2015)) %>%
        top_n   (20) %>%
        ggplot  (aes(x= reorder(Country, desc(X2015)), y= X2015)) +
        geom_col(width = 0.5, fill = 'dark red') +
        labs    (x = '', y = '') +
        theme   (axis.text.x        = element_text(angle = 90, hjust = 1, vjust = 0, size = 12),
                 panel.background   = element_rect(fill='white'),
                 panel.grid.major.y = element_line(color = 'grey'),
                 panel.grid.minor.y = element_line(color = 'grey50'))
    )
# TOP 20 TABLES ###############################################################
    output$t20_table = DT::renderDataTable({
      datatable(data = complete.df %>%
                filter  (Record == as.character(input$type_20)) %>%
                select  (Country, X2015) %>%
                arrange (desc(X2015)) %>%
                top_n   (20),
        colnames = c('Country', input$type_20))
    })
# ABOUT #######################################################################
      output$about1 = renderText(
        'Hello! I am a proud student of the Winter 2019 cohort for the NYC Data
        Science Academy in New York, NY. This is my first complete ShinyApp and I
        am very excited to share the data visialization capabilities of the 
        Shiny package within R.'
      )
      output$about2 = renderText(
        'This app aims to fashion itself primarily as an interactive and dynamic
        visualization tool. Parsing a robust and elaborate data source, the goal 
        is to provide users an intersectional view not available simply through
        the .csv files. Looking to bring the numbers to life, the purpose is not
        necessarily to provide inferential information but rather a visual insight
        into the story the data is trying to tell.'
      )
      output$about3 = renderText(
        'The primary source of data for this app was derived from the worldmrio.com 
        website. The Eora site describes itself as "global supply chain database" 
        that "consists of a multi-region input-output table (MRIO) model". The
        particular data set consists of 1,522 observations of 48 variables. Namely,
        it displays data for 190 countries from 1970 until 2015. The records are 
        categorized over consumption and production based accounting of emissions.
        The complete information and data is available here: 
        <http://worldmrio.com/footprints/carbon/>'
      )
      output$about4 = renderText(
        'This app was built using the Shiny! package on RStudio. The layout is
        using the ShinyDashboard format. The "World Map" and "Global Aggregates"
        pages were built using googleVis, while the rest of the app uses ggplot2.
        The additional packages used to process the data are tidyverse and DT'
      )
      output$about5 = renderText(
        'The image from the "Home" page was taken from the following site, under 
        free public domain: 
        <https://free-images.com/display/chimney_pollution_air_pollution.html>'
      )
###############################################################################
})
###############################################################################