# WorldBank-Shiny
![gif](static/worldbank-shiny.gif)

You can try this out online on [shinyapps.io](https://aguimaraesduarte.shinyapps.io/worldbank-visualizer/), or by running `shiny::runGitHub("WorldBank-Shiny", "aguimaraesduarte")` in [RStudio](https://www.rstudio.com/). Have fun!

[The World Bank](http://databank.worldbank.org/data/reports.aspx?source=2&series=SP.POP.1564.TO.ZS&country=) provides a lot of data for all countries between 1960 and 2015, such as Life Expectancy, Fertility Rate, Population, and many others. In this project, I chose those three variables in order to build an R Shiny app that plots the Fertility Rate vs Life Expectancy throughout the years. Each country is a single point that moves through the plot with time, and the size of the point is proportional to the total population for that country. My goal was to reproduce [this interactive graph](https://www.google.com/publicdata/explore?ds=d5bncppjof8f9_&ctype=b&strail=false&nselm=s&met_x=sp_dyn_le00_in&scale_x=lin&ind_x=false&met_y=sp_dyn_tfrt_in&scale_y=lin&ind_y=false&met_s=sp_pop_totl&scale_s=lin&ind_s=false&dimp_c=country:region&ifdim=country&iconSize=0.5&uniSize=0.035) from Google. The result is fairly close.

# Prepping the data
The downloaded CSV is a "short and fat" table, where each country represents one row, and there is a column for each year. All three indicator variables are also concatenated into one single column, which is not ideal for this analysis. Therefore, we use `reshape2` and `dplyr` to _melt_ and _cast_ the data frame and iteratively construct one that is "long and thin". My final columns are **Country**, **Region**, **Year**, **Fertility**, **LifeExp**, and **Population**. The geographical region for each country was downloaded separately from the metadata, and needs to be merged into our data frame by country.

```
# Read data
df <- read.csv("WorldBankData.csv", stringsAsFactors = F)

# Remove trailing data (garbage from WDI)
df <- df[1:(nrow(df)-5),]

# Change names
names(df) <- c("Indicator", "IndCode", "Country", "Country.Code", as.character(1960:2015))

# Get metadata
meta <- read.csv("Metadata.csv")
meta <- meta[,c("Country.Code", "Region")]
meta <- meta[meta$Region!="",]

# Merge tables
df <- merge(df, meta, "Country.Code")

# Keep only columns of interest
df <- df[,c(2, 4, 5:ncol(df))]
df$Indicator <- factor(df$Indicator)
df$Country <- factor(df$Country)
df$Region <- factor(df$Region)

# Melt years
df <- melt(df, c("Indicator", "Country", "Region"), 3:(ncol(df)-1), variable.name = "Year", value.name = "Value")
df$Value <- as.numeric(df$Value)

# Cast indicators
df <- dcast(df, Country+Region+Year~Indicator, value.var = "Value")

# Change names
names(df) <- c("Country", "Region", "Year", "Fertility", "LifeExp", "Population")

# Define regions vector
regions <- sort(as.vector(unique(df$Region)))
regions <- append(regions, "All", 0)

# Define countries vector
countries <- sort(as.vector(unique(df$Country)))
```

# Shiny UI
Here, I decided to keep a clean interface for the UI:
- a sidebar panel with a region selection, country selection, the year slider, and the population size slider;
- a main panel with the plot and legends.

The user choose which countries to plot by selecting regions from the selectize menu (no selection keeps all regions), and can also select some countries to keep track of (text will appear with their name). The user may also choose the specific year for which to plot the points. This year slider has a "play" button that animates the graph. The user can also make the circles bigger or smaller according to taste.

I opted to use `ggvis` instead of `ggplot2` for this project, but it should not be too complicated to switch.

```
ui <- fluidPage(
  headerPanel("Gapminder Interactive Plot"),
  sidebarPanel(width = 3,
               selectizeInput("regions", "Select Region", regions, multiple = T,
                              options = list(placeholder = 'Select regions')),
               selectizeInput("countries", "Select Countries", countries, multiple = T,
                              options = list(placeholder = 'Select countries')),
               sliderInput("year", "Select Year",
                           min = 1960, max = 2014, value = 1970, sep = "",
                           animate = animationOptions(interval = 100)),
               sliderInput('pop_size', "Population",
                           min = 500, max = 5000, value = 3000, step = 500, sep = "", ticks = F)
  ),
  mainPanel(
    ggvisOutput("plot"),
    uiOutput("plot_ui")
  )
)
```

# Shiny Server
The server part of the app is where the cool stuff happens. First, I subset the dataframe for the year selected in the slider as well as for the region selected in the selectize. If no region is selected, we don't need to subset per region.

Aftewards, it's a matter of piping the data through `ggvis`. We color by Region, and play with the opacity so that when the mouse hovers over a country, the opacity increases. Each circle size is proportional to the country population. The smallest population will have a circle with and area of 10 square pixels, and the biggest one will have and area defined by the slider on the sidebar. I opted to hide the population size legend, as it wasn't very informative and caused more issues than having it present. Indeed, since hovering over a country shows its population, it would be slightly redundant to have both information on the screen. Finally, the selected countries have their names as a label that follows the circle for easy tracking. However, this makes the animation a little bit more sluggish, as more computation is needed each step.

Talking about tooltips, these show up when the mouse hovers over a circle. The circle in question will become opaque, and a tooltip will pop up showing several information about the country, such as name, region, population, life expectancy, and fertility rate.

```
server <- function(input, output) {
  
  vis <- reactive({
    
    sub_df <- subset(df, Year == input$year, drop=T)
    sub_df <- subset(sub_df, !is.na(sub_df$LifeExp))
    sub_df <- subset(sub_df, !is.na(sub_df$Fertility))
    
    regions <- input$regions
    if(!is.null(regions)){
      sub_df <- subset(sub_df, Region %in% regions, drop = T)
    }
    
    popsize <- input$pop_size
    
    selected_countries <- input$countries
    
    sub_df %>%
      ggvis(~LifeExp, ~Fertility, fill = ~Region,
            fillOpacity := 0.5, fillOpacity.hover := 1,
            stroke := NA, stroke.hover = ~Region, strokeWidth := 4, strokeOpacity := 0.5) %>%
      layer_text(text := ~Country, data = subset(sub_df, Country %in% selected_countries)) %>%
      
      set_options(width = 1000, height = 600, renderer = "svg") %>%
      
      add_axis("x", title = "Life expectancy", title_offset = 50) %>%
      add_axis("y", title = "Fertility rate", title_offset = 50) %>%
      scale_numeric("x", domain = c(10, 90), nice = FALSE) %>%
      scale_numeric("y", domain = c(0, 10), nice = FALSE) %>%
      scale_numeric("size", range = c(10, popsize), nice = FALSE) %>%
      
      layer_points(size = ~Population, key := ~Country) %>%
      hide_legend("size") %>%
      set_options(duration = 0) %>%
      
      add_tooltip(function(data){
        paste0("Country: <b>", as.character(data$Country), "</b><br>",
               "Region: <b>", as.character(data$Region), "</b><br>",
               "Population: <b>", prettyNum(data$"Population", big.mark=",", scientific=FALSE), "</b><br>",
               "Life Expectancy: <b>", as.character(round(data$LifeExp, 2)), "</b><br>",
               "Fertility Rate: <b>", as.character(round(data$Fertility, 2)), "</b>")
      }, "hover")
  })
  
  vis %>%
    bind_shiny("plot", "plot_ui")
}
```

# Final thoughts
The app is then run by calling `shinyApp(ui = ui, server = server)`, and produces a fairly similar output to what we were trying to replicate. Future work will include being able to select the x and y axes, as well as maybe trying to optimize the subsetting so that the animation can become smooth even with the option to select and highlight countries.
