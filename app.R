if(!"shiny" %in% rownames(installed.packages())){
  install.packages("shiny", dependencies = T)
}
if(!"ggplot2" %in% rownames(installed.packages())){
  install.packages("ggplot2", dependencies = T)
}
if(!"reshape2" %in% rownames(installed.packages())){
  install.packages("reshape2", dependencies = T)
}
if(!"ggvis" %in% rownames(installed.packages())){
  install.packages("ggvis", dependencies = T)
}

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

ui <- fluidPage(
  headerPanel("Gapminder Interactive Plot"),
  sidebarPanel(width = 3,
    selectInput("region", "Select Region", regions),
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

server <- function(input, output) {
  
  sub_df <- reactive({
    s <- subset(df, Year == input$year, drop=T)
    s <- subset(s, !is.na(s$LifeExp))
    s <- subset(s, !is.na(s$Fertility))
    
    region <- input$region
    if(region != "All"){
      s <- subset(s, Region == region, drop = T)
    }
    
    return(s)
  })
  
  vis <- reactive({
    
    popsize <- input$pop_size
    
    sub_df %>%
      ggvis(~LifeExp, ~Fertility, fill = ~Region,
            fillOpacity := 0.5, fillOpacity.hover := 1,
            stroke := NA, stroke.hover = ~Region, strokeWidth := 4, strokeOpacity := 0.5) %>%
      #layer_text(text := ~Country) %>%
      
      set_options(width = 1000, height = 600, renderer = "svg") %>%
      
      add_axis("x", title = "Life expectancy", title_offset = 50) %>%
      add_axis("y", title = "Fertility rate", title_offset = 50) %>%
      scale_numeric("x", domain = c(10, 90), nice = FALSE) %>%
      scale_numeric("y", domain = c(0, 10), nice = FALSE) %>%
      scale_numeric("size", range = c(10, popsize), nice = FALSE) %>%
      
      layer_points(size = ~Population, key := ~Country) %>%
      #add_legend("size", orient = "left", title="Population") %>%
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

shinyApp(ui = ui, server = server)


