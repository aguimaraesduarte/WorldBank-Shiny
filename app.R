library(shiny)
library(reshape2)
library(ggvis)

setwd("~/Documents/Module5/msan622/WorldBank-Shiny/")

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
df <- melt(df, c("Indicator", "Country", "Region"), 5:(ncol(df)-1), variable.name = "Year", value.name = "Value")
df$Value <- as.numeric(df$Value)
# Cast indicators
df <- dcast(df, Country+Region+Year~Indicator, value.var = "Value")
# Change names
names(df) <- c("Country", "Region", "Year", "Fertility", "LifeExp", "Population")
# Define regions vector
regions <- sort(as.vector(unique(df$Region)))
regions <- append(regions, "All", 0)

ui <- fluidPage(
  headerPanel("Gapminder Interactive Plot"),
  sidebarPanel(width = 3,
    selectInput("region", "Select Region", regions),
    sliderInput("year", "Select Year",
                min = 1962, max = 2014, value = 1998, sep = "",
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
    s <- subset.data.frame(df, Year == input$year, drop=T)
    s[is.na(s$LifeExp), "Fertility"] <- NA
    s[is.na(s$Fertility), "LifeExp"] <- NA
    
    region <- input$region
    if(region != "All"){
      s <- subset.data.frame(s, Region == region, drop = T)
    }
    
    s <- s[!is.na(s$LifeExp),]
    return(s)
  })
  
  vis <- reactive({
    popsize <- input$pop_size
    
    sub_df %>%
      ggvis(~LifeExp, ~Fertility, fill = ~Region,
            fillOpacity := 0.5, fillOpacity.hover := 1) %>%
      #layer_text(text := ~Country) %>%
      
      set_options(width = 1000, height = 600, renderer = "svg") %>%
      
      add_axis("x", title = "Life expectancy", title_offset = 50) %>%
      add_axis("y", title = "Fertility rate", title_offset = 50) %>%
      scale_numeric("x", domain = c(10, 90), nice = FALSE) %>%
      scale_numeric("y", domain = c(0, 10), nice = FALSE) %>%
      scale_numeric("size", range = c(10, popsize), nice = FALSE) %>%
      
      layer_points(size = ~Population, key := ~Country) %>%
      add_legend("size", orient = "left", title="Population") %>%
      set_options(duration = 0) %>%
      
      add_tooltip(function(data){
        paste0("Country: ", as.character(data$Country), "<br>",
               "Region: ", as.character(data$Region), "<br>",
               "Population: ", prettyNum(data$"Population", big.mark=",", scientific=FALSE), "<br>",
               "Life Expectancy: ", as.character(round(data$LifeExp, 2)), "<br>",
               "Fertility Rate: ", as.character(round(data$Fertility, 2)))
      }, "hover")
  })
  
  vis %>%
    bind_shiny("plot", "plot_ui")
}

shinyApp(ui = ui, server = server)

