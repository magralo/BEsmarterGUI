
library(shiny)
options(shiny.maxRequestSize=100*1024^2)
image<- img(src="logo.png", height = 200, width = "90%") #Local variable


###############################################
################### UI ########################


## Load General conditions, packages and other stuff necesary for run the app UI
source(file.path("genUI.R"),  local = TRUE)$value


ui <- navbarPage(a(em(strong("BEsmarter",style = "color:light blue")),href = "http://www.besmarter-team.org/"),
                
  source(file.path("ui", "presentation.R"),  local = TRUE)$value,
  source(file.path("ui", "univariate.R"),  local = TRUE)$value,
  source(file.path("ui", "multivariate.R"),  local = TRUE)$value,
  source(file.path("ui", "hierarchical.R"),  local = TRUE)$value,
  source(file.path("ui", "nonpar.R"),  local = TRUE)$value,
  source(file.path("ui", "BMAGLM.R"),  local = TRUE)$value,
  source(file.path("ui", "help.R"),  local = TRUE)$value
)

#################################
########## Server ###############

## Load General conditions, packages and other stuff necessary for run the app 
source(file.path("genServer.R"),  local = TRUE)$value


server <- function(input, output, session) {
# Include the logic (server) for each tab
  source(file.path("server", "presentation.R"),  local = TRUE)$value
  source(file.path("server", "univariate.R"),  local = TRUE)$value
  source(file.path("server", "multivariate.R"),  local = TRUE)$value
  source(file.path("server", "hierarchical.R"),  local = TRUE)$value
  source(file.path("server", "nonpar.R"),  local = TRUE)$value
  source(file.path("server", "BMAGLM.R"),  local = TRUE)$value
  source(file.path("server", "help.R"),  local = TRUE)$value
  }

shinyApp(ui = ui, server = server)
