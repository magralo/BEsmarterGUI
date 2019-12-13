dep=c("rsconnect" ,"shiny","mlogit" ,"MASS" ,"AER" ,         
      "mcmc","bayesm","MCMCpack",
      "abind","bayesboot","bibtex" ,      
      "BMA","car","carData","cellranger" ,"clipr",        
      "coda","DT","Formula","ivbma",    
      "Matrix" ,"matrixcalc","RcppEigen",    
      "Rdpack","rhandsontable" ,"statmod", "truncnorm"  ,  
      "stringr")

needed=dep[!dep%in%installed.packages()]

if (length(needed)>0){
  for (p in needed){
    if (p=="ivbma"){
      install.packages("ivbma_1.05.tar.gz", repos = NULL, type = "source")
    }else if (p=="MCMCpack"){
      install.packages("MCMCpack_1.4-4.tar.gz", repos = NULL, type = "source")
    }else{
      install.packages(p)
    }
  }
}



library(shiny)
options(shiny.maxRequestSize=100*1024^2)
image<- img(src="logo.png", height = 200, width = "90%") #Local variable


###############################################
################### UI ########################


## Load General conditions, packages and other stuff necesary for run the app UI
source(file.path("R/genUI.R"),  local = TRUE)$value


ui <- navbarPage(a(em(strong("BEsmarter",style = "color:light blue")),href = "http://www.besmarter-team.org/"),

  # source(file.path("R/ui", "presentation.R"),  local = TRUE)$value,
  # source(file.path("R/ui", "univariate.R"),  local = TRUE)$value,
  # source(file.path("R/ui", "multivariate.R"),  local = TRUE)$value,
  # source(file.path("R/ui", "hierarchical.R"),  local = TRUE)$value,
  # source(file.path("R/ui", "nonpar.R"),  local = TRUE)$value,
  # source(file.path("R/ui", "BMAGLM.R"),  local = TRUE)$value,
  # source(file.path("R/ui", "help.R"),  local = TRUE)$value
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
source(file.path("R/genServer.R"),  local = TRUE)$value


server <- function(input, output, session) {
# Include the logic (server) for each tab
  # source(file.path("R/server", "presentation.R"),  local = TRUE)$value
  # source(file.path("R/server", "univariate.R"),  local = TRUE)$value
  # source(file.path("R/server", "multivariate.R"),  local = TRUE)$value
  # source(file.path("R/server", "hierarchical.R"),  local = TRUE)$value
  # source(file.path("R/server", "nonpar.R"),  local = TRUE)$value
  # source(file.path("R/server", "BMAGLM.R"),  local = TRUE)$value
  # source(file.path("R/server", "help.R"),  local = TRUE)$value
  source(file.path("server", "presentation.R"),  local = TRUE)$value
  source(file.path("server", "univariate.R"),  local = TRUE)$value
  source(file.path("server", "multivariate.R"),  local = TRUE)$value
  source(file.path("server", "hierarchical.R"),  local = TRUE)$value
  source(file.path("server", "nonpar.R"),  local = TRUE)$value
  source(file.path("server", "BMAGLM.R"),  local = TRUE)$value
  source(file.path("server", "help.R"),  local = TRUE)$value

  }

shinyApp(ui = ui, server = server)
