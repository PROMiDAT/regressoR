load_globals <- function(envir = .GlobalEnv){
  source("R/GlobalEnv/options_regressor.R", local = envir, echo = FALSE )
  #source("R/GlobalEnv/Utilities.R", local = envir, echo = FALSE )
  #source("R/GlobalEnv/data_manipulation.R", local = envir, echo = FALSE )
  #source("R/GlobalEnv/language.R", local = envir, echo = FALSE )
  #source("R/GlobalEnv/plots_manipulation.R", local = envir, echo = FALSE )
  #source("R/GlobalEnv/report_manipulation.R", local = envir, echo = FALSE )
  #source("R/GlobalEnv/utils_inputs.R", local = envir, echo = FALSE )
  #source("R/GlobalEnv/shiny_tools.R", local = envir, echo = FALSE )
  #source("R/GlobalEnv/code_generate.R", local = envir, echo = FALSE )
  #source("R/GlobalEnv/string_manipulation.R", local = envir, echo = FALSE )
}

load_global_variables <- function(envir = .GlobalEnv){
  source("R/GlobalEnv/Global_Variables.R", local = envir, echo = FALSE )
}