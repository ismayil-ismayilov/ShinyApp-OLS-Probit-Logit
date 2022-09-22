
formula_model <- function(outcome, variables ){
  f <- as.formula(
    paste(outcome, 
          paste(variables, collapse = " + "), 
          sep = " ~ "))
  return(f)
  
}


draw_plot_1 <- function(data_input, num_var_1, num_var_2, fact_var){
  # browser()
  
  
  
  if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
      geom_point( alpha = 0.6 ) + theme_minimal()
  }
  else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1, y = num_var_2)) +
      geom_point( alpha = 0.6)+ theme_minimal()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_1)) +
      geom_violin()+ theme_minimal()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var, y = num_var_2)) +
      geom_violin()+ theme_minimal()
  }
  else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_1)) +
      geom_histogram()+ theme_minimal()
  }
  else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
    ggplot(data = data_input,
           aes_string(x = num_var_2)) +
      geom_histogram()+ theme_minimal()
  }
  else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
    ggplot(data = data_input,
           aes_string(x = fact_var)) +
      geom_bar()+ theme_minimal()
  }
}

validateInputData <- function(dataSet){
  
  
  
  
}

linktest = function(model) {
  # written by dr Rafal Wozniak, Faculty of Economic Sciences, University of Warsaw
  # 2019-04-18
  #
  # arguments:
  # ------------------
  # model - model estimated by glm function
  
  # check if it is of class 'glm'
  
  # Linktest
  y = model$y
  yhat = log(model$fitted.values/(1-model$fitted.values))
  yhat2 = yhat^2
  # auxiliary regression
  aux.reg = glm(y~yhat+yhat2, family=binomial(link=model$family$link))
  show(summary(aux.reg))
  return(aux.reg)
}