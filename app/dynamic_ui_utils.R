

# Create an Input object based on the discretization used for the parameter
# slider: evenly spaced
# drop-down: otherwise
parameter_input <- function(x, inputId, label){
  if (is.factor(x)) {
    x = as.numeric(levels(x))[x]
  }
  
  x = sort(x)
  dx = diff(x)
  selected_value = x[round(length(x)/2 + 0.5)]
  if (length(x) > 2 & all(abs(dx - dx[1]) < 1e-6)){
    s = sliderInput(
      inputId=inputId, label=label, min=min(x), max=max(x),
      step=dx[1], value=selected_value
    )
  } else {
    s = selectInput(inputId=inputId, label=label, choices=x, selected=selected_value)
  }
}