#Colour cell value by Cqvalue to indicate if species is present or absent
what_clr <- function(value) {
  if (value >= input$cqValueCutoff)
  {return ("#8FBACB")}

  else
  {return("#ffb14e")}
}


available_threshold <- function(value) {
  if (value == "Unable to Determine Threshold")
  {return ("#ffd700")}
}
