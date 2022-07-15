library(rsvg)
library(readr)
library(dplyr)

read_file('Rsvg/svg.txt')%>%
  charToRaw() %>%
  #rsvg_png('Rsvg/svg.png')
  rsvg_eps('Rsvg/svg.ps')
