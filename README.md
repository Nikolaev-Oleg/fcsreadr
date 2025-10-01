# fcsreadr
R code for fcs file processing

Four functions are currently available:
1) get_fcs_metadata() consumes a .fcs file and returns a list with all keywords. Note that numeric keqwords are 0-indexed in .fcs files and get_fcs_metadata() to 1-indexed values. Funtion was tested on Biosino MSLFCY01 cytometer files.
2) read_fcs() consumes a .fcs file and returns a dataframe with parameters in columns and events in raws. In parameter names white spaces are removed and "-" are replaced with "_" in order to make them more processable. Funtion was tested on Biosino MSLFCY01 cytometer files. What it cannot do: currently doesn't read files with $DATATYPE other than "F" and $MODE other than "L". Does not process Analisys segment, does not support multiple Data segments.
3) pseudocolour() consumes two vectors **x** and **y** of the same length (i.e two fcs parameters) and a hyperparameter **r**. Calculates 2d density for each point in **y~x** space as a number of events within a sircle of radius **r** from it, returns a vector of the same length as **x** and **y**. Works very slow.
4) gui_gate_editor() consumes dataframe produced by read_fcs() and opens a Shiny app for manual gate editing. Gates are drawn as polygons and are written as dataframe containing **x** and **y** cordinates of vertices, **gate** name and names of x and y cordinates (**x_par** and **y_par** respectively)

Also, there is a std_gradient variable, producing a FlowJo-like colour scheme when plotting points coloured by pseudocolour() output

Requirements:
1) RStudio
2) R >= 4.2.2
3) packages: BMS, tidyverse, glue, shiny
