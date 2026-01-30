# fcsreadr
R tool for fcs file processing

Currently available functions:

**get_fcs_metadata(file)** consumes an .fcs file and returns a list with all keywords and corresponding values. Note that get_fcs_metadata(file) transforms 0-indexed values to 1-indexed values. Function was tested on FCS3.0 and FCS3.1 files from Biosino MSLFCY01 and FACSCantoII machines.

**read_fcs(file)** consumes an .fcs file and returns a dataframe with parameters in columns and events in raws. In parameter names white spaces are removed and "-" are replaced with "_" to make them more processable. Funtion was tested on Biosino MSLFCY01 and FACSCantoII files. The function currently doesn't read files with $DATATYPE other than "F" and $MODE other than "L", does not process Analisys segment and does not support multiple Data segments.

**pseudocolour(x, y, r)** consumes two vectors **x** and **y** of the same length (i.e two fcs parameters) and a hyperparameter **r**. Calculates 2d density for each point in **y~x** space as a number of events within a sircle of radius **r** from it. Returns a vector of the same length as **x** and **y**. Currently works slow.

**gui_gate_editor(df, scale = 'linear')** consumes a dataframe, e.g. produced by read_fcs(), and opens a Shiny app for manual gate editing. Gates are drawn as polygons. Gate editing is currently available in linear, log and biexponential scales (**scale** = 'linear', **scale** = 'log' and **scale** = 'biex' respectively; **x** and **y** are in the same type of scale). Returns a dataframe containing **x** and **y** cordinates of vertices, **gate** name and names of x and y cordinates (**x_par** and **y_par** respectively).

**gate_mask(df, gates)** consumes a dataframe **df** and a dataframe **gates** produced by gui_gate_editor() and returns a named vector (for single gate) of length n = nrow(df) or a named matrix (for multiple gates) with n = nrow(df) with masks: 1 for observation within the gate, 0 otherwise.

**comp.matrix(data, channels, singlestains, nostain = NULL, method = 'constant')** consumes a dataframe **data**, vector of channels to compensate **channels**, a list of dataframes with single-stain controls **singlestains** and an optional dataframe with no-stain control **nostain** (required if method = 'nostain'). Provides three compensation methods: 'constant' (default, calculates compensation matrix based on simple linear regression), 'zero' (assumes true flourescence value for unstained control to be exactly 0) and 'nostain' (considers SSC and FCS values from no-stain control). Note that channel names in **channels** must be 1) present in **data**, **singlestains** and **nostain**, and 2) be in the same order as single-stain controls in **singlestains**. Returns a compensation matrix for selected channels.

**compensate(data, compensation_matrix, channels = NULL)** consumes a dataframe **data**, a named matrix **compensation_matrix** (e.g. produced by comp_matrix()) and an optional vector of channels to take into account **channels**. Returns a dataframe of the same dimentions as **df**, but with conpensated values.

**decompensate(data, metadata)** consumes a dataframe **data** and a **metadata** list produced by get_fcs_metadata() and performs decompensation. Returns a decompensated dataframe. Recommend to use if compensation matrix is non-singular (if so, you get the warning while executing get_fcs_metadata() or read_fcs()).

**geom_pseudocolour(aes(r = 0.1), grad = grad = colorRampPalette(std_gradient)(1000))** is a ggplot geom, producing a pseudocolour plot. 0 < **r** < 1 defines a radius for pseudodensity estimation, the greater is **r**, the more global (and slower) is the estimation. **grad** defines a colour scheme for the gradient (std_gradient is default).

Also, there is a **std_gradient** variable, which produces a FlowJo-like colour scheme for pseudocolour plots

Requirements:
1) RStudio
2) R >= 4.2.2
3) packages: BMS, tidyverse, glue, shiny

Test code is provided in test.R
