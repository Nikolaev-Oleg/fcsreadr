#Packages####
library(BMS)
library(tidyverse)
library(glue)
library(shiny)
library(tigers)
library(scales)

#Functions####
get_fcs_metadata<-function(file){
  file<-read_file(file = file)
  
  #|||||||||||||||||||||||| HEADER |||||||||||||||||||||||||||||||||||||||||||||||
  
  fcs_version<-str_sub(file, 1, 6) # NB: indexing from 1
  
  begin_text_from_header = str_sub(file, 11, 18) %>%
    str_remove_all(' ') %>%
    as.numeric()
  if(begin_text_from_header != 0) begin_text_from_header <- begin_text_from_header+1
  
  end_text_from_header = str_sub(file, 19, 26) %>%
    str_remove_all(' ') %>%
    as.numeric()
  if(end_text_from_header != 0) end_text_from_header <- end_text_from_header+1
  
  begin_data_from_header = str_sub(file, 27, 34) %>%
    str_remove_all(' ') %>%
    as.numeric()
  if(begin_data_from_header != 0) begin_data_from_header <- begin_data_from_header+1
  
  end_data_from_header = str_sub(file, 35, 42) %>%
    str_remove_all(' ') %>%
    as.numeric()
  if(end_data_from_header != 0) end_data_from_header <- end_data_from_header+1
  
  begin_analysis_from_header = str_sub(file, 43, 50) %>%
    str_remove_all(' ') %>%
    as.numeric()
  if(begin_analysis_from_header != 0) begin_analysis_from_header <- begin_analysis_from_header+1
  
  end_analysis_from_header = str_sub(file, 51, 58) %>%
    str_remove_all(' ') %>%
    as.numeric()
  if(end_analysis_from_header != 0) end_analysis_from_header <- end_analysis_from_header+1
  
  #||||||||||||||||||||||| TEXT ||||||||||||||||||||||||||||||||||||||||||||||||||
  
  delimiter<-str_extract(file, regex("\\$beginanalysis.", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$beginanalysis", ignore_case = T))
  if(delimiter != '\f'){
    file<-str_replace_all(file, glue('(?<!{delimiter}){delimiter}(?!{delimiter})'), '\f')
  }
  
  beginanalysis<-str_extract(file, regex("\\$beginanalysis(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$beginanalysis", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  if(beginanalysis != 0) beginanalysis <- beginanalysis+1
  
  endanalysis<-str_extract(file, regex("\\$endanalysis(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$endanalysis", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  if(endanalysis != 0) endanalysis <- endanalysis+1
  
  beginstext<-str_extract(file, regex("\\$beginstext(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$beginstext", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  if(beginstext != 0) beginstext <- beginstext+1
  
  endstext<-str_extract(file, regex("\\$beginstext(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$beginstext", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  if(endstext != 0) endstext <- endstext+1
  
  begindata<-str_extract(file, regex("\\$begindata(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$begindata", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  if(begindata != 0) begindata <- begindata+1
  
  enddata<-str_extract(file, regex("\\$enddata(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$enddata", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  if(enddata != 0) enddata <- enddata+1
  
  byteord<-str_extract(file, regex("\\$byteord(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$byteord", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$')
  if(byteord == '4,3,2,1'){
    endian <- 'big'} else if(byteord == '1,2,3,4'){
      endian <- 'little'} else {
        endian = 'swap'
        warning("Unknown byteord. Use endian = 'swap'")
      }
  
  datatype<-str_extract(file, regex("\\$datatype(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$datatype", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$')
  if(datatype != 'F') warning("Data types other than F are not supported yet")
  
  mode<-str_extract(file, regex("\\$mode(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$mode", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$')
  if(mode != 'L') warning("Storage modes other than L are deprecated in FCS3.1")
  
  nextdata<-str_extract(file, regex("\\$nextdata(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$nextdata", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  if(nextdata != 0) nextdata <- nextdata+1
  
  proj<-str_extract(file, regex("\\$proj(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$proj", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$')
  
  fil<-str_extract(file, regex("\\$fil(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$fil", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$')
  
  tot<-str_extract(file, regex("\\$tot(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$tot", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  
  par<-str_extract(file, regex("\\$par(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    str_remove(regex("\\$par", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  
  if(fcs_version == 'FCS3.1'){
    spillover<-str_extract(file, regex("\\$spillover(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
      str_remove(regex("\\$spillover", ignore_case = T)) %>% 
      str_remove_all('\\f') %>% 
      str_remove_all('\\$') %>%
      str_split(',') %>%
      unlist()
  } else if (str_detect(file, regex("(?<=\\f)spil(.*?)\\f(.+)\\f", ignore_case = T))){ # for some reason, some manufacturers (e.g. FACSCantoII) ignore optional keyword $COMP, but use uncolventional SPILL
    spillover<-str_extract(file, regex("(?<=\\f)spil(.*?)\\f(.+)\\f", ignore_case = T)) %>%
      str_remove(regex("spil(.*?)\\f", ignore_case = T)) %>% 
      str_remove_all('\\f') %>% 
      str_split(',') %>%
      unlist()
  } else if(str_detect(file, regex("\\$comp(.+?)\\f", ignore_case = T, dotall = T))){
    spillover<-str_extract(file, regex("\\$comp(.+?)\\f", ignore_case = T)) %>%
      str_remove(regex("spil(.*?)\\f", ignore_case = T)) %>% 
      str_remove_all('\\f') %>% 
      str_split(',') %>%
      unlist()
  } else{
    spillover <- NA
  }
  if(length(spillover)>1){
    n<-as.numeric(spillover[1])
    spillover<-spillover[-1]
    
    channels <- spillover[1:n]
    spillover <- as.numeric(spillover[-c(1:n)])
    
    spillover<-matrix(spillover, nrow = n, ncol = n, byrow = T)
    colnames(spillover) <- channels
    rownames(spillover) <- channels
    
    singular<-1
    for(i in 1:n){
      for(j in 1:n){
        singularity_check <- (i == j & spillover[i,j] == 1)|(i != j & spillover[i,j] == 0)
        singular<-singular*singularity_check
        if(!singular){
          break
        }
      }
      if(!singular){
        warning("Compensation matrix is not singular, try decompensate()")
        break
      }
    }
    if(singular)print('Compensation matrix checked')
  } else{
    warning('Cannot obtain compensation matrix')
  }
  
  pnn<-str_extract_all(file, regex("\\$p[:digit:]+n(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_remove(regex("\\$p[:digit:]+n", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$')
  pnn_channels<-str_extract_all(file, regex("\\$p[:digit:]+n(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_extract(regex("(?<=\\$p)[:digit:]+(?=n)", ignore_case = T)) %>% 
    as.numeric()
  pnn<-tibble(n = pnn_channels,
              PnN = pnn)
  
  pns<-str_extract_all(file, regex("\\$p[:digit:]+s(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_remove(regex("\\$p[:digit:]+s", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$')
  pns_channels<-str_extract_all(file, regex("\\$p[:digit:]+s(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_extract(regex("(?<=\\$p)[:digit:]+(?=s)", ignore_case = T)) %>% 
    as.numeric()
  pns<-tibble(n = pns_channels,
              PnS = pns)
  
  pnd<-str_extract_all(file, regex("\\$p[:digit:]+d(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_remove(regex("\\$p[:digit:]+d", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$')
  pnd_channels<-str_extract_all(file, regex("\\$p[:digit:]+d(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_extract(regex("(?<=\\$p)[:digit:]+(?=d)", ignore_case = T)) %>% 
    as.numeric()
  pnd<-tibble(n = pnd_channels,
              PnD = pnd)
  
  png<-str_extract_all(file, regex("\\$p[:digit:]+g(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_remove(regex("\\$p[:digit:]+g", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  png_channels<-str_extract_all(file, regex("\\$p[:digit:]+g(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_extract(regex("(?<=\\$p)[:digit:]+(?=g)", ignore_case = T)) %>% 
    as.numeric()
  png<-tibble(n = png_channels,
              PnG = png)
  
  pnr<-str_extract_all(file, regex("\\$p[:digit:]+r(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_remove(regex("\\$p[:digit:]+r", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  pnr_channels<-str_extract_all(file, regex("\\$p[:digit:]+r(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_extract(regex("(?<=\\$p)[:digit:]+(?=r)", ignore_case = T)) %>% 
    as.numeric()
  pnr<-tibble(n = pnr_channels,
              PnR = pnr)
  
  pnb<-str_extract_all(file, regex("\\$p[:digit:]+b(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_remove(regex("\\$p[:digit:]+b", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$') %>%
    as.numeric()
  pnb_channels<-str_extract_all(file, regex("\\$p[:digit:]+b(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_extract(regex("(?<=\\$p)[:digit:]+(?=b)", ignore_case = T)) %>% 
    as.numeric()
  pnb<-tibble(n = pnb_channels,
              PnB = pnb)
  
  pne<-str_extract_all(file, regex("\\$p[:digit:]+e(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_remove(regex("\\$p[:digit:]+e", ignore_case = T)) %>% 
    str_remove_all('\\f') %>% 
    str_remove_all('\\$')
  pne_channels<-str_extract_all(file, regex("\\$p[:digit:]+e(.+?)\\f", ignore_case = T, dotall = TRUE)) %>%
    unlist() %>%
    str_extract(regex("(?<=\\$p)[:digit:]+(?=e)", ignore_case = T)) %>% 
    as.numeric()
  pne<-tibble(n = pne_channels,
              PnE = pne)
  
  channels_data<-tibble(n = 1:par) %>%
    full_join(pnn) %>%
    full_join(pns) %>%
    full_join(pnd) %>%
    full_join(png) %>%
    full_join(pnr) %>%
    full_join(pnb) %>%
    full_join(pne)
  
  metadata <- list(begin_analysis_from_header = begin_analysis_from_header,
                   end_analysis_from_header = end_analysis_from_header,
                   begin_text_from_header = begin_text_from_header,
                   end_text_from_header = end_text_from_header,
                   begin_data_from_header = begin_data_from_header,
                   end_data_from_header = end_data_from_header,
                   beginanalysis = beginanalysis,
                   endanalysis = endanalysis,
                   beginstext = beginstext,
                   endstext = endstext,
                   begindata = begindata,
                   enddata = enddata,
                   byteord = byteord,
                   endian = endian,
                   datatype = datatype,
                   mode = mode,
                   nextdata = nextdata,
                   proj = proj,
                   fil = fil,
                   tot = tot,
                   par = par, 
                   spillover = spillover,
                   channels_data = channels_data)
  
  print(glue('File {fil} processed.
           Project: {proj}.
           Total number of events: {tot}'))
  
  return(metadata)
}
read_fcs<-function(file){
  metadata<-get_fcs_metadata(file)
  if(length(unique(metadata$channels_data$PnB)) == 1){
    intesity<-c()
    n <- max(metadata$enddata, metadata$end_data_from_header)
    size <- metadata$channels_data$PnB[1]/8
    bytes_remove <- max(metadata$begindata, metadata$begin_data_from_header) - 1
    
    binary_data<-readBin(con = file,
                         what = 'raw',
                         n = n,
                         endian = metadata$endian)[-c(1:bytes_remove)]
    
    intensity<-readBin(binary_data, 
                       what = 'numeric',
                       n = length(binary_data),
                       size = size,
                       endian = metadata$endian)
    df <- matrix(intensity, ncol = metadata$par, byrow = T)
  } else{
    warning('PnB not equal for all parameters. Processing will take much longer than usual')
    bytes_per_event = sum(metadata$channels_data$PnB)
    intensity <- c()
    for(i in 1:metadata$tot){
      
      n <- max(metadata$begindata, metadata$begin_data_from_header) +
        i*bytes_per_event - 1
      
      bytes_remove <- max(metadata$begindata, metadata$begin_data_from_header) +
        (i-1)*bytes_per_event  - 1
      
      binary_data<-readBin(con = file,
                           what = 'raw',
                           n = n,
                           endian = metadata$endian)[-c(1:bytes_remove)]
      
      for(j in metadata$channels_data$n){
        temp <- binary_data[1:(metadata$channels_data$PnB[j]/8)]
        value <- readBin(temp, 
                         what = 'numeric',
                         n = length(temp),
                         size = length(temp),
                         endian = metadata$endian)
        intensity<-append(intensity, value)
        binary_data<-binary_data[-c(1:(metadata$channels_data$PnB[j]/8))]
      }
    }
    df<-matrix(intensity, ncol = metadata$par, byrow = T)
    df<-na.omit(df)
  }
  if(sum(is.na(metadata$channels_data$PnS)) == 0 & length(unique(metadata$channels_data$PnS)) == length(metadata$channels_data$PnS)){
    colnames(df)<-c(metadata$channels_data$PnS) %>%
      str_remove_all(' ') %>%
      str_replace_all('-', '_')
  } else{
    colnames(df)<-c(metadata$channels_data$PnN) %>%
      str_remove_all(' ') %>%
      str_replace_all('-', '_')
    warning('Not enough unique long names, use short parameter names')
  }
  df<-as.data.frame(df)
  
  return(df)
}
pseudocolour<-function(x, y, r){
  if(length(x)!=length(y))
    stop('length(x) must be equal to length(y)')
  df<-data.frame(x=x, y=y)
  density<-rep(NA, length(x))
  for(i in 1:length(x)){
    x.center<-x[i]
    y.center<-y[i]
    neighbours<-subset(df, (x-x.center)^2+(y-y.center)^2<r^2)
    n.neighbours<-nrow(neighbours)
    density[i]<-n.neighbours
  }
  return(density)
}
gui_gate_editor.biex<-function(df){
  global_points<-NULL
  
  trans_fun <- function(x, a, b = 1, c, f = 1, w = 1)a*exp(b*(x-w))-c*exp(-b*(x-w))+f
  
  ui <- fluidPage(
    titlePanel("Gate editor"),
    sidebarLayout(
      sidebarPanel(
        selectInput("x_var", "x variable:", choices = names(df)),
        selectInput("y_var", "y variable:", choices = names(df)),
        numericInput("alpha", "alpha:", min = 0, max = 1, value = 0.3, step = 0.001),
        #numericInput("xmin", "xmin", min = -1e7, max = 1e6, value = 0, step = 0.1),
        #numericInput("xmax", "xmax", min = -1e6, max = 1e7, value = 1e5, step = 0.1),
        #numericInput("ymin", "ymin", min = -1e7, max = 1e6, value = 0, step = 0.1),
        #numericInput("ymax", "ymax", min = -1e6, max = 1e7, value = 1e5, step = 0.1),
        numericInput("a.x", "a.x", min = -1e5, max = 1e5, value = 0.5, step = 0.1),
        numericInput("a.y", "a.y", min = -1e5, max = 1e5, value = 0.5, step = 0.1),
        numericInput("c.x", "c.x", min = -1e5, max = 1e5, value = 0.5, step = 0.1),
        numericInput("c.y", "c.y", min = -1e5, max = 1e5, value = 0.5, step = 0.1),
        numericInput("f.x", "f.x", min = -1e5, max = 1e5, value = 0, step = 0.1),
        numericInput("f.y", "f.y", min = -1e5, max = 1e5, value = 0, step = 0.1)
      ),
      mainPanel(
        plotOutput("plot", click = "plot_click")
      )
    ),
    actionButton("reset", "Reset Points"),
    actionButton("completegate", "Complete gate"),
    actionButton("ending","Done")
  )
  
  server <- function(input, output, session) {
    # Reactive value to store points
    points <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
    
    # Observe plot clicks
    observeEvent(input$plot_click, {
      new_point <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
      points(rbind(points(), new_point))
    })
    
    # Reset points when reset button is clicked
    observeEvent(input$reset, {
      points(data.frame(x = numeric(0), y = numeric(0)))
    })
    
    # Finish current gate
    observeEvent(input$completegate, {
      new_point <- points()[1,]
      points(rbind(points(), new_point))
      
      showModal(modalDialog(
        title = "Please enter some text",
        textInput("user_input", "Tap and type here:"),
        footer = tagList(
          actionButton("cancel", "Cancel"),
          actionButton("submit", "Submit")
        )
      ))
    })
    
    #Submit gate name
    observeEvent(input$submit, {
      points(mutate(points(),
                    gate = input$user_input,
                    par_x = input$x_var,
                    par_y = input$y_var,
                    a.x = input$a.x,
                    c.x = input$c.x,
                    f.x = input$f.x,
                    a.y = input$a.y,
                    c.y = input$c.y,
                    f.y = input$f.y))
      global_points<<-rbind(global_points, points())
      points(data.frame(x = numeric(0), y = numeric(0)))
      removeModal()
    })
    
    #Cancel submission if required
    observeEvent(input$cancel, {
      points(points()[1:(nrow(points())-1), ])
      removeModal()
    })
    
    #Close the app
    observeEvent(input$ending, {
      stopApp()
    })
    
    output$plot <- renderPlot({
      pts <- points() 
      pts$x <- trans_fun(pts$x, a = input$a.x,
                         c = input$c.x,
                         f = input$f.x)
      pts$y = trans_fun(pts$y, a = input$a.y,
                        c = input$c.y,
                        f = input$f.y)
      gg <- ggplot(df, aes_string(input$x_var, input$y_var))+
        geom_point(colour = '#2288dd', alpha = input$alpha, size = 1.5)+
        scale_x_continuous(transform = transform_biex(a = input$a.x,
                                                      c = input$c.x,
                                                      f = input$f.x))+
        scale_y_continuous(transform = transform_biex(a = input$a.y,
                                                      c = input$c.y,
                                                      f = input$f.y))
      #xlim(c(input$xmin, input$xmax))+
      #ylim(c(input$ymin, input$ymax))
      
      if(nrow(as.data.frame(global_points))>0){
        glob_pts <- subset(global_points, par_x == input$x_var & par_y == input$y_var) %>%
          rowwise() %>%
          mutate(x = trans_fun(x, a = a.x, c = c.x, f = f.x),
                 y = trans_fun(y, a = a.y, c = c.y, f = f.y))
        #glob_pts$x <- trans_fun(glob_pts$x, a = input$a.x,
        #                  c = input$c.x,
        #                 f = input$f.x)
        #glob_pts$y = trans_fun(glob_pts$y, a = input$a.y,
        #                 c = input$c.y,
        #                f = input$f.y)
        
        gg <- gg + geom_path(data = glob_pts,
                             aes(x = x, y = y, color = gate))
      }
      
      if (nrow(pts) > 0) {
        gg <- gg + geom_point(data = pts, aes(x = x, y = y), color = "black")
        
        if(nrow(pts > 1)) {
          gg <- gg+
            geom_point(data = pts, aes(x = x, y = y), color = "black")+
            geom_path(data = pts, aes(x = x, y = y), color = "black")
        }
      }
      gg
    })
  }
  
  app<-shinyApp(ui, server)
  runApp(app)
  global_points<-global_points %>%
    rowwise() %>%
    mutate(x = trans_fun(x, a = a.x, c = c.x, f = f.x),
           y = trans_fun(y, a = a.y, c = c.y, f = f.y)) %>%
    ungroup() %>%
    mutate(a.x = NULL,
           a.y = NULL,
           c.x = NULL,
           c.y = NULL,
           f.x = NULL,
           f.y = NULL)
  return(global_points)
}
gui_gate_editor.linear<-function(df){
  global_points<-NULL
  
  ui <- fluidPage(
    titlePanel("Gate editor"),
    sidebarLayout(
      sidebarPanel(
        selectInput("x_var", "x variable:", choices = names(df)),
        selectInput("y_var", "y variable:", choices = names(df)),
        numericInput("alpha", "alpha:", min = 0, max = 1, value = 0.3, step = 0.001),
        numericInput("xmin", "xmin", min = -1e7, max = 1e6, value = 0, step = 0.1),
        numericInput("xmax", "xmax", min = -1e6, max = 1e7, value = 1e5, step = 0.1),
        numericInput("ymin", "ymin", min = -1e7, max = 1e6, value = 0, step = 0.1),
        numericInput("ymax", "ymax", min = -1e6, max = 1e7, value = 1e5, step = 0.1)
      ),
      mainPanel(
        plotOutput("plot", click = "plot_click")
      )
    ),
    actionButton("reset", "Reset Points"),
    actionButton("completegate", "Complete gate"),
    actionButton("ending","Done")
  )
  
  server <- function(input, output, session) {
    # Reactive value to store points
    points <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
    
    # Observe plot clicks
    observeEvent(input$plot_click, {
      new_point <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
      points(rbind(points(), new_point))
    })
    
    # Reset points when reset button is clicked
    observeEvent(input$reset, {
      points(data.frame(x = numeric(0), y = numeric(0)))
    })
    
    # Finish current gate
    observeEvent(input$completegate, {
      new_point <- points()[1,]
      points(rbind(points(), new_point))
      
      showModal(modalDialog(
        title = "Please enter some text",
        textInput("user_input", "Tap and type here:"),
        footer = tagList(
          actionButton("cancel", "Cancel"),
          actionButton("submit", "Submit")
        )
      ))
    })
    
    #Submit gate name
    observeEvent(input$submit, {
      points(mutate(points(), gate = input$user_input,
                    par_x = input$x_var,
                    par_y = input$y_var))
      global_points<<-rbind(global_points, points())
      points(data.frame(x = numeric(0), y = numeric(0)))
      removeModal()
    })
    
    #Cancel submission if required
    observeEvent(input$cancel, {
      points(points()[1:(nrow(points())-1), ])
      removeModal()
    })
    
    #Close the app
    observeEvent(input$ending, {
      stopApp()
    })
    
    output$plot <- renderPlot({
      pts <- points()
      gg <- ggplot(df, aes_string(input$x_var, input$y_var))+
        geom_point(colour = '#2288dd', alpha = input$alpha, size = 1.5)+
        xlim(c(input$xmin, input$xmax))+
        ylim(c(input$ymin, input$ymax))
      
      if(nrow(as.data.frame(global_points))>0){
        gg <- gg + geom_path(data = subset(global_points, par_x == input$x_var & par_y == input$y_var),
                             aes(x = x, y = y, color = gate))
      }
      
      if (nrow(pts) > 0) {
        gg <- gg + geom_point(data = pts, aes(x = x, y = y), color = "black")
        
        if(nrow(pts > 1)) {
          gg <- gg+
            geom_point(data = pts, aes(x = x, y = y), color = "black")+
            geom_path(data = pts, aes(x = x, y = y), color = "black")
        }
      }
      gg
    })
  }
  
  app<-shinyApp(ui, server)
  runApp(app)
  return(global_points)
}
gui_gate_editor.log<-function(df){
  global_points<-NULL
  trans_fun <- function(x, base) base^x
  
  ui <- fluidPage(
    titlePanel("Gate editor"),
    sidebarLayout(
      sidebarPanel(
        selectInput("x_var", "x variable:", choices = names(df)),
        selectInput("y_var", "y variable:", choices = names(df)),
        numericInput("alpha", "alpha:", min = 0, max = 1, value = 0.3, step = 0.001),
        #numericInput("xmin", "xmin", min = -1e7, max = 1e6, value = 0, step = 0.1),
        #numericInput("xmax", "xmax", min = -1e6, max = 1e7, value = 1e5, step = 0.1),
        #numericInput("ymin", "ymin", min = -1e7, max = 1e6, value = 0, step = 0.1),
        #numericInput("ymax", "ymax", min = -1e6, max = 1e7, value = 1e5, step = 0.1),
        numericInput("base", "base", min = -1e3, max = 1e3, value = 10, step = 0.1)
      ),
      mainPanel(
        plotOutput("plot", click = "plot_click")
      )
    ),
    actionButton("reset", "Reset Points"),
    actionButton("completegate", "Complete gate"),
    actionButton("ending","Done")
  )
  
  server <- function(input, output, session) {
    # Reactive value to store points
    points <- reactiveVal(data.frame(x = numeric(0), y = numeric(0)))
    
    # Observe plot clicks
    observeEvent(input$plot_click, {
      new_point <- data.frame(x = input$plot_click$x, y = input$plot_click$y)
      points(rbind(points(), new_point))
    })
    
    # Reset points when reset button is clicked
    observeEvent(input$reset, {
      points(data.frame(x = numeric(0), y = numeric(0)))
    })
    
    # Finish current gate
    observeEvent(input$completegate, {
      new_point <- points()[1,]
      points(rbind(points(), new_point))
      
      showModal(modalDialog(
        title = "Please enter some text",
        textInput("user_input", "Tap and type here:"),
        footer = tagList(
          actionButton("cancel", "Cancel"),
          actionButton("submit", "Submit")
        )
      ))
    })
    
    #Submit gate name
    observeEvent(input$submit, {
      points(mutate(points(), gate = input$user_input,
                    par_x = input$x_var,
                    par_y = input$y_var,
                    base = input$base))
      global_points<<-rbind(global_points, points())
      points(data.frame(x = numeric(0), y = numeric(0)))
      removeModal()
    })
    
    #Cancel submission if required
    observeEvent(input$cancel, {
      points(points()[1:(nrow(points())-1), ])
      removeModal()
    })
    
    #Close the app
    observeEvent(input$ending, {
      stopApp()
    })
    
    output$plot <- renderPlot({
      pts <- points()
      pts$x <- trans_fun(pts$x, base = input$base)
      pts$y = trans_fun(pts$y, base = input$base)
      
      gg <- ggplot(df, aes_string(input$x_var, input$y_var))+
        geom_point(colour = '#2288dd', alpha = input$alpha, size = 1.5)+
        scale_x_continuous(transform = transform_anylog(base = input$base))+
        scale_y_continuous(transform = transform_anylog(base = input$base))
      #xlim(c(input$xmin, input$xmax))+
      #ylim(c(input$ymin, input$ymax))
      
      if(nrow(as.data.frame(global_points))>0){
        glob_pts <- subset(global_points, par_x == input$x_var & par_y == input$y_var) %>%
          rowwise() %>%
          mutate(x = trans_fun(x, base = input$base),
                 y = trans_fun(y, base = input$base))
        gg <- gg + geom_path(data = glob_pts,
                             aes(x = x, y = y, color = gate))
      }
      
      if (nrow(pts) > 0) {
        gg <- gg + geom_point(data = pts, aes(x = x, y = y), color = "black")
        
        if(nrow(pts > 1)) {
          gg <- gg+
            geom_point(data = pts, aes(x = x, y = y), color = "black")+
            geom_path(data = pts, aes(x = x, y = y), color = "black")
        }
      }
      gg
    })
  }
  
  app<-shinyApp(ui, server)
  runApp(app)
  global_points <- global_points %>%
    rowwise() %>%
    mutate(x = trans_fun(x, base = base),
           y = trans_fun(y, base = base),
           base = NULL)
  return(global_points)
}
gui_gate_editor<-function(df, scale = 'linear'){
  scale <- scale
  if(scale == 'linear') global_points<-gui_gate_editor.linear(df)
  if(scale == 'biex') global_points<-gui_gate_editor.biex(df)
  if(scale == 'log') global_points<-gui_gate_editor.log(df)
  if(!scale %in% c('linear', 'biex', 'log'))errorCondition('Unknown scale type')
  if(scale %in% c('linear', 'biex', 'log'))return(global_points)
}

gate_mask<-function(df, gates){
  gates<-as.data.frame(gates)
  gate_mask<-NULL
  for(g in unique(gates$gate)){
    x_par <- subset(gates, gate == g)$par_x[1]
    y_par <- subset(gates, gate == g)$par_y[1]
    XY <- subset(gates, gate == g)%>%
      select(c('x', 'y'))%>%
      as.matrix()
    points <- select(df, c(x_par, y_par)) %>%
      as.matrix()
    gate_mask <- cbind(gate_mask, as.numeric(tigers::is.insidePolygon(XY, points)))
  }
  gate_mask<-as.data.frame(gate_mask)
  names(gate_mask)<-unique(gates$gate)
  return(gate_mask)
}
transform_biex <- function(x, a = 1000, b = 1, c = 100, f = 1, w = 1){
  scales::new_transform(
    name = 'biex',
    transform =function(x){
      t <- ((x-f)+ sqrt((x-f)^2+4*a*c))/(2*a)
      z <- log(t)/(b)
      y <- z+w
      return(y)
    },
    inverse = function(x) a*exp(b*(x-w))-c*exp(-b*(x-w))+f,
  )
}
transform_anylog <- function(x, base){
  scales::new_transform(
    name = 'anylog',
    transform =function(x)log(x, base = base),
    inverse = function(x)base^x,
  )
}

std_gradient<-c('blue', 'cyan', 'green', 'red','orange','yellow', 'white')

