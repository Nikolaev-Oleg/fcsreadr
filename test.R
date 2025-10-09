file = "11.fcs"

metadata<-get_fcs_metadata(file) #get metadata
df<-read_fcs(file) #read .fcs file

a <- Sys.time()
density<-pseudocolour(df$`FSC_A`, df$`SSC_A`, r = 100) #calculate 2d density
Sys.time()-a

#Pseudocolor plot
ggplot(df, aes(FSC_A, SSC_A, colour = density))+
  geom_point()+
  scale_x_continuous(transform = transform_biex(c=10),
                     breaks = c(10, 10^2, 10^3, 10^4, 10^5, 10^6),
                     labels = trans_format("log10", math_format(10^.x)))+
  scale_y_continuous(transform = transform_biex(c=10),
                     breaks = c(10, 10^2, 10^3, 10^4, 10^5, 10^6),
                     labels = trans_format("log10", math_format(10^.x)))+
  theme_classic()+
  scale_color_gradientn(colours=std_gradient)

gates<-gui_gate_editor(df) # GUI gate editor

gate_masks<-gate_mask(df, gates) # apply masks based on gates

df_gated<-cbind(df, gate_masks) # merge masks with data


