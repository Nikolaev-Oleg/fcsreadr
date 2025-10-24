GeomPseudocolour <- ggproto("GeomPseudocolour", Geom,
                            required_aes = c("x", "y"),
                            default_aes = aes(shape = 19, r = 0.1),
                            draw_key = draw_key_point,
                            
                            draw_panel = function(data, panel_params, coord) {
                              coords <- coord$transform(data, panel_params)
                              colour = pseudocolour(coords$x, coords$y, coords$r)
                              
                              grad <- colorRampPalette(std_gradient)(1000)
                              coords$colour <- grad[as.numeric(cut(colour, breaks = 1000))]
                              
                              grid::pointsGrob(
                                coords$x, coords$y,
                                pch = coords$shape,
                                size = unit(0.4, "char"),
                                gp = grid::gpar(col = coords$colour)
                              )
                            })

geom_pseudocolour <- function(mapping = NULL, data = NULL, stat = "identity",
                              position = "identity", na.rm = FALSE, show.legend = NA, 
                              inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomPseudocolour, data = data, mapping = mapping, stat = stat, 
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}
