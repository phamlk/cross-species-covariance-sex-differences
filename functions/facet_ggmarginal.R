getPanelPos_facet <- function(gtableGrob) {
  layDF <- gtableGrob$layout
  layDF[layDF$name == "panel-1-1" | layDF$name == "panel-2-1", 
        c("t", "l", "b", "r")]
}

getMargGrob_facet <- function(margPlot) {
  margG <- ggplot2::ggplotGrob(margPlot)
  gtable::gtable_filter(margG, pattern = "panel")
}

addTopMargPlot_facet <- function(ggMargGrob, top, size) {
  
  panelPos <- getPanelPos_facet(ggMargGrob)
  topMargG <- getMargGrob_facet(top)

  gt_rows <- gtable::gtable_add_rows(
    x = ggMargGrob,
    heights = grid::unit(1 / size, "null"), pos = 0
  )
  
  gtable::gtable_add_grob(
    x = gt_rows, grobs = list(topMargG, topMargG), t = c(1,1), b = c(1,1),
    l = panelPos[["l"]], r = panelPos[["r"]],
    z = Inf, clip = "on", name = "topMargPlot"
  )
}

addRightMargPlot_facet <- function(ggMargGrob, right, size) {
  panelPos <- getPanelPos_facet(ggMargGrob)
  rightMargG <- getMargGrob_facet(right)
  gt <- gtable::gtable_add_cols(
    x = ggMargGrob,
    widths = grid::unit(1 / size, "null"),
    pos = -1
  )
  gtable::gtable_add_grob(
    x = gt, grobs = rightMargG, t = panelPos[["t"]][[2]],
    b = panelPos[["b"]][[2]], r = ncol(gt), l = ncol(gt),
    z = Inf, clip = "on", name = "rightMargPlot"
  )
}

facet_ggmarginal <- function(ggMargGrob, top, right, size) { 
  
  ggMargGrob <- ggplotGrob(ggMargGrob)
    
  tmp <- addTopMargPlot_facet(ggMargGrob, top, size)
  
  output <- addRightMargPlot_facet(tmp, right, size)
  
  return(output)

}