library(grid)
library(ggplot2)
library(gridSVG)

# Adds a data attribute to each point
geom_tooltip = function(...) {
  
  point = geom_point(...)

  point$geom = ggproto('geom_tooltip', point$geom, 
    draw_panel = function(self, data, ...) {
      grobs = list()
      
      for (i in 1:nrow(data)) {
        
        row = data[i,]
        title = paste0('x: ', row$x, ' y: ', row$y)
        grob = ggproto_parent(GeomPoint, self)$draw_panel(data = row, ...)
        grobs[[i]] = garnishGrob(grob, `data-toggle` = "tooltip", `title` = title)
      }
      
      ggplot2:::ggname('geom_tooltip', gTree(
        children = do.call('gList', grobs)
      ))
    }
  )
  
  point
}

ggplot(cars, aes(x = speed, y = dist)) + geom_tooltip()
#grid.force()
grid.export(name = "test.svg", strict = F)

