
library(grid)
library(gridSVG)
library(ggplot2)

data(cars)

testGraph <- function() {
  
  plot = ggplot(cars, aes(x = speed, y = dist))
  
  # Use individual layers for points
  points = sapply(1:nrow(cars), function(x) geom_point(data = cars[x,]))
  for (i in 1:length(points))
    plot = plot + points[i]

  print(plot)
  
  grid.force()
  grobNames = grid.ls(print = F)$name
  pointGrobNames = grobNames[grepl("geom_point.points", grobNames)]

  # Set attributes for each point
  for (k in 1:length(pointGrobNames)) {
    text = paste(cars[k,], collapse = ' ')
    grid.garnish(pointGrobNames[k], `data-toggle` = 'tooltip', `title` = text, `id` = randomString())
  }

  grid.export("grid2.svg", strict = F)
}

randomString <- function(n=1, lenght=12)
{
  randomString <- c(1:n)                  # initialize vector
  for (i in 1:n)
  {
    randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                    lenght, replace=TRUE),
                             collapse="")
  }
  return(randomString)
}