#' Randomly place circular plots within a given (sf) area and with a given radius
#'
#' Randomly place circular plots, with a given radius, within an sf object, so that the plots will not overlap.
#' @param n Sample size of plots to be included.
#' @param inner_polygon Area that the centre of plots should be randomised within
#' @param radius Radius of the requested plot(s) in metres
#' @param outer_polygon Area that the entirety of the plot should be randomised within. NB - This can be the same as the inner polygon
#' @keywords circular plots
#' @export
#' @examples
#' plot_selection(n=450, inner_polygon=Soton, radius=11.4, outer_polygon=Soton)

#Repeat function with adjustable shp file
plot_selection <- function(n, inner_polygon, radius, outer_polygon=NULL) {
  #Create line version of outer polygon for measurements if outer polygon entered
  if (!is.null(outer_polygon)) {
    poly_line <- st_cast(outer_polygon, "MULTILINESTRING")
  }

  #Create function which ensures entire plot lies within Soton
  outer_select <- function(n){

    #Create n random plots
    plots_random <- st_sample(inner_polygon, size=n)
    #Create vector of distance to boundary
    plot2poly <- st_distance(plots_random,poly_line)
    #Calculate vector with the lowest distance on each row for multiline polygons
    plot2poly_min <- apply(plot2poly, 1,min,na.rm = T)

    #Create vector containing instances where plots to close too border
    tooclose <- which(as.numeric(plot2poly_min) < radius)

    while (length(tooclose)!=0) {
      #Remove relevant overlaping plots===
      plots_random <- plots_random[-tooclose]

      #Create new plots to replace old
      new_plots <- st_sample(inner_polygon, size=length(tooclose))
      #append new plots to overall
      plots_random <- append(plots_random,new_plots)

      #Recalculate vector and check
      plot2poly <- st_distance(plots_random, poly_line)
      plot2poly_min <- apply(plot2poly, 1,min,na.rm = T)
      tooclose <- which(as.numeric(plot2poly_min) < radius)

    }
    return(plots_random)
  }

  #Create n random plots - approach differs depending on whether outer_polygon specified
  if (is.null(outer_polygon)) {
    plots_random <- st_sample(inner_polygon,n)
  } else {
    plots_random <- outer_select(n)
  }
  #Create matrix of distance between each point
  plots_dist <- st_distance(plots_random)
  #Set diagonal to larger than diameter as diagonals are the distance between the plot and itself
  diag(plots_dist) <- 2*radius + 1
  #Set upper triangle to larger than diameter to avoid repeatedly removing plots
  plots_dist[upper.tri(plots_dist)] <- 2*radius + 1

  #Calculate vector with the lowest distance on each row of the distance matrix
  min_dist <- apply(plots_dist, 1,min,na.rm = T)
  #Create vector containing instances where plots overlap (nb - this is not all instances as not all should need to be replaced)
  overlap <- which(min_dist < 2*radius)
  #Set count for number of times code looped through to 1
  loops <- 1
  print(paste0("Loop number:", loops))
  while (length(overlap)!=0) {
    #Remove relevant overlaping plots
    plots_random <- plots_random[-overlap]

    #Create new plots to replace old - approach used dependent on whether outer is specified
    if (is.null(outer_polygon)) {
      new_plots <- st_sample(inner_polygon,length(overlap))
    } else {
      new_plots <- outer_select(length(overlap))
    }
    #append new plots to overall
    plots_random <- append(plots_random,new_plots)

    #Recalculate matrix and check
    plots_dist <- st_distance(plots_random)
    diag(plots_dist) <- 2*radius + 1
    #Set upper triangle to zero to avoid repeatedly including plots
    plots_dist[upper.tri(plots_dist)] <- 2 *radius + 1
    min_dist <- apply(plots_dist, 1, min, na.rm = T)
    overlap <- which(min_dist < 2*radius)
    #Create loops var and print to indicate how many times code looped through
    loops <- loops + 1
    print(paste0("Loop number:",loops))
  }
  return(plots_random)
}
