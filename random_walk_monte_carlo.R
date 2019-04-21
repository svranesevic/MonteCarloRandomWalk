if (!require("pacman"))
  install.packages("pacman", repos = "http://cran.us.r-project.org")

pacman::p_load(MonteCarlo)

main <- function() {
  walk.lengths.to.simulate <- 1:30
  number.of.simulations <- 20000
  no.transport.distance <- 4
  
  distances <-
    monte.carlo.distances(walk.lengths.to.simulate, number.of.simulations)
  
  print.summary(
    distances,
    walk.lengths.to.simulate,
    number.of.simulations,
    no.transport.distance
  )
}

monte.carlo.distances <-
  function(walk.lengths.to.simulate,
           number.of.simulations) {
    mc <-
      MonteCarlo(
        func = distance.from.home,
        nrep = number.of.simulations,
        param_list = list("walk.length" = walk.lengths.to.simulate),
        ncpus = 1
      )
    
    mc$results$distance
  }

distance.from.home <- function(walk.length) {
  vec.distance <- random.walk(walk.length)
  x.distance <- vec.distance[1]
  y.distance <- vec.distance[2]
  
  distance <- abs(x.distance) + abs(y.distance)
  list("distance" = distance)
}

random.walk <- function(n) {
  x <- 0
  y <- 0
  
  for (i in 1:n) {
    moves <-
      rbind(c(1, 0),
            c(-1, 0),
            c(0, 1),
            c(0,-1))
    
    move <- sample(c(1:4), 1)
    
    x = x + moves[move, 1]
    y = y + moves[move, 2]
  }
  
  c(x, y)
}

print.summary <- function(distances,
                          walk.lengths.to.simulate,
                          number.of.simulations,
                          no.transport.distance) {
  for (walk.size in walk.lengths.to.simulate) {
    no.transport <- 0
    for (mc.run in 1:number.of.simulations) {
      d <- distances[walk.size, mc.run]
      if (d <= no.transport.distance) {
        no.transport = no.transport + 1
      }
    }
    
    print(
      paste(
        "Walk size:",
        walk.size,
        " / % of no transport =",
        as.double(no.transport) / number.of.simulations * 100
      )
    )
  }
}

if (!interactive()) {
  main()
}