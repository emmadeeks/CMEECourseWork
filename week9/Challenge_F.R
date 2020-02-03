##### Fractal 1 
trytree <- function(start_position, direction, length, colour){
  first <- turtle(start_position, direction, length, colour)
  limit = 0.1
  if (length > limit) {
    trytree(first, (direction - pi/2), length*0.65, colour = "pink")
    trytree(first, (direction + pi/2), length*0.65, colour = "red")
  }
  return("type your written answer here")
}

graphics.off()
plot(-15:15,-5:25, type = "n")
trytree(c(0,0), pi/2, 10, "green")


##### Plot 2


trytree2 <- function(start_position, direction, length, colour){
  first <- turtle(start_position, direction, length, colour = "green")
  limit = 0.1
  if (length > limit) {
    trytree2(first, (direction * pi/18), length*0.95, colour = "red")
    trytree2(first, (direction * pi/8), length*0.75, colour = "gold")
    #trytree2(first, (direction * pi/2), length*0.5, colour = "brown")
    #tree(first, (direction * pi/4), length*0.25, colour = "dark green")
  }
  return("type your written answer here")
}

trytree2 <- function(start_position, direction, length, colour){
  first <- turtle(start_position, direction, length, colour = "green")
  limit = 0.1
  if (length > limit) {
    trytree2(first, (direction + pi/10), length*0.65, colour = "red")
    trytree2(first, (direction - pi/4), length*0.55, colour = "gold")
  }
  return("type your written answer here")
}

graphics.off()
plot(-50:50, 0:100, type = "n")
trytree2(c(2,2), pi/2, 20)

###### Plot 3
tryfern2 <- function(start_position, direction, length, dir, colour){
  first <- turtle(start_position, direction, length, colour)
  limit = 0.1
  if (length > limit) {
    fern2(first, (direction + (dir * pi/4)), length*0.38, (dir*1), colour)
    fern2(first, direction, length*0.87, (dir*-1), colour)
  }
  return("type your written answer here")
}

plot(-40:40, 0:80, type = "n")

tryfern2(start_position2, pi/2, 10, -1, "blue")

#### Plot 4

tryfern <- function(start_position, direction, length, colour = "dark green"){
  first <- turtle(start_position, direction, length, colour)
  limit = 0.1
  if (length > limit) {
    tryfern(first, (direction - pi/4), length*0.85, colour = "dark red") #this branch is going 45 degrees to the left 
    tryfern(first, (direction + pi/2), length*0.35, colour)#this branch is going stright on 
  }
}

graphics.off()
plot(-10:50, -30:30, type = "n")
tryfern(c(2,2), pi/4, 15)


secondtryfern <- function(start_position, direction, length, colour = "dark green"){
  first <- turtle(start_position, direction, length, colour)
  limit = 0.1
  if (length > limit) {
    secondtryfern(first, (direction + pi/13), length*0.9, colour) #this branch is going 45 degrees to the left 
    secondtryfern(first, direction, length*0.37, colour) #this branch is going stright on 
  }
}

graphics.off()
plot(-40:40, -40:40, type = "n")
for(i in 1:6){
  secondtryfern(c(2,2), i, 5, colour = i)
}


turtle <- function(start_position, direction, length, colour){
  newx <- cos(direction) * length 
  newy <- sin(direction) * length
  newpoints <- c(newx,newy)
  newpositions <- c(newpoints + start_position)
  segments(start_position[1], start_position[2], newpositions[1], newpositions[2], col = colour)
  return(newpositions) # you should return your endpoint here.
}



Challenge_F <- function(start_position, direction, length, dir, limit = 0.1){
  first <- turtle(start_position, direction, length)
  if (length > limit) {
    fern2(first, (direction + (dir * pi/18)), length*0.38, (dir*1), limit)
    #fern2(first, direction, length*0.87, (dir*-1), limit)
  }
  return("As you increase e, the line size threshold, the programme takes longer to run
         as the lines plotted take longer to plot. As you decrease e the programme runs much quicker")
}

fern <- function(start_position, direction, length, limit = 0.1){
  first <- turtle(start_position, direction, length)
  if (length > limit) {
    fern(first, pi/18, length*0.38, limit)
    fern(first, direction, length*0.87, limit)
  }
  return("type your written answer here")
}

Challenge_F <- function(start_position, direction, length, dir, limit = 0.1){
  first <- turtle(start_position, direction, length)
  if (length > limit) {
    fern2(first, (direction + (dir * pi/4)), length*0.38, (dir*1), limit)
    fern2(first, direction, length*0.87, (dir*-1), limit)
  }
  return("As you increase e, the line size threshold, the programme takes longer to run
         as the lines plotted take longer to plot. As you decrease e the programme runs much quicker")
}


start_position2 = c(2,2)
fern(start_position2, 1, 10, -1)
fern2(start_position2, 1, 10, -1)
Challenge_F(start_position2, pi/2, 10, -1)
Challenge_F(start_position2, 1, 10, -1)

graphics.off()
plot(-15:15,-5:25, type = "n")
trytree(c(0,0), pi/2, 10, "green")

graphics.off()
plot(-40:40, 0:80, type = "n")
tryfern2(start_position2, pi/2, 10, -1, "blue")

graphics.off()
plot(-10:50, -30:30, type = "n")
tryfern(c(2,2), pi/4, 15)

graphics.off()
plot(-40:40, -40:40, type = "n")
for(i in 1:6){
  secondtryfern(c(2,2), i, 5, colour = i)
}

F_challenge2(c(2,2), pi/4, 15)
plot(-40:40, -40:40, type = "n")
for(i in 1:6){
  F_challenge3(c(2,2), i, 5, colour = i)
}





trytree2 <- function(start_position, direction, length, colour){
  first <- turtle(start_position, direction, length, colour = "green")
  limit = 0.1
  if (length > limit) {
    tree(first, (direction + pi/8), length*0.85, colour = "red")
    trytree2(first, (direction - pi/2), length*0.35, colour = "gold")
    #trytree2(first, (direction * pi/2), length*0.5, colour = "brown")
    #tree(first, (direction * pi/4), length*0.25, colour = "dark green")
  }
  return("type your written answer here")
}

graphics.off()
plot(-50:50, 0:100, type = "n")
trytree2(c(2,2), pi/2, 20, "green")
