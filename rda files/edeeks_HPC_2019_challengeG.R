G <- function(sp, d, l, dir= -1){
  NP <- c((cos(d) * l), (sin(d) * l))
  n <- c(NP + sp)
  segments(sp[1], sp[2], n[1], n[2])
  f <- n
  L = 0.1
  if (l > L) {
    G(f, (d + (dir * pi/4)), l*0.38, (dir*1))
    G(f, d, l*0.87, (dir*-1))
  }
}

GD <- function(){
graphics.off()
plot(-50:50,-5:95, type = "n", ann = FALSE, axes = 0)
G(c(0,0), pi/2, 10)
}

