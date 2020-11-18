## ---- include = FALSE---------------------------------------------------------

knitr::opts_chunk$set(
  message = FALSE, 
  warning = FALSE,
  collapse = TRUE,
  comment = "#>", 
  fig.width = 6,
  fig.height = 4
)



## ----setup, fig.cap="$O_j$ and $_i$", echo=FALSE------------------------------
library(sf)
library(potential)
library(cartography)
opar <- par(mar=c(0,0,0,0)) 

x <- n3_pt[substr(n3_pt$ID,1,3) %in% c( "FRJ"), ]
x_poly <- n3_poly[substr(n3_poly$ID,1,3) %in% c( "FRJ"),]
x$POP19 <- round(x$POP19 / 1000, 0) 
plot(st_geometry(x_poly), col= "grey80", border = "white", lwd = .4)
propSymbolsLayer(x = x, var ="POP19", legend.pos = NA, 
                 col = "#940000", border = "white")
labelLayer(x, txt = "POP19", halo = T, pos = 4)

y <- st_as_sf(data.frame(ID = "A", x = 3700000, y = 2290000), 
              coords = c("x", "y"), crs = st_crs(x))

plot(y, pch = 23, add = T, bg = "blue", cex = 2)
par(opar)

## ----setup2, fig.cap = "$d_{ij}$", echo = FALSE-------------------------------
opar <- par(mar=c(0,0,0,0)) 

xy <- getLinkLayer(x = rbind(x[,"ID"], y), df = expand.grid(x = x$ID, y = y$ID))
xy$dist <- round(as.numeric(st_length(xy)) / 1000, 0)
xy$distlab <- paste0(round(as.numeric(st_length(xy)) / 1000, 0), " km")

plot(st_geometry(x_poly), col= "grey80", border = "white", lwd = .4)
propSymbolsLayer(x = x, var ="POP19", legend.pos = NA, 
                 col = "#94000033", border = "white")
plot(st_geometry(xy), add = T, lty = 2)
plot(y, pch = 23, add = T, bg = "blue", cex = 2)

labelLayer(xy, txt = "distlab", halo = T, col = "blue")
par(opar)

## ----curve, echo = FALSE, fig.cap="$f(d_{ij})$"-------------------------------
plot_inter(fun = "e", span = 75 , beta = 2, limit = 300)

## ----setup3, fig.cap = "$f(d_{ij})$", echo = FALSE----------------------------
opar <- par(mar=c(0,0,0,0)) 

span <- 75
beta <- 2
alpha <- log(2) / span^beta
fric <- function(alpha, matdist, beta) {
    exp(-alpha * matdist^beta)
}

xy$friction <- fric(alpha, xy$dist, beta)

plot(st_geometry(x_poly), col= "grey80", border = "white", lwd = .4)
propSymbolsLayer(x = x, var ="POP19", legend.pos = NA, 
                 col = "#94000033", border = "white")
plot(st_geometry(xy), add = T, lty = 2)
plot(y, pch = 23, add = T, bg = "blue", cex = 2)

xy$frictionlab <- round(xy$friction,2)
labelLayer(xy, txt = "frictionlab", halo = T, col = "blue")
par(opar)

## ---- fig.cap="$O_j f(d_{ij})$", echo = FALSE---------------------------------
opar <- par(mar=c(0,0,0,0)) 

xy <- merge(xy, st_drop_geometry(x), by.x = "x", by.y = "ID")
xy$m <- xy$POP19 * xy$friction
xy$mlab <- paste0(xy$frictionlab, " * ", xy$POP19, " = ", round(xy$m, 0))
plot(st_geometry(x_poly), col= "grey80", border = "white", lwd = .4)
propSymbolsLayer(x = x, var ="POP19", legend.pos = NA, 
                 col = "#94000033", border = "white")
plot(st_geometry(xy), add = T, lty = 2)
plot(y, pch = 23, add = T, bg = "blue", cex = 2)
xy$frictionlab <- round(xy$friction,2)
labelLayer(xy, txt = "mlab", halo = T, col = "blue")
par(opar)

## ---- fig.cap ="$O_j f(d_{ij})$", echo = FALSE--------------------------------
opar <- par(mar=c(0,0,0,0)) 

x <- merge(x, st_drop_geometry(xy), by.x = "ID", by.y = "x")

x$mroun <- round(x$m, 0)
plot(st_geometry(x_poly), col= "grey80", border = "white", lwd = .4)
propSymbolsLayer(x = x, var ="POP19.x", legend.pos = NA, 
                 col = "#94000033", border = "white")
propSymbolsLayer(x = x, var ="mroun", legend.pos = NA, fixmax = max(x$POP19.x),
                 col = "#940000", border = "white")
plot(st_geometry(xy), add = T, lty = 2)

labelLayer(x, txt = "mroun", halo = T, col = "blue", pos = 4)
plot(y, pch = 23, add = T, bg = "blue", cex = 2)
par(opar)

## ---- fig.height= 6-----------------------------------------------------------
library(sf)
library(potential)
library(cartography)
plot(st_geometry(n3_poly), col= "grey80", border = "white", lwd = .4)
y <- create_grid(x = n3_poly, res = 100000)
plot(st_geometry(y), pch = 23, add = TRUE, bg = "blue", cex = .5 )


## -----------------------------------------------------------------------------
d <- create_matrix(x = n3_pt, y = y)
d[1:5, 1:5]

## ---- eval = T, fig.height= 6-------------------------------------------------
y$pot <- potential(x = n3_pt, y = y, d = d,
                   var = "POP19", fun = "e",
                   span = 75000, beta = 2)
plot(st_geometry(n3_poly), col= "grey80", border = "white", lwd = .4)
propSymbolsLayer(y, var = "pot", inches= .06, legend.style = "e", 
                 lwd = .5, legend.frame = TRUE, legend.pos = "topright")

## ---- eval = T, fig.height= 6-------------------------------------------------
y$pot2 <- 100 * y$pot / max(y$pot)
plot(st_geometry(n3_poly), col= "grey80", border = "white", lwd = .4)
propSymbolsLayer(y, var = "pot2", inches= .06, legend.style = "e", 
                 lwd = .5, legend.frame = TRUE, legend.pos = "topright")

## ---- fig.width = 7, fig.height= 6--------------------------------------------
iso <- equipotential(x = y, var = "pot2", breaks = seq(0,100, 10), mask = n3_poly)
choroLayer(iso, var = "center", breaks =  seq(0,100, 10), 
           col = hcl.colors(10, 'teal'), 
           border = "#121725", legend.pos = "topright", 
           lwd = .2, legend.title.txt = "Potential of\nPopulation")

## ----smooth, fig.path="./", fig.width = 7, fig.height= 6, results=FALSE, eval = FALSE----
#  y <- create_grid(x = n3_poly, res = 20000)
#  y$pot <- mcpotential(x = n3_pt, y = y,
#                       var = "POP19", fun = "e",
#                       span = 75000, beta = 2,
#                       limit = 200000, ncl = 2)
#  y$pot2 <- 100 * y$pot / max(y$pot)
#  iso <- equipotential(x = y, var = "pot2", breaks = seq(0,100, 10), mask = n3_poly)
#  opar <- par(mar=c(0,0,0,0))
#  choroLayer(iso, var = "center", breaks =  seq(0,100, 10),
#             col = hcl.colors(10, 'teal'),
#             border = "#121725", legend.pos = c(6194790,3428230),
#             lwd = .2, legend.title.txt = "Potential Intensity")
#  mtext(text = "© EuroGeographics for the administrative boundaries and © Eurostat for data",
#        side=1, line = -1, cex = .7, adj = .99)
#  mtext(text = "Potential of population",
#        side=3, line = -2, cex = 1.1, adj = 0.01)
#  par(opar)

