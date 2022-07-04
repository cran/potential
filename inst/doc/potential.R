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
library(potential)
library(sf)
library(mapsf)
mf_theme(mar=c(0,0,0,0), bg = "white") 
x <- n3_pt[substr(n3_pt$ID,1,3) %in% c( "FRJ"), ]
x_poly <- n3_poly[substr(n3_poly$ID,1,3) %in% c( "FRJ"),]
x$POP19 <- round(x$POP19 / 1000, 0) 
mf_map(x_poly, col= "grey80", border = "white", lwd = .4)
mf_map(x = x, var ="POP19", type = "prop", leg_pos = NA, 
       col = "#940000", border = "white")
mf_label(x = x, var = "POP19", halo = T, pos = 4)

y <- st_as_sf(data.frame(ID = "A", x = 3700000, y = 2290000), 
              coords = c("x", "y"), crs = st_crs(x))

mf_map(y, pch = 23, add = T, bg = "blue", cex = 2)

## ----setup2, fig.cap = "$d_{ij}$", echo = FALSE-------------------------------
xy <- mf_get_links(x = rbind(x[,"ID"], y), df = expand.grid(x = x$ID, y = y$ID))
xy$dist <- round(as.numeric(st_length(xy)) / 1000, 0)
xy$distlab <- paste0(round(as.numeric(st_length(xy)) / 1000, 0), " km")

mf_map(x_poly, col= "grey80", border = "white", lwd = .4)
mf_map(x = x, var ="POP19", type = "prop", leg_pos = NA, 
       col = "#94000033", border = "white")
mf_map(xy, add = T, lty = 2)
mf_map(y, pch = 23, add = T, bg = "blue", cex = 2)

mf_label(x = xy, var = "distlab", halo = T, col = "blue")

## ----curve, echo = FALSE, fig.cap="$f(d_{ij})$"-------------------------------
plot_inter(fun = "e", span = 75 , beta = 2, limit = 300)

## ----setup3, fig.cap = "$f(d_{ij})$", echo = FALSE----------------------------

span <- 75
beta <- 2
alpha <- log(2) / span^beta
fric <- function(alpha, matdist, beta) {
  exp(-alpha * matdist^beta)
}

xy$friction <- fric(alpha, xy$dist, beta)

mf_map(x_poly, col= "grey80", border = "white", lwd = .4)
mf_map(x = x, var ="POP19", type = "prop", leg_pos = NA, 
       col = "#94000033", border = "white")
mf_map(xy, add = T, lty = 2)
mf_map(y, pch = 23, add = T, bg = "blue", cex = 2)

xy$frictionlab <- round(xy$friction,2)
mf_label(x = xy, var = "frictionlab", halo = T, col = "blue")


## ---- fig.cap="$O_j f(d_{ij})$", echo = FALSE---------------------------------
xy <- merge(xy, st_drop_geometry(x), by.x = "x", by.y = "ID")
xy$m <- xy$POP19 * xy$friction
xy$mlab <- paste0(xy$frictionlab, " * ", xy$POP19, " = ", round(xy$m, 0))
mf_map(x_poly, col= "grey80", border = "white", lwd = .4)
mf_map(x = x, var ="POP19", type = "prop", leg_pos = NA, 
       col = "#94000033", border = "white")
mf_map(xy, add = T, lty = 2)
mf_map(y, pch = 23, add = T, bg = "blue", cex = 2)
xy$frictionlab <- round(xy$friction,2)
mf_label(x = xy, var = "mlab", halo = T, col = "blue", cex = .6 )

## ---- fig.cap ="$O_j f(d_{ij})$", echo = FALSE--------------------------------

x <- merge(x, st_drop_geometry(xy), by.x = "ID", by.y = "x")

x$mroun <- round(x$m, 0)
mf_map(x_poly, col= "grey80", border = "white", lwd = .4)
mf_map(x = x, var ="POP19.x", type = "prop", leg_pos = NA, 
       col = "#94000033", border = "white")
mf_map(x = x, var ="mroun", type = "prop", leg_pos = NA, 
       col = "#940000", border = "white",val_max = max(x$POP19.x))
mf_map(xy, add = T, lty = 2)

mf_label(x = x, var = "mroun", halo = T, col = "blue", pos = 4)
mf_map(y, pch = 23, add = T, bg = "blue", cex = 2)

## ---- fig.height= 6-----------------------------------------------------------
library(potential)
library(mapsf)
mf_map(n3_poly, col= "grey80", border = "white", lwd = .4)
y <- create_grid(x = n3_poly, res = 100000)
mf_map(y, pch = 23, add = TRUE, bg = "blue", cex = .5 )


## -----------------------------------------------------------------------------
d <- create_matrix(x = n3_pt, y = y)
d[1:5, 1:5]

## ---- eval = T, fig.height= 6-------------------------------------------------
y$pot <- potential(x = n3_pt, y = y, d = d,
                   var = "POP19", fun = "e",
                   span = 75000, beta = 2)
mf_map(n3_poly, col= "grey80", border = "white", lwd = .4)
mf_map(y, var = "pot", type = "prop", 
       inches= .1, 
       lwd = .5, 
       leg_val_cex = .5, 
       leg_val_rnd = -3,
       leg_frame = TRUE, 
       leg_pos = "topright")

## ---- eval = T, fig.height= 6-------------------------------------------------
y$pot2 <- 100 * y$pot / max(y$pot)
mf_map(n3_poly, col= "grey80", border = "white", lwd = .4)
mf_map(y, var = "pot2", type = "prop", 
       inches= .1, 
       lwd = .5, 
       leg_val_cex = .5, 
       leg_val_rnd = 0,
       leg_frame = TRUE, 
       leg_pos = "topright")

## ---- fig.width = 7, fig.height= 6--------------------------------------------
bks <- seq(0, 100, 10)
iso <- equipotential(x = y, var = "pot2", breaks = bks, mask = n3_poly)
mf_map(x = iso, var = "min", type = "choro", 
       breaks = bks, 
       pal = hcl.colors(10, 'Teal'),
       lwd = .2,
       border = "#121725", 
       leg_pos = "topright",
       leg_val_rnd = 0,
       leg_title = "Potential of\nPopulation")

## ----smooth, fig.path="./", fig.width = 7, fig.height= 6, results=FALSE, eval = FALSE----
#  y <- create_grid(x = n3_poly, res = 20000)
#  y$pot <- mcpotential(x = n3_pt, y = y,
#                       var = "POP19", fun = "e",
#                       span = 75000, beta = 2,
#                       limit = 200000, ncl = 2)
#  y$pot2 <- 100 * y$pot / max(y$pot)
#  bks <- seq(0, 100, 10)
#  iso <- equipotential(x = y, var = "pot2", breaks = bks, mask = n3_poly)
#  mf_map(x = iso, var = "min", type = "choro",
#         breaks =  seq(0,100, 10),
#         pal = hcl.colors(10, 'Teal'),
#         lwd = .2,
#         border = "#121725",
#         leg_pos = c(6084270,4253383),
#         leg_val_rnd = 0,
#         leg_title = "Potential Intensity")
#  mf_credits(txt = "© EuroGeographics for the administrative boundaries and © Eurostat for data",
#             pos = "bottomright", cex = .7)
#  mf_title(txt = "Potential of population", bg = "white", fg = "black", inner = TRUE, cex = 1.1)

