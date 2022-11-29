# library(plot3D)

# x, y, z variables
x <- mtcars$wt
y <- mtcars$drat
z <- mtcars$mpg

# Compute the linear regression (z = ax + by + d)
fit_additive <- lm(z ~ x + y)
fit_interact <- lm(z ~ x * y)

# predict values on regular xy grid
grid_lines <- 30

# Create x and y predicted values
x_pred     <- seq(min(x), max(x), length.out = grid_lines)
y_pred     <- seq(min(y), max(y), length.out = grid_lines)
xy         <- expand.grid(x = x_pred, y = y_pred)
z_pred_add <- matrix(predict(fit_additive, newdata = xy), 
                     nrow = grid_lines, ncol = grid_lines)
z_pred_int <- matrix(predict(fit_interact, newdata = xy), 
                     nrow = grid_lines, ncol = grid_lines)

# fitted points for droplines to surface
fitpoints_add <- predict(fit_additive)
fitpoints_int <- predict(fit_interact)





# Final from corner
# require(tikzDevice)
#   options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
#   "\\usepackage{tipa}"))
#   tikz(here::here('static', 'slides', '05_lm', '03_mrc', 
#                           'assets', 'img', "additive_3d_corner.tex"), 
#        standAlone = TRUE, width = 5, height = 5)
# 
# scatter3D(x, y, z, 
#     pch = 21, cex = 1, expand = 0.75, colkey = F,
#     theta = 45, phi = 20, ticktype = "detailed",
#     xlab = "wt", ylab = "drat", zlab = "mpg", 
#     surf = list(x = x_pred, y = y_pred, z = z_pred_add,  
#                 facets = NA, col = 'grey60', fit = fitpoints_add)) 
# dev.off()



## @knitr threejs1
library(htmlwidgets)
library(threejs)

threejs1 <- scatterplot3js(x, y, z, color=rainbow(length(z)), bg = 'black')
#saveWidget(widget = threejs1, 
#           file = here::here('static', 'slides', '05_lm', '03_mrc', 
#                              'assets', 'html', 'threejs1.html'))





#require(tikzDevice)
#   options(tikzLatexPackages = c(getOption("tikzLatexPackages"), 
#   "\\usepackage{tipa}"))
#   tikz(here::here('static', 'slides', '05_lm', '03_mrc', 
#                           'assets', 'img', "additive_3d_corner.tex"), 
#        standAlone = TRUE, width = 5, height = 5)
# 
# Same but with interaction
#scatter3D(x, y, z, 
#    pch = 21, cex = 1, expand = 0.75, colkey = F,
#    theta = 45, phi = 20, ticktype = "detailed",
#    xlab = "wt", ylab = "drat", zlab = "mpg", 
#    surf = list(x = x_pred, y = y_pred, z = z_pred_int,  
#                facets = NA, col = 'grey60'))

# dev.off()

















# Assumptions
# Evaluate Collinearity
#car::vif(mod1) # variance inflation factors
#sqrt(vif(mod1)) > 2 # problem?


#library(boot)
#glm.diag.plots(mod2)




