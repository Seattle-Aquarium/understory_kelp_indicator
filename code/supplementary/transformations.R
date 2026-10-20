## How the transformation changes the NMDS and the db-RDA
## Reads:  results/dbrda_algae_points.csv            (substrate_algae.R)
## Writes: results/supplementary/transformations.csv          stress, R^2, Procrustes r
##         results/supplementary/transformation_weights.csv   each species' weight
##         figs/supplementary/transformations_nmds.png
##
## A check behind the methods, not part of the app: the same 881 transects and
## ten understory algae under five transformations -- raw densities, square
## root, log10(count + 1), presence/absence, and the Wisconsin double
## standardization of square roots that nmds_algae.R uses. For each: 2-D NMDS
## stress (20 random starts each, so a little above the app's 100-start
## figure), how much of the community substrate explains in the db-RDA, and
## how alike the NMDS arrangement of transects is to the app's (Procrustes r,
## 1 = the same up to rotation and scale). A species' "weight" is its share of
## all the values Bray-Curtis adds up: on raw densities half of it is sugar kelp.
##
## Run from code/supplementary (it sets the working directory to the
## repository). About five minutes on a many-core computer.

rm(list=ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(vegan)
  library(ggplot2)
})
setwd("../../")
results <- "results"
out_res <- file.path(results, "supplementary"); out_fig <- file.path("figs", "supplementary")
dir.create(out_res, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)

env  <- read.csv(file.path(results, "dbrda_algae_points.csv"), check.names = FALSE)
spp  <- grep("^ak_", names(env), value = TRUE)
dens <- as.matrix(env[spp])                       # individuals per m^2
cnt  <- round(dens * 60)                          # individuals per 30 x 2 m transect
env$year <- factor(env$year)

tf <- list(
  "Raw densities"                  = dens,
  "Square root"                    = sqrt(dens),
  "log10(count + 1)"               = log10(cnt + 1),
  "Presence / absence"             = (dens > 0) * 1,
  "Wisconsin + square root (used)" = wisconsin(sqrt(dens)))
label <- function(s) gsub("_", " ", sub("^ak_", "", s))

## workers that see this R's library, as in substrate_algae.R
cores <- max(1, parallel::detectCores() - 2)
cl <- parallel::makeCluster(cores)
invisible(parallel::clusterCall(cl, function(p) { .libPaths(p); invisible(NULL) }, .libPaths()))
invisible(parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(vegan))))

rows <- list(); weights <- list(); ords <- list()
for (nm in names(tf)) {
  x  <- tf[[nm]]
  bc <- vegdist(x, "bray")
  weights[[nm]] <- round(100 * colSums(x) / sum(x), 1)
  m  <- dbrda(bc ~ hard + soft + relief + Condition(year), data = env)
  r2 <- RsquareAdj(m)
  ax <- eigenvals(m, "constrained") / m$tot.chi
  set.seed(1)
  ords[[nm]] <- metaMDS(x, distance = "bray", k = 2, trymax = 20, autotransform = FALSE,
                        wascores = TRUE, parallel = cl, trace = 0)
  rows[[nm]] <- data.frame(transformation = nm, nmds_stress_2d = ords[[nm]]$stress,
                           dbrda_r2 = r2$r.squared, dbrda_adj_r2 = r2$adj.r.squared,
                           dbrda_axis1 = ax[1], dbrda_axis2 = ax[2])
  cat(nm, "done\n")
}
parallel::stopCluster(cl)

## every solution rotated (and reflected) onto the one the app uses
ref <- scores(ords[["Wisconsin + square root (used)"]], "sites")
panels <- list()
for (nm in names(ords)) {
  ord <- ords[[nm]]
  p <- procrustes(ref, scores(ord, "sites"), scale = TRUE)
  rows[[nm]]$procrustes_r <- protest(ref, scores(ord, "sites"), permutations = 0)$t0
  sc <- as.data.frame(p$Yrot); names(sc) <- c("x", "y"); sc$basin <- gsub("_", " ", env$basin)
  sp <- as.data.frame(p$scale * scores(ord, "species") %*% p$rotation); names(sp) <- c("x", "y")
  sp$label <- label(spp)
  ## framed on 99.5% of the transects; the few beyond fall outside
  lim <- max(quantile(abs(c(sc$x, sc$y)), 0.995), abs(c(sp$x, sp$y))) * 1.08
  panels[[nm]] <- ggplot(sc, aes(x, y)) +
    geom_point(aes(color = basin), alpha = 0.35, size = 0.9, shape = 16) +
    geom_segment(data = sp, aes(x = 0, y = 0, xend = x, yend = y), linewidth = 0.4,
                 arrow = arrow(length = unit(0.12, "cm"))) +
    ggrepel::geom_label_repel(data = sp, aes(label = label), size = 2.4, seed = 1, label.size = 0.15,
                              max.overlaps = Inf, min.segment.length = 0, segment.color = "gray50") +
    coord_fixed(xlim = c(-lim, lim), ylim = c(-lim, lim)) +
    labs(title = nm, x = NULL, y = NULL, color = "Basin",
         subtitle = sprintf("NMDS stress %.3f \u00b7 substrate explains %.1f%%%s",
                            ord$stress, 100 * rows[[nm]]$dbrda_r2,
                            if (rows[[nm]]$procrustes_r < 0.9999)
                              sprintf(" \u00b7 Procrustes r = %.2f with the app's", rows[[nm]]$procrustes_r) else "")) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 2.5), nrow = 2)) +
    theme_bw(base_size = 9) +
    theme(panel.grid = element_blank(), plot.title = element_text(face = "bold", size = 10),
          axis.text = element_blank(), axis.ticks = element_blank(), legend.position = "bottom")
}

res <- bind_rows(rows)
w <- as.data.frame(do.call(cbind, weights)); w <- cbind(species = label(spp), w)
print(res, digits = 3, row.names = FALSE)
write.csv(res, file.path(out_res, "transformations.csv"), row.names = FALSE)
write.csv(w, file.path(out_res, "transformation_weights.csv"), row.names = FALSE)

fig <- patchwork::wrap_plots(panels[c("Raw densities", "Square root", "log10(count + 1)",
                                      "Wisconsin + square root (used)")], ncol = 2, guides = "collect") +
  patchwork::plot_annotation(
    title = "The same 881 transects under four transformations",
    subtitle = "Each NMDS rotated onto the one the app uses, so the panels share an orientation. Colors: basin.",
    theme = theme(plot.title = element_text(face = "bold"))) &
  theme(legend.position = "bottom")
ggsave(file.path(out_fig, "transformations_nmds.png"), fig, width = 11, height = 11.5, dpi = 200, bg = "white")
cat("Wrote", file.path(out_res, "transformations.csv"), "and", file.path(out_fig, "transformations_nmds.png"), "\n")
