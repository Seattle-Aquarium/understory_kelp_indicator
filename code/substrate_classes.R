## The UPC substrate classes, and how the analyses group them. Sourced by
## substrate_algae.R and glmm_substrate.R, so the db-RDA, the NMDS's substrate
## vectors and the GLMMs all use one definition -- change a group here and every
## one of them follows (each re-runs, since its fingerprint covers the shares).
##
## Reef Check's Washington UPC substrate codes, by size:
##   pebble 0.5-5 cm, cobble 5-15 cm, rock 15-25 cm, small boulder 25-50 cm,
##   large boulder 50 cm-1 m, reef (bedrock); sand, clay, shell hash; other.
## "boulder" and "cobble" without -wa are the older California codes, kept in
## case a table carries them.

## the three gradients the grouped analyses use; their shares sum to one, so
## the db-RDA leaves mixed out as the reference
substrate_groups <- list(
  hard  = c("sb_reef", "sb_l_boulder-wa", "sb_s_boulder-wa", "sb_rock", "sb_boulder"),
  mixed = c("sb_cobble-wa", "sb_pebble-wa", "sb_cobble"),
  soft  = c("sb_sand", "sb_clay-wa", "sb_shell_hash-wa"))

## the individual classes the GLMMs model one at a time, in the order the
## figures show them. Clay (first recorded in 2023, on 19 of 721 transects
## since) and "other" (0.3% of points) are too scarce to model.
substrate_individual <- c("reef" = "sb_reef", "large boulder" = "sb_l_boulder-wa",
                          "small boulder" = "sb_s_boulder-wa", "cobble" = "sb_cobble-wa",
                          "rock" = "sb_rock", "pebble" = "sb_pebble-wa", "sand" = "sb_sand",
                          "shell hash" = "sb_shell_hash-wa")

## relief as one number: the mean relief of the transect's points, in metres,
## from the class midpoints
relief_midpoints_m <- c("rf_0-10cm" = 0.05, "rf_10cm-1m" = 0.55, "rf_1m-2m" = 1.5,
                        "rf_over_2m" = 2.5)

## A group's share of each row's points. NA only when none of its columns was
## recorded (no UPC on that transect); a class missing from the datasheet that
## year counts as nothing rather than blanking the whole group.
substrate_share <- function(df, cols) {
  m <- as.matrix(df[intersect(cols, names(df))])
  out <- rowSums(m, na.rm = TRUE)
  out[rowSums(!is.na(m)) == 0] <- NA
  out
}

mean_relief <- function(df) {
  as.vector(as.matrix(df[names(relief_midpoints_m)]) %*% relief_midpoints_m)
}
