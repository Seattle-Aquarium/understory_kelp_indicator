# Methods: understory kelp indicator analyses

## Purpose and scope

This document explains, in plain language, how the Understory Kelp Indicator turns Reef Check dive surveys into metrics, analyses and figures. It follows the order the Data Interpreter app runs them, and names the script behind each step so any number can be traced to its code.

1. **Clean data** (`rc_full_data_clean.R`): the four raw Reef Check exports become one table with one row per transect.
2. **Build metrics**: average density and its spread at four spatial scales (`metrics_across_scale.R`); the temporal trend (`temporal_trend.R`); an ordination of the algae community (`nmds_algae.R`); how substrate relates to that community (`substrate_algae.R`); and a model of each alga against each substrate class (`glmm_substrate.R`).
3. **Data** (`figure_functions.R`, `visualizations.R`): every figure, previewed in the app or written to the `figs/` folder.

## The data

The analyses use 899 Reef Check transects at 42 sites in seven Puget Sound basins, surveyed from 2021 to 2025. The survey grew quickly: 30 transects in 2021, 144 in 2022, and 222 to 252 a year since.

Each transect is a 30 m × 2 m strip (60 m²). A site survey normally has six transects, numbered 1 to 6; transects 1–3 are offshore and 4–6 inshore, which the analyses call the depth zone. Of the 150 site-years, 149 have all six transects and one has five.

**What divers record.** Each dive produces four datasheets, exported as four files:

- **Algae:** the number of individuals of each kelp and other large brown alga on the transect.
- **Uniform point contact (UPC):** what lies under about 30 evenly spaced points along the transect — the substrate (bedrock reef, boulders, rock, cobble, pebble, sand, clay, shell hash), what covers it, its relief (how high the bottom rises there, in four classes: 0–10 cm, 10 cm–1 m, 1–2 m, over 2 m) and anything lying over the cover (the superlayer).
- **Invertebrates and fish:** counts of each species on the datasheet.

**Cleaning** (`rc_full_data_clean.R`) runs these steps in order:

1. The nine Canadian sites are removed, and one misspelled site name is corrected (“Elliot” to “Elliott Bay Marina”).
2. Two winter 2025 dives are dropped (Elliott Bay Marina on 28 January and Sirens of Spring on 27 January), so every site has one survey per year. Both sites were dived again that August.
3. Each site takes its basin from a fixed table of 57 sites in ten basins; a site not in the table is dropped.
4. Northern Coast and the Western and Eastern Strait of Juan de Fuca are withheld by default. Checkboxes in the app bring any of them back.
5. Site coordinates come from the UPC, invertebrate and fish files, which agree with each other; the algae file had errors.
6. Fish depths are matched to the other three files' depth for the same dive. Only fish transects 1–6 are kept, since those share their numbering with the other files.
7. A count made over less than the full 30 m (a dense patch counted over part of the transect) is scaled up to 30 m: count × 30 ÷ distance counted, rounded to a whole individual. This applies to algae and invertebrates.
8. Each UPC value becomes a share: the points in that class divided by the points actually read on that transect (usually 30, but 24 to 42 on some).
9. Every dive gets one key — basin, site, transect and year — shared by all four files, so they join into one row per transect.

**Zero versus blank.** The datasheets changed over the years: the UPC sheet gained the superlayer in 2022, clay in 2023 and acid weed in 2024, and the invertebrate and fish sheets gained species in 2022–2023. A category that was on the sheet but not seen is recorded as 0. A category that was not yet on that survey's sheet is left blank, so a change of datasheet is never read as a decline or an arrival.

**From counts to density** (`metrics_across_scale.R`). A density is individuals per m²: the transect's count divided by 60. UPC values stay as shares of points, from 0 to 1.

Ten understory species are analysed: acid weed (*Desmarestia* spp.), broad-ribbed kelp (*Pleurophycus gardneri*), five-ribbed kelp (*Costaria costata*), sieve kelp (*Agarum clathratum* and *Neoagarum fimbriatum*), sugar kelp (*Saccharina latissima*), three-ribbed kelp (*Cymathaere triplicata*), torn kelp (*Laminaria setchellii*), winged kelp (*Alaria marginata*), wire weed (*Sargassum muticum*) and woody kelp (*Pterygophora californica*). The canopy-forming kelps (bull, giant and feather boa) stay in the tables but are left out of every figure and analysis, since they fall outside an understory indicator.

## Average density, variance and standard deviation

Every density and UPC share is averaged up three steps — transect to site, site to basin, basin to Puget Sound — one year at a time (`metrics_across_scale.R`). Each average is stored with the standard deviation and variance of the values it averaged.

| Scale | One value per | The mean of | Each input counts |
| --- | --- | --- | --- |
| Transect | transect | — (the transect's own value) | — |
| Site | site and year | that site's transects that year (usually six) | equally |
| Basin | basin and year | that basin's site means that year | equally, however many transects a site had |
| Puget Sound | year | that year's basin means | equally, however many sites a basin had |

**Means of means.** A basin's value is the mean of its site means, not of all its transects, so every site weighs the same in its basin and every basin the same in Puget Sound. A basin with one heavily surveyed site is not dominated by it.

**Spread.** The standard deviation and variance at each scale describe how much the units one level down differ from each other: transects within a site, sites within a basin, basins within Puget Sound. Both use the usual sample formula, dividing by n − 1. With a single unit (one site in a basin that year) there is no spread to estimate, and both are left blank.

**Missing values.** A blank (a category not on that survey's sheet) is left out of both the total and the count; it is never treated as zero. A category with no values at all at a scale is blank there too.

**Precision.** Nothing is rounded between scales. The saved tables are trimmed to six significant figures only when written.

**Puget Sound and Washington-wide.** Puget Sound always means the seven inner basins. When a withheld outer basin is brought back in, a second region, Washington-wide, averages every basin present.

### The density figures

The app draws each average over the years in one of two forms:

- **Average density:** the blue line is the mean at the chosen scale; gray lines are the units behind it (transects behind a site, sites behind a basin, basins behind Puget Sound).
- **Average density with standard deviation:** the same blue line inside a band of ± 1 standard deviation among those units. The band replaces the gray lines, and its lower edge stops at zero because a density cannot be negative.

The band shows how much the units differ from one another, not how precisely the mean is known. The figures do not draw a standard error, which would be the standard deviation divided by √n (n being the number of units averaged) and so narrower by that factor.

Three views lay the same values out differently: one place with a panel per species, one species with a panel per place, or one place with every species on one set of axes (the last without bands, which would bury each other). A switch locks every panel to one y-axis range so they can be compared by eye.

Four details keep the figures honest:

- **Gaps:** a year with no value (the category was not on that year's sheet) breaks the line rather than being bridged.
- **Who is behind each year:** the subtitle lists how many units each year averages, for example “Sites per year: 2022 (1) · 2023 (2)”. Many sites joined in 2023–2024, so part of a change between years can come from which sites were surveyed.
- **Presence:** each panel title ends with, for example, “(2 of 3 sites)”: the units where the category was recorded at least once, of those surveyed.
- **Data types:** algae are drawn as individuals per m²; the four UPC groups (cover, relief, substrate, superlayer) as shares of points.

## Temporal trend

The temporal trend expresses each year's density as a percentage of the same place's mean over up to five preceding years: 100% means no change, 50% half and 200% double (`temporal_trend.R`, following Frieder et al. 2025). It is computed for every year with at least one earlier year to compare against, so it forms a time series rather than a single number.

**At a site.** For species *s* at site *i* in year *t*:

$$
K_{s,i,t} = 100 \times \frac{D_{s,i,t}}{\frac{1}{m}\sum_{k \in B} D_{s,i,t-k}}
$$

Here *D* is the site's annual mean density (the mean of that year's transects), and the baseline in the denominator averages the *m* years, among the five before *t*, in which the site has a value for that species. The survey began in 2021, so a 2025 value rests on at most four baseline years, and a site first surveyed in 2024 has only one; every value records how many it used.

**Zeros.** A ratio breaks down when either side is zero, so every value carries a status:

| Status | This year | Baseline | Trend value |
| --- | --- | --- | --- |
| present | above 0 | above 0 | an ordinary percentage |
| lost | 0 | above 0 | 0%, a real decline, kept |
| new | above 0 | 0 | none (it would be infinite) |
| absent | 0 | 0 | none (nothing to compare) |

**Basins and Puget Sound: a ratio of averages.** A basin's trend is 100 × the mean of its sites' current densities ÷ the mean of the same sites' baselines. Only sites with both a current value and a baseline enter either mean, so this year's sites are never compared with a different set of earlier ones. Puget Sound repeats the step over basins, each basin weighted equally.

Averaging the site percentages instead would let one site with a near-zero baseline swamp the result. In 2025 the site values for understory algae run from 0% to 2,550%; in the San Juan Islands, winged kelp averages 521% as a mean of 11 site percentages, while the basin's density across those same sites is 102% of their baseline. The ratio of averages also lets lost and new sites add their zeros to one side, rather than being dropped. The mean of site percentages is still written to the tables for comparison.

### The trend figures

- **Axis:** a doubling (log) scale centred on 100%, so 50% and 200% sit the same distance from “no change”, which is marked by a dashed line.
- **Lost and new:** triangles on the bottom edge mark lost; on the top edge, new. At the basin and Puget Sound scales, the number beside a triangle is how many sites or basins it stands for.
- **Extremes:** values beyond 6.25% and 1,600% (four doublings either side of 100%) are drawn at those limits so the rest stays readable; blue ones are labelled with their real value.
- **Lines:** at a site, one line; at a basin, the blue line is the basin and gray lines its sites; at Puget Sound, gray lines are the basins. A line joins only consecutive years with an ordinary percentage, so it never runs through a lost, new or absent year.

## NMDS of the understory algae community

The NMDS places each transect on a two-dimensional map so that transects with similar mixes of understory algae sit close together (`nmds_algae.R`). It shows composition — which species, in what proportions — rather than abundance, which the density and trend metrics already cover.

**What NMDS does.** Non-metric multidimensional scaling starts from how different every pair of transects is, then searches for an arrangement of points whose distances keep the same rank order: the most different pair farthest apart, and so on down. It uses only that order, not the sizes of the differences, so it assumes little about the data.

**Input.** 881 transects: every transect with an algae survey and at least one understory alga, described by the ten species' densities per m². Fifteen transects with no understory algae and three without an algae survey cannot be placed; they are counted and reported rather than silently dropped.

**Transformation.** Each density is first square-rooted, which damps the influence of the most abundant species. A Wisconsin double standardization follows: each species is divided by its largest value on any transect, then each transect by its own total, so every transect's values sum to one. Without it, sugar kelp (on 87% of transects, at up to 20 per m²) dominated every comparison, and the two-dimensional map fitted worse (stress 0.196 against 0.160).

**Dissimilarity.** Two transects *j* and *k* are compared with the Bray–Curtis dissimilarity, which runs from 0 (identical composition) to 1 (no species in common):

$$
BC_{jk} = \frac{\sum_{s} \lvert x_{sj} - x_{sk} \rvert}{\sum_{s} \left( x_{sj} + x_{sk} \right)}
$$

**Fitting.** The search starts from a random arrangement and can settle on a poor one, so it is repeated from many random starts and the best kept: two rounds of at least 100 random starts for the two-dimensional map, the second beginning from the first round's best (216 tries in all, since the starts run in batches, one per processor core). Solutions in one, three and four dimensions are fitted too, to show how many dimensions the data need. A fixed random seed makes a re-run on the same computer reproduce the same map; a computer with a different number of cores runs different batches and may settle on an equally good but slightly different one. The finished map is centred and rotated so its first axis carries the most spread (the `metaMDS` function in the R package vegan).

**Stress.** Stress measures how far the map's distances depart from the rank order of the dissimilarities; lower is better. A common rule of thumb (Clarke 1993) reads below 0.1 as good, below 0.2 as usable and above 0.3 as close to arbitrary.

| Dimensions | Stress |
| --- | --- |
| 1 | 0.294 |
| 2 | 0.160 |
| 3 | 0.105 |
| 4 | 0.077 |

The figures use the two-dimensional map: good enough for the broad pattern, but fine detail should not be over-read. With 881 transects the very best arrangement is seldom found twice, so the map is the best of 216 tries rather than one confirmed by repetition. The three-dimensional positions are stored too.

**Species arrows.** Each species' position is the average of the transect positions, weighted by that species' transformed values on each transect. An arrow from the centre points to it, so transects lying in that direction carry more of that species.

### The NMDS figures

- **Points:** each is one transect. The two axes (NMDS1 and NMDS2) share one scale but have no units or fixed meaning; only relative positions matter.
- **Color by:** basin, site, year or depth zone. With site, one basin's sites are coloured and the rest gray; with basin, *Highlight* can pick out one basin the same way.
- **Ellipses:** for each group of five or more transects, a 95% ellipse — the region that would hold 95% of the group's transects if their positions followed a two-dimensional normal distribution.
- **Size by:** points can be sized by one species' density on the transect, or by all understory algae together.
- **Substrate vectors:** blue arrows for substrate, fitted onto the map as the next section describes.

## Substrate and the algae community

Substrate explains about a fifth of the variation in algae composition among transects (18.8%), mostly through differences between sites; within a site, transects with more or less hard or soft bottom differ only slightly (`substrate_algae.R`). The community is exactly the one the NMDS maps: the same transects, transformation and Bray–Curtis dissimilarities.

**Substrate as predictors.** The UPC substrate classes are shares of a transect's points and sum to one, so they are collapsed into three groups, defined once in `substrate_classes.R` and shared with the GLMMs:

- **hard:** bedrock reef, large and small boulder, and rock;
- **mixed:** cobble and pebble;
- **soft:** sand, clay and shell hash.

“Other” substrate (0.3% of points) is left out. Relief enters as one number, the transect's mean relief in metres: each point counts at the midpoint of its relief class (0.05, 0.55 and 1.5 m, and 2.5 m for over 2 m). A transect with 40%, 50%, 10% and 0% of its points in the four classes has a mean relief of 0.4 × 0.05 + 0.5 × 0.55 + 0.1 × 1.5 = 0.445 m.

Since the three shares sum to one, a model can hold only two of them. Mixed is left out as the reference, so hard and soft read as more hard (or soft) bottom at the expense of mixed; with all three in, the variance inflation factors run to 160–230, and with two they stay under 3.

**Two questions, two tests.** Transects at one site share its location, exposure and history, so they are not independent, and no test shuffles them freely across sites. Each question gets its own permutation scheme (999 permutations, with a fixed random seed):

- **Between sites:** do sites with more hard substrate, say, carry different algae? Each site is averaged over all its transects and years, and sites are shuffled freely.
- **Within sites:** at the same site, do transects with more hard substrate carry different algae? Transects are shuffled only among their own site's.

A permutation test asks how often a result as strong as the one observed arises once the link between algae and substrate is broken by shuffling. A p of 0.001 means none of the 999 shuffles did as well.

### Substrate vectors on the NMDS

Each substrate variable is fitted onto the NMDS map as an arrow (the `envfit` function in vegan): the direction across the map in which it increases most steeply, with r² saying how well the transects' positions predict it. Each variable is fitted on its own, so the individual classes can be drawn together even though their shares sum to one. On the figures, an arrow's length is proportional to the square root of its r², and every arrow is stretched by one shared factor to fill the plot.

| Vector | r² across transects | p, between sites | p, within sites |
| --- | --- | --- | --- |
| **Groups** |  |  |  |
| hard substrate | 0.37 | 0.001 | 0.059 |
| mixed substrate | 0.04 | 0.066 | 0.142 |
| soft substrate | 0.20 | 0.001 | 0.265 |
| mean relief | 0.30 | 0.001 | 0.010 |
| **Individual classes** |  |  |  |
| reef | 0.33 | 0.001 | 0.006 |
| large boulder | 0.11 | 0.001 | 0.423 |
| small boulder | 0.08 | 0.003 | 0.641 |
| rock | 0.02 | 0.260 | 0.022 |
| cobble | 0.04 | 0.089 | 0.052 |
| pebble | 0.04 | 0.036 | 0.100 |
| sand | 0.18 | 0.001 | 0.123 |
| shell hash | 0.02 | 0.188 | 0.131 |

Reef carries most of the hard group's signal, and sand most of the soft group's. Only reef and mean relief also hold within sites; the boulders separate one site from another but not transects at the same site.

### db-RDA: the community constrained by substrate

Distance-based redundancy analysis (db-RDA) asks how much of the variation in the Bray–Curtis dissimilarities substrate can explain, much as a regression's R² does for a single response. The dissimilarities are turned into coordinates, those are regressed on hard, soft and mean relief, and the fitted part is ordinated, so the axes show only what substrate can explain. Year is partialled out first, so differences between years cannot pass for substrate effects.

- **All transects:** substrate explains 18.8% of the variation (adjusted R² 18.6%). Axis 1 holds nearly all of it (17.7% of the total, against 1.0% on axis 2), so the figure reads left to right: soft bottoms with sugar kelp and acid weed on the left, hard, high-relief reef with sieve kelp on the right.
- **Between sites:** substrate explains 31% of the differences among the 42 site means (adjusted 26%; F = 5.7, p = 0.001). Each term, given the other two: hard p = 0.007, soft p = 0.37, relief p = 0.025.
- **Within sites:** with site and year partialled out, substrate explains 1.4% of the variation among a site's transects (F = 4.0, p = 0.001). Each term: hard p = 0.032, soft p = 0.004, relief p = 0.037.

**Variation partitioning.** The variation is split into what substrate, basin and year explain alone and together (adjusted R²):

| Fraction | Share of the variation |
| --- | --- |
| substrate alone | 5.0% |
| basin alone | 18.9% |
| year alone | 0.2% |
| substrate and basin together | 13.6% |
| basin and year together | 0.3% |
| substrate and year together | 0.1% |
| all three together | 0.7% |
| unexplained | 61.1% |

Most of what substrate explains (13.6 of its 19.4 percentage points) is shared with basin. Basins differ in their substrate, so the two cannot be fully told apart.

### The db-RDA figures

Each point is one transect, placed where its algae put it: the weighted average of the species' positions. The axes are scaled so that distances between transects are kept (vegan's scaling 1); the default scaling stretched the weak second axis to 2.5 times the cloud's informative width. Blue arrows are the substrate terms and black arrows the species, each set stretched to fit; colouring, *Highlight*, ellipses and point sizes work as on the NMDS.

## GLMMs: each alga against each substrate class

For each understory alga and each substrate class, a negative binomial mixed model (GLMM) estimates how the species' count changes with more of that class, separately between sites and within sites (`glmm_substrate.R`). Where the ordinations describe the whole community, these models answer for one species at a time, for example how many more sieve kelp are found where there is more hard substrate.

The models use 894 transects at 42 sites: every transect with an algae count, a UPC survey and a recorded depth (three transects lack one). Recorded depths run from 0 to 62 ft, with a median of 14.5 ft.

### Why a negative binomial mixed model

- **Counts.** Kelp counts are whole numbers that cannot fall below zero: many transects have none of a species, and a few have hundreds. Ordinary regression assumes values scatter symmetrically around an average with the same spread at every level, and counts do neither; a generalized linear model is built for them.
- **Effects that multiply.** The model works on a log scale, so more substrate multiplies the expected count (doubling it, say) rather than adding a fixed number. This keeps every prediction above zero and suits species whose numbers range over orders of magnitude.
- **Clumped counts.** The simplest count model, the Poisson, assumes counts vary about as much as their average. Kelp grow in patches, so counts vary far more; the negative binomial allows for this and estimates how much from the data, as the ROV–diver comparison report did.
- **Transects that are not independent.** Transects at one site share water, exposure and history, and most sites were surveyed in several years. A random intercept for site gives every site its own baseline level, which is what makes the model “mixed”.
- **Year and depth.** Each year gets its own adjustment, and depth enters as a straight-line effect on the log scale, per 10 ft, so differences between years (including which sites were surveyed) and between shallower and deeper transects are not mistaken for substrate effects. Depth is used in feet rather than as the inshore/offshore depth zone, because at many sites the two sets of transects run on into each other rather than forming distinct groups.

### One substrate class at a time

A transect's substrate shares add up to 100%, so with every class in one model their effects could not be told apart. Each model therefore holds a single class and asks whether the species becomes more or less common as that class covers more of the bottom. Since the shares are linked, more of one class always means less of the others.

Two sets were modelled: the three groups plus mean relief (four predictors) and the eight individual classes. With ten species, that makes 120 species–class combinations. Effects are given per +10 percentage points of the transect's UPC points (for example, from 30% to 40% sand), and per +0.25 m for mean relief.

### Between sites and within sites

A share varies in two ways: some sites are mostly rock and others mostly sand, and at one site one transect can be sandier than its neighbours. These are different questions. Between sites asks which kinds of sites a species lives at; within sites asks where at a site it grows, with the site's water, exposure and history held constant.

A model with a single substrate term blends the two answers, weighting each by how precisely the data pin it down. The roughly 850 within-site comparisons are far more numerous than the 42 sites, so in these data a single term was about 95% the within-site answer. Each share is therefore split into two predictors, entered together in one model (van de Pol and Wright 2009):

- **Between:** the site's average share over all its transects and years, the same for every transect at that site.
- **Within:** the transect's share minus its site's average, positive for a transect with more of the class than usual there.

The response does not change: it is still each transect's own count. The six 2024 transects at South Shaw Island show the split; their site average comes from all 12 transects surveyed there.

| Transect | Soft share (%) | Between: site average (%) | Within: departure (points) | Sugar kelp count |
| --- | --- | --- | --- | --- |
| 6 | 3 | 46.7 | −43.3 | 38 |
| 5 | 33 | 46.7 | −13.3 | 61 |
| 4 | 47 | 46.7 | 0.0 | 61 |
| 1 | 100 | 46.7 | +53.3 | 5 |
| 2 | 100 | 46.7 | +53.3 | 9 |
| 3 | 100 | 46.7 | +53.3 | 20 |

The split mattered: for sugar kelp and soft substrate, and for sieve kelp and hard substrate, the two answers differed significantly (p = 0.013 and p < 0.001). A single-term version of every model was fitted too, for comparison.

### The model

For each species and class, the expected count on transect *i* at site *j* is a baseline multiplied by five factors: the site's average share, the transect's departure from it, its depth, the year and the site's own level. On the log scale these add up:

$$
\log(\mu_{ij}) = \beta_0 + \beta_B \, \bar{x}_j + \beta_W \, (x_{ij} - \bar{x}_j) + \beta_D \, \mathrm{depth}_i + \gamma_{\mathrm{year}(i)} + u_j
$$

| Term | What it is | In plain words |
| --- | --- | --- |
| μ*ij* | expected count on transect *i* at site *j* | what the model predicts for that transect |
| β0 | intercept | the baseline count |
| βB × site average | between-site term | how counts change with the site's average share |
| βW × departure | within-site term | how counts change when a transect has more or less than its site's average |
| βD × depth | depth | how counts change per 10 ft deeper (depth in feet, minus 15, divided by 10) |
| γ year | year | one adjustment per survey year, 2021–2025 |
| u*j* | site random intercept | the site's own level, above or below a typical site |

The observed count varies around μ following a negative binomial distribution whose extra-variation parameter is estimated from the data. The site levels are assumed to scatter around zero like a bell curve, with the model estimating how widely. In R: `count ~ x_between + x_within + depth + year + (1 | site)`, fitted with glmmTMB, family `nbinom2`.

### Reporting

**Rate ratios.** Each coefficient is reported as a rate ratio: the factor by which the expected count changes for +10 percentage points of the class (+0.25 m for relief). A rate ratio of 1 means no change, 2 twice as many and 0.5 half as many. At sites whose transects average 10 points more hard substrate, sieve kelp counts were 2.5 times higher (95% confidence interval 1.9–3.2); within a site, a transect with 10 points more hard substrate than its site's average carried 1.10 times as many (1.04–1.17).

**Many tests at once.** Ten species, twelve classes and two terms make 240 estimates, so a few would look significant by chance. The p-values are adjusted with the Benjamini–Hochberg procedure, which holds the expected share of false positives among results called significant at 5%; a result counts as significant when its adjusted p-value, q, is below 0.05.

### The GLMM figures

- **Heatmaps:** every rate ratio for one set of classes, coloured on a doubling scale (blue more, red fewer), with stars for q below 0.05, 0.01 and 0.001. Between sites and within sites can be drawn on their own or stacked; both halves share the colour scale, so they compare directly.
- **Between sites, one species and one class:** one point per site at its average share and average density over all its transects and years, sized by how many transects that average spans (12–30). The line is the expected density at a typical (median) site, averaged over the depths and years of the transects in the years the species was recorded.
- **Within sites, one species and one class:** the line is the model's change in a transect's count relative to its site's typical transect, over the middle 95% of departures, with its 95% confidence band.

**The binned points behind the within-site line.** Single transects cannot be drawn on that axis: a transect with none of the species has no place on a log scale, and transects at one site vary 10- to 100-fold, which would bury the line. Instead, each transect's count is set against what the model expects for its site's typical transect at that year and depth — everything in the model except the within-site term.

1. Transects across the line's range (the middle 95% of departures) are cut into ten steps of equal width.
2. Each point is one step's total count divided by its total expected count, placed at the step's mean departure and sized by how many transects it holds. Zeros count as they are.
3. The 95% interval comes from resampling the step's sites 999 times with replacement, since transects at one site are not independent.
4. Steps with fewer than 10 transects or 3 sites are left out. A step off the axis — below 1/16× (including one with none of the species) or above 16× — is drawn as an open triangle on the edge it passed.

If the model describes the data well, the points scatter around the line. For sieve kelp and hard substrate they climb with it; for sugar kelp and reef they sit around 1×, matching its flat line. For patchy species the points are noisier, since a few transects where the model expects large counts can dominate a step's total; their wide intervals show it. Torn kelp, on fewer than 5% of transects, is not binned at all.

### Model checks and sensitivity

The two highlighted models, sugar kelp with soft substrate and sieve kelp with hard substrate, were checked by simulating 500 data sets from each fitted model and comparing them with the real data; the other 118 were not checked one by one.

- **Sieve kelp** matched well: 55% of transects had none, against 49% in the simulations (p = 0.12), and the spread of counts was consistent with the model (p = 0.35).
- **Sugar kelp** had more zeros than expected (14% of transects against 6%), and the spread of counts did not fit (p < 0.001); most of the extra zeros came from Hood Canal, where 69% of transects had none. A zero-inflated version, which lets an extra share of transects have none at all, fitted better (AIC 45 lower). It gave the same between-site result (1.20×, p = 0.02) and turned the within-site trend into a small decline (0.96×, p = 0.01).

Both models were refitted twice, once leaving depth out and once adding basin as a second random intercept. The rate ratios barely moved:

| Model | Sieve kelp × hard: between | Sieve kelp × hard: within | Sugar kelp × soft: between | Sugar kelp × soft: within |
| --- | --- | --- | --- | --- |
| As reported | 2.47 | 1.10 | 1.20 | 0.97 |
| without depth | 2.50 | 1.09 | 1.22 | 0.95 |
| + basin as a random intercept | 2.36 | 1.10 | 1.21 | 0.97 |

Depth itself matters for several species: for every 10 ft deeper, sugar kelp fell to 0.50×, wire weed to 0.08×, and winged, woody and three-ribbed kelp to roughly 0.25–0.47×. Of the 240 fits, 238 finished without warnings. Of the two that warned, three-ribbed kelp with shell hash is marked † in the figures; the other is broad-ribbed kelp's single-term model with shell hash, which no figure draws.

### Caveats

- **Association, not cause.** These are field observations, not experiments. A between-site rate ratio describes the kinds of sites a species lives at, where substrate comes bundled with currents, exposure, water clarity and basin; within-site ratios hold the site constant and are the closer guide to substrate's own effect.
- **Why between-site ratios run larger.** Much of the gap is presence versus abundance across sites. Below about 40% hard substrate, sieve kelp is usually present but at trace densities (site medians 0.003–0.008 per m²), against 1.2–1.7 per m² above it. Within a site, spores probably spread across substrate patches and blur local differences.
- **Large ratios for scarce classes.** Few sites average more than 10% large or small boulder, so +10 points of a site's average is a big step for those classes; their large between-site ratios (up to 28×) show direction, not a reliable size.
- **Rare species.** Torn kelp (3% of transects), three-ribbed kelp (7%) and winged kelp (9%) rest on few sites, and their estimates are uncertain even when significant.
- **Sampling noise.** With about 30 points per transect, each share is itself an estimate. That noise flattens within-site slopes by roughly 10–20%, too little to explain the gap between the two.
- **Data notes.** In 2021 the 30 transects recorded almost no reef or shell hash and much more cobble than later years, probably a datasheet or training change that the year term absorbs. No 2021 transect had sieve kelp, so that year's adjustment cannot be estimated for it; the substrate terms are unaffected. Fourteen transects are recorded at under 3 ft (two at 0 ft); they are used as recorded.

## Software, code and reproducibility

Everything runs in R 4.6.1 inside the Data Interpreter's own environment, with packages from one fixed day's CRAN snapshot (1 October 2026), so every computer runs the same versions: vegan 2.7.6 for the NMDS, vectors and db-RDA; glmmTMB 1.1.15.2 for the GLMMs; dplyr and tidyr for the tables; ggplot2, patchwork and ggrepel for the figures; shiny and bslib for the app.

| Step | Script | Writes, in `results/` |
| --- | --- | --- |
| 1. Clean data | `code/rc_full_data_clean.R` | `reef_check_cleaned.csv` |
| 2. Density and spread | `code/metrics_across_scale.R` | `metrics_*_scale.csv` (transect, site, basin, Puget Sound) |
| 2. Temporal trend | `code/temporal_trend.R` | `trend_*_scale.csv` |
| 2. NMDS | `code/nmds_algae.R` | `nmds_algae_points.csv`, `_species.csv`, `_stress.csv` |
| 2. Substrate vectors and db-RDA | `code/substrate_algae.R` | `nmds_algae_envfit.csv`, `dbrda_algae_*.csv` |
| 2. GLMMs | `code/glmm_substrate.R` | `glmm_algae_terms.csv`, `_curves.csv`, `_sites.csv`, `_bins.csv`, `_summary.csv` |
| 3. Figures | `code/figure_functions.R`, `code/visualizations.R` | `figs/<analysis>/<scale>/<view>/<data type>/` |

- **Shared definitions.** The substrate groups, the individual classes and the relief midpoints are defined once, in `code/substrate_classes.R`, and used by both the db-RDA and the GLMMs.
- **Fixed random seeds.** The NMDS, the permutation tests and the binned intervals each use a fixed seed, so a re-run gives the same results (for the NMDS, on a computer with the same number of cores).
- **Re-run only when needed.** The NMDS, the substrate analyses and the GLMMs take minutes, so each records a fingerprint of its inputs and settings and is reused until they change.
- **Checks run by hand.** The transformation comparison, the predictor sets, the between-versus-within figure and the GLMM checks and sensitivity analyses are in `code/supplementary/`, with outputs in `results/supplementary/` and `figs/supplementary/`.
- **The rules, for checking.** `INVARIANTS.md` in the repository lists every rule above in a form a person or an agent can check a new version of the code against.

## References

Frieder et al. (2025) is taken from the repository's temporal trend note (`documents/temporal_trend_figure`); the others are cited from memory, so check their details before publication.

- Benjamini, Y., and Y. Hochberg. 1995. Controlling the false discovery rate: a practical and powerful approach to multiple testing. Journal of the Royal Statistical Society, Series B 57:289–300.
- Borcard, D., P. Legendre, and P. Drapeau. 1992. Partialling out the spatial component of ecological variation. Ecology 73:1045–1055.
- Brooks, M. E., K. Kristensen, K. J. van Benthem, A. Magnusson, C. W. Berg, A. Nielsen, H. J. Skaug, M. Mächler, and B. M. Bolker. 2017. glmmTMB balances speed and flexibility among packages for zero-inflated generalized linear mixed modeling. The R Journal 9:378–400.
- Clarke, K. R. 1993. Non-parametric multivariate analyses of changes in community structure. Australian Journal of Ecology 18:117–143.
- Frieder, C. A., T. W. Bell, H. Berry, K. Cavanaugh, D. C. Claar, J. Freiwald, B. Grime, S. Hamilton, H. F. Houskeeper, N. Lombardo, S. Marion, T. A. McHugh, G. McKenna, P. E. Parnell, P. Spector, and S. B. Weisberg. 2025. Developing a status and trends assessment for floating kelp canopies across large geographic areas. Environmental Science & Technology. [doi:10.1021/acs.est.5c07501](https://doi.org/10.1021/acs.est.5c07501)
- Legendre, P., and M. J. Anderson. 1999. Distance-based redundancy analysis: testing multispecies responses in multifactorial ecological experiments. Ecological Monographs 69:1–24.
- Oksanen, J., and others. vegan: Community Ecology Package. R package version 2.7.6.
- van de Pol, M., and J. Wright. 2009. A simple method for distinguishing within- versus between-subject effects using mixed models. Animal Behaviour 77:753–758.
