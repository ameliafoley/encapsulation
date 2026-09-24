##Run all figure files first: 2.1 PAH, 2.1 plate counts, 2.2 qpcr, 2.2 pah, 2.2 mass balance

# Figure output directory
fig_dir <- here("results", "aim 2")

# Create directory if it doesn't already exist
dir.create(fig_dir, recursive = TRUE, showWarnings = FALSE)

# Export settings
fig_width  <- 6.5
fig_height <- 4.5
fig_dpi    <- 600

## FIGURE 1 - FLA REMOVAL MONO
ggsave(
  file.path(fig_dir, "fla_mono.png"),
  fla_mono,
  width = fig_width,
  height = 4.0,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)

## FIGURE 2 - PHE REMOVAL MONO
ggsave(
  file.path(fig_dir, "phe_mono.png"),
  phe_mono,
  width = fig_width,
  height = 4.0,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)

## FIGURE 3 - NAP REMOVAL MONO
ggsave(
  file.path(fig_dir, "nap_mono.png"),
  nap_mono,
  width = fig_width,
  height = 4.0,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)

## FIGURE 4 - MONOCULTURE VIABILITY
ggsave(
  file.path(fig_dir, "cfu_mono.png"),
  cfu_mono,
  width = fig_width,
  height = fig_height,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)

## FIGURE 5 - FLA REMOVAL CONSORTIA
ggsave(
  file.path(fig_dir, "fla_cons.png"),
  fla_cons,
  width = fig_width,
  height = 4.75,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)

## FIGURE 6 - PHE REMOVAL CONSORTIA
ggsave(
  file.path(fig_dir, "phe_cons.png"),
  phe_cons,
  width = fig_width,
  height = 4.75,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)

## FIGURE 7 - NAP REMOVAL CONSORTIA
ggsave(
  file.path(fig_dir, "nap_cons.png"),
  nap_cons,
  width = fig_width,
  height = 4.75,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)

## FIGURE 8 - % MASS REMAINING
ggsave(
  file.path(fig_dir, "multi_panel.png"),
  multi_panel,
  width = fig_width,
  height = 7,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)

## FIGURE 9 - GENE COPIES ALL
ggsave(
  file.path(fig_dir, "qpcr_all_fig.png"),
  qpcr_all_fig,
  width = fig_width,
  height = 4.75,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)

## FIGURE 10 - GENE COPIES SHARED STRAINS
ggsave(
  file.path(fig_dir, "qpcr_shared.png"),
  qpcr_shared,
  width = fig_width,
  height = 4.25,
  units = "in",
  dpi = fig_dpi,
  bg = "white"
)
