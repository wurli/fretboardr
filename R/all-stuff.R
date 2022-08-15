# library(ggplot2)
# library(dplyr)
#
# chords <- list(
#   E = fretboard("E") +
#     points(
#       fngr("A", 2, "M"),
#       fngr("D", 2, "R"),
#       fngr("G", 1, "I")
#     ),
#   A = fretboard("A") +
#     points(
#       fngr("A", 7, "M"),
#       fngr("D", 7, "R"),
#       fngr("G", 6, "I")
#     ),
#   A_with_thumb = fretboard("A") +
#     points(
#       thumb("E1", 5),
#       fngr("A", 7, "R"),
#       fngr("D", 7, "P"),
#       fngr("G", 6, "M")
#     ),
#   B = fretboard("B") +
#     points(
#       fngr("A", 9, "M"),
#       fngr("D", 9, "R"),
#       fngr("G", 8, "I")
#     ),
#   B_with_thumb = fretboard("B") +
#     points(
#       thumb("E1", 7),
#       fngr("A", 9, "R"),
#       fngr("D", 9, "P"),
#       fngr("G", 8, "M")
#     ),
#   A_barre = fretboard("A") +
#     points(
#       barre(c("D", "G", "B"), 2, "R"),
#       fngr("E2", 2, shape = "cross"),
#       mute("E1")
#     ),
#   `F#m7` = fretboard("F#m7") +
#     points(
#       fngr("A", 4, "R"),
#       fngr("D", 4, "P"),
#       fngr("G", 2, "I")
#     ),
#   `F#m7_with_thumb` = fretboard("F#m7*") +
#     points(
#       thumb("E1", 2, "T"),
#       fngr("A", 4, "R"),
#       fngr("D", 4, "P"),
#       fngr("G", 2, "I")
#     ) +
#     labs(caption = "*technically an F#m7add4"),
#   `G#m7` = fretboard("G#m*") +
#     points(
#       fngr("A", 6, "R"),
#       fngr("D", 6, "P"),
#       fngr("G", 4, "I")
#     ) +
#     labs(caption = "*actually it's an Emaj7"),
#   `G#m7_with_thumb` = fretboard("G#m*") +
#     points(
#       thumb("E1", 4, "T"),
#       fngr("A", 6, "R"),
#       fngr("D", 6, "P"),
#       fngr("G", 4, "I")
#     ) +
#     labs(caption = "*actually it's a first-inversion Emaj7"),
#   `C#m7` = fretboard("C#m7*") +
#     points(
#       fngr("A", 11, "R"),
#       fngr("D", 11, "P"),
#       fngr("G", 9, "I")
#     ) +
#     labs(caption = "*actually more like an E6"),
#   `C#m7_with_thumb` = fretboard("C#m7") +
#     points(
#       thumb("E1", 9, "T"),
#       fngr("A", 11, "R"),
#       fngr("D", 11, "P"),
#       fngr("G", 9, "I")
#     )
# )
#
# # Pretty sweet!
# patchwork::wrap_plots(chords, nrow = 2)
#
# if (FALSE) {
#   purrr::iwalk(chords, function(plot, name) {
#     ggsave(
#       filename = paste0("outputs/", name, ".jpg"),
#       plot = plot,
#       device = ragg::agg_jpeg,
#       width = 600,
#       height = 1800,
#       dpi = 500,
#       units = "px",
#       bg = "white",
#       scale = 2
#     )
#   })
# }
