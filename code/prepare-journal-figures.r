# JOURNAL FIGURES
#
# PURPOSE
# -------
# This script takes the final PDF figures generated for the
# manuscript and reformats them to the dimensions required
# by the target journal.
#
# WORKFLOW
# --------
# The figures are:
#   1. Read from the local `images/original/` directory
#   2. Trimmed from the bottom by a figure-specific amount
#   3. Rescaled to a fixed width of 174 mm
#   4. Saved to `images/journal/`
#
# NOTE
# ----
# `images/original/` contains the original input figures and is
# intentionally excluded from version control. The directory
# therefore does not exist in the GitHub repository and must be
# created locally when reproducing the workflow.
#
# Place the required input PDFs in `images/original/` before
# running this script. The expected filenames are defined in
# `figures` below.
#
# The journal-ready output figures are saved in
# `images/journal/` and are tracked in the repository.
#

pdflatex <- Sys.which("pdflatex")

input_dir  <- "images/original"
output_dir <- "images/journal"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

target_width_mm <- 174

# Ændr kun denne værdi:
# 0  = ingen trim
# 50 = fjern 50 mm fra BUNDEN
trim_bottom_mm <- c(
  figure_1 = 40,
  figure_2 = 10,
  supplementary_figure_1 = 20
)


figures <- c(
  figure_1 = "figure_1_final_corrected.pdf",
  figure_2 = "figure_2_final_corrected.pdf",
  supplementary_figure_1 =
    "supplementary_figure_1_forest_plot_final.pdf"
)


# ============================================================
# FUNCTION
# ============================================================

make_figure <- function(input_file, output_file, trim_bottom_mm) {

  ps <- pdftools::pdf_pagesize(input_file)

  w_mm <- ps$width[1]  * 25.4 / 72
  h_mm <- ps$height[1] * 25.4 / 72

  cropped_h_mm <- h_mm - trim_bottom_mm

  scale <- target_width_mm / w_mm
  final_h_mm <- cropped_h_mm * scale

  cat("\n", basename(input_file), "\n")
  cat("Original: ",
      round(w_mm, 2), " x ",
      round(h_mm, 2), " mm\n")

  cat("Removing from BOTTOM: ",
      trim_bottom_mm, " mm\n")

  cat("Final: ",
      target_width_mm, " x ",
      round(final_h_mm, 2), " mm\n")


  td <- tempfile("journal_pdf_")
  dir.create(td)

  tex_file <- file.path(td, "figure.tex")

  tex <- paste0(
    '\\documentclass{article}
\\usepackage{geometry}
\\usepackage{pdfpages}

\\geometry{
  paperwidth=', target_width_mm, 'mm,
  paperheight=', final_h_mm, 'mm,
  margin=0mm
}

\\pagestyle{empty}

\\begin{document}

\\includepdf[
  pages=1,
  pagecommand={},
  width=', target_width_mm, 'mm,
  trim=0mm ', trim_bottom_mm, 'mm 0mm 0mm,
  clip
]{', normalizePath(input_file, mustWork = TRUE), '}

\\end{document}
'
  )

  writeLines(tex, tex_file)

  status <- system2(
    pdflatex,
    args = c(
      "-interaction=nonstopmode",
      "-halt-on-error",
      "-output-directory", td,
      tex_file
    ),
    stdout = FALSE,
    stderr = FALSE
  )

  if (status != 0) {
    stop("LaTeX fejlede for ", basename(input_file))
  }

  file.copy(
    file.path(td, "figure.pdf"),
    output_file,
    overwrite = TRUE
  )

  cat("Saved:", output_file, "\n")
}


# ============================================================
# CREATE ALL THREE
# ============================================================

for (i in seq_along(figures)) {

  trim_this_figure <- trim_bottom_mm[names(figures)[i]]

  make_figure(
    file.path(input_dir, figures[i]),
    file.path(
      output_dir,
      paste0(names(figures)[i], "_174mm.pdf")
    ),
    trim_this_figure
  )
}

cat("\nDONE.\n")
