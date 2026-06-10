# _tabla_helper.R para compilar el libro en Word o en Gitbook ─────────────────
# tabla_fmt(): tabla numerada con caption para bookdown::gitbook Y word_document2
# ─────────────────────────────────────────────────────────────────────────────

tabla_fmt <- function(df, digits = 3, col_names = NA) {

  fmt     <- knitr::opts_knit$get("rmarkdown.pandoc.to")
  is_word <- !is.null(fmt) && fmt %in% c("docx", "odt")

  # Redondear columnas numéricas
  num_cols <- which(sapply(df, is.numeric))
  if (length(num_cols) > 0)
    df[num_cols] <- lapply(df[num_cols], round, digits)

  cn    <- if (identical(col_names, NA)) colnames(df) else col_names
  ncols <- ncol(df)
  align <- c("l", rep("c", max(ncols - 1L, 0L)))

  if (is_word) {
    library(flextable)
    library(officer)

    nms <- colnames(df)
    nms[is.na(nms)] <- "NA"
    colnames(df) <- nms

    rn <- rownames(df)
    if (!is.null(rn) && !identical(rn, as.character(seq_len(nrow(df))))) {
      df <- cbind(` ` = rn, df)
    }

    flextable(df) %>%
      border_remove() %>%
      hline_top(    border = fp_border(width = 1.5), part = "header") %>%
      hline_bottom( border = fp_border(width = 1.5), part = "header") %>%
      hline_bottom( border = fp_border(width = 1.5), part = "body"  ) %>%
      font(fontname = "Times New Roman", part = "all") %>%
      fontsize(size = 10, part = "all") %>%
      bold(part = "header") %>%
      align(align = "center", part = "all") %>%
      align(j = 1, align = "left", part = "body") %>%
      autofit() %>%
      set_table_properties(width = 1, layout = "autofit")

  } else {
    library(knitr)

    knitr::kable(df,
                 format    = "pipe",
                 col.names = cn,
                 digits    = digits,
                 align     = align)
  }
}
