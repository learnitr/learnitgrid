#' Computes the content of a DT::datatable for a correction set
#'
#' @description
#' The most important function for the learnitgrid Shiny app to fill in the
#' table according to selected items with a list of grids (or "all") or
#' according to a selected grid with a list of items (or "all")
#'
#' @param items The items of the evaluation grid to display in the table,
#'   usually either one item, or "all" for everything
#' @param grids The evaluation grids to display, usually either "all" if only
#'   one item, or one grid if "all" items
#' @param context A context object as computed by [create_context()].
#' @param reorder Should the rows in the table be reordered by similarities?
#'   This is usually `TRUE` for a single items, or `FALSE` otherwise (and it is
#'   computed as such by default).
#' @param highlight Syntax highlighting for code (slow, thus `FALSE` by default)
#' @param max_lines The maximum number of content lines that are displayed
#'   (truncate very long contents).
#' @param on_github Should the links point to the GitHub repository or to the
#'   local files (default is `TRUE`)?
#'
#' @return A data frame with the content to be displayed in a DT::datatable object.
#' @export
populate_table <- function(items, grids = "all", context,
    reorder = (length(items) == 1), highlight = FALSE, max_lines = 30L,
    on_github = TRUE) {
  on_github <- isTRUE(on_github)
  if (grids == "all")
    grids <- context$repos_names
  # This selects the grids to use (one, several or all)
  gsel <- context$repos_names %in% grids
  n <- sum(gsel) # Number of grids to use

  templ_corrs <- context$templ_corrs
  if (items == "all") {
    if (length(grids) == 1) {
      # Single grid, all items, also !{login} items resolved for the group
      # so, need to get these from the correction grid directly
      templ_corrs <- suppressMessages(read.csv(context$corr_files[gsel]))
    }
    items <- templ_corrs$criterion
  }

  res <- NULL # The resulting object
  for (item in items) {
    pos <- (1:NROW(templ_corrs))[templ_corrs$criterion == item]
    templ_corr <- templ_corrs[pos, ]

    # Get some more data from the correction items
    item_score_max <- templ_corr$max
    item_path <- templ_corr$file
    # We have different situations: *[.R|Rmd], dir/*[.R|Rmd], [dir/]script.R,
    # [dir/]report.Rmd
    item_subdir <- dirname(item_path)
    if (item_subdir == ".")
      item_subdir <- ""
    item_file <- basename(item_path)
    if (item_file == "*") {
      item_type <- "*"
    } else if (tolower(substring(item_file, nchar(item_file) - 1L)) == ".r") {
      item_type <- "R"
    } else if (tolower(substring(item_file, nchar(item_file) - 3L)) == ".rmd") {
      item_type <- "Rmd"
    } else if (tolower(substring(item_file, nchar(item_file) - 3L)) == ".qmd") {
      item_type <- "Rmd" # Currently, Quarto documents are treated ad R Markdown
    } else {
      item_type <- "*"
    }

    # Fill the table with all corrections for the item
    # Get all scores, authors and comments
    scores <- rep(NA, n)
    evaluators <- rep("", n)
    comments <- rep("", n)
    for (i in 1:n) {
      corrs <- suppressMessages(read.csv(context$corr_files[gsel][i]))
      pos1 <- (1:NROW(corrs))[corrs$criterion == item]
      if (length(pos1) < 1)
        stop("Item ", item, " not found")
      if (length(pos1) > 1)
        stop("Item ", item, " found ", length(pos1), " times")
      scores[i] <- corrs[pos1, ]$score
      evaluators[i] <- corrs[pos1, ]$evaluator
      comments[i] <- corrs[pos1, ]$comment
    }
    comments[is.na(comments)] <- ""

    # Construct an HTML fragments with useful links (template, repo, file, ...)
    repos_urls <- path(context$github_url, context$repos_names3[gsel])
    template <- context$assign_infos$template
    links <- c('<a href="{template}" target="githubwindow">template</a>',
      '<a href="{repos_urls}" target="githubwindow">repo</a>')

    # If we have a subdir, we offer the link to go there directly
    if (!is.null(item_subdir) && item_subdir != "") {
      repos_subdirs <- path(repos_urls, "blob", context$branch, item_subdir)
      links <- c(links,
        '<a href="{repos_subdirs}" target="githubwindow">{item_subdir}</a>')
    } else {
      repos_subdirs <- repos_urls
    }

    # If the file type is an R script, add a link to it + another one to get it
    if (item_type == "R") {
      if (is.null(item_subdir)) {
        r_files <- path(repos_urls, "blob", context$branch, item_file)
        r_local_files <- path(context$repos_dirs[gsel], item_file)
        #        r_corr_files <- path(paste0(context$corr_dirs[gsel], "_correction"),
        #          item_file)
      } else {# There is an item_subdir
        r_files <- path(repos_urls, "blob",
          context$branch, item_subdir, item_file)
        r_local_files <- path(context$repos_dirs[gsel], item_subdir, item_file)
        #        r_corr_files <- path(paste0(context$corr_dirs[gsel], "_correction"),
        #          item_subdir, item_file)
      }
      if (on_github) {
        links <- c(links,
          '<a href="{r_files}" target="sourcewindow">R script</a>',
          '<a href="{www_relative(r_local_files)}" target="_blank">(get it)</a>')
      } else {# Local files only
        links <- c(links,
          '<a href="{www_relative(r_local_files)}" target="_blank">(get R script)</a>')
      }

      # If a correction repo is available, add a link to the corrected file
      #      links <- ifelse(fs::file_exists(sub("^\\.", "./www", www_relative(r_corr_files))), c(links,
      #        '<a href="{www_relative(r_corr_files)}" target="corrwindow">solution</a>'),
      #        links)
    } else {
      r_files <- character(0)
      r_local_files <- character(0)
      #      r_corr_files <- character(0)
    }

    # If the file type is an Rmd report, add a link to the HTML report + a link
    # to the Rmd file on GitHub + a get it from local version
    if (item_type == "Rmd") {
      if (is.null(item_subdir)) {
        rmd_files <- path(repos_urls, "blob", context$branch, item_file)
        rmd_local_files <- path(context$repos_dirs[gsel], item_file)
        #        rmd_corr_files <- path(paste0(context$repos_dirs[gsel], "_correction"),
        #          item_file)
      } else {# There is an item_subdir
        rmd_files <- path(repos_urls, "blob",
          context$branch, item_subdir, item_file)
        rmd_local_files <- path(context$repos_dirs[gsel], item_subdir, item_file)
        #        rmd_corr_files <- path(paste0(context$repos_dirs[gsel], "_correction"),
        #          item_subdir, item_file)
      }
      html_local_files <- sub("\\.[qR]md$", ".html", rmd_local_files)
      # When the Rmd does not compile, there is no associated HTML file.
      # However, there may be a _corr.html file instead... Here we don't look
      # for it, but we just replace .html by _corr.html where the .html file
      # does not exist.
      exists_html_file <- file_exists(html_local_files)
      if (any(!exists_html_file)) {
        corr_html_local_files <- sub("\\.html", "_corr.html",
          html_local_files[!exists_html_file])
        html_local_files[!exists_html_file] <- corr_html_local_files
      }
      doc_type <- if (any(grepl("\\.qmd$", rmd_local_files))) "quarto" else "Rmd"
      if (on_github) {
        links <- c(links,
          '<a href="{www_relative(html_local_files)}" target="htmlwindow">html</a>',
          '<a href="{rmd_files}" target="sourcewindow">{doc_type}</a>',
          '<a href="{www_relative(rmd_local_files)}" target="_blank">(get it)</a>'
        )
      } else {
        links <- c(links,
          '<a href="{www_relative(html_local_files)}" target="htmlwindow">html</a>',
          '<a href="{www_relative(rmd_local_files)}" target="_blank">(get {doc_type})</a>'
        )
      }

      # If there are _corr.Rmd and .Rmd files, add a _diff.html and a link to it
      corr_rmd_local_files <- sub("\\.([qR])md$", "_corr.\\1md", rmd_local_files)
      diff_files <- sub("\\.[qR]md$", "_diff.html", rmd_local_files)
      exists_both_rmd_files <- file_exists(rmd_local_files) &
        file_exists(corr_rmd_local_files)
      if (any(exists_both_rmd_files)) {
        # Create or update the _diff.html files
        pos_diffs <- (1:length(rmd_local_files))[exists_both_rmd_files]
        for (pos_diff in pos_diffs)
          try(htmlwidgets::saveWidget(diffr::diffr(
            # Original Rmd file
            sub("^\\.", "./www", www_relative(rmd_local_files[pos_diff])),
            # Corr. Rmd file
            sub("^\\.", "./www", www_relative(corr_rmd_local_files[pos_diff])),
            before = "original", after = "corrig\u00e9"),
            title = basename(rmd_local_files[pos_diff]),
            file = diff_files[pos_diff]))
        # Add a link to these files (only where diff_files exists)
        diff_links <- rep('', length(rmd_local_files))
        diff_links[pos_diffs] <- "diff"
        links <- c(links,
          '<a href="{www_relative(diff_files)}" target="diffwindow">{diff_links}</a>')
        # Possibly add a link to the corrected Rmd file
        #        links <- ifelse(fs::file_exists(sub("^\\.", "./www", www_relative(rmd_corr_files))), c(links,
        #          '<a href="{www_relative(rmd_corr_files)}" target="corrwindow">solution</a>'),
        #          links)
      }
    } else {
      rmd_files <- character(0)
      rmd_local_files <- character(0)
      html_local_files <- character(0)
      #      rmd_corr_files <- character(0)
    }

    # We paste all links items together, separated by ' ', then, we resolve them
    # using glue
    links <- glue(paste(links, collapse = ' '))

    # Data for the table (we still don't know what to place in content and plot
    # at this stage, but it will be filled later on)
    dat <- data.frame(check.names = FALSE, stringsAsFactors = FALSE,
      max = item_score_max,
      # Only one entry combining score ansd comment for faster correction
      `score_comment` = paste(scores, comments),
      criterion = item,
      content = "", # Don't know yet
      plot = "", # Don't know yet
      links = links,
      evaluator = evaluators,
      user = context$user_logins[gsel]
    )

    # Fill in content and plot

    # Extraction of content and plot according to criterion type:
    # - R script -> code in content and nothing in plot
    # - YAML = ... => YAML header in content and nothing in plot
    # - #(####) Title = ... => Markdown section in content and nothing in plot
    # - @label = ... (chunk label) => code in content + chart generated in plot
    # - Something else => has to Rmd compiled or not?
    # TODO: lot of repeated code here...needs heavy reworking!
    is_content <- FALSE # By default

    if (item_type == "R") {
      # We place the code of the R file in content (the max_lines first lines)
      codes <- rep("", n)
      for (i in 1:n) {
        r_content <- try(suppressWarnings(readLines(r_local_files[i])),
          silent = TRUE)
        if (inherits(r_content, "try-error")) {
          codes[i] <- "<b>FICHIER INTROUVABLE</b>"
        } else {
          # Syntax highlighting
          if (isTRUE(highlight))
            r_content <- suppressWarnings(highr::hi_html(r_content))
          if (length(r_content) > max_lines)
            r_content <- c(r_content[1:max_lines], "<i>[...]</i>")
          codes[i] <- paste0("<pre><code>", paste(r_content, collapse = "\n"),
            "</code></pre>")
        }
      }
      dat$content <- codes
      is_content <- TRUE

    } else if (item_type == "Rmd") {
      # Target sections of the Rmd file are like key = message in item
      key <- trimws(strsplit(item, "=", fixed = TRUE)[[1]][1])

      if (key == "YAML") {# YAML header
        headers <- rep("", n)
        for (i in 1:n) {
          rmd_content <- try(suppressWarnings(readLines(rmd_local_files[i])),
            silent = TRUE)
          if (inherits(rmd_content, "try-error")) {
            headers[i] <- "<b>FICHIER INTROUVABLE</b>"
          } else {
            # Knitr now accepts spaces in chunks but not parse_rmd => replace
            # them with another character
            rmd_content <- correct_rmd(rmd_content)
            # Try parsing the Rmd content
            rmd <- try(parse_rmd(rmd_content, allow_incomplete = TRUE),
              silent = TRUE)
            if (inherits(rmd, "try-error")) {
              headers[i] <- "<b>ERREUR SYNTAXE RMD</b>"
            } else {
              header <- as_document(rmd_select(rmd, has_type("rmd_yaml_list")))
              # Eliminate empty lines, or lines with only ---
              header <- header[!grepl("^-+$", header) & !grepl("^ *$", header)]
              # Write key in bold
              header <- sub("^([^:]+)(:)(.*)$", "<b>\\1</b>:\\3", header)
              # Restrict to max_lines
              if (length(header) > max_lines)
                header <- c(header[1:max_lines], "<i>[...]</i>")
              # Collapse into a single string
              headers[i] <- paste0("<pre><code>",
                paste(header, collapse = "\n"), "</code></pre>")
            }
          }
        }
        dat$content <- headers
        is_content <- TRUE

      } else if (grepl("^#{1,5} +", key)) {# A section title
        header <- trimws(sub("^#+", "", key))

        # Get the content of the section for all repositories
        sections <- rep("", n)
        for (i in 1:n) {
          rmd_content <- try(suppressWarnings(readLines(rmd_local_files[i])),
            silent = TRUE)
          if (inherits(rmd_content, "try-error")) {
            sections[i] <- "<b>FICHIER INTROUVABLE</b>"
          } else {
            # Knitr now accepts spaces in chunks but not parse_rmd => replace
            # them with another character
            rmd_content <- correct_rmd(rmd_content)
            # Try parsing the Rmd content
            rmd <- try(parse_rmd(rmd_content, allow_incomplete = TRUE),
              silent = TRUE)
            if (inherits(rmd, "try-error")) {
              sections[i] <- "<b>ERREUR SYNTAXE RMD</b>"
            } else {
              section <- try(as_document(rmd_select(rmd,
                by_section(header, keep_parents = FALSE))), silent = TRUE)
              # If there are trailing spaces, it does not find it and returns
              # NULL => retry it with up to two trailing spaces
              if (is.null(section))
                section <- try(as_document(rmd_select(rmd,
                  by_section(paste0(header, " "), keep_parents = FALSE))),
                  silent = TRUE)
              if (is.null(section))
                section <- try(as_document(rmd_select(rmd,
                  by_section(paste0(header, "  "), keep_parents = FALSE))),
                  silent = TRUE)
              if (inherits(section, "try-error")) {
                section[i] <- "<b>TITRE INTROUVABLE</b>"
              } else {
                # Eliminate empty lines, or containing only spaces
                section <- section[!grepl("^[ \t]*$", section)]
                # TODO: R Markdown syntax highlighting
                # Put headers starting with # in bold
                is_header <- substring(section, 1, 1) == "#"
                section[is_header] <- paste0("<b>", section[is_header], "</b>")
                # Restrict to max_lines
                if (length(section) > max_lines)
                  section <- c(section[1:max_lines], "<i>[...]</i>")
                # Collapse into a single string
                sections[i] <- paste(section, collapse = "<br/>")
              }
            }
          }
        }
        dat$content <- sections
        is_content <- TRUE

      } else if (substring(key, 1, 1) == "@") {# A chunk label like @label
        key <- trimws(substring(key, 2)) # The original chunk label
        chunk <- chunk_labels(key) # "Repair" labels for parsermd::parse_rmd()

        # Get the code inside the chunk for all repositories
        codes <- rep("", n)
        for (i in 1:n) {
          rmd_content <- try(suppressWarnings(readLines(rmd_local_files[i])),
            silent = TRUE)
          if (inherits(rmd_content, "try-error")) {
            codes[i] <- "<b>FICHIER INTROUVABLE</b>"
          } else {
            # Knitr now accepts spaces in chunks but not parse_rmd => replace
            # them with another character
            rmd_content <- correct_rmd(rmd_content)
            # Try parsing the Rmd content
            rmd <- try(parse_rmd(rmd_content, allow_incomplete = TRUE),
              silent = TRUE)
            if (inherits(rmd, "try-error")) {
              codes[i] <- "<b>ERREUR SYNTAXE RMD</b>"
            } else {
              code <- try(as_document(rmd_select(rmd, has_label(chunk))),
                silent = TRUE)
              if (inherits(code, "try-error")) {
                codes[i] <- "<b>CHUNK NON TROUVE</b>"
              } else {
                # TODO: extract fig.cap if present
                # Eliminate empty lines, containing only spaces,
                # or beginning with ```
                code <- code[!grepl("^[ \t]*$", code)]
                code <- code[substring(code, 1, 3) != "```"]
                # Syntax highlighting
                if (isTRUE(highlight))
                  code <- suppressWarnings(highr::hi_html(code))
                # Restrict to max_lines
                if (length(code) > max_lines)
                  code <- c(code[1:max_lines], "<i>[...]</i>")
                # Collapse to a single string
                codes[i] <- paste0("<pre><code>", paste(code, collapse = "\n"),
                  "</code></pre>")
              }
            }
          }
        }
        dat$content <- codes
        is_content <- TRUE

        # Path to the plots (if they exist, or to the first one if there are
        # several plots that are generated by that chunk)
        if (is.null(item_subdir)) {
          plot_local_files <- path(context$repos_dirs[gsel], sub(
            "\\.[qR]md$", "_files", item_file), "figure-html", paste0(key, "-1.png"))
        } else {# There is an item_subdir
          plot_local_files <- path(context$repos_dirs[gsel], item_subdir, sub(
            "\\.[qR]md$", "_files", item_file), "figure-html", paste0(key, "-1.png"))
        }
        dat$plot <- glue(paste0(
          '<a href="{www_relative(plot_local_files)}" target="plotwindow">',
          '<img src="{www_relative(plot_local_files)}" height="200px"></a>'))
        # Eliminate items that do not point to an existing image file
        dat$plot[!file_exists(plot_local_files)] <- ""

      } else if (substring(key, 1, 1) == "+" || grepl("@[a-zA-Z]+", item)) {
        # +chunk = ... item, or message @chunk
        if (substring(key, 1, 1) == "+") {
          key <- trimws(substring(key, 2)) # The original chunk label
        } else {# Get key from inside the item (only allows a-zA-Z0-9_)
          key <- sub("^.+@([a-zA-Z0-9_]+).*$", "\\1", item)
        }
        chunk <- chunk_labels(key) # "Repair" labels for parsermd::parse_rmd()
        # Get the content between the chunk and another one, or a new section
        paras <- rep("", n)
        for (i in 1:n) {
          rmd_content <- try(suppressWarnings(readLines(rmd_local_files[i])),
            silent = TRUE)
          if (inherits(rmd_content, "try-error")) {
            paras[i] <- "<b>FICHIER INTROUVABLE</b>"
          } else {
            # Knitr now accepts spaces in chunks but not parse_rmd => replace
            # them with another character
            rmd_content <- correct_rmd(rmd_content)
            # Try parsing the Rmd content
            rmd <- try(parse_rmd(rmd_content, allow_incomplete = TRUE),
              silent = TRUE)
            if (inherits(rmd, "try-error")) {
              paras[i] <- "<b>ERREUR SYNTAXE RMD</b>"
            } else {
              # We collect markdown paragraphs beneath the chunk and before
              # another chunk or a section
              # Transform in a tibble
              #rmd <- as_tibble(rmd)
              rmd <- as.data.frame(rmd)
              # Get the position of the chunk
              rmd_pos <- (1:NROW(rmd))[rmd$label == chunk]
              rmd_pos <- rmd_pos[!is.na(rmd_pos)]
              if (length(rmd_pos) < 1) {
                paras[i] <- "<b>CHUNK NOT FOUND</b>"
              } else {
                # Keep only data after the chunk
                rmd <- rmd[-(1:rmd_pos), ]
                # Seek for the first non R markdown item in the truncated df
                rmd_pos2 <- (1:NROW(rmd))[rmd$type != "rmd_markdown"][1]
                if (!is.na(rmd_pos2)) # Otherwise, we keep everything to the end
                  rmd <- rmd[-(rmd_pos2:NROW(rmd)), ]
                para <- as_document(rmd)
                # Eliminate empty lines, containing only spaces, or comments
                # starting with <!--
                para <- para[!grepl("^[ \t]*$", para)]
                para <- para[substring(para, 1, 4) != "<!--"]
                # TODO: syntax highlighting of R Markdown text
                # Restrict to max_lines
                if (length(para) > max_lines)
                  para <- c(para[1:max_lines], "<i>[...]</i>")
                # Collapse to a single string
                paras[i] <- paste(para, collapse = "<br/>")
              }
            }
          }
        }
        dat$content <- paras
        is_content <- TRUE

        # Path to the plots (if they exist, or to the first one if there are
        # several plots that are generated by that chunk)
        if (is.null(item_subdir)) {
          plot_local_files <- path(context$repos_dirs[gsel], sub(
            "\\.[qR]md$", "_files", item_file), "figure-html", paste0(key, "-1.png"))
        } else {# There is an item_subdir
          plot_local_files <- path(context$repos_dirs[gsel], item_subdir, sub(
            "\\.[qR]md$", "_files", item_file), "figure-html", paste0(key, "-1.png"))
        }
        dat$plot <- glue(paste0(
          '<a href="{www_relative(plot_local_files)}" target="plotwindow">',
          '<img src="{www_relative(plot_local_files)}" height="200px"></a>'))
        # Eliminate items that do not point to an existing image file
        dat$plot[!file_exists(plot_local_files)] <- ""

      } else {# Simple entry
        # We just check if the Rmd compiled or not by looking for the HTML file
        compiled <- rep("<b>qmd/Rmd pas compil\u00e9</b>", n)
        compiled[exists_html_file] <- "qmd/Rmd compil\u00e9"
        dat$content <- compiled
      }

    } else {# item_type is neither R, nor Rmd
      # TODO: what to place in content? Nothing for the moment.
      dat$content <- ""
    }

    attr(dat, "is_content") <- is_content
    dat
    res <- rbind(res, dat)
  }
  attr(res, "reordered") <- reorder
  res
}
