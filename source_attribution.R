# =============================================================================
# source_attribution.R
#
# Which parts of an AI-written biography come from which source document?
#
# For every sentence in the biography the script finds the passage in each
# source that covers it best, then labels the sentence:
#     - one source  (e.g. "Jamestown" or "ICG")
#     - "Both"      (both sources cover it about equally well)
#     - "Neither"   (no passage in either source covers it well)
# It also lists specific details (names, numbers, dates) in each sentence that
# do not appear anywhere in either source - the best candidates for invented
# or embellished content.
#
# Outputs (written to `output_dir`):
#     attribution_report.html   colour-coded biography + evidence for each sentence
#     attribution_sentences.csv one row per sentence, with scores and passages
#     attribution_summary.csv   how much of the biography falls under each label
#
# How to run:  put this script in the same folder as the three documents, set
# the working directory to that folder, then  source("source_attribution.R")
# =============================================================================


# ---- 1. SETTINGS (edit these) ----------------------------------------------

bio_file <- "Ai_gen_two_source.docx"

# Source documents. The names on the left are the labels used in the output.
# .docx, .pdf and .txt files are supported; you can list more than two.
source_files <- c(
  Jamestown = "Jamestown_biography.docx",
  ICG       = "114-terrorism-in-indonesia-noordin-s-networks-3.pdf"
)

# Optional: restrict a PDF to certain pages (drops cover, contents page and
# back-matter). NULL or a missing entry means "use every page".
# For the ICG report, page 1 is the cover and page 2 the table of contents.
pdf_pages <- list(ICG = 3:33)

output_dir <- "attribution_output"

# A "passage" is this many consecutive source sentences. An AI summary usually
# condenses a few neighbouring source sentences into one, so 3-4 works well.
window_size <- 3

# Coverage score = share of a biography sentence's (rarity-weighted) content
# words that also occur in one source passage. 0 = nothing shared, 1 = all.
# Reworded text scores much lower than copied text, hence the three bands.
min_support    <- 0.30   # below this in every source -> "Neither"
moderate_score <- 0.40   # 0.30-0.40 = "weak" match: always read the evidence
strong_score   <- 0.55   # at or above this = "strong" match
tie_margin     <- 0.05   # sources closer than this (both supported) -> "Both"

# Sentences with fewer content words than this are not classified (headings).
min_terms <- 5

# Different spellings that should count as the same word (lower case).
# The script also auto-matches near-identical spellings (one letter different)
# and prints what it matched, so you only need to add the cases it misses.
aliases <- c(
  mohammed   = "mohammad",
  azhari     = "azahari",
  lukman     = "luqmanul",
  lukmanul   = "luqmanul",
  hakim      = "hakiem",
  qaida      = "qaeda",
  zawaheri   = "zawahiri",
  aiman      = "ayman",
  madrassa   = "school",
  pesantren  = "school"
)


# ---- 2. PACKAGES ------------------------------------------------------------

needed  <- c("stringi", "xml2", "pdftools", "SnowballC")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
library(stringi)


# ---- 3. READING THE DOCUMENTS ----------------------------------------------

# Split text into sentences, without breaking at "Dr.", "U.S." and the like.
split_sentences <- function(text) {
  dot  <- intToUtf8(0xE000)   # stand-in for a full stop that does not end a sentence
  abbr <- "Dr|Mr|Mrs|Ms|Prof|Gen|Col|Lt|Capt|Sgt|St|Jr|Sr|No|Nos|vs|etc|Inc|Ltd|Co|Sci|Vol|pp|ibid|op|cit"
  text <- stri_replace_all_regex(text, paste0("\\b(", abbr, ")\\."), paste0("$1", dot))
  text <- stri_replace_all_regex(text, "\\b(\\p{Lu})\\.", paste0("$1", dot))   # initials, U.S.
  text <- stri_replace_all_regex(text, "\\b(e\\.g|i\\.e)\\.", paste0("$1", dot))
  loc  <- stri_locate_all_boundaries(text, type = "sentence")[[1]]
  out  <- data.frame(start = loc[, 1], text = stri_sub(text, loc[, 1], loc[, 2]),
                     stringsAsFactors = FALSE)
  out$text <- stri_trim_both(stri_replace_all_fixed(out$text, dot, "."))
  out[nzchar(out$text), , drop = FALSE]
}

# Word (.docx): one string per paragraph.
read_docx_paragraphs <- function(path) {
  tmp <- tempfile("docx_"); dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE))
  utils::unzip(path, files = "word/document.xml", exdir = tmp)
  doc   <- xml2::read_xml(file.path(tmp, "word", "document.xml"))
  paras <- xml2::xml_find_all(doc, "//w:body//w:p")
  txt   <- vapply(paras, function(p) {
    paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t")), collapse = "")
  }, character(1))
  txt <- stri_trim_both(stri_replace_all_regex(txt, "\\s+", " "))
  txt[nzchar(txt)]
}

# PDF: returns one row per sentence with the page it starts on.
# pdftools::pdf_text() reads straight across the page, which scrambles
# two-column reports like ICG's. This reader uses the position of every word
# instead: it detects columns, removes running headers and footnote markers,
# re-joins words hyphenated across lines, and keeps footnotes as a separate
# stream so they do not interrupt sentences that run on to the next column.
read_pdf_sentences <- function(path, pages = NULL) {
  dat <- tryCatch(pdftools::pdf_data(path, font_info = TRUE),
                  error = function(e) pdftools::pdf_data(path))
  page_size <- pdftools::pdf_pagesize(path)
  if (is.null(pages)) pages <- seq_along(dat)
  pages <- pages[pages %in% seq_along(dat)]

  words <- do.call(rbind, lapply(pages, function(p) {
    d <- as.data.frame(dat[[p]], stringsAsFactors = FALSE)
    if (nrow(d) == 0) return(NULL)
    if (!"font_size" %in% names(d)) d$font_size <- d$height   # older pdftools
    d$page <- p
    d$rel_y <- d$y / page_size$height[p]          # 0 = top of page, 1 = bottom
    mid <- page_size$width[p] / 2
    crosses  <- d$x < mid - 3 & d$x + d$width > mid + 3
    two_cols <- mean(crosses) < 0.02 && mean(d$x < mid) > 0.2 && mean(d$x >= mid) > 0.2
    d$col <- if (two_cols) ifelse(d$x >= mid, 2L, 1L) else 1L
    d[, c("page", "col", "x", "y", "rel_y", "font_size", "text")]
  }))
  if (is.null(words)) stop("No text found in ", path, " (is it a scanned PDF?)")

  # Font size tells body text, footnotes and superscript markers apart.
  size_chars <- tapply(nchar(words$text), round(words$font_size, 1), sum)
  body_size  <- as.numeric(names(size_chars)[which.max(size_chars)])
  words <- words[words$font_size >= body_size - 2.5, ]            # drop superscripts
  words$stream <- ifelse(words$font_size < body_size - 0.4, "notes", "body")

  # Group words into lines, in reading order.
  words <- words[order(words$stream, words$page, words$col, words$y, words$x), ]
  key   <- paste(words$stream, words$page, words$col)
  new_line <- c(TRUE, key[-1] != key[-length(key)] | abs(diff(words$y)) > 3)
  words$line <- cumsum(new_line)
  words <- words[order(words$line, words$x), ]
  lines <- data.frame(
    text   = as.vector(tapply(words$text, words$line, paste, collapse = " ")),
    page   = as.vector(tapply(words$page, words$line, `[`, 1)),
    rel_y  = as.vector(tapply(words$rel_y, words$line, `[`, 1)),
    stream = as.vector(tapply(words$stream, words$line, `[`, 1)),
    stringsAsFactors = FALSE
  )

  # Running headers / footers: the same line (ignoring digits) on 3+ pages,
  # or on 2+ pages when it sits in the top or bottom margin.
  norm      <- stri_trans_tolower(stri_replace_all_regex(lines$text, "\\d+", "#"))
  n_pages   <- tapply(lines$page, norm, function(p) length(unique(p)))[norm]
  in_margin <- lines$rel_y < 0.15 | lines$rel_y > 0.93
  # Headings: lines with no lower-case letters ("II. THE MARRIOTT BOMBING"),
  # unless they end a sentence, plus bare outline markers ("A.", "IV.", "2.").
  heading   <- (!stri_detect_regex(lines$text, "\\p{Ll}") & !stri_detect_regex(lines$text, "[.,;!?]$")) |
    stri_detect_regex(lines$text, "^([IVX]+|\\p{Lu}|\\d{1,2})\\.$")
  lines     <- lines[!(n_pages >= 3 | (n_pages >= 2 & in_margin) | heading), ]

  do.call(rbind, lapply(c("body", "notes"), function(s) {
    ln <- lines[lines$stream == s, ]
    if (nrow(ln) == 0) return(NULL)
    nxt       <- c(ln$text[-1], "")
    soft      <- stri_detect_regex(ln$text, "\\p{L}-$") & stri_detect_regex(nxt, "^\\p{Ll}")
    hard      <- stri_detect_regex(ln$text, "-$") & !soft
    piece     <- ifelse(soft, stri_sub(ln$text, 1, -2), ln$text)   # "organisa-" + "tion"
    piece     <- paste0(piece, ifelse(soft | hard, "", " "))
    sent      <- split_sentences(paste(piece, collapse = ""))
    line_start <- cumsum(c(1, nchar(piece)[-length(piece)]))
    data.frame(text = sent$text, page = ln$page[findInterval(sent$start, line_start)],
               stream = s, stringsAsFactors = FALSE)
  }))
}

# Any supported file -> data frame of sentences (text, page, stream).
read_source_sentences <- function(path, pages = NULL) {
  if (!file.exists(path)) stop("File not found: ", path, "\nWorking directory is ", getwd())
  ext <- tolower(tools::file_ext(path))
  if (ext == "pdf") return(read_pdf_sentences(path, pages))
  paras <- switch(ext,
    docx = read_docx_paragraphs(path),
    txt  = { x <- stri_trim_both(readLines(path, warn = FALSE, encoding = "UTF-8")); x[nzchar(x)] },
    stop("Unsupported file type: ", path))
  sent <- do.call(rbind, lapply(paras, split_sentences))
  data.frame(text = sent$text, page = NA_integer_, stream = "body", stringsAsFactors = FALSE)
}


# ---- 4. TURNING TEXT INTO COMPARABLE TERMS ---------------------------------

stopwords <- c(
  "a","about","above","after","again","against","all","also","although","am","among","an","and",
  "any","are","as","at","be","became","because","become","been","before","being","below","between",
  "both","but","by","can","could","did","do","does","doing","down","during","each","either","even",
  "ever","few","for","from","further","had","has","have","having","he","her","here","hers","herself",
  "him","himself","his","how","however","i","if","in","into","is","it","its","itself","just","later",
  "may","me","might","more","most","much","must","my","no","nor","not","of","off","on","once","one",
  "only","or","other","others","our","out","over","own","rather","same","several","she","should",
  "since","so","some","still","such","than","that","the","their","theirs","them","themselves","then",
  "there","these","they","this","those","though","through","thus","to","too","under","until","up",
  "upon","very","was","we","were","what","when","where","whether","which","while","who","whom",
  "whose","why","will","with","within","without","would","you","your","al","s","t","alias"
)

word_pattern <- "[\\p{L}\\p{N}]+(?:['\u2019][\\p{L}]+)?"

# Raw words -> normalised terms (NA for stop words). Lower-cases, strips
# possessives, maps British to American spelling, applies aliases, then stems
# so that "recruiter", "recruited" and "recruitment" all become "recruit".
normalise_words <- function(w, alias = aliases) {
  w <- stri_trans_tolower(stri_trans_general(w, "Latin-ASCII"))
  w <- stri_replace_all_regex(w, "'(s|t|d|ll|re|ve)?$", "")
  w <- stri_replace_all_regex(w, "is(e|ed|es|ing|er|ers)$", "iz$1")
  w <- stri_replace_all_fixed(w, "isation", "ization")
  w <- stri_replace_all_regex(w, "^(\\w{3,})our(s|ed|ing)?$", "$1or$2")
  w <- stri_replace_all_regex(w, "^(\\w{2,})tre(s)?$", "$1ter$2")
  w <- stri_replace_all_regex(w, "^(defen|offen|licen)ce(s)?$", "$1se$2")
  hit <- w %in% names(alias); w[hit] <- alias[w[hit]]
  stop_w <- w %in% stopwords | (nchar(w) < 2 & !stri_detect_regex(w, "^\\d$"))
  w <- SnowballC::wordStem(w, language = "english")
  hit <- w %in% names(alias); w[hit] <- alias[w[hit]]
  w[stop_w] <- NA_character_
  w
}

# One text -> data frame of its words: where they are, and their term.
tokenise <- function(text) {
  loc <- stri_locate_all_regex(text, word_pattern, omit_no_match = TRUE)[[1]]
  raw <- stri_sub(text, loc[, 1], loc[, 2])
  data.frame(start = loc[, 1], end = loc[, 2], raw = raw, term = normalise_words(raw),
             stringsAsFactors = FALSE)
}
terms_of <- function(text) { t <- tokenise(text)$term; unique(t[!is.na(t)]) }


# ---- 5. LOAD EVERYTHING -----------------------------------------------------

message("Reading documents ...")
bio_paras <- read_docx_paragraphs(bio_file)
bio <- do.call(rbind, lapply(seq_along(bio_paras), function(i) {
  s <- split_sentences(bio_paras[i])
  data.frame(paragraph = i, text = s$text, stringsAsFactors = FALSE)
}))
bio$id <- seq_len(nrow(bio))

src_sent <- lapply(names(source_files), function(nm) {
  s <- read_source_sentences(source_files[[nm]], pdf_pages[[nm]])
  s$source <- nm
  s
})
names(src_sent) <- names(source_files)
for (nm in names(src_sent)) {
  message(sprintf("  %-10s %5d sentences, %6d words", nm, nrow(src_sent[[nm]]),
                  sum(stri_count_regex(src_sent[[nm]]$text, word_pattern))))
}
message(sprintf("  %-10s %5d sentences, %6d words", "Biography", nrow(bio),
                sum(stri_count_regex(bio$text, word_pattern))))

# Terms for every source sentence, and the full vocabulary of each source.
src_terms <- lapply(src_sent, function(s) lapply(s$text, terms_of))
src_vocab <- lapply(src_terms, function(tl) unique(unlist(tl)))
all_vocab <- unique(unlist(src_vocab))

# Names are often transliterated differently (Islamiya / Islamiyah, Azhari /
# Azahari). If a name in the biography (6+ letters) is missing from a source
# but that source has the same word with one letter added or dropped, the
# source's spelling is treated as the biography's.
bio_tok <- lapply(bio$text, tokenise)
proper  <- unique(unlist(lapply(bio_tok, function(t) {   # capitalised mid-sentence
  t$raw[-1][stri_detect_regex(t$raw[-1], "^\\p{Lu}") & !is.na(t$term[-1])]
})))
name_terms <- unique(unlist(lapply(bio_tok, function(t) t$term[t$raw %in% proper & !is.na(t$term)])))
variant <- character(0)
for (s in names(src_vocab)) {
  for (w in name_terms[!name_terms %in% src_vocab[[s]] & nchar(name_terms) >= 6]) {
    cand <- src_vocab[[s]][substr(src_vocab[[s]], 1, 1) == substr(w, 1, 1) &
                             abs(nchar(src_vocab[[s]]) - nchar(w)) == 1]
    cand <- cand[utils::adist(w, cand)[1, ] == 1 & !cand %in% c(names(variant), name_terms)]
    if (length(cand) > 0) variant[cand] <- w
  }
}
if (length(variant) > 0) {
  message("Spelling variants treated as the same word (add to `aliases` to change):\n  ",
          paste(sprintf("%s = %s", names(variant), variant), collapse = ", "))
}
unify <- function(t) { hit <- t %in% names(variant); t[hit] <- variant[t[hit]]; t }
src_terms <- lapply(src_terms, function(tl) lapply(tl, function(t) unique(unify(t))))
src_vocab <- lapply(src_terms, function(tl) unique(unlist(tl)))
all_vocab <- unique(unlist(src_vocab))

bio_tok   <- lapply(bio_tok, function(t) { t$term <- unify(t$term); t })
bio_terms <- lapply(bio_tok, function(t) unique(t$term[!is.na(t$term)]))


# ---- 6. WEIGHTS AND PASSAGES ------------------------------------------------

# Rare words are better evidence than common ones. A word found in few source
# sentences gets a high weight; a word found in nearly all of them (such as
# "Noordin") gets a weight close to zero. Words in no source get the top weight.
n_sent <- sum(vapply(src_terms, length, integer(1)))
df     <- table(unlist(lapply(src_terms, function(tl) unlist(tl))))
idf    <- log(n_sent / as.numeric(df)); names(idf) <- names(df)
max_w  <- log(n_sent)
weight_of <- function(terms) { w <- idf[terms]; w[is.na(w)] <- max_w; unname(w) }

# Sliding windows of `window_size` sentences (never mixing body and footnotes).
make_windows <- function(sent, terms) {
  out <- list()
  for (s in unique(sent$stream)) {
    idx <- which(sent$stream == s)
    k   <- min(window_size, length(idx))
    for (i in seq_len(length(idx) - k + 1)) {
      rows <- idx[i:(i + k - 1)]
      out[[length(out) + 1]] <- list(rows = rows, terms = unique(unlist(terms[rows])))
    }
  }
  out
}
windows <- Map(make_windows, src_sent, src_terms)


# ---- 7. MATCH EVERY BIOGRAPHY SENTENCE TO EVERY SOURCE ---------------------

message("Matching ", nrow(bio), " biography sentences ...")
src_names <- names(source_files)

best_match <- function(terms, src) {
  w     <- weight_of(terms)
  total <- sum(w)
  if (length(terms) == 0 || total == 0) return(list(score = 0, rows = integer(0), matched = character(0)))
  scores <- vapply(windows[[src]], function(win) sum(w[terms %in% win$terms]), numeric(1)) / total
  b      <- which.max(scores)
  rows   <- windows[[src]][[b]]$rows
  # Trim sentences off either end of the passage while that costs (almost)
  # nothing, so the evidence shown is as tight as possible.
  cover <- function(r) sum(w[terms %in% unlist(src_terms[[src]][r])])
  full  <- cover(rows)
  while (length(rows) > 1 && cover(rows[-1]) >= 0.97 * full) rows <- rows[-1]
  while (length(rows) > 1 && cover(rows[-length(rows)]) >= 0.97 * full) rows <- rows[-length(rows)]
  list(score = scores[b], rows = rows, matched = terms[terms %in% unlist(src_terms[[src]][rows])])
}

matches <- lapply(seq_len(nrow(bio)), function(i) {
  m <- lapply(src_names, function(s) best_match(bio_terms[[i]], s))
  names(m) <- src_names
  m
})

# Specific details = numbers, and words the biography capitalises in the middle
# of a sentence (names, places, months, nationalities) - the `proper` list above.
details_of <- function(t) {
  is_detail <- !is.na(t$term) & (stri_detect_regex(t$raw, "^\\d") | t$raw %in% proper)
  t[is_detail & !duplicated(t$term), c("raw", "term")]
}

classify <- function(scores, n_terms) {
  if (n_terms < min_terms) return("Not assessed")
  ok <- scores >= min_support
  if (!any(ok)) return("Neither")
  top <- max(scores)
  near <- names(scores)[ok & scores >= top - tie_margin]
  if (length(near) > 1) return(if (length(near) == length(scores)) "Both" else paste(near, collapse = " + "))
  names(scores)[which.max(scores)]
}

passage_text <- function(src, rows) paste(src_sent[[src]]$text[rows], collapse = " ")
passage_page <- function(src, rows) {
  p <- unique(src_sent[[src]]$page[rows]); p <- p[!is.na(p)]
  if (length(p) == 0) "" else paste(range(p)[!duplicated(range(p))], collapse = "-")
}

results <- do.call(rbind, lapply(seq_len(nrow(bio)), function(i) {
  m      <- matches[[i]]
  scores <- vapply(m, function(x) x$score, numeric(1))
  label  <- classify(scores, length(bio_terms[[i]]))
  terms  <- bio_terms[[i]]
  w      <- weight_of(terms)
  union_cov <- if (sum(w) > 0) sum(w[terms %in% unlist(lapply(m, `[[`, "matched"))]) / sum(w) else 0

  d <- details_of(bio_tok[[i]])
  in_src <- matrix(FALSE, nrow(d), length(src_names), dimnames = list(NULL, src_names))
  for (s in src_names) in_src[, s] <- d$term %in% src_vocab[[s]]
  row <- data.frame(
    id = bio$id[i], paragraph = bio$paragraph[i], sentence = bio$text[i],
    label = label,
    confidence = if (label %in% c("Neither", "Not assessed")) "" else
      if (max(scores) >= strong_score) "strong" else if (max(scores) >= moderate_score) "moderate" else "weak",
    combined_coverage = round(union_cov, 2),
    details_in_no_source = paste(d$raw[rowSums(in_src) == 0], collapse = "; "),
    stringsAsFactors = FALSE
  )
  for (s in src_names) {
    only <- in_src[, s] & rowSums(in_src) == 1
    row[[paste0("score_", s)]]        <- round(scores[[s]], 2)
    row[[paste0("details_only_in_", s)]] <- paste(d$raw[only], collapse = "; ")
    row[[paste0("page_", s)]]         <- passage_page(s, m[[s]]$rows)
    row[[paste0("passage_", s)]]      <- passage_text(s, m[[s]]$rows)
  }
  row
}))


# ---- 8. SUMMARY -------------------------------------------------------------

results$words <- stri_count_regex(results$sentence, word_pattern)
label_order <- c(src_names, "Both", setdiff(unique(results$label), c(src_names, "Both", "Neither", "Not assessed")),
                 "Neither", "Not assessed")
label_order <- label_order[label_order %in% results$label]
summary_tbl <- data.frame(
  label     = label_order,
  sentences = as.vector(table(factor(results$label, label_order))),
  words     = as.vector(tapply(results$words, factor(results$label, label_order), sum)),
  stringsAsFactors = FALSE
)
summary_tbl$pct_of_words <- round(100 * summary_tbl$words / sum(summary_tbl$words), 1)

cat("\n================ SOURCE ATTRIBUTION SUMMARY ================\n")
print(summary_tbl, row.names = FALSE)
flagged <- results[nzchar(results$details_in_no_source), c("id", "details_in_no_source")]
cat("\nSpecific details found in NO source (check these by hand):\n")
if (nrow(flagged) == 0) cat("  none\n") else
  cat(sprintf("  sentence %2d: %s\n", flagged$id, flagged$details_in_no_source), sep = "")


# ---- 9. WRITE THE CSV FILES AND THE HTML REPORT ----------------------------

dir.create(output_dir, showWarnings = FALSE)
write.csv(results, file.path(output_dir, "attribution_sentences.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(summary_tbl, file.path(output_dir, "attribution_summary.csv"), row.names = FALSE)

esc <- function(x) {
  x <- stri_replace_all_fixed(x, "&", "&amp;")
  x <- stri_replace_all_fixed(x, "<", "&lt;")
  x <- stri_replace_all_fixed(x, ">", "&gt;")
  stri_replace_all_fixed(x, "\"", "&quot;")
}

# Wrap chosen words of `text` in a tag. `flag` says which tokens to wrap.
mark_words <- function(text, tok, flag, open, close) {
  if (!any(flag)) return(esc(text))
  tok <- tok[flag, , drop = FALSE]
  out <- character(0); pos <- 1
  for (j in seq_len(nrow(tok))) {
    out <- c(out, esc(stri_sub(text, pos, tok$start[j] - 1)), open, esc(tok$raw[j]), close)
    pos <- tok$end[j] + 1
  }
  paste(c(out, esc(stri_sub(text, pos, nchar(text)))), collapse = "")
}

palette <- c("#2f6fb5", "#c8741a", "#2e8b6f", "#b0478c", "#6b6fc0")
colours <- setNames(palette[seq_along(src_names)], src_names)
colours <- c(colours, Both = "#7a52a8", Neither = "#b3261e", "Not assessed" = "#8a8a8a")
css_class <- function(label) paste0("lab-", stri_replace_all_regex(label, "[^A-Za-z0-9]+", "-"))
all_labels <- unique(c(label_order, results$label))
label_css <- paste(vapply(all_labels, function(l) {
  col <- if (l %in% names(colours)) colours[[l]] else "#7a52a8"
  sprintf(".%s{--c:%s}", css_class(l), col)
}, character(1)), collapse = "\n")

# Biography with each sentence tinted by its label; unsupported details marked.
bio_html <- vapply(split(results, results$paragraph), function(p) {
  spans <- vapply(seq_len(nrow(p)), function(k) {
    i   <- p$id[k]
    tok <- bio_tok[[i]]
    bad <- stri_split_fixed(p$details_in_no_source[k], "; ")[[1]]
    txt <- mark_words(p$sentence[k], tok, tok$raw %in% bad & !is.na(tok$term), "<u class=\"nosrc\">", "</u>")
    tip <- paste0(p$label[k], " | ", paste(sprintf("%s %.2f", src_names,
                  unlist(p[k, paste0("score_", src_names)])), collapse = ", "))
    sprintf("<a class=\"sent %s%s\" href=\"#s%d\" title=\"%s\"><sup>%d</sup>%s</a>",
            css_class(p$label[k]), if (p$confidence[k] == "weak") " weak" else "", i, esc(tip), i, txt)
  }, character(1))
  paste0("<p>", paste(spans, collapse = " "), "</p>")
}, character(1))

legend_html <- paste(vapply(label_order, function(l) {
  sprintf("<span class=\"chip %s\">%s</span>", css_class(l), esc(l))
}, character(1)), collapse = " ")

summary_html <- paste(sprintf(
  "<tr><td><span class=\"chip %s\">%s</span></td><td>%d</td><td>%d</td><td>%.1f%%</td><td><div class=\"bar %s\" style=\"width:%.1f%%\"></div></td></tr>",
  css_class(summary_tbl$label), esc(summary_tbl$label), summary_tbl$sentences, summary_tbl$words,
  summary_tbl$pct_of_words, css_class(summary_tbl$label), summary_tbl$pct_of_words), collapse = "\n")

detail_html <- vapply(seq_len(nrow(results)), function(i) {
  r <- results[i, ]
  src_blocks <- vapply(src_names, function(s) {
    m    <- matches[[i]][[s]]
    txt  <- passage_text(s, m$rows)
    tok  <- tokenise(txt)
    page <- r[[paste0("page_", s)]]
    only <- r[[paste0("details_only_in_", s)]]
    sprintf("<div class=\"src %s\"><div class=\"srchead\"><b>%s</b> &middot; coverage %.2f%s%s</div><div class=\"passage\">%s</div></div>",
            css_class(s), esc(s), m$score,
            if (nzchar(page)) paste0(" &middot; PDF page ", page) else "",
            if (nzchar(only)) paste0(" &middot; details found only here: <i>", esc(only), "</i>") else "",
            mark_words(txt, tok, tok$term %in% m$matched, "<b>", "</b>"))
  }, character(1))
  sprintf("<div class=\"card %s\" id=\"s%d\"><div class=\"cardhead\"><span class=\"num\">%d</span><span class=\"chip %s\">%s</span>%s<span class=\"muted\">combined coverage %.2f</span></div><div class=\"bio\">%s</div>%s%s</div>",
          css_class(r$label), r$id, r$id, css_class(r$label), esc(r$label),
          if (nzchar(r$confidence)) sprintf("<span class=\"muted\">%s</span>", r$confidence) else "",
          r$combined_coverage, esc(r$sentence),
          if (nzchar(r$details_in_no_source)) sprintf("<div class=\"warn\">Not found in any source: %s</div>", esc(r$details_in_no_source)) else "",
          paste(src_blocks, collapse = ""))
}, character(1))

html <- c(
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "<title>Source attribution report</title><style>",
  "body{font:16px/1.6 Georgia,'Times New Roman',serif;color:#1d1d1f;background:#faf9f6;margin:0}",
  "main{max-width:900px;margin:0 auto;padding:32px 20px 80px}",
  "h1{font-size:28px;margin:0 0 4px}h2{font-size:20px;margin:40px 0 12px;border-bottom:1px solid #ddd;padding-bottom:6px}",
  ".muted,.note{color:#666;font:13px/1.5 system-ui,sans-serif}.note{margin:6px 0 16px}",
  ".chip{display:inline-block;font:600 12px/1 system-ui,sans-serif;color:#fff;background:var(--c);padding:5px 9px;border-radius:999px;margin-right:6px}",
  "table{border-collapse:collapse;width:100%;font:14px system-ui,sans-serif}td,th{padding:6px 10px;text-align:left;border-bottom:1px solid #e6e3dc}",
  "td:nth-child(2),td:nth-child(3),td:nth-child(4),th:nth-child(2),th:nth-child(3),th:nth-child(4){text-align:right;width:80px}",
  ".bar{height:12px;border-radius:3px;background:var(--c);min-width:2px}",
  ".text p{margin:0 0 14px}",
  ".sent{color:inherit;text-decoration:none;background:color-mix(in srgb,var(--c) 16%,transparent);border-bottom:2px solid var(--c);padding:1px 0}",
  ".sent.weak{background:color-mix(in srgb,var(--c) 6%,transparent);border-bottom-style:dashed}",
  ".sent:hover{background:color-mix(in srgb,var(--c) 32%,transparent)}",
  ".sent sup{font:600 10px system-ui,sans-serif;color:var(--c);margin-right:2px}",
  "u.nosrc{text-decoration:underline wavy #b3261e;text-decoration-thickness:2px;font-weight:bold}",
  ".card{background:#fff;border:1px solid #e3e0d8;border-left:5px solid var(--c);border-radius:6px;padding:14px 16px;margin:0 0 14px}",
  ".card:target{box-shadow:0 0 0 3px color-mix(in srgb,var(--c) 40%,transparent)}",
  ".cardhead{display:flex;align-items:center;gap:8px;flex-wrap:wrap;margin-bottom:8px}",
  ".num{font:700 14px system-ui,sans-serif;color:#666}.bio{font-size:16px;margin-bottom:10px}",
  ".warn{font:600 13px system-ui,sans-serif;color:#b3261e;margin-bottom:10px}",
  ".src{border-top:1px solid #eee;padding-top:8px;margin-top:8px}",
  ".srchead{font:13px system-ui,sans-serif;color:#555;margin-bottom:4px}.srchead b{color:var(--c)}",
  ".passage{font-size:14px;color:#444;line-height:1.5}.passage b{color:#000;background:color-mix(in srgb,var(--c) 22%,transparent)}",
  label_css,
  "</style></head><body><main>",
  "<h1>Source attribution report</h1>",
  sprintf("<div class=\"muted\">Biography: %s &middot; Sources: %s &middot; Generated %s</div>",
          esc(basename(bio_file)), esc(paste(sprintf("%s (%s)", src_names, basename(source_files)), collapse = "; ")),
          format(Sys.time(), "%d %B %Y")),
  "<h2>Summary</h2>",
  "<table><tr><th>Label</th><th>Sentences</th><th>Words</th><th>% of words</th><th></th></tr>", summary_html, "</table>",
  sprintf("<p class=\"note\">Coverage = share of a sentence's rarity-weighted content words found in the single best-matching passage of a source (0 to 1). A sentence is assigned to a source when coverage is at least %.2f; &ldquo;Both&rdquo; means the sources are within %.2f of each other; &ldquo;Neither&rdquo; means no source reaches %.2f. These are word-overlap measures, so treat &ldquo;Neither&rdquo; as &ldquo;check this&rdquo;: it catches invented content but also the AI's own summarising and heavily reworded passages.</p>",
          min_support, tie_margin, min_support),
  "<h2>Biography, colour-coded by source</h2>",
  sprintf("<p>%s</p>", legend_html),
  "<p class=\"note\">Click a sentence to jump to its evidence. Dashed underline = weak match (coverage below the moderate band), so read the evidence before relying on it. <u class=\"nosrc\">Wavy red underline</u> = a name, number or date that appears nowhere in any source.</p>",
  "<div class=\"text\">", bio_html, "</div>",
  "<h2>Evidence for each sentence</h2>",
  "<p class=\"note\">For each sentence: the best-matching passage from every source, with the shared words in bold.</p>",
  detail_html,
  "</main></body></html>"
)
writeLines(enc2utf8(html), file.path(output_dir, "attribution_report.html"), useBytes = TRUE)

cat("\nDone. Open ", file.path(output_dir, "attribution_report.html"), " in a browser.\n", sep = "")
