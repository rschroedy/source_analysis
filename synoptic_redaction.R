# =============================================================================
# synoptic_redaction.R
#
# Redaction criticism of Matthew and Luke, with Mark as their source.
#
# This is redaction_criticism.R pointed at the gospels themselves. Under the
# two-source hypothesis Matthew and Luke each rewrote Mark, and each added
# material shared with the other (Q) and material of their own (M and L).
# The script takes Matthew and then Luke as the redactor. For each it uses
# two "sources":
#   Mark               the source the redactor rewrote
#   the other gospel   a stand-in for the lost Q
# and classes every verse of the redactor's gospel as
#   Triple             parallel in Mark and in the other gospel
#   Mark only          parallel in Mark but not in the other gospel
#   Double (Q)         parallel in the other gospel only
#   own (M or L)       no parallel at all (special material, "Sondergut")
# Where a verse has a parallel in Mark, Mark is taken as its main source
# (Markan priority).
#
# Then the same analyses as redaction_criticism.R:
#   1. Composition   triple / double / Mark-only / special material, and where
#                    the special material sits
#   2. Selection     how much of Mark was used, from which chapters, and which
#                    stretches of Mark were left out
#   3. Order         whether the redactor keeps Mark's sequence
#   4. Rewording     how closely Mark's wording is kept; "minor agreements"
#                    of Matthew and Luke against Mark
#   5. Tendencies    "immediately", emotion words, names, direct speech,
#                    scripture citations; vivid Markan words dropped
#   6. Own style     the redactor's characteristic vocabulary and phrases
#   7. Synopsis      the redactor's verses beside Mark and the other gospel
#
# Text: the World English Bible (public domain), one verse per line in
# Matthew_WEB.txt, Mark_WEB.txt and Luke_WEB.txt ("chapter:verse<TAB>text",
# blank line between the WEB's paragraphs). One translation for all three
# keeps the wording comparable, but the counts measure the English: where the
# translators varied their wording, Greek agreement will be under-counted, and
# features the translation smooths over (such as Mark's historic presents,
# which the WEB renders in the past tense) cannot be measured at all.
#
# The script is self-contained (it does not need source_attribution.R). Put it
# and the three text files in one folder, set the working directory to that
# folder, then:  source("synoptic_redaction.R")
#
# Outputs (one folder per redactor in `output_dir`, e.g. synoptic_output/Matthew):
#   redaction_report.html          the full report
#   redaction_synopsis.csv         one row per verse of the redactor's gospel
#   redaction_source_verses.csv    every verse of Mark and the other gospel: used or omitted
#   redaction_vocabulary.csv       the redactor's characteristic vocabulary
# =============================================================================


# ---- 1. SETTINGS (edit these) ----------------------------------------------

gospel_files <- c(
  Matthew = "Matthew_WEB.txt",
  Mark    = "Mark_WEB.txt",
  Luke    = "Luke_WEB.txt"
)
source_gospel <- "Mark"                 # the source both redactors used
redactors     <- c("Matthew", "Luke")   # each is analysed in turn; the other stands in for Q

output_dir <- "synoptic_output"

# A "passage" is this many consecutive source verses.
window_size <- 3

# Coverage score = share of a verse's (rarity-weighted) content words that also
# occur in one source passage. 0 = nothing shared, 1 = all.
min_support    <- 0.35   # below this = no parallel in that source
moderate_score <- 0.50   # 0.35-0.50 = "weak" parallel: read the evidence
strong_score   <- 0.65   # at or above this = "strong" parallel

# Single verses share words with some passage by chance far more often than
# whole pericopes do. So a parallel only counts when a neighbouring verse
# (within `context_verses`) has its parallel at the same place in the source
# (within `context_gap` verses), unless it scores at least `lone_score` alone.
context_verses <- 2
context_gap    <- 6
lone_score     <- 0.75

# Verses with fewer content words than this are not classified.
min_terms <- 4

# A Mark-based verse is a possible minor agreement when the other gospel's
# parallel adds at least this much coverage on top of Mark's.
conflation_gain <- 0.20

top_n <- 20    # rows shown in the "top ..." tables

# Word lists for the tendency counts (lower case). Edit freely.
immediacy_words <- c("immediately", "straightway", "forthwith")
emotion_words   <- c("angry", "anger", "indignant", "grieved", "grieving", "sighed", "sighing",
                     "amazed", "astonished", "marveled", "compassion", "distressed", "troubled",
                     "loved", "afraid", "terrified", "sorrowful")
vivid_words     <- c(immediacy_words, emotion_words)   # "dropped" check in section 7
scripture_pattern <- "(?i)\\b(it is written|(was|be|been|being) fulfilled|scriptures?)\\b"
# First words of verse-opening linking phrases ("Then Jesus, ...", "When ..., ").
seam_openers <- c("then", "now", "after", "when", "while", "as", "in", "on", "at", "from",
                  "immediately", "again", "behold", "it", "but", "so", "and", "having")

aliases <- character(0)   # spelling equivalences, e.g. c(colour = "color"); none needed for one translation


# ---- 2. PACKAGES ------------------------------------------------------------

needed  <- c("stringi", "SnowballC")
missing <- needed[!vapply(needed, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) {
  message("Installing missing packages: ", paste(missing, collapse = ", "))
  install.packages(missing)
}
library(stringi)


# ---- 3. READING THE GOSPELS -------------------------------------------------

# One row per verse: ref ("3:16"), chapter, paragraph (the WEB's paragraphs),
# text.
read_gospel <- function(path) {
  if (!file.exists(path)) stop("File not found: ", path, "\nWorking directory is ", getwd())
  x    <- readLines(path, warn = FALSE, encoding = "UTF-8")
  para <- cumsum(!nzchar(stri_trim_both(x)))
  keep <- nzchar(stri_trim_both(x))
  part <- stri_split_fixed(x[keep], "\t", n = 2, simplify = TRUE)
  data.frame(ref = part[, 1], chapter = as.integer(stri_extract_first_regex(part[, 1], "^\\d+")),
             paragraph = match(para[keep], unique(para[keep])), text = stri_trim_both(part[, 2]),
             stringsAsFactors = FALSE)
}


# ---- 4. TURNING TEXT INTO COMPARABLE TERMS (as in source_attribution.R) ----

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
  "whose","why","will","with","within","without","would","you","your","s","t"
)

word_pattern <- "[\\p{L}\\p{N}]+(?:['\u2019][\\p{L}]+)?"

normalise_words <- function(w, alias = aliases) {
  w <- stri_trans_tolower(stri_trans_general(w, "Latin-ASCII"))
  w <- stri_replace_all_regex(w, "'(s|t|d|ll|re|ve)?$", "")
  hit <- w %in% names(alias); w[hit] <- alias[w[hit]]
  stop_w <- w %in% stopwords | (nchar(w) < 2 & !stri_detect_regex(w, "^\\d$"))
  w <- SnowballC::wordStem(w, language = "english")
  w[stop_w] <- NA_character_
  w
}

tokenise <- function(text) {
  loc <- stri_locate_all_regex(text, word_pattern, omit_no_match = TRUE)[[1]]
  raw <- stri_sub(text, loc[, 1], loc[, 2])
  data.frame(start = loc[, 1], end = loc[, 2], raw = raw, term = normalise_words(raw),
             stringsAsFactors = FALSE)
}

lower   <- function(raw) stri_trans_tolower(stri_trans_general(raw, "Latin-ASCII"))
n_words <- function(x) sum(stri_count_regex(x, word_pattern))
pct     <- function(a, b) if (b == 0) 0 else 100 * a / b

esc <- function(x) {
  x <- stri_replace_all_fixed(x, "&", "&amp;")
  x <- stri_replace_all_fixed(x, "<", "&lt;")
  x <- stri_replace_all_fixed(x, ">", "&gt;")
  stri_replace_all_fixed(x, "\"", "&quot;")
}

# c("3:1", "3:6") -> "3:1-6"; c("3:1", "4:2") -> "3:1-4:2"
ref_range <- function(r) {
  r <- unique(r)
  if (length(r) == 2 && sub(":.*", "", r[1]) == sub(":.*", "", r[2])) r[2] <- sub(".*:", "", r[2])
  paste(r, collapse = "\u2013")
}

# Words inside quotation marks. The WEB re-opens a quotation at each new
# paragraph without closing the previous one, so an opening mark inside an
# open quotation is ignored.
speech_words <- function(texts) {
  x  <- paste(texts, collapse = " ")
  ev <- rbind(data.frame(p = stri_locate_all_fixed(x, "\u201c")[[1]][, 1], open = TRUE),
              data.frame(p = stri_locate_all_fixed(x, "\u201d")[[1]][, 1], open = FALSE))
  ev <- ev[!is.na(ev$p), ]; ev <- ev[order(ev$p), ]
  inside <- FALSE; start <- 0; n <- 0
  for (k in seq_len(nrow(ev))) {
    if (ev$open[k] && !inside) { inside <- TRUE; start <- ev$p[k] }
    else if (!ev$open[k] && inside) { inside <- FALSE; n <- n + n_words(stri_sub(x, start, ev$p[k])) }
  }
  n
}


# ---- 5. LOAD EVERYTHING -----------------------------------------------------

message("Reading the gospels ...")
gospels   <- lapply(gospel_files, read_gospel)
tok_all   <- lapply(gospels, function(g) lapply(g$text, tokenise))
terms_all <- lapply(tok_all, function(tl) lapply(tl, function(t) unique(t$term[!is.na(t$term)])))
for (g in names(gospels)) {
  message(sprintf("  %-8s %5d verses, %6d words", g, nrow(gospels[[g]]), n_words(gospels[[g]]$text)))
}


# ---- 6. ONE REDACTOR --------------------------------------------------------

analyse <- function(redactor) {
  other     <- setdiff(redactors, redactor)[1]
  src_names <- c(source_gospel, other)
  own       <- sprintf("%s's own (%s)", redactor, substr(redactor, 1, 1))
  lab_mark  <- sprintf("%s only", source_gospel)
  lab_dbl   <- sprintf("Double (%s only, Q)", other)
  lab_tri   <- sprintf("Triple (%s + %s)", source_gospel, other)
  out_dir   <- file.path(output_dir, redactor)
  message("\n==== ", redactor, " as redactor; sources ", source_gospel, " and ", other, " (for Q) ====")

  red       <- gospels[[redactor]]
  n_red     <- nrow(red)
  red_tok   <- tok_all[[redactor]]
  red_terms <- terms_all[[redactor]]
  src_sent  <- gospels[src_names]
  src_tok   <- tok_all[src_names]
  src_terms <- terms_all[src_names]
  src_vocab <- lapply(src_terms, function(tl) unique(unlist(tl)))
  full_ref  <- function(g, r) paste(g, r)

  # ---- weights and passages ----
  n_sent <- sum(lengths(src_terms))
  df     <- table(unlist(src_terms))
  idf    <- log(n_sent / as.numeric(df)); names(idf) <- names(df)
  max_w  <- log(n_sent)
  weight_of <- function(terms) { w <- idf[terms]; w[is.na(w)] <- max_w; unname(w) }

  windows <- lapply(src_terms, function(tl) {
    k <- min(window_size, length(tl))
    lapply(seq_len(length(tl) - k + 1), function(i) {
      rows <- i:(i + k - 1)
      list(rows = rows, terms = unique(unlist(tl[rows])))
    })
  })
  # term -> windows that contain it (makes scoring fast)
  win_index <- lapply(windows, function(ws) {
    split(rep(seq_along(ws), vapply(ws, function(w) length(w$terms), integer(1))),
          unlist(lapply(ws, `[[`, "terms")))
  })

  best_match <- function(terms, s) {
    w     <- weight_of(terms)
    total <- sum(w)
    if (length(terms) == 0 || total == 0) return(list(score = 0, rows = integer(0), matched = character(0)))
    sc <- numeric(length(windows[[s]]))
    for (j in seq_along(terms)) {
      ix <- win_index[[s]][[terms[j]]]
      if (!is.null(ix)) sc[ix] <- sc[ix] + w[j]
    }
    sc   <- sc / total
    b    <- which.max(sc)
    rows <- windows[[s]][[b]]$rows
    cover <- function(r) sum(w[terms %in% unlist(src_terms[[s]][r])])
    full  <- cover(rows)
    while (length(rows) > 1 && cover(rows[-1]) >= 0.97 * full) rows <- rows[-1]
    while (length(rows) > 1 && cover(rows[-length(rows)]) >= 0.97 * full) rows <- rows[-length(rows)]
    list(score = sc[b], rows = rows, matched = terms[terms %in% unlist(src_terms[[s]][rows])])
  }

  message("Matching ", n_red, " verses of ", redactor, " ...")
  matches <- lapply(seq_len(n_red), function(i) {
    m <- lapply(src_names, function(s) best_match(red_terms[[i]], s))
    names(m) <- src_names
    m
  })

  passage_text <- function(s, rows) paste(src_sent[[s]]$text[rows], collapse = " ")
  passage_ref  <- function(s, rows) ref_range(src_sent[[s]]$ref[range(rows)])

  # ---- classification ----
  score <- matrix(0, n_red, length(src_names), dimnames = list(NULL, src_names))
  for (s in src_names) score[, s] <- vapply(matches, function(m) m[[s]]$score, numeric(1))
  assessed  <- lengths(red_terms) >= min_terms
  candidate <- assessed & score >= min_support
  start_row <- matrix(0, n_red, length(src_names), dimnames = list(NULL, src_names))
  for (s in src_names) start_row[, s] <- vapply(matches, function(m) min(c(m[[s]]$rows, Inf)), numeric(1))
  supported <- candidate
  for (s in src_names) for (i in which(candidate[, s])) {
    nb <- setdiff(max(1, i - context_verses):min(n_red, i + context_verses), i)
    nb <- nb[candidate[nb, s]]
    supported[i, s] <- score[i, s] >= lone_score || any(abs(start_row[nb, s] - start_row[i, s]) <= context_gap)
  }
  uses      <- supported
  in_mark   <- supported[, source_gospel]; in_other <- supported[, other]
  tradition <- ifelse(!assessed, "Not assessed", ifelse(in_mark & in_other, lab_tri,
               ifelse(in_mark, lab_mark, ifelse(in_other, lab_dbl, own))))
  primary   <- ifelse(in_mark, source_gospel, ifelse(in_other, other, NA_character_))
  confidence <- ifelse(is.na(primary), "", ifelse(apply(score, 1, max) >= strong_score, "strong",
                ifelse(apply(score, 1, max) >= moderate_score, "moderate", "weak")))
  trad_levels <- c(lab_tri, lab_mark, lab_dbl, own, "Not assessed")
  trad_levels <- trad_levels[trad_levels %in% tradition]
  red_words   <- stri_count_regex(red$text, word_pattern)


  # ---- composition ----
  para_n <- ave(seq_len(n_red), red$paragraph, FUN = length)
  para_i <- ave(seq_len(n_red), red$paragraph, FUN = seq_along)
  position <- ifelse(para_n < 3, "short paragraph",
                     ifelse(para_i == 1, "opening", ifelse(para_i == para_n, "closing", "middle")))

  composition <- data.frame(
    tradition = trad_levels,
    verses    = as.vector(table(factor(tradition, trad_levels))),
    words     = as.vector(tapply(red_words, factor(tradition, trad_levels), sum)),
    stringsAsFactors = FALSE
  )
  composition$pct_of_words <- round(100 * composition$words / sum(composition$words), 1)

  pos_tbl <- do.call(rbind, lapply(c("opening", "middle", "closing"), function(p) {
    k <- assessed & position == p
    data.frame(position = p, verses = sum(k), own = sum(tradition[k] == own),
               pct_own = round(pct(sum(tradition[k] == own), sum(k))), stringsAsFactors = FALSE)
  }))

  runs <- rle(tradition[assessed])
  tl   <- trad_levels[trad_levels != "Not assessed"]
  block_tbl <- data.frame(
    tradition   = tl,
    blocks      = vapply(tl, function(t) sum(runs$values == t), numeric(1)),
    longest_run = vapply(tl, function(t) max(c(0, runs$lengths[runs$values == t])), numeric(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )

  all_source_text <- lower(paste(unlist(lapply(src_sent, function(sn) sn$text)), collapse = " "))
  opening <- stri_match_first_regex(red$text, "^([^,]{2,60}),")[, 2]
  is_seam <- !is.na(opening) & stri_count_regex(opening, word_pattern) <= 6 &
    lower(stri_extract_first_regex(red$text, word_pattern)) %in% seam_openers
  seams <- data.frame(verse = red$ref[is_seam], phrase = opening[is_seam], tradition = tradition[is_seam],
                      in_a_source = ifelse(stri_detect_fixed(all_source_text, lower(opening[is_seam])), "yes", "no"),
                      stringsAsFactors = FALSE)
  seam_tbl <- if (nrow(seams) == 0) data.frame(phrase = character(0)) else {
    k <- lower(seams$phrase)
    cnt <- sort(table(k), decreasing = TRUE)
    d <- data.frame(phrase = names(cnt), times = as.vector(cnt), stringsAsFactors = FALSE)
    d$in_a_source <- seams$in_a_source[match(d$phrase, k)]
    d$verses <- vapply(d$phrase, function(p) paste(head(seams$verse[k == p], 8), collapse = ", "), character(1))
    d[seq_len(min(top_n, nrow(d))), ]
  }


  # ---- selection ----
  used <- lapply(src_names, function(s) {
    u <- rep(FALSE, nrow(src_sent[[s]]))
    for (i in which(uses[, s])) u[matches[[i]][[s]]$rows] <- TRUE
    u
  })
  names(used) <- src_names

  # From the source's side: how much of each source verse turns up in its
  # best-matching verse of the redactor (0 to 1).
  red_index <- split(rep(seq_len(n_red), lengths(red_terms)), unlist(red_terms))
  taken_up <- lapply(src_names, function(s) vapply(src_terms[[s]], function(t) {
    if (length(t) == 0) return(0)
    w <- weight_of(t); sc <- numeric(n_red)
    for (j in seq_along(t)) { ix <- red_index[[t[j]]]; if (!is.null(ix)) sc[ix] <- sc[ix] + w[j] }
    max(sc) / sum(w)
  }, numeric(1)))
  names(taken_up) <- src_names

  selection <- do.call(rbind, lapply(src_names, function(s) {
    wd <- stri_count_regex(src_sent[[s]]$text, word_pattern)
    data.frame(source = s, verses = length(wd), verses_used = sum(used[[s]]),
               pct_verses_used = round(pct(sum(used[[s]]), length(wd)), 1),
               words = sum(wd), words_used = sum(wd[used[[s]]]),
               pct_words_used = round(pct(sum(wd[used[[s]]]), sum(wd)), 1),
               redactor_verses_drawn = sum(uses[, s]), stringsAsFactors = FALSE)
  }))

  first_row <- function(i, s) min(matches[[i]][[s]]$rows)
  location <- lapply(src_names, function(s) {
    loc   <- src_sent[[s]]$chapter
    lv    <- sort(unique(loc))
    drawn <- if (any(uses[, s])) loc[vapply(which(uses[, s]), first_row, numeric(1), s = s)] else integer(0)
    data.frame(source = s, chapter = lv, source_verses = as.vector(table(factor(loc, lv))),
               used_verses = as.vector(tapply(used[[s]], factor(loc, lv), sum)),
               redactor_verses_drawn = as.vector(table(factor(drawn, lv))), stringsAsFactors = FALSE)
  })
  names(location) <- src_names

  # Stretches of a source with no parallel in the redactor: runs of 2+
  # consecutive verses that no verse of the redactor was matched to.
  omitted_runs <- lapply(src_names, function(s) {
    om <- !used[[s]]
    r  <- rle(om); end <- cumsum(r$lengths); start <- end - r$lengths + 1
    k  <- r$values & r$lengths >= 2
    if (!any(k)) return(data.frame(passage = character(0)))
    d <- data.frame(passage = vapply(which(k), function(j) passage_ref(s, start[j]:end[j]), character(1)),
                    verses = r$lengths[k],
                    words = vapply(which(k), function(j) n_words(src_sent[[s]]$text[start[j]:end[j]]), numeric(1)),
                    begins = stri_sub(src_sent[[s]]$text[start[k]], 1, 110), stringsAsFactors = FALSE)
    d$begins <- ifelse(stri_length(src_sent[[s]]$text[start[k]]) > 110, paste0(d$begins, "\u2026"), d$begins)
    d[order(-d$verses), ]
  })
  names(omitted_runs) <- src_names

  is_cap <- function(t) stri_detect_regex(t$raw, "^\\p{Lu}") & !is.na(t$term) & seq_len(nrow(t)) > 1
  red_term_all <- unlist(lapply(red_tok, function(t) t$term[!is.na(t$term)]))
  omitted_names <- lapply(src_names, function(s) {
    tk <- do.call(rbind, lapply(src_tok[[s]], function(t) t[is_cap(t), c("raw", "term")]))
    tk <- tk[!tk$term %in% red_term_all & stri_length(tk$raw) > 2, ]
    if (is.null(tk) || nrow(tk) == 0) return(data.frame(name = character(0), mentions = integer(0)))
    cnt <- sort(table(tk$term), decreasing = TRUE)
    cnt <- cnt[seq_len(min(top_n, length(cnt)))]
    data.frame(name = vapply(names(cnt), function(k) names(which.max(table(tk$raw[tk$term == k]))), character(1)),
               mentions = as.vector(cnt), stringsAsFactors = FALSE, row.names = NULL)
  })
  names(omitted_names) <- src_names


  # ---- order ----
  order_pts <- do.call(rbind, lapply(src_names, function(s) {
    idx <- which(uses[, s])
    if (length(idx) == 0) return(NULL)
    row <- vapply(idx, first_row, numeric(1), s = s)
    data.frame(source = s, verse = idx, source_row = row,
               source_pct = 100 * row / nrow(src_sent[[s]]), stringsAsFactors = FALSE)
  }))
  order_tbl <- do.call(rbind, lapply(src_names, function(s) {
    p <- order_pts[order_pts$source == s, ]
    rho  <- if (nrow(p) >= 4) suppressWarnings(cor(p$verse, p$source_row, method = "spearman")) else NA
    back <- if (nrow(p) >= 2) sum(diff(p$source_row[order(p$verse)]) < -window_size) else 0
    data.frame(source = s, parallels = nrow(p), order_correlation = round(rho, 2),
               backward_jumps = back, stringsAsFactors = FALSE)
  }))


  # ---- rewording ----
  longest_run <- function(a, b) {
    best <- 0; end_a <- 0; prev <- integer(length(b) + 1)
    for (i in seq_along(a)) {
      cur <- integer(length(b) + 1)
      m <- which(b == a[i])
      if (length(m) > 0) {
        cur[m + 1] <- prev[m] + 1L
        if (max(cur) > best) { best <- max(cur); end_a <- i }
      }
      prev <- cur
    }
    if (best == 0) character(0) else a[(end_a - best + 1):end_a]
  }

  message("Comparing wording ...")
  wording <- do.call(rbind, lapply(seq_len(n_red), function(i) {
    s <- primary[i]
    out <- data.frame(shared_content_words_pct = NA_real_, longest_verbatim_run = "", run_length = 0L,
                      agreement_with = "", agreement_words = "", stringsAsFactors = FALSE)
    if (is.na(s)) return(out)
    terms <- red_terms[[i]]
    m     <- matches[[i]][[s]]
    out$shared_content_words_pct <- round(pct(sum(terms %in% m$matched), length(terms)))
    src_words <- lower(unlist(lapply(src_tok[[s]][m$rows], function(t) t$raw)))
    run <- longest_run(lower(red_tok[[i]]$raw), src_words)
    out$longest_verbatim_run <- paste(run, collapse = " ")
    out$run_length <- length(run)
    # Minor agreement: Mark is the main source, but the other gospel's
    # parallel shares words with this verse that Mark's lacks.
    if (s == source_gospel && supported[i, other]) {
      w <- weight_of(terms)
      extra <- setdiff(matches[[i]][[other]]$matched, m$matched)
      if (length(extra) >= 2 && sum(w[terms %in% extra]) / sum(w) >= conflation_gain) {
        t <- red_tok[[i]]
        out$agreement_with  <- other
        out$agreement_words <- paste(unique(t$raw[t$term %in% extra]), collapse = " ")
      }
    }
    out
  }))

  wording_tbl <- do.call(rbind, lapply(src_names, function(s) {
    k <- which(primary == s)
    if (length(k) == 0) return(NULL)
    data.frame(main_source = s, verses = length(k),
               median_shared_content_words_pct = median(wording$shared_content_words_pct[k]),
               median_longest_run = median(wording$run_length[k]),
               longest_run = max(wording$run_length[k]),
               verses_with_run_of_8_plus = sum(wording$run_length[k] >= 8), stringsAsFactors = FALSE)
  }))
  top_runs <- data.frame(verse = red$ref, source = primary, words = wording$run_length,
                         run = wording$longest_verbatim_run, stringsAsFactors = FALSE)
  top_runs <- top_runs[order(-top_runs$words), ][seq_len(min(top_n, sum(top_runs$words >= 4))), ]
  agreements <- data.frame(verse = red$ref, words_shared_with_other = wording$agreement_words,
                           text = red$text, stringsAsFactors = FALSE)[nzchar(wording$agreement_with), ]
  names(agreements)[2] <- sprintf("words shared with %s, not in %s", other, source_gospel)


  # ---- tendencies ----
  doc_stats <- function(tok_list, texts) {
    raw   <- unlist(lapply(tok_list, function(t) t$raw))
    lw    <- lower(raw)
    named <- unlist(lapply(tok_list, function(t) stri_detect_regex(t$raw, "^\\p{Lu}") & !is.na(t$term) & seq_len(nrow(t)) > 1))
    c(words = length(raw),
      immediacy = sum(lw %in% immediacy_words),
      emotion = sum(lw %in% emotion_words),
      names = sum(named),
      speech = speech_words(texts),
      scripture = sum(stri_count_regex(texts, scripture_pattern)))
  }
  stats <- cbind(doc_stats(red_tok, red$text),
                 sapply(src_names, function(s) doc_stats(src_tok[[s]], src_sent[[s]]$text)))
  colnames(stats)[1] <- redactor
  per_1000 <- function(row) round(1000 * stats[row, ] / stats["words", ], 1)
  tendency_tbl <- data.frame(
    measure = c("\u201cImmediately\u201d and similar per 1,000 words",
                "Emotion words (angry, grieved, amazed, compassion ...) per 1,000 words",
                "Names (capitalised words inside a verse) per 1,000 words",
                "Direct speech: words inside quotation marks (%)",
                "Scripture citations (\u201cit is written\u201d, \u201cwas fulfilled\u201d, \u201cScriptures\u201d) (count)"),
    rbind(per_1000("immediacy"), per_1000("emotion"), per_1000("names"),
          round(100 * stats["speech", ] / stats["words", ]), stats["scripture", ]),
    check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
  )

  # Vivid words dropped: the source parallel has "immediately" or an emotion
  # word next to the shared wording; the redactor's verse has none.
  is_vivid <- function(t) lower(t$raw) %in% vivid_words
  vivid_dropped <- do.call(rbind, lapply(which(!is.na(primary)), function(i) {
    if (any(is_vivid(red_tok[[i]]))) return(NULL)
    s <- primary[i]
    hit <- do.call(rbind, lapply(matches[[i]][[s]]$rows, function(r) {
      t <- src_tok[[s]][[r]]
      w <- weight_of(red_terms[[i]])
      if (!any(is_vivid(t)) || sum(w[red_terms[[i]] %in% t$term]) / sum(w) < min_support) return(NULL)
      near <- vapply(which(is_vivid(t)), function(h) {
        sum(t$term[max(1, h - 8):min(nrow(t), h + 8)] %in% red_terms[[i]]) >= 2
      }, logical(1))
      if (!any(near)) return(NULL)
      data.frame(word = paste(unique(lower(t$raw[is_vivid(t)])), collapse = ", "), row = r, stringsAsFactors = FALSE)
    }))
    if (is.null(hit)) return(NULL)
    data.frame(verse = i, source = s, word_in_source = paste(unique(hit$word), collapse = ", "),
               redactor = red$text[i], source_text = paste(src_sent[[s]]$text[hit$row], collapse = " "),
               source_ref = passage_ref(s, hit$row), stringsAsFactors = FALSE)
  }))
  if (is.null(vivid_dropped)) vivid_dropped <- data.frame(verse = integer(0))


  # ---- the redactor's own style ----
  src_term_all <- unlist(lapply(src_tok, function(tl) unlist(lapply(tl, function(t) t$term[!is.na(t$term)]))))
  red_raw_all  <- unlist(lapply(red_tok, function(t) lower(t$raw[!is.na(t$term)])))
  tok_cap <- rbind(do.call(rbind, lapply(red_tok, function(t) t[seq_len(nrow(t)) > 1 & !is.na(t$term), c("raw", "term")])),
                   do.call(rbind, lapply(src_tok, function(tl) do.call(rbind, lapply(tl, function(t) t[seq_len(nrow(t)) > 1 & !is.na(t$term), c("raw", "term")])))))
  cap_share  <- tapply(stri_detect_regex(tok_cap$raw, "^[\\p{Lu}\\d]"), tok_cap$term, mean)
  name_terms <- names(cap_share)[cap_share > 0.5]

  n_b <- stats["words", redactor]; n_s <- sum(stats["words", src_names])
  b_cnt <- table(red_term_all); s_cnt <- table(src_term_all)
  vocab <- data.frame(term = names(b_cnt), redactor_count = as.vector(b_cnt),
                      sources_count = as.vector(s_cnt[names(b_cnt)]), stringsAsFactors = FALSE)
  vocab$sources_count[is.na(vocab$sources_count)] <- 0
  vocab$word <- vapply(vocab$term, function(k) names(which.max(table(red_raw_all[red_term_all == k]))), character(1))
  vocab$redactor_per_1000 <- round(1000 * vocab$redactor_count / n_b, 1)
  vocab$sources_per_1000  <- round(1000 * vocab$sources_count / n_s, 1)
  vocab$times_more_frequent <- round(((vocab$redactor_count + 0.5) / (n_b + 1)) / ((vocab$sources_count + 0.5) / (n_s + 1)), 1)
  vocab <- vocab[!vocab$term %in% name_terms & vocab$redactor_count >= 5 & vocab$times_more_frequent >= 3, ]
  vocab <- vocab[order(-vocab$redactor_count, -vocab$times_more_frequent),
                 c("word", "redactor_count", "sources_count", "redactor_per_1000", "sources_per_1000", "times_more_frequent")]

  avoided <- s_cnt[!names(s_cnt) %in% names(b_cnt) & !names(s_cnt) %in% name_terms]
  avoided <- sort(avoided, decreasing = TRUE)[seq_len(min(top_n, length(avoided)))]
  src_raw_all <- unlist(lapply(src_tok, function(tl) unlist(lapply(tl, function(t) lower(t$raw[!is.na(t$term)])))))
  avoided_tbl <- data.frame(word = vapply(names(avoided), function(k) names(which.max(table(src_raw_all[src_term_all == k]))), character(1)),
                            sources_count = as.vector(avoided), stringsAsFactors = FALSE, row.names = NULL)

  # Stock phrases: neighbouring content-word pairs used 3+ times by the
  # redactor and never in either source.
  pair_keys <- function(t) {
    t <- t[!is.na(t$term), ]
    if (nrow(t) < 2) return(data.frame(key = character(0), start = integer(0), end = integer(0)))
    data.frame(key = paste(t$term[-nrow(t)], t$term[-1]), start = t$start[-nrow(t)], end = t$end[-1], stringsAsFactors = FALSE)
  }
  red_pairs <- do.call(rbind, lapply(seq_len(n_red), function(i) {
    p <- pair_keys(red_tok[[i]])
    if (nrow(p) == 0) return(NULL)
    data.frame(key = p$key, phrase = lower(stri_sub(red$text[i], p$start, p$end)), verse = red$ref[i], stringsAsFactors = FALSE)
  }))
  src_pairs <- unique(unlist(lapply(src_tok, function(tl) unlist(lapply(tl, function(t) pair_keys(t)$key)))))
  pair_n  <- table(red_pairs$key[!red_pairs$key %in% src_pairs])
  pair_n  <- sort(pair_n[pair_n >= 3], decreasing = TRUE)
  phrases <- data.frame(
    phrase = vapply(names(pair_n), function(k) red_pairs$phrase[red_pairs$key == k][1], character(1)),
    times  = as.vector(pair_n),
    verses = vapply(names(pair_n), function(k) paste(head(red_pairs$verse[red_pairs$key == k], 8), collapse = ", "), character(1)),
    stringsAsFactors = FALSE, row.names = NULL)
  phrases <- phrases[seq_len(min(top_n, nrow(phrases))), ]


  # ---- CSV output ----
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  synopsis <- data.frame(verse = red$ref, paragraph = red$paragraph, position_in_paragraph = position,
                         tradition = tradition, confidence = confidence, main_source = primary,
                         stringsAsFactors = FALSE)
  for (s in src_names) synopsis[[paste0("score_", s)]] <- round(score[, s], 2)
  synopsis <- cbind(synopsis, wording,
                    vivid_word_dropped = ifelse(seq_len(n_red) %in% vivid_dropped$verse, "yes", ""),
                    text = red$text, stringsAsFactors = FALSE)
  for (s in src_names) {
    synopsis[[paste0("parallel_ref_", s)]] <- vapply(seq_len(n_red), function(i) {
      if (supported[i, s]) passage_ref(s, matches[[i]][[s]]$rows) else ""
    }, character(1))
    synopsis[[paste0("parallel_", s)]] <- vapply(seq_len(n_red), function(i) {
      if (supported[i, s]) passage_text(s, matches[[i]][[s]]$rows) else ""
    }, character(1))
  }
  write.csv(synopsis, file.path(out_dir, "redaction_synopsis.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  source_out <- do.call(rbind, lapply(src_names, function(s) {
    data.frame(source = s, verse = src_sent[[s]]$ref, used = used[[s]],
               taken_up_score = round(taken_up[[s]], 2), text = src_sent[[s]]$text, stringsAsFactors = FALSE)
  }))
  write.csv(source_out, file.path(out_dir, "redaction_source_verses.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(vocab, file.path(out_dir, "redaction_vocabulary.csv"), row.names = FALSE, fileEncoding = "UTF-8")


  # ---- HTML report ----
  hues_light <- c("#2a78d6", "#eb6834")
  hues_dark  <- c("#3987e5", "#d95926")
  kc <- function(label) {
    if (label %in% c(source_gospel, lab_mark)) return("k1")
    if (label %in% c(other, lab_dbl)) return("k2")
    if (label == own) return("kown")
    if (label == "Not assessed") return("kna")
    "kboth"
  }
  key <- function(label) sprintf("<span class=\"key %s\"><i></i>%s</span>", kc(label), esc(label))

  html_table <- function(df, raw_cols = character(0), num_cols = names(df)[vapply(df, is.numeric, logical(1))]) {
    if (nrow(df) == 0) return("<p class=\"note\">None found.</p>")
    head <- paste(sprintf("<th%s>%s</th>", ifelse(names(df) %in% num_cols, " class=\"n\"", ""),
                          esc(stri_replace_all_fixed(names(df), "_", " "))), collapse = "")
    body <- vapply(seq_len(nrow(df)), function(r) {
      paste0("<tr>", paste(vapply(names(df), function(cn) {
        v <- df[[cn]][r]
        v <- if (is.na(v)) "" else if (cn %in% raw_cols) as.character(v) else esc(format(v, trim = TRUE))
        sprintf("<td%s>%s</td>", if (cn %in% num_cols) " class=\"n\"" else "", v)
      }, character(1)), collapse = ""), "</tr>")
    }, character(1))
    paste0("<div class=\"scroll\"><table><tr>", head, "</tr>", paste(body, collapse = ""), "</table></div>")
  }

  paint <- function(text, tok, cls) {
    keep <- !is.na(cls)
    if (!any(keep)) return(esc(text))
    tok <- tok[keep, , drop = FALSE]; cls <- cls[keep]
    out <- character(0); pos <- 1
    for (j in seq_len(nrow(tok))) {
      out <- c(out, esc(stri_sub(text, pos, tok$start[j] - 1)), "<span class=\"", cls[j], "\">", esc(tok$raw[j]), "</span>")
      pos <- tok$end[j] + 1
    }
    paste(c(out, esc(stri_sub(text, pos, stri_length(text)))), collapse = "")
  }

  # One row per chapter, one cell per verse.
  svg_strip <- function() {
    chs <- unique(red$chapter); vmax <- max(table(red$chapter))
    ml <- 26; W <- 880; rh <- 13; gap <- 4
    cw <- (W - ml) / vmax
    vi <- ave(seq_len(n_red), red$chapter, FUN = seq_along)
    x  <- ml + (vi - 1) * cw; y <- (match(red$chapter, chs) - 1) * (rh + gap)
    wcell <- max(cw - 1, 0.5)
    cells <- vapply(seq_len(n_red), function(i) {
      tip <- esc(sprintf("%s %s. %s: %s", redactor, red$ref[i], tradition[i], red$text[i]))
      if (kc(tradition[i]) == "kboth") {
        sprintf("<g><title>%s</title><rect class=\"k1\" x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\"/><rect class=\"k2\" x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\"/></g>",
                tip, x[i], y[i], wcell, rh / 2, x[i], y[i] + rh / 2, wcell, rh / 2)
      } else {
        sprintf("<rect class=\"%s\" x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%d\" rx=\"1\"><title>%s</title></rect>",
                kc(tradition[i]), x[i], y[i], wcell, rh, tip)
      }
    }, character(1))
    labs <- sprintf("<text x=\"%d\" y=\"%.1f\" class=\"ax\" text-anchor=\"end\">%d</text>", ml - 6,
                    (seq_along(chs) - 1) * (rh + gap) + rh - 2, chs)
    H <- length(chs) * (rh + gap)
    sprintf("<svg viewBox=\"0 0 %d %d\" class=\"chart\" role=\"img\" aria-label=\"Each verse of %s coloured by tradition\">%s%s</svg>",
            W, H, redactor, paste(cells, collapse = ""), paste(labs, collapse = ""))
  }

  svg_bars <- function(labels, values, tips, cls, x_title) {
    W <- 880; H <- 210; ml <- 34; mr <- 8; mt <- 18; mb <- 40
    n <- length(values); pw <- W - ml - mr; ph <- H - mt - mb
    ticks <- pretty(c(0, max(1, values)), n = 4); ticks <- ticks[ticks == round(ticks)]; ymax <- max(ticks)
    step <- pw / n; bw <- min(22, step - 2)
    x <- ml + (seq_len(n) - 0.5) * step - bw / 2
    y <- mt + ph * (1 - values / ymax); y0 <- mt + ph
    r <- pmin(4, bw / 2, y0 - y)
    bars <- ifelse(values > 0, sprintf("<path class=\"%s\" d=\"M%.1f,%.1f V%.1f Q%.1f,%.1f %.1f,%.1f H%.1f Q%.1f,%.1f %.1f,%.1f V%.1f Z\"><title>%s</title></path>",
                                       cls, x, y0, y + r, x, y, x + r, y, x + bw - r, x + bw, y, x + bw, y + r, y0, esc(tips)), "")
    grid <- sprintf("<line class=\"grid\" x1=\"%d\" x2=\"%d\" y1=\"%.1f\" y2=\"%.1f\"/><text class=\"ax\" text-anchor=\"end\" x=\"%d\" y=\"%.1f\">%d</text>",
                    ml, W - mr, mt + ph * (1 - ticks / ymax), mt + ph * (1 - ticks / ymax), ml - 6, mt + ph * (1 - ticks / ymax) + 4, ticks)
    xl <- sprintf("<text class=\"ax\" text-anchor=\"middle\" x=\"%.1f\" y=\"%.1f\">%s</text>", x + bw / 2, y0 + 15, esc(labels))
    top <- order(-values)[seq_len(min(3, sum(values > 0)))]
    vl <- sprintf("<text class=\"val\" text-anchor=\"middle\" x=\"%.1f\" y=\"%.1f\">%d</text>", x[top] + bw / 2, y[top] - 5, values[top])
    sprintf("<svg viewBox=\"0 0 %d %d\" class=\"chart\" role=\"img\" aria-label=\"%s\">%s%s%s%s<text class=\"ax\" text-anchor=\"middle\" x=\"%.1f\" y=\"%d\">%s</text></svg>",
            W, H, esc(x_title), paste(grid, collapse = ""), paste(bars, collapse = ""), paste(xl, collapse = ""),
            paste(vl, collapse = ""), ml + pw / 2, H - 4, esc(x_title))
  }

  # Axes are marked at chapter starts of both gospels.
  svg_order <- function(s) {
    p <- order_pts[order_pts$source == s, ]
    W <- 430; H <- 320; ml <- 44; mr <- 12; mt <- 12; mb <- 40
    pw <- W - ml - mr; ph <- H - mt - mb
    n_src <- nrow(src_sent[[s]])
    X <- function(v) ml + pw * (v - 1) / max(1, n_red - 1)
    Y <- function(v) mt + ph * (1 - (v - 1) / max(1, n_src - 1))
    ch_start <- function(ch) { k <- which(!duplicated(ch)); k[ch[k] %in% unique(c(1, pretty(range(ch), n = 6)))] }
    xk <- ch_start(red$chapter); yk <- ch_start(src_sent[[s]]$chapter)
    grid <- c(sprintf("<line class=\"grid\" x1=\"%d\" x2=\"%d\" y1=\"%.1f\" y2=\"%.1f\"/><text class=\"ax\" text-anchor=\"end\" x=\"%d\" y=\"%.1f\">%d</text>",
                      ml, W - mr, Y(yk), Y(yk), ml - 6, Y(yk) + 4, src_sent[[s]]$chapter[yk]),
              sprintf("<text class=\"ax\" text-anchor=\"middle\" x=\"%.1f\" y=\"%d\">%d</text>", X(xk), H - mb + 16, red$chapter[xk]))
    dots <- sprintf("<circle class=\"dot %s\" cx=\"%.1f\" cy=\"%.1f\" r=\"3\"><title>%s</title></circle>", kc(s), X(p$verse), Y(p$source_row),
                    esc(sprintf("%s %s \u2192 %s %s", redactor, red$ref[p$verse], s, src_sent[[s]]$ref[p$source_row])))
    sprintf("<figure><figcaption>%s</figcaption><svg viewBox=\"0 0 %d %d\" class=\"chart\" role=\"img\" aria-label=\"Order of %s material\">%s%s<text class=\"ax\" text-anchor=\"middle\" x=\"%.1f\" y=\"%d\">Chapter of %s</text></svg></figure>",
            esc(sprintf("%s: chapter of the parallel in %s (vertical) against chapter of %s (horizontal)", s, s, redactor)), W, H, esc(s),
            paste(grid, collapse = ""), paste(dots, collapse = ""), ml + pw / 2, H - 6, redactor)
  }

  # --- headline findings, written from the numbers ---
  comp <- setNames(composition$pct_of_words, composition$tradition)
  get  <- function(v, k) if (k %in% names(v)) v[[k]] else 0
  sel_mark <- selection[selection$source == source_gospel, ]
  ord_mark <- order_tbl[order_tbl$source == source_gospel, ]
  wd_mark  <- wording_tbl[wording_tbl$main_source == source_gospel, ]
  biggest  <- omitted_runs[[source_gospel]]
  profile <- c(
    sprintf("<b>Composition.</b> Of %s's words, %.0f%% are triple tradition, %.0f%% shared with %s only, %.0f%% double tradition (shared with %s only, i.e. Q), and %.0f%% %s's own special material.",
            redactor, get(comp, lab_tri), get(comp, lab_mark), source_gospel, get(comp, lab_dbl), other, get(comp, own), redactor),
    sprintf("<b>Selection.</b> %s has a parallel for %d of %s's %d verses (%.0f%%)%s.",
            redactor, sel_mark$verses_used, source_gospel, sel_mark$verses, sel_mark$pct_verses_used,
            if (nrow(biggest)) sprintf("; the longest stretch of %s with no parallel is %s (%d verses)", source_gospel, biggest$passage[1], biggest$verses[1]) else ""),
    sprintf("<b>Order.</b> %s order correlation %s; %s %s (1 = the source's order is kept exactly, 0 = no relation).",
            source_gospel, format(ord_mark$order_correlation, nsmall = 2), other,
            format(order_tbl$order_correlation[order_tbl$source == other], nsmall = 2)),
    sprintf("<b>Rewording.</b> Verses based on %s share a median %.0f%% of their content words with the parallel; the longest stretch kept word for word is %d words. %d possible minor agreements with %s against %s.",
            source_gospel, wd_mark$median_shared_content_words_pct, max(wording$run_length), nrow(agreements), other, source_gospel),
    sprintf("<b>Tendencies.</b> Per 1,000 words, \u201cimmediately\u201d: %s %s, %s %s; emotion words: %s %s, %s %s. A vivid word of the parallel is dropped in %d verses.",
            redactor, per_1000("immediacy")[[redactor]], source_gospel, per_1000("immediacy")[[source_gospel]],
            redactor, per_1000("emotion")[[redactor]], source_gospel, per_1000("emotion")[[source_gospel]], nrow(vivid_dropped)),
    sprintf("<b>Own material.</b> %s of paragraph-closing verses are %s's own, against %s of opening and %s of middle verses.",
            paste0(pos_tbl$pct_own[3], "%"), redactor, paste0(pos_tbl$pct_own[1], "%"), paste0(pos_tbl$pct_own[2], "%"))
  )

  sec <- function(title, lead, ...) c(sprintf("<h2>%s</h2><p class=\"lead\">%s</p>", title, lead), ...)

  comp_show <- composition; comp_show$tradition <- vapply(comp_show$tradition, key, character(1))
  sel_show <- selection[, c("source", "verses", "verses_used", "pct_verses_used", "pct_words_used", "redactor_verses_drawn")]
  names(sel_show) <- c("source", "verses in source", "verses with a parallel", "% of verses", "% of words",
                       sprintf("verses of %s drawn from it", redactor))

  loc_html <- unlist(lapply(src_names, function(s) {
    l <- location[[s]]
    c(sprintf("<h3>%s: which chapters %s drew on</h3>", esc(s), esc(redactor)),
      svg_bars(l$chapter, l$redactor_verses_drawn,
               sprintf("%s %d: %d verse(s) of %s drawn; %d of %d verses used", s, l$chapter,
                       l$redactor_verses_drawn, redactor, l$used_verses, l$source_verses),
               kc(s), sprintf("Chapter of %s (bars: verses of %s whose parallel starts there)", s, redactor)))
  }))

  omit_html <- unlist(lapply(src_names, function(s) {
    c(sprintf("<h3>%s: stretches with no parallel in %s</h3>", esc(s), esc(redactor)),
      "<p class=\"note\">Runs of two or more consecutive verses that no verse of the redactor was matched to, longest first. A verse inside a run can still have a loose parallel elsewhere: its best single-verse score is in redaction_source_verses.csv (taken_up_score).</p>",
      html_table(head(omitted_runs[[s]], top_n)),
      sprintf("<h3>%s: names absent from %s</h3>", esc(s), esc(redactor)),
      sprintf("<p class=\"wordlist\">%s</p>", if (nrow(omitted_names[[s]])) paste(sprintf("%s <span class=\"cnt\">%d</span>", esc(omitted_names[[s]]$name),
                                                         omitted_names[[s]]$mentions), collapse = " &middot; ") else "None."))
  }))

  vivid_html <- if (nrow(vivid_dropped) == 0) "<p class=\"note\">None found.</p>" else vapply(seq_len(nrow(vivid_dropped)), function(r) {
    h <- vivid_dropped[r, ]
    tok <- tokenise(h$source_text)
    sprintf("<div class=\"pair\"><div><span class=\"num\">%s</span> %s</div><div class=\"srcline\">%s <span class=\"num\">%s</span> %s</div></div>",
            red$ref[h$verse], esc(h$redactor), key(h$source), h$source_ref,
            paint(h$source_text, tok, ifelse(lower(tok$raw) %in% vivid_words, "hedge", NA)))
  }, character(1))

  word_class <- function(i) {
    t <- red_tok[[i]]
    hit <- matrix(FALSE, nrow(t), length(src_names))
    for (j in seq_along(src_names)) hit[, j] <- supported[i, j] & t$term %in% matches[[i]][[j]]$matched
    ifelse(is.na(t$term), NA, ifelse(rowSums(hit) > 1, "w wboth", ifelse(rowSums(hit) == 1,
           paste0("w wk", max.col(hit, ties.method = "first")), if (assessed[i]) "wown" else NA)))
  }
  synopsis_html <- unlist(lapply(split(seq_len(n_red), red$paragraph), function(idx) {
    rows <- vapply(idx, function(i) {
      cols <- vapply(seq_along(src_names), function(j) {
        s <- src_names[j]
        if (!supported[i, s]) return("<div class=\"par none\">no parallel</div>")
        m <- matches[[i]][[s]]
        txt <- passage_text(s, m$rows); tok <- tokenise(txt)
        sprintf("<div class=\"par\"><div class=\"meta\">%s %s &middot; coverage %.2f</div>%s</div>",
                esc(s), passage_ref(s, m$rows), m$score,
                paint(txt, tok, ifelse(tok$term %in% m$matched, paste0("w wk", j), NA)))
      }, character(1))
      notes <- c(if (nzchar(wording$agreement_with[i])) sprintf("possible minor agreement: \u201c%s\u201d", esc(wording$agreement_words[i])),
                 if (i %in% vivid_dropped$verse) "vivid word dropped",
                 if (wording$run_length[i] >= 8) sprintf("%d-word verbatim run", wording$run_length[i]))
      sprintf("<div class=\"syn\" id=\"v%s\"><div class=\"bio\"><div class=\"meta\"><span class=\"num\">%s</span> %s%s</div>%s</div>%s</div>",
              stri_replace_all_fixed(red$ref[i], ":", "_"), red$ref[i], key(tradition[i]),
              if (length(notes)) paste0(" <span class=\"flag\">", paste(notes, collapse = " &middot; "), "</span>") else "",
              paint(red$text[i], red_tok[[i]], word_class(i)), paste(cols, collapse = ""))
    }, character(1))
    head_ref <- ref_range(red$ref[range(idx)])
    c(sprintf("<h3 class=\"para\">%s %s</h3>", redactor, head_ref), rows)
  }))

  hue_vars <- function(h) paste(sprintf("--h%d:%s", seq_along(h), h), collapse = ";")

  html <- c(
    "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
    "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    sprintf("<title>Redaction criticism of %s</title><style>", redactor),
    sprintf(":root{color-scheme:light dark;--bg:#fcfcfb;--panel:#ffffff;--ink:#0b0b0b;--ink2:#52514e;--line:#e4e2dc;--own:#1baf7a;--na:#b9b7b0;--hedge:#b3261e;%s}", hue_vars(hues_light)),
    sprintf("@media (prefers-color-scheme:dark){:root{--bg:#1a1a19;--panel:#232321;--ink:#ffffff;--ink2:#c3c2b7;--line:#3a3936;--own:#199e70;--na:#6b6a65;--hedge:#ff8a80;%s}}", hue_vars(hues_dark)),
    "body{font:16px/1.6 Georgia,'Times New Roman',serif;color:var(--ink);background:var(--bg);margin:0}",
    "main{max-width:1180px;margin:0 auto;padding:32px 20px 80px}",
    "h1{font-size:28px;margin:0 0 4px}h2{font-size:21px;margin:48px 0 6px;border-bottom:1px solid var(--line);padding-bottom:6px}",
    "h3{font:600 15px/1.4 system-ui,sans-serif;margin:26px 0 8px}h3.para{color:var(--ink2);margin-top:30px}",
    ".muted,.note,.lead,.meta,figcaption{color:var(--ink2);font:13px/1.5 system-ui,sans-serif}.lead{font-size:14px;margin:0 0 16px;max-width:820px}.note{margin:6px 0 12px}",
    "ul.profile{padding-left:20px;margin:12px 0}ul.profile li{margin:0 0 8px}",
    ".scroll{overflow-x:auto}table{border-collapse:collapse;width:100%;font:14px/1.45 system-ui,sans-serif}",
    "td,th{padding:6px 10px;text-align:left;border-bottom:1px solid var(--line);vertical-align:top}th{color:var(--ink2);font-weight:600}td.n,th.n{text-align:right;font-variant-numeric:tabular-nums}",
    ".k1,.wk1{--c:var(--h1)}.k2,.wk2{--c:var(--h2)}.kown{--c:var(--own)}.kna{--c:var(--na)}",
    ".key{display:inline-flex;align-items:center;gap:6px;font:600 12px/1 system-ui,sans-serif;color:var(--ink);margin-right:12px;white-space:nowrap}",
    ".key i{width:11px;height:11px;border-radius:3px;background:var(--c);display:inline-block}.key.kboth i{background:linear-gradient(to bottom,var(--h1) 50%,var(--h2) 50%)}",
    ".chart{width:100%;height:auto;display:block;margin:8px 0}.chart rect,.chart path,.chart circle{fill:var(--c)}",
    ".chart .dot{stroke:var(--bg);stroke-width:1}.grid{stroke:var(--line);stroke-width:1}.ax{font:11px system-ui,sans-serif;fill:var(--ink2)}.val{font:600 11px system-ui,sans-serif;fill:var(--ink)}",
    ".charts{display:flex;flex-wrap:wrap;gap:20px}.charts figure{flex:1 1 380px;margin:0;min-width:0}",
    ".wordlist{font:14px/2 system-ui,sans-serif}.cnt{color:var(--ink2);font-size:12px}",
    ".pair{background:var(--panel);border:1px solid var(--line);border-radius:6px;padding:10px 14px;margin:0 0 10px;font-size:15px}",
    ".srcline{font-size:14px;color:var(--ink2);margin-top:6px}.hedge{color:var(--hedge);font-weight:bold;text-decoration:underline}",
    ".num{font:700 13px system-ui,sans-serif;color:var(--ink2);margin-right:4px}.flag{font:12px system-ui,sans-serif;color:var(--ink2);font-style:italic}",
    ".syn{display:grid;grid-template-columns:repeat(3,minmax(0,1fr));gap:16px;border-top:1px solid var(--line);padding:12px 0}",
    ".syn .bio{font-size:15.5px}.par{font-size:13.5px;line-height:1.5;color:var(--ink2)}.par.none{font:italic 13px system-ui,sans-serif;opacity:.7}.meta{margin-bottom:4px}",
    ".synhead{position:sticky;top:0;background:var(--bg);z-index:1;border-top:none;padding:8px 0;font:600 13px system-ui,sans-serif}",
    ".w{background:color-mix(in srgb,var(--c) 24%,transparent);color:var(--ink);border-radius:2px;padding:0 1px}",
    ".wboth{background:linear-gradient(to bottom,color-mix(in srgb,var(--h1) 30%,transparent) 50%,color-mix(in srgb,var(--h2) 30%,transparent) 50%)}",
    ".wown{border-bottom:2px dotted var(--own)}",
    "@media (max-width:760px){.syn{grid-template-columns:1fr;gap:8px}.synhead{display:none}}",
    "</style></head><body><main>",
    sprintf("<h1>Redaction criticism of %s</h1>", redactor),
    sprintf("<div class=\"muted\">Redactor: %s &middot; Source: %s &middot; Stand-in for Q: %s &middot; Text: World English Bible &middot; Generated %s</div>",
            redactor, source_gospel, other, format(Sys.time(), "%d %B %Y")),

    "<h2>Redactional profile</h2>",
    sprintf("<p class=\"lead\">The headline numbers, each unpacked in the sections below. Parallels are found by word overlap in one English translation, verse by verse, so heavily reworded material can be counted as %s's own, and a short verse can pick up a chance parallel.</p>", redactor),
    paste0("<ul class=\"profile\">", paste(sprintf("<li>%s</li>", profile), collapse = ""), "</ul>"),

    sec("1. Composition: triple, double and special material",
        sprintf("Which tradition each verse of %s belongs to, in %s's order. Verses with a parallel in %s are %s material (triple tradition if %s has them too); verses shared only with %s are the double tradition usually assigned to Q; verses with no parallel are %s's special material.",
                redactor, redactor, source_gospel, source_gospel, other, other, redactor),
        sprintf("<p>%s</p>", paste(vapply(trad_levels, key, character(1)), collapse = "")),
        svg_strip(),
        sprintf("<p class=\"note\">One row per chapter of %s, one cell per verse. Hover for the text.</p>", redactor),
        html_table(comp_show, raw_cols = "tradition"),
        sprintf("<h3>Is %s's own material concentrated at the ends of paragraphs?</h3>", redactor),
        html_table(setNames(pos_tbl, c("position in paragraph", "verses", "own material", "% own"))),
        "<p class=\"note\">Paragraphs of the WEB with three or more verses only.</p>",
        "<h3>Blocks: how long does the redactor stay with one tradition?</h3>",
        html_table(setNames(transform(block_tbl, tradition = vapply(tradition, key, character(1))),
                            c("tradition", "blocks", "longest run of consecutive verses")), raw_cols = "tradition"),
        "<h3>Seams: linking phrases that open a verse</h3>",
        html_table(setNames(seam_tbl, c("opening phrase", "times", "found in a source?", "verses")[seq_along(seam_tbl)]))),

    sec(sprintf("2. Selection: what %s used and left out", redactor),
        sprintf("How much of %s (and of %s) has a parallel in %s, which chapters were drawn on, and which stretches have none.", source_gospel, other, redactor),
        html_table(sel_show), loc_html, omit_html,
        sprintf("<h3>Source vocabulary %s never uses</h3>", redactor),
        sprintf("<p class=\"wordlist\">%s</p>", paste(sprintf("%s <span class=\"cnt\">%d</span>", esc(avoided_tbl$word), avoided_tbl$sources_count), collapse = " &middot; ")),
        "<p class=\"note\">Words common in the sources (count shown) that the redactor avoids entirely, names excluded.</p>"),

    sec("3. Order: does the redactor keep the source's sequence?",
        sprintf("The argument from order: an editor who follows a source tends to keep its sequence. Each dot is a verse of %s placed at the position of its parallel. A rising diagonal means the order is kept; dots off the diagonal are material moved.", redactor),
        paste0("<div class=\"charts\">", paste(vapply(src_names[src_names %in% order_pts$source], svg_order, character(1)), collapse = ""), "</div>"),
        html_table(setNames(order_tbl, c("source", "parallels", "order correlation (Spearman)", "backward jumps")))),

    sec("4. Rewording: how closely is the source wording kept?",
        "Verbatim agreement is the basic statistic of synoptic comparison. Here it shows how far the redactor rewrites, where wording survives intact, and where Matthew and Luke agree with each other against Mark.",
        html_table(setNames(wording_tbl, c("main source", "verses", "median % of content words shared with the parallel", "median longest verbatim run (words)",
                                           "longest run (words)", "verses with a run of 8+ words"))),
        "<h3>Longest stretches taken over word for word</h3>",
        html_table(setNames(top_runs, c("verse", "source", "words", "shared wording"))),
        sprintf("<h3>Possible minor agreements against %s</h3>", source_gospel),
        sprintf("<p class=\"note\">Verses of %s that follow %s but also share words with %s that %s's parallel lacks. These are the candidates for the &ldquo;minor agreements&rdquo; of Matthew and Luke against Mark (or for overlaps of Mark and Q). Many will be translation coincidences: check the Greek.</p>",
                redactor, source_gospel, other, source_gospel),
        html_table(agreements)),

    sec("5. Tendencies: systematic changes",
        "Changes made consistently reveal an editor's outlook: Matthew and Luke are usually said to cut Mark's &ldquo;immediately&rdquo; and his references to Jesus' emotions. These counts compare the three gospels on such features. (Mark's historic presents are not counted: the WEB translates them as past tenses.)",
        html_table(tendency_tbl),
        sprintf("<h3>Vivid words dropped (%d verses)</h3>", nrow(vivid_dropped)),
        sprintf("<p class=\"note\">The parallel has &ldquo;immediately&rdquo; or an emotion word (in red) next to the shared wording; the verse of %s has none. Check each: the word may belong to a different clause.</p>", redactor),
        vivid_html),

    sec(sprintf("6. %s's own style", redactor),
        "Words and phrases an editor favours, and the sources do not, mark the editor's hand.",
        "<h3>Characteristic vocabulary</h3>",
        html_table(setNames(vocab[seq_len(min(top_n + 10, nrow(vocab))), ],
                            c("word", paste("in", redactor), "in sources", sprintf("per 1,000 words (%s)", redactor), "per 1,000 words (sources)", "times more frequent"))),
        sprintf("<h3>Stock phrases (used 3+ times in %s, never in %s or %s)</h3>", redactor, source_gospel, other),
        html_table(phrases)),

    sec("7. Synopsis",
        sprintf("%s beside its parallels, verse by verse, in the WEB's paragraphs. Highlighted words are shared with a parallel (%s); words with a dotted underline are content words found in no parallel. A low-coverage parallel is often a chance overlap of common words.",
                redactor, paste(c(key(source_gospel), key(other), key(lab_tri)), collapse = "")),
        sprintf("<div class=\"syn synhead\"><div>%s</div><div>%s</div><div>%s</div></div>", redactor, source_gospel, other),
        synopsis_html),
    "</main></body></html>"
  )
  writeLines(enc2utf8(html), file.path(out_dir, "redaction_report.html"), useBytes = TRUE)

  cat(sprintf("\n================ %s: REDACTIONAL PROFILE ================\n", toupper(redactor)))
  cat(paste0(" - ", stri_replace_all_regex(profile, "<[^>]+>", ""), collapse = "\n"), "\n")
  cat("Report: ", file.path(out_dir, "redaction_report.html"), "\n", sep = "")
  invisible(synopsis)
}


# ---- 7. RUN -----------------------------------------------------------------

for (r in redactors) analyse(r)
cat("\nDone. Open the redaction_report.html files in ", output_dir, " in a browser.\n", sep = "")
