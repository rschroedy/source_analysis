# =============================================================================
# redaction_criticism.R
#
# Redaction criticism of an AI-written biography.
#
# In synoptic studies, redaction criticism asks what an evangelist DID with his
# sources: what he took over, left out, moved, reworded, combined and added,
# and what those choices reveal about his aims. This script asks the same of
# the biography, treating the AI as the redactor and the source documents
# (Jamestown, ICG) as its "Mark" and "Q":
#
#   1. Composition   which sentences rest on which source, and which are the
#                    redactor's own (the "Sondergut", special material)
#   2. Selection     how much of each source was used, from where, and what
#                    was left out
#   3. Order         whether the redactor keeps each source's sequence
#   4. Rewording     how closely the source wording is kept; which source's
#                    spelling of names is followed; where sources are combined
#   5. Tendencies    systematic changes: hedges dropped, tense, evaluative
#                    language, loss of specifics and citations
#   6. Own style     the redactor's characteristic vocabulary, stock phrases,
#                    linking formulae ("seams") and summary sentences
#   7. Synopsis      biography and sources in parallel columns, colour-coded
#
# The script gathers and counts the evidence. Deciding what a pattern MEANS
# (the redactor's "tendency") is still the analyst's job.
#
# It builds on source_attribution.R, which reads the documents and finds the
# parallels. Keep both scripts and the three documents in one folder, set the
# working directory to that folder, then:  source("redaction_criticism.R")
#
# Outputs (in `redaction_dir`):
#   redaction_report.html          the full report
#   redaction_synopsis.csv         one row per biography sentence
#   redaction_source_sentences.csv every source sentence: used or omitted
#   redaction_vocabulary.csv       the redactor's characteristic vocabulary
# =============================================================================


# ---- 1. SETTINGS (edit these) ----------------------------------------------

attribution_script <- "source_attribution.R"   # file names, thresholds and
                                               # spelling aliases are set there
redaction_dir <- "redaction_output"

# A sentence counts as a possible conflation when a second source adds at
# least this much coverage on top of the main source.
conflation_gain <- 0.20

top_n <- 20    # rows shown in the "top ..." tables

# Word lists for the tendency counts (lower case). Edit freely.
hedge_words <- c("reportedly", "apparently", "allegedly", "probably", "perhaps", "possibly",
                 "presumably", "seemingly", "believed", "thought", "may", "might", "seem", "seems",
                 "seemed", "appear", "appears", "appeared", "likely", "unclear", "suggest",
                 "suggests", "suggested", "suggesting", "according", "claimed", "claiming", "attributed")
evaluative_words <- c("most", "significant", "important", "major", "key", "prominent", "dangerous",
                      "notable", "crucial", "critical", "principal", "leading", "senior",
                      "extensive", "aggressive", "distinctive", "remarkable", "spectacular",
                      "particularly", "increasingly", "significantly", "highly", "especially",
                      "primarily", "closely", "deeply")
present_words <- c("is", "are", "has", "have", "remains", "remain", "continues", "continue")
past_words    <- c("was", "were", "had", "became", "did")
# First words of sentence-opening linking phrases ("At the same time, ...").
seam_openers  <- c("after", "at", "by", "during", "following", "for", "in", "later", "throughout",
                   "while", "unlike", "instead", "among", "since", "when", "before", "meanwhile",
                   "however", "moreover", "furthermore", "additionally", "ultimately", "despite",
                   "over", "subsequently", "eventually")


# ---- 2. FIND THE PARALLELS (runs the attribution script) -------------------

if (!file.exists(attribution_script)) {
  stop("Cannot find ", attribution_script, " in ", getwd(),
       ". Put it in the same folder as this script and the documents.")
}
message("Running ", attribution_script, " to read the documents and find parallels ...")
invisible(capture.output(source(attribution_script)))
library(stringi)

n_bio    <- nrow(bio)
own      <- "Redactor's own"
assessed <- results$label != "Not assessed"
n_words  <- function(x) sum(stri_count_regex(x, word_pattern))
pct      <- function(a, b) if (b == 0) 0 else 100 * a / b

# Coverage score of every biography sentence in every source.
score <- matrix(0, n_bio, length(src_names), dimnames = list(NULL, src_names))
for (s in src_names) score[, s] <- vapply(matches, function(m) m[[s]]$score, numeric(1))

# `supported`: the source has a parallel at all. `uses`: the sentence is
# attributed to that source (the label from the attribution script).
supported <- assessed & score >= min_support
label_set <- strsplit(results$label, " + ", fixed = TRUE)
uses <- supported
for (s in src_names) {
  uses[, s] <- supported[, s] & vapply(label_set, function(l) s %in% l || identical(l, "Both"), logical(1))
}
primary <- vapply(seq_len(n_bio), function(i) {
  if (!any(uses[i, ])) NA_character_ else src_names[which.max(ifelse(uses[i, ], score[i, ], -1))]
}, character(1))

tradition <- results$label
tradition[tradition == "Neither"] <- own
trad_levels <- unique(c(src_names, "Both", setdiff(tradition, c(src_names, "Both", own, "Not assessed")),
                        own, "Not assessed"))
trad_levels <- trad_levels[trad_levels %in% tradition]

# Tokens of every source sentence (needed for word-level comparisons).
message("Comparing wording ...")
src_tok <- lapply(src_sent, function(sn) lapply(sn$text, function(tx) {
  t <- tokenise(tx); t$term <- unify(t$term); t
}))
lower <- function(raw) stri_trans_tolower(stri_trans_general(raw, "Latin-ASCII"))


# ---- 3. COMPOSITION: WHERE THE REDACTOR'S OWN MATERIAL SITS ----------------

para_n <- ave(bio$id, bio$paragraph, FUN = length)
para_i <- ave(bio$id, bio$paragraph, FUN = seq_along)
position <- ifelse(para_n < 3, "short paragraph",
                   ifelse(para_i == 1, "opening", ifelse(para_i == para_n, "closing", "middle")))

composition <- data.frame(
  tradition = trad_levels,
  sentences = as.vector(table(factor(tradition, trad_levels))),
  words     = as.vector(tapply(results$words, factor(tradition, trad_levels), sum)),
  stringsAsFactors = FALSE
)
composition$pct_of_words <- round(100 * composition$words / sum(composition$words), 1)

pos_levels <- c("opening", "middle", "closing")
pos_tbl <- do.call(rbind, lapply(pos_levels, function(p) {
  k <- assessed & position == p
  data.frame(position = p, sentences = sum(k), redactors_own = sum(tradition[k] == own),
             pct_own = round(pct(sum(tradition[k] == own), sum(k))), stringsAsFactors = FALSE)
}))

# Blocks: runs of consecutive sentences from the same tradition.
runs <- rle(tradition[assessed])
block_tbl <- data.frame(
  tradition    = trad_levels[trad_levels != "Not assessed"],
  blocks       = vapply(trad_levels[trad_levels != "Not assessed"], function(t) sum(runs$values == t), numeric(1)),
  longest_run  = vapply(trad_levels[trad_levels != "Not assessed"], function(t) max(c(0, runs$lengths[runs$values == t])), numeric(1)),
  stringsAsFactors = FALSE, row.names = NULL
)

# Seams: short linking phrases at the start of a sentence.
all_source_text <- lower(paste(unlist(lapply(src_sent, function(sn) sn$text)), collapse = " "))
opening <- stri_match_first_regex(bio$text, "^([^,]{2,60}),")[, 2]
is_seam <- !is.na(opening) & stri_count_regex(opening, word_pattern) <= 6 &
  lower(stri_extract_first_regex(bio$text, word_pattern)) %in% seam_openers
seams <- data.frame(sentence = bio$id[is_seam], phrase = opening[is_seam], tradition = tradition[is_seam],
                    in_a_source = ifelse(stri_detect_fixed(all_source_text, lower(opening[is_seam])), "yes", "no"),
                    stringsAsFactors = FALSE)


# ---- 4. SELECTION: WHAT WAS USED AND WHAT WAS LEFT OUT ---------------------

# A source sentence is "used" if it lies in the parallel passage of a
# biography sentence attributed to that source.
used <- lapply(src_names, function(s) {
  u <- rep(FALSE, nrow(src_sent[[s]]))
  for (i in which(uses[, s])) u[matches[[i]][[s]]$rows] <- TRUE
  u
})
names(used) <- src_names

# Second check, from the source's side: how much of each source sentence's
# content turns up in its best-matching biography sentence (0 to 1).
taken_up <- lapply(src_names, function(s) vapply(src_terms[[s]], function(t) {
  if (length(t) == 0) return(0)
  w <- weight_of(t)
  max(vapply(bio_terms, function(b) sum(w[t %in% b]), numeric(1))) / sum(w)
}, numeric(1)))
names(taken_up) <- src_names

selection <- do.call(rbind, lapply(src_names, function(s) {
  wd <- stri_count_regex(src_sent[[s]]$text, word_pattern)
  data.frame(source = s, sentences = length(wd), sentences_used = sum(used[[s]]),
             pct_sentences_used = round(pct(sum(used[[s]]), length(wd)), 1),
             words = sum(wd), words_used = sum(wd[used[[s]]]),
             pct_words_used = round(pct(sum(wd[used[[s]]]), sum(wd)), 1),
             biography_sentences_drawn = sum(uses[, s]), stringsAsFactors = FALSE)
}))

# Where in each source the redactor drew from: by PDF page when there are
# pages, otherwise by tenths of the document.
first_row <- function(i, s) min(matches[[i]][[s]]$rows)
location <- lapply(src_names, function(s) {
  sn <- src_sent[[s]]
  paged <- !all(is.na(sn$page))
  loc   <- if (paged) sn$page else pmin(10, ceiling(10 * seq_len(nrow(sn)) / nrow(sn)))
  lv    <- sort(unique(loc))
  drawn <- if (any(uses[, s])) loc[vapply(which(uses[, s]), first_row, numeric(1), s = s)] else integer(0)
  data.frame(source = s, unit = if (paged) "PDF page" else "tenth", location = lv,
             source_sentences = as.vector(table(factor(loc, lv))),
             used_sentences = as.vector(tapply(used[[s]], factor(loc, lv), sum)),
             biography_sentences_drawn = as.vector(table(factor(drawn, lv))), stringsAsFactors = FALSE)
})
names(location) <- src_names

# Names that are prominent in a source but never appear in the biography.
is_cap <- function(t) stri_detect_regex(t$raw, "^\\p{Lu}") & !is.na(t$term) & seq_len(nrow(t)) > 1
bio_term_all <- unlist(lapply(bio_tok, function(t) t$term[!is.na(t$term)]))
omitted_names <- lapply(src_names, function(s) {
  tk  <- do.call(rbind, lapply(src_tok[[s]], function(t) t[is_cap(t), c("raw", "term")]))
  tk  <- tk[!tk$term %in% bio_term_all & stri_length(tk$raw) > 2 & !tk$raw %in% c(month.name, "See"), ]
  if (is.null(tk) || nrow(tk) == 0) return(data.frame(name = character(0), mentions = integer(0)))
  cnt <- sort(table(tk$term), decreasing = TRUE)
  cnt <- cnt[seq_len(min(top_n, length(cnt)))]
  data.frame(name = vapply(names(cnt), function(k) names(which.max(table(tk$raw[tk$term == k]))), character(1)),
             mentions = as.vector(cnt), stringsAsFactors = FALSE, row.names = NULL)
})
names(omitted_names) <- src_names


# ---- 5. ORDER: DOES THE REDACTOR KEEP THE SOURCE'S SEQUENCE? ---------------

order_pts <- do.call(rbind, lapply(src_names, function(s) {
  idx <- which(uses[, s])
  if (length(idx) == 0) return(NULL)
  row  <- vapply(idx, first_row, numeric(1), s = s)
  body <- which(src_sent[[s]]$stream == "body")
  keep <- row %in% body                                  # ignore footnote parallels
  data.frame(source = s, sentence = idx[keep], source_row = row[keep],
             source_pct = 100 * match(row[keep], body) / length(body),
             page = src_sent[[s]]$page[row[keep]], stringsAsFactors = FALSE)
}))
order_tbl <- do.call(rbind, lapply(src_names, function(s) {
  p <- order_pts[order_pts$source == s, ]
  rho <- if (nrow(p) >= 4) suppressWarnings(cor(p$sentence, p$source_row, method = "spearman")) else NA
  back <- if (nrow(p) >= 2) sum(diff(p$source_row[order(p$sentence)]) < 0) else 0
  data.frame(source = s, parallels = nrow(p), order_correlation = round(rho, 2),
             backward_jumps = back, stringsAsFactors = FALSE)
}))


# ---- 6. REWORDING: HOW CLOSELY IS THE SOURCE WORDING KEPT? -----------------

# Longest run of consecutive identical words shared by two word vectors.
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

wording <- do.call(rbind, lapply(seq_len(n_bio), function(i) {
  s <- primary[i]
  out <- data.frame(shared_content_words_pct = NA_real_, longest_verbatim_run = "", run_length = 0L,
                    conflation_from = "", conflation_words = "", stringsAsFactors = FALSE)
  if (is.na(s)) return(out)
  terms <- bio_terms[[i]]
  m     <- matches[[i]][[s]]
  out$shared_content_words_pct <- round(pct(sum(terms %in% m$matched), length(terms)))
  src_words <- lower(unlist(lapply(src_tok[[s]][m$rows], function(t) t$raw)))
  run <- longest_run(lower(bio_tok[[i]]$raw), src_words)
  out$longest_verbatim_run <- paste(run, collapse = " ")
  out$run_length <- length(run)
  # Conflation: another supported source supplies words the main one lacks.
  w <- weight_of(terms)
  for (q in setdiff(src_names[supported[i, ]], s)) {
    extra <- setdiff(matches[[i]][[q]]$matched, m$matched)
    if (length(extra) >= 2 && sum(w[terms %in% extra]) / sum(w) >= conflation_gain) {
      t <- bio_tok[[i]]
      out$conflation_from  <- q
      out$conflation_words <- paste(unique(t$raw[t$term %in% extra]), collapse = " ")
    }
  }
  out
}))

wording_tbl <- do.call(rbind, lapply(src_names, function(s) {
  k <- which(primary == s)
  if (length(k) == 0) return(NULL)
  data.frame(main_source = s, sentences = length(k),
             median_shared_content_words_pct = median(wording$shared_content_words_pct[k]),
             median_longest_run = median(wording$run_length[k]),
             longest_run = max(wording$run_length[k]),
             sentences_with_run_of_5_plus = sum(wording$run_length[k] >= 5), stringsAsFactors = FALSE)
}))
top_runs <- data.frame(sentence = bio$id, source = primary, words = wording$run_length,
                       run = wording$longest_verbatim_run, stringsAsFactors = FALSE)
top_runs <- top_runs[order(-top_runs$words), ]
top_runs <- top_runs[top_runs$words >= 4, ][seq_len(min(top_n, sum(top_runs$words >= 4))), ]
conflations <- data.frame(sentence = bio$id, main_source = primary, also_from = wording$conflation_from,
                          words_from_second_source = wording$conflation_words, text = bio$text,
                          stringsAsFactors = FALSE)[nzchar(wording$conflation_from), ]

# Which source's spelling of a name does the redactor follow?
# For each name in the biography, collect how each source spells it: the same
# spelling, a spelling listed in `aliases`, or one with a letter added/dropped.
strip_s  <- function(raw) stri_replace_all_regex(raw, "['\u2019]s$", "")
bio_names <- unique(strip_s(proper))
bio_names <- bio_names[stri_length(bio_names) >= 4]
bio_keys  <- lower(bio_names)
src_forms <- lapply(src_tok, function(tl) {
  raw <- strip_s(unlist(lapply(tl, function(t) t$raw[stri_detect_regex(t$raw, "^\\p{Lu}") & !is.na(t$term)])))
  cnt <- table(raw)
  data.frame(raw = names(cnt), n = as.vector(cnt), key = lower(names(cnt)),
             term = normalise_words(names(cnt)), stringsAsFactors = FALSE)
})
same_but_suffix <- function(a, b) a == paste0(b, "n") | a == paste0(b, "s") | b == paste0(a, "n") | b == paste0(a, "s")
spelling <- do.call(rbind, lapply(seq_along(bio_names), function(j) {
  k <- bio_keys[j]; tm <- normalise_words(bio_names[j])
  per_src <- lapply(src_names, function(s) {
    f <- src_forms[[s]]
    one_letter <- utils::adist(k, f$key)[1, ] == 1 & abs(stri_length(f$key) - stri_length(k)) == 1 &
      stri_length(k) >= 5 & substr(f$key, 1, 1) == substr(k, 1, 1)
    variant_of <- (f$term == tm | one_letter) & f$key != k & !f$key %in% bio_keys & !same_but_suffix(f$key, k)
    f <- f[f$key == k | variant_of, ]
    f <- f[order(-f$n), ]
    list(usual = if (nrow(f)) f$key[1] else NA, differs = nrow(f) > 0 && f$key[1] != k,
         forms = if (nrow(f)) paste(sprintf("%s (%d)", f$raw, f$n), collapse = ", ") else "(absent)")
  })
  names(per_src) <- src_names
  if (!any(vapply(per_src, function(p) p$differs, logical(1)))) return(NULL)
  agrees <- vapply(per_src, function(p) identical(p$usual, k), logical(1))
  row <- data.frame(biography = bio_names[j], stringsAsFactors = FALSE)
  for (s in src_names) row[[s]] <- per_src[[s]]$forms
  row$follows <- if (!any(agrees)) "neither (redactor's own spelling)" else paste(src_names[agrees], collapse = " + ")
  row
}))
if (is.null(spelling)) spelling <- data.frame(biography = character(0), follows = character(0))


# ---- 7. TENDENCIES: SYSTEMATIC CHANGES --------------------------------------

doc_stats <- function(tok_list, texts, footnotes = 0) {
  raw <- unlist(lapply(tok_list, function(t) t$raw))
  lw  <- lower(raw)
  first <- unlist(lapply(tok_list, function(t) seq_len(nrow(t)) == 1))
  named <- unlist(lapply(tok_list, function(t) stri_detect_regex(t$raw, "^\\p{Lu}") & !is.na(t$term)))
  pres <- sum(lw %in% present_words); past <- sum(lw %in% past_words)
  c(words = length(raw),
    hedges = sum(lw %in% hedge_words & raw != "May"),
    evaluative = sum(lw %in% evaluative_words),
    numbers = sum(stri_detect_regex(raw, "^\\d")),
    names = sum(named & !first),
    quotations = stri_count_regex(paste(texts, collapse = " "), "[\u201c\"][^\u201c\u201d\"]{25,900}[\u201d\"]"),
    citations = sum(stri_count_regex(texts, paste0("\\([^()]*\\b((19|20)\\d{2}|", paste(month.name, collapse = "|"),
                                                   ")\\b[^()]*\\)"))) + footnotes,
    present = pres, past = past)
}
stats <- cbind(Biography = doc_stats(bio_tok, bio$text),
               sapply(src_names, function(s) doc_stats(src_tok[[s]], src_sent[[s]]$text,
                                                       footnotes = sum(src_sent[[s]]$stream == "notes"))))
per_1000 <- function(row) round(1000 * stats[row, ] / stats["words", ], 1)
tendency_tbl <- data.frame(
  measure = c("Hedging and attribution words (reportedly, believed, may ...) per 1,000 words",
              "Evaluative and intensifying words (most, significant, increasingly ...) per 1,000 words",
              "Numbers and dates per 1,000 words",
              "Names (capitalised words inside sentences) per 1,000 words",
              "Direct quotations (count)",
              "Citations: dated references in brackets, plus footnote sentences (count)",
              "Present tense share of is/has/remains vs was/had/became (%)"),
  rbind(per_1000("hedges"), per_1000("evaluative"), per_1000("numbers"), per_1000("names"),
        stats["quotations", ], stats["citations", ],
        round(100 * stats["present", ] / pmax(1, stats["present", ] + stats["past", ]))),
  check.names = FALSE, stringsAsFactors = FALSE, row.names = NULL
)

# Hedges dropped: the source sentence behind a biography sentence is hedged,
# the biography sentence is not.
is_hedge <- function(t) lower(t$raw) %in% hedge_words & t$raw != "May"
hedges_dropped <- do.call(rbind, lapply(which(!is.na(primary)), function(i) {
  if (any(is_hedge(bio_tok[[i]]))) return(NULL)
  s <- primary[i]
  hit <- do.call(rbind, lapply(matches[[i]][[s]]$rows, function(r) {
    t <- src_tok[[s]][[r]]
    w <- weight_of(bio_terms[[i]])
    if (!any(is_hedge(t)) || sum(w[bio_terms[[i]] %in% t$term]) / sum(w) < min_support) return(NULL)
    # ... and the hedge must sit next to the shared wording (2+ shared words within 8).
    near <- vapply(which(is_hedge(t)), function(h) {
      sum(t$term[max(1, h - 8):min(nrow(t), h + 8)] %in% bio_terms[[i]]) >= 2
    }, logical(1))
    if (!any(near)) return(NULL)
    data.frame(hedge = paste(unique(lower(t$raw[is_hedge(t)])), collapse = ", "), row = r, stringsAsFactors = FALSE)
  }))
  if (is.null(hit)) return(NULL)
  data.frame(sentence = i, source = s, hedge_in_source = paste(unique(hit$hedge), collapse = ", "),
             biography = bio$text[i], source_text = paste(src_sent[[s]]$text[hit$row], collapse = " "),
             source_row = hit$row[1], stringsAsFactors = FALSE)
}))
if (is.null(hedges_dropped)) hedges_dropped <- data.frame(sentence = integer(0))


# ---- 8. THE REDACTOR'S OWN STYLE --------------------------------------------

# Characteristic vocabulary: words the biography uses far more often than the
# sources do (after stemming), names excluded.
src_term_all <- unlist(lapply(src_tok, function(tl) unlist(lapply(tl, function(t) t$term[!is.na(t$term)]))))
bio_raw_all  <- unlist(lapply(bio_tok, function(t) lower(t$raw[!is.na(t$term)])))
tok_all <- rbind(do.call(rbind, lapply(bio_tok, function(t) t[seq_len(nrow(t)) > 1 & !is.na(t$term), c("raw", "term")])),
                 do.call(rbind, lapply(src_tok, function(tl) do.call(rbind, lapply(tl, function(t) t[seq_len(nrow(t)) > 1 & !is.na(t$term), c("raw", "term")])))))
cap_share  <- tapply(stri_detect_regex(tok_all$raw, "^[\\p{Lu}\\d]"), tok_all$term, mean)
name_terms2 <- names(cap_share)[cap_share > 0.5]

n_b <- stats["words", "Biography"]; n_s <- sum(stats["words", src_names])
b_cnt <- table(bio_term_all); s_cnt <- table(src_term_all)
vocab <- data.frame(term = names(b_cnt), biography_count = as.vector(b_cnt),
                    sources_count = as.vector(s_cnt[names(b_cnt)]), stringsAsFactors = FALSE)
vocab$sources_count[is.na(vocab$sources_count)] <- 0
vocab$word <- vapply(vocab$term, function(k) names(which.max(table(bio_raw_all[bio_term_all == k]))), character(1))
vocab$biography_per_1000 <- round(1000 * vocab$biography_count / n_b, 1)
vocab$sources_per_1000   <- round(1000 * vocab$sources_count / n_s, 1)
vocab$times_more_frequent <- round(((vocab$biography_count + 0.5) / (n_b + 1)) / ((vocab$sources_count + 0.5) / (n_s + 1)), 1)
vocab <- vocab[!vocab$term %in% name_terms2 & vocab$biography_count >= 3 & vocab$times_more_frequent >= 3, ]
vocab <- vocab[order(-vocab$biography_count, -vocab$times_more_frequent),
               c("word", "biography_count", "sources_count", "biography_per_1000", "sources_per_1000", "times_more_frequent")]

# Source vocabulary the redactor avoids: common in the sources, never used.
avoided <- s_cnt[!names(s_cnt) %in% names(b_cnt) & !names(s_cnt) %in% name_terms2]
avoided <- sort(avoided, decreasing = TRUE)[seq_len(min(top_n, length(avoided)))]
src_raw_all <- unlist(lapply(src_tok, function(tl) unlist(lapply(tl, function(t) lower(t$raw[!is.na(t$term)])))))
avoided_tbl <- data.frame(word = vapply(names(avoided), function(k) names(which.max(table(src_raw_all[src_term_all == k]))), character(1)),
                          sources_count = as.vector(avoided), stringsAsFactors = FALSE, row.names = NULL)

# Stock phrases: pairs of neighbouring content words (small words skipped,
# endings ignored) used 2+ times in the biography and never in any source.
pair_keys <- function(t) {
  t <- t[!is.na(t$term), ]
  if (nrow(t) < 2) return(data.frame(key = character(0), start = integer(0), end = integer(0)))
  data.frame(key = paste(t$term[-nrow(t)], t$term[-1]), start = t$start[-nrow(t)], end = t$end[-1], stringsAsFactors = FALSE)
}
bio_pairs <- do.call(rbind, lapply(seq_len(n_bio), function(i) {
  p <- pair_keys(bio_tok[[i]])
  if (nrow(p) == 0) return(NULL)
  data.frame(key = p$key, phrase = lower(stri_sub(bio$text[i], p$start, p$end)), sentence = i, stringsAsFactors = FALSE)
}))
src_pairs <- unique(unlist(lapply(src_tok, function(tl) unlist(lapply(tl, function(t) pair_keys(t)$key)))))
pair_n  <- table(bio_pairs$key[!bio_pairs$key %in% src_pairs])
pair_n  <- sort(pair_n[pair_n >= 2], decreasing = TRUE)
phrases <- data.frame(
  phrase = vapply(names(pair_n), function(k) bio_pairs$phrase[bio_pairs$key == k][1], character(1)),
  times  = as.vector(pair_n),
  sentences = vapply(names(pair_n), function(k) paste(bio_pairs$sentence[bio_pairs$key == k], collapse = ", "), character(1)),
  stringsAsFactors = FALSE, row.names = NULL)
phrases <- phrases[seq_len(min(top_n, nrow(phrases))), ]


# ---- 9. CSV OUTPUT -----------------------------------------------------------

dir.create(redaction_dir, showWarnings = FALSE)
synopsis <- data.frame(id = bio$id, paragraph = bio$paragraph, position_in_paragraph = position,
                       tradition = tradition, confidence = results$confidence, main_source = primary,
                       stringsAsFactors = FALSE)
for (s in src_names) synopsis[[paste0("score_", s)]] <- round(score[, s], 2)
synopsis <- cbind(synopsis, wording,
                  hedge_dropped = ifelse(bio$id %in% hedges_dropped$sentence, "yes", ""),
                  biography = bio$text, stringsAsFactors = FALSE)
for (s in src_names) {
  synopsis[[paste0("parallel_", s)]] <- vapply(seq_len(n_bio), function(i) {
    if (supported[i, s]) passage_text(s, matches[[i]][[s]]$rows) else ""
  }, character(1))
}
write.csv(synopsis, file.path(redaction_dir, "redaction_synopsis.csv"), row.names = FALSE, fileEncoding = "UTF-8")
source_out <- do.call(rbind, lapply(src_names, function(s) {
  data.frame(source = s, sentence_no = seq_len(nrow(src_sent[[s]])), page = src_sent[[s]]$page,
             stream = src_sent[[s]]$stream, used = used[[s]], taken_up_score = round(taken_up[[s]], 2),
             text = src_sent[[s]]$text, stringsAsFactors = FALSE)
}))
write.csv(source_out, file.path(redaction_dir, "redaction_source_sentences.csv"), row.names = FALSE, fileEncoding = "UTF-8")
write.csv(vocab, file.path(redaction_dir, "redaction_vocabulary.csv"), row.names = FALSE, fileEncoding = "UTF-8")


# ---- 10. HTML REPORT ---------------------------------------------------------

# Colours: one hue per source, a third for the redactor's own material;
# "Both" is drawn as a split of the two source colours.
hues_light <- c("#2a78d6", "#eb6834", "#eda100", "#e87ba4", "#4a3aa7")
hues_dark  <- c("#3987e5", "#d95926", "#c98500", "#d55181", "#9085e9")
kc <- function(label) {       # CSS class for a tradition or source label
  if (label %in% src_names) return(paste0("k", match(label, src_names)))
  if (label == own) return("kown")
  if (label == "Not assessed") return("kna")
  "kboth"
}
kcs <- function(labels) vapply(labels, kc, character(1))
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

# Wrap every token of `text` that has a class in a <span>.
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

# --- charts (inline SVG, no extra packages) ---
svg_strip <- function() {
  W <- 880; gap_para <- 8; cw_gap <- 2; h <- 30
  n_par <- length(unique(bio$paragraph))
  cw <- (W - gap_para * (n_par - 1)) / n_bio
  x <- (seq_len(n_bio) - 1) * cw + (match(bio$paragraph, unique(bio$paragraph)) - 1) * gap_para
  cells <- vapply(seq_len(n_bio), function(i) {
    tip <- esc(sprintf("%d. %s: %s", i, tradition[i], bio$text[i]))
    if (kc(tradition[i]) == "kboth") {
      sprintf("<g><title>%s</title><rect class=\"k1\" x=\"%.1f\" y=\"0\" width=\"%.1f\" height=\"%.1f\"/><rect class=\"k2\" x=\"%.1f\" y=\"%.1f\" width=\"%.1f\" height=\"%.1f\"/></g>",
              tip, x[i], cw - cw_gap, h / 2, x[i], h / 2, cw - cw_gap, h / 2)
    } else {
      sprintf("<rect class=\"%s\" x=\"%.1f\" y=\"0\" width=\"%.1f\" height=\"%d\" rx=\"1.5\"><title>%s</title></rect>",
              kc(tradition[i]), x[i], cw - cw_gap, h, tip)
    }
  }, character(1))
  px <- tapply(x, bio$paragraph, min)
  labs <- sprintf("<text x=\"%.1f\" y=\"%d\" class=\"ax\">\u00b6%s</text>", px, h + 14, names(px))
  sprintf("<svg viewBox=\"0 0 %d %d\" class=\"chart\" role=\"img\" aria-label=\"Each biography sentence coloured by its source\">%s%s</svg>",
          W, h + 20, paste(cells, collapse = ""), paste(labs, collapse = ""))
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
  show <- if (n > 40) seq_len(n) %% 2 == 1 else rep(TRUE, n)
  xl <- sprintf("<text class=\"ax\" text-anchor=\"middle\" x=\"%.1f\" y=\"%.1f\">%s</text>", x + bw / 2, y0 + 15, esc(labels))[show]
  top <- order(-values)[seq_len(min(3, sum(values > 0)))]
  vl <- sprintf("<text class=\"val\" text-anchor=\"middle\" x=\"%.1f\" y=\"%.1f\">%d</text>", x[top] + bw / 2, y[top] - 5, values[top])
  sprintf("<svg viewBox=\"0 0 %d %d\" class=\"chart\" role=\"img\" aria-label=\"%s\">%s%s%s%s<text class=\"ax\" text-anchor=\"middle\" x=\"%.1f\" y=\"%d\">%s</text></svg>",
          W, H, esc(x_title), paste(grid, collapse = ""), paste(bars, collapse = ""), paste(xl, collapse = ""),
          paste(vl, collapse = ""), ml + pw / 2, H - 4, esc(x_title))
}

svg_order <- function(s) {
  p <- order_pts[order_pts$source == s, ]
  W <- 430; H <- 300; ml <- 44; mr <- 12; mt <- 12; mb <- 40
  pw <- W - ml - mr; ph <- H - mt - mb
  X <- function(v) ml + pw * (v - 1) / max(1, n_bio - 1)
  Y <- function(v) mt + ph * (1 - v / 100)
  yt <- c(0, 25, 50, 75, 100); xt <- pretty(c(1, n_bio), n = 5); xt <- xt[xt >= 1 & xt <= n_bio]
  grid <- c(sprintf("<line class=\"grid\" x1=\"%d\" x2=\"%d\" y1=\"%.1f\" y2=\"%.1f\"/><text class=\"ax\" text-anchor=\"end\" x=\"%d\" y=\"%.1f\">%d%%</text>",
                    ml, W - mr, Y(yt), Y(yt), ml - 6, Y(yt) + 4, yt),
            sprintf("<text class=\"ax\" text-anchor=\"middle\" x=\"%.1f\" y=\"%d\">%d</text>", X(xt), H - mb + 16, xt))
  dots <- sprintf("<circle class=\"dot %s\" cx=\"%.1f\" cy=\"%.1f\" r=\"4.5\"><title>%s</title></circle>", kc(s), X(p$sentence), Y(p$source_pct),
                  esc(sprintf("Biography sentence %d \u2192 %s, %s%.0f%% of the way through", p$sentence, s,
                              ifelse(is.na(p$page), "", paste0("PDF page ", p$page, ", ")), p$source_pct)))
  sprintf("<figure><figcaption>%s</figcaption><svg viewBox=\"0 0 %d %d\" class=\"chart\" role=\"img\" aria-label=\"Order of %s material\">%s%s<text class=\"ax\" text-anchor=\"middle\" x=\"%.1f\" y=\"%d\">Biography sentence number</text></svg></figure>",
          esc(sprintf("%s: position in the source (0%% = start, 100%% = end)", s)), W, H, esc(s),
          paste(grid, collapse = ""), paste(dots, collapse = ""), ml + pw / 2, H - 6)
}

# --- headline findings, written from the numbers ---
comp <- setNames(composition$pct_of_words, composition$tradition)
get  <- function(v, k) if (k %in% names(v)) v[[k]] else 0
profile <- c(
  sprintf("<b>Composition.</b> %s of the biography's words rest on a single source (%s); %.0f%% are attested in both; %.0f%% are the redactor's own material with no clear parallel.",
          paste0(round(sum(comp[names(comp) %in% src_names])), "%"),
          paste(sprintf("%s %.0f%%", src_names, vapply(src_names, function(s) get(comp, s), numeric(1))), collapse = ", "),
          get(comp, "Both"), get(comp, own)),
  sprintf("<b>Selection.</b> The redactor drew on %s.", paste(sprintf("%d of %s's %d sentences (%.0f%%)", selection$sentences_used,
                                             selection$source, selection$sentences, selection$pct_sentences_used), collapse = " and ")),
  unlist(lapply(src_names, function(s) {
    l <- location[[s]]
    if (sum(l$biography_sentences_drawn) < 5 || nrow(l) < 4) return(NULL)
    top <- l[order(-l$biography_sentences_drawn), ][1:3, ]
    sprintf("<b>Where in %s.</b> %.0f%% of the sentences drawn from %s have their parallel in just three of its %d %ss (%s).",
            esc(s), pct(sum(top$biography_sentences_drawn), sum(l$biography_sentences_drawn)), esc(s), nrow(l),
            l$unit[1], paste(sort(top$location), collapse = ", "))
  })),
  sprintf("<b>Order.</b> %s (1 = the source's order is kept exactly, 0 = no relation).",
          paste(sprintf("%s order correlation %s", order_tbl$source, ifelse(is.na(order_tbl$order_correlation), "n/a",
                                                                            format(order_tbl$order_correlation, nsmall = 2))), collapse = "; ")),
  sprintf("<b>Rewording.</b> %s. The longest stretch copied word for word is %d words.",
          paste(sprintf("%s %s share a median %.0f%% of their content words with the parallel",
                        c("Sentences based on", rep("those based on", nrow(wording_tbl) - 1)), wording_tbl$main_source,
                        wording_tbl$median_shared_content_words_pct), collapse = "; "),
          max(wording$run_length)),
  sprintf("<b>Tendencies.</b> Hedging words per 1,000: biography %s, %s. Direct quotations: biography %d, sources %d. Citations: biography %d, sources %d. Hedge present in the source but gone from the biography in %d sentences.",
          per_1000("hedges")[["Biography"]], paste(sprintf("%s %s", src_names, per_1000("hedges")[src_names]), collapse = ", "),
          stats["quotations", "Biography"], sum(stats["quotations", src_names]), stats["citations", "Biography"],
          sum(stats["citations", src_names]), nrow(hedges_dropped)),
  sprintf("<b>Own material.</b> %s of paragraph-closing sentences are the redactor's own, against %s of opening and %s of middle sentences.",
          paste0(pos_tbl$pct_own[3], "%"), paste0(pos_tbl$pct_own[1], "%"), paste0(pos_tbl$pct_own[2], "%"))
)

# --- section pieces ---
sec <- function(title, lead, ...) c(sprintf("<h2>%s</h2><p class=\"lead\">%s</p>", title, lead), ...)

comp_show <- composition; comp_show$tradition <- vapply(comp_show$tradition, key, character(1))
sel_show <- selection[, c("source", "sentences", "sentences_used", "pct_sentences_used", "pct_words_used", "biography_sentences_drawn")]
names(sel_show) <- c("source", "sentences_in_source", "used", "%_of_sentences_used", "%_of_words_used", "biography_sentences_drawn")

loc_html <- unlist(lapply(src_names, function(s) {
  l <- location[[s]]
  c(sprintf("<h3>%s: where the redactor drew from</h3>", esc(s)),
    svg_bars(l$location, l$biography_sentences_drawn,
             sprintf("%s %s: %d biography sentence(s) drawn; %d of %d source sentences used", l$unit, l$location,
                     l$biography_sentences_drawn, l$used_sentences, l$source_sentences),
             kc(s), sprintf("%s of %s (bars: number of biography sentences whose parallel starts there)",
                            if (l$unit[1] == "tenth") "Tenth of the document" else l$unit[1], s)))
}))

omit_html <- unlist(lapply(src_names, function(s) {
  sn <- src_sent[[s]]
  if (nrow(sn) <= 80) {       # short source: list every sentence left out
    om <- which(!used[[s]] & taken_up[[s]] < min_support & stri_count_regex(sn$text, word_pattern) >= 6)
    c(sprintf("<h3>%s: sentences left out (%d of %d)</h3>", esc(s), length(om), nrow(sn)),
      "<p class=\"note\">Source sentences that no biography sentence was matched to and whose content does not resurface elsewhere.</p>",
      if (length(om)) paste0("<ol class=\"omit\">", paste(sprintf("<li value=\"%d\">%s</li>", om, esc(sn$text[om])), collapse = ""), "</ol>")
      else "<p class=\"note\">None.</p>")
  } else {                    # long source: show the names that vanish
    c(sprintf("<h3>%s: names prominent in the source, absent from the biography</h3>", esc(s)),
      sprintf("<p class=\"wordlist\">%s</p>", paste(sprintf("%s <span class=\"cnt\">%d</span>", esc(omitted_names[[s]]$name),
                                                           omitted_names[[s]]$mentions), collapse = " &middot; ")))
  }
}))

hedge_html <- if (nrow(hedges_dropped) == 0) "<p class=\"note\">None found.</p>" else vapply(seq_len(nrow(hedges_dropped)), function(r) {
  h <- hedges_dropped[r, ]
  tok <- tokenise(h$source_text)
  sprintf("<div class=\"pair\"><div><span class=\"num\">%d</span> %s</div><div class=\"srcline\">%s %s</div></div>",
          h$sentence, esc(h$biography), key(h$source),
          paint(h$source_text, tok, ifelse(lower(tok$raw) %in% hedge_words & tok$raw != "May", "hedge", NA)))
}, character(1))

word_class <- function(i) {
  t <- bio_tok[[i]]
  hit <- matrix(FALSE, nrow(t), length(src_names))
  for (j in seq_along(src_names)) hit[, j] <- supported[i, j] & t$term %in% matches[[i]][[j]]$matched
  ifelse(is.na(t$term), NA, ifelse(rowSums(hit) > 1, "w wboth", ifelse(rowSums(hit) == 1,
         paste0("w wk", max.col(hit, ties.method = "first")), if (assessed[i]) "wown" else NA)))
}
synopsis_html <- unlist(lapply(split(seq_len(n_bio), bio$paragraph), function(idx) {
  rows <- vapply(idx, function(i) {
    cols <- vapply(seq_along(src_names), function(j) {
      s <- src_names[j]
      if (!supported[i, s]) return("<div class=\"par none\">no parallel</div>")
      m <- matches[[i]][[s]]
      txt <- passage_text(s, m$rows); tok <- tokenise(txt); tok$term <- unify(tok$term)
      pg <- passage_page(s, m$rows)
      sprintf("<div class=\"par\"><div class=\"meta\">%s coverage %.2f%s</div>%s</div>", if (isTRUE(primary[i] == s)) "main parallel &middot;" else "secondary parallel &middot;",
              m$score, if (nzchar(pg)) paste0(" &middot; PDF p. ", pg) else "",
              paint(txt, tok, ifelse(tok$term %in% m$matched, paste0("w wk", j), NA)))
    }, character(1))
    notes <- c(if (nzchar(wording$conflation_from[i])) sprintf("possible conflation: \u201c%s\u201d from %s", esc(wording$conflation_words[i]), esc(wording$conflation_from[i])),
               if (bio$id[i] %in% hedges_dropped$sentence) "hedge dropped",
               if (wording$run_length[i] >= 5) sprintf("%d-word verbatim run", wording$run_length[i]))
    sprintf("<div class=\"syn\" id=\"r%d\"><div class=\"bio\"><div class=\"meta\"><span class=\"num\">%d</span> %s%s</div>%s</div>%s</div>",
            i, i, key(tradition[i]), if (length(notes)) paste0(" <span class=\"flag\">", paste(notes, collapse = " &middot; "), "</span>") else "",
            paint(bio$text[i], bio_tok[[i]], word_class(i)), paste(cols, collapse = ""))
  }, character(1))
  c(sprintf("<h3 class=\"para\">Paragraph %s</h3>", bio$paragraph[idx[1]]), rows)
}))

src_css <- paste(c(
  sprintf(".k%d{--c:var(--h%d)}", seq_along(src_names), seq_along(src_names)),
  sprintf(".wk%d{--c:var(--h%d)}", seq_along(src_names), seq_along(src_names))), collapse = "")
hue_vars <- function(h) paste(sprintf("--h%d:%s", seq_along(h), h), collapse = ";")
ncol_syn <- length(src_names) + 1

html <- c(
  "<!doctype html><html lang=\"en\"><head><meta charset=\"utf-8\">",
  "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
  "<title>Redaction criticism report</title><style>",
  sprintf(":root{color-scheme:light dark;--bg:#fcfcfb;--panel:#ffffff;--ink:#0b0b0b;--ink2:#52514e;--line:#e4e2dc;--own:#1baf7a;--na:#b9b7b0;--hedge:#b3261e;%s}", hue_vars(hues_light)),
  sprintf("@media (prefers-color-scheme:dark){:root{--bg:#1a1a19;--panel:#232321;--ink:#ffffff;--ink2:#c3c2b7;--line:#3a3936;--own:#199e70;--na:#6b6a65;--hedge:#ff8a80;%s}}", hue_vars(hues_dark)),
  "body{font:16px/1.6 Georgia,'Times New Roman',serif;color:var(--ink);background:var(--bg);margin:0}",
  "main{max-width:1080px;margin:0 auto;padding:32px 20px 80px}",
  "h1{font-size:28px;margin:0 0 4px}h2{font-size:21px;margin:48px 0 6px;border-bottom:1px solid var(--line);padding-bottom:6px}",
  "h3{font:600 15px/1.4 system-ui,sans-serif;margin:26px 0 8px}h3.para{color:var(--ink2);margin-top:30px}",
  ".muted,.note,.lead,.meta,figcaption{color:var(--ink2);font:13px/1.5 system-ui,sans-serif}.lead{font-size:14px;margin:0 0 16px;max-width:820px}.note{margin:6px 0 12px}",
  "ul.profile{padding-left:20px;margin:12px 0}ul.profile li{margin:0 0 8px}",
  ".scroll{overflow-x:auto}table{border-collapse:collapse;width:100%;font:14px/1.45 system-ui,sans-serif}",
  "td,th{padding:6px 10px;text-align:left;border-bottom:1px solid var(--line);vertical-align:top}th{color:var(--ink2);font-weight:600}td.n,th.n{text-align:right;font-variant-numeric:tabular-nums}",
  src_css, ".kown{--c:var(--own)}.kna{--c:var(--na)}",
  ".key{display:inline-flex;align-items:center;gap:6px;font:600 12px/1 system-ui,sans-serif;color:var(--ink);margin-right:12px;white-space:nowrap}",
  ".key i{width:11px;height:11px;border-radius:3px;background:var(--c);display:inline-block}.key.kboth i{background:linear-gradient(to bottom,var(--h1) 50%,var(--h2) 50%)}",
  ".chart{width:100%;height:auto;display:block;margin:8px 0}.chart rect,.chart path,.chart circle{fill:var(--c)}",
  ".chart .dot{stroke:var(--bg);stroke-width:2}.grid{stroke:var(--line);stroke-width:1}.ax{font:11px system-ui,sans-serif;fill:var(--ink2)}.val{font:600 11px system-ui,sans-serif;fill:var(--ink)}",
  ".charts{display:flex;flex-wrap:wrap;gap:20px}.charts figure{flex:1 1 380px;margin:0;min-width:0}",
  ".wordlist{font:14px/2 system-ui,sans-serif}.cnt{color:var(--ink2);font-size:12px}",
  "ol.omit{font-size:14.5px;line-height:1.5;padding-left:34px}ol.omit li{margin:0 0 8px}",
  ".pair{background:var(--panel);border:1px solid var(--line);border-radius:6px;padding:10px 14px;margin:0 0 10px;font-size:15px}",
  ".srcline{font-size:14px;color:var(--ink2);margin-top:6px}.hedge{color:var(--hedge);font-weight:bold;text-decoration:underline}",
  ".num{font:700 13px system-ui,sans-serif;color:var(--ink2);margin-right:4px}.flag{font:12px system-ui,sans-serif;color:var(--ink2);font-style:italic}",
  sprintf(".syn{display:grid;grid-template-columns:repeat(%d,minmax(0,1fr));gap:16px;border-top:1px solid var(--line);padding:12px 0}", ncol_syn),
  ".syn .bio{font-size:15.5px}.par{font-size:13.5px;line-height:1.5;color:var(--ink2)}.par.none{font:italic 13px system-ui,sans-serif;opacity:.7}.meta{margin-bottom:4px}",
  ".synhead{position:sticky;top:0;background:var(--bg);z-index:1;border-top:none;padding:8px 0;font:600 13px system-ui,sans-serif}",
  ".w{background:color-mix(in srgb,var(--c) 24%,transparent);color:var(--ink);border-radius:2px;padding:0 1px}",
  ".wboth{background:linear-gradient(to bottom,color-mix(in srgb,var(--h1) 30%,transparent) 50%,color-mix(in srgb,var(--h2) 30%,transparent) 50%)}",
  ".wown{border-bottom:2px dotted var(--own)}",
  "@media (max-width:760px){.syn{grid-template-columns:1fr;gap:8px}.synhead{display:none}}",
  "</style></head><body><main>",
  "<h1>Redaction criticism report</h1>",
  sprintf("<div class=\"muted\">Redactor: the AI that wrote %s &middot; Sources: %s &middot; Generated %s</div>",
          esc(basename(bio_file)), esc(paste(sprintf("%s (%s)", src_names, basename(source_files)), collapse = "; ")),
          format(Sys.time(), "%d %B %Y")),

  "<h2>Redactional profile</h2>",
  "<p class=\"lead\">The headline numbers. Each is unpacked, with its evidence, in the sections below. Parallels are found by word overlap, so heavily reworded source material can be counted as the redactor's own.</p>",
  paste0("<ul class=\"profile\">", paste(sprintf("<li>%s</li>", profile), collapse = ""), "</ul>"),

  sec("1. Composition: sources and the redactor's own material",
      "Which tradition each sentence belongs to, in the order the redactor arranged them. Material with no parallel in any source is the redactor's own contribution (what gospel critics call special material, or Sondergut). Redaction critics look hardest at where it sits: an editor's own voice is clearest in the introductions, links and summaries that frame the inherited material.",
      sprintf("<p>%s</p>", paste(vapply(trad_levels, key, character(1)), collapse = "")),
      svg_strip(),
      "<p class=\"note\">One cell per sentence, grouped by paragraph (\u00b6). Hover for the text.</p>",
      html_table(comp_show, raw_cols = "tradition"),
      "<h3>Is the redactor's own material concentrated at the ends of paragraphs?</h3>",
      html_table(setNames(pos_tbl, c("position in paragraph", "sentences", "redactor's own", "% redactor's own"))),
      "<p class=\"note\">Paragraphs of three or more sentences only.</p>",
      "<h3>Blocks: does the redactor follow one source for long stretches?</h3>",
      html_table(setNames(transform(block_tbl, tradition = vapply(tradition, key, character(1))),
                          c("tradition", "blocks", "longest run of consecutive sentences")), raw_cols = "tradition"),
      "<h3>Seams: linking phrases that open a sentence</h3>",
      html_table(setNames(transform(seams, tradition = vapply(tradition, key, character(1))),
                          c("sentence", "opening phrase", "tradition", "phrase found in a source?")), raw_cols = "tradition")),

  sec("2. Selection: what the redactor used and left out",
      "An editor's interests show in what is chosen and what is passed over. This section measures how much of each source was taken up, which parts were mined, and what never made it into the biography.",
      html_table(sel_show), loc_html, omit_html,
      "<h3>Source vocabulary the redactor never uses</h3>",
      sprintf("<p class=\"wordlist\">%s</p>", paste(sprintf("%s <span class=\"cnt\">%d</span>", esc(avoided_tbl$word), avoided_tbl$sources_count), collapse = " &middot; ")),
      "<p class=\"note\">Words common in the sources (count shown) that the biography avoids entirely, names excluded.</p>"),

  sec("3. Order: does the redactor keep the source's sequence?",
      "The argument from order is central to the synoptic problem: an editor who follows a source tends to keep its sequence, and departures from it are deliberate. Each dot is a biography sentence placed at the position of its parallel in the source. A rising diagonal means the source's order is kept; a flat band means the redactor keeps returning to the same part of the source; scatter means rearrangement.",
      paste0("<div class=\"charts\">", paste(vapply(src_names[src_names %in% order_pts$source], svg_order, character(1)), collapse = ""), "</div>"),
      html_table(setNames(order_tbl, c("source", "parallels", "order correlation (Spearman)", "backward jumps")))),

  sec("4. Rewording: how closely is the source wording kept?",
      "Verbatim agreement is the basic statistic of synoptic comparison. Here it shows how far the redactor paraphrases, where source wording survives intact, which source's spelling of names is followed, and where two sources are woven into one sentence.",
      html_table(setNames(wording_tbl, c("main source", "sentences", "median % of content words shared with the parallel", "median longest verbatim run (words)",
                                         "longest run (words)", "sentences with a run of 5+ words"))),
      "<h3>Longest stretches taken over word for word</h3>",
      html_table(setNames(top_runs, c("sentence", "source", "words", "shared wording"))),
      "<h3>Whose spelling of names does the redactor follow?</h3>",
      "<p class=\"note\">Names the sources spell differently. Agreement in spelling is a classic sign of which source lay in front of the editor; a spelling found in neither points to knowledge from outside the sources.</p>",
      html_table(spelling),
      "<h3>Possible conflations</h3>",
      "<p class=\"note\">Sentences whose main parallel is in one source but which also contain words found only in the other source's parallel.</p>",
      html_table(setNames(conflations, c("sentence", "main source", "also from", "words from the second source", "biography sentence")))),

  sec("5. Tendencies: systematic changes",
      "Redaction critics infer an editor's outlook from changes made consistently, for example an evangelist who regularly removes expressions of doubt. These counts compare the biography with its sources on features an AI summariser is likely to alter: caution, evaluation, concrete detail, attribution and tense.",
      html_table(tendency_tbl),
      sprintf("<h3>Hedges dropped (%d sentences)</h3>", nrow(hedges_dropped)),
      "<p class=\"note\">The source sentence is qualified (word shown in red); the biography sentence built on it states the point without qualification. Check each: the hedge may attach to a different clause.</p>",
      hedge_html),

  sec("6. The redactor's own style",
      "Words and phrases an editor favours, and the sources do not, mark the editor's hand. Finding them inside a sentence that otherwise follows a source shows where the source has been retouched.",
      "<h3>Characteristic vocabulary</h3>",
      html_table(setNames(vocab[seq_len(min(top_n + 10, nrow(vocab))), ],
                          c("word", "in biography", "in sources", "per 1,000 words (biography)", "per 1,000 words (sources)", "times more frequent"))),
      "<h3>Stock phrases (used 2+ times in the biography, never in a source)</h3>",
      html_table(phrases)),

  sec("7. Synopsis",
      sprintf("The biography and its parallels side by side, sentence by sentence. Highlighted words are shared with a parallel (%s); words with a dotted underline are content words found in no parallel, i.e. the redactor's own wording. A secondary parallel with low coverage is often a coincidental overlap of common words.",
              paste(c(vapply(src_names, key, character(1)), key("Both")), collapse = "")),
      sprintf("<div class=\"syn synhead\"><div>Biography</div>%s</div>", paste(sprintf("<div>%s</div>", esc(src_names)), collapse = "")),
      synopsis_html),
  "</main></body></html>"
)
writeLines(enc2utf8(html), file.path(redaction_dir, "redaction_report.html"), useBytes = TRUE)

cat("\n================ REDACTIONAL PROFILE ================\n")
cat(paste0(" - ", stri_replace_all_regex(profile, "<[^>]+>", ""), collapse = "\n"), "\n")
cat("\nDone. Open ", file.path(redaction_dir, "redaction_report.html"), " in a browser.\n", sep = "")

