library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)
library(tidytext)
library(textdata)

`%||%` <- function(x, y) if (is.null(x)) y else x

MIN_WORDS <- 5
GITHUB_URL <- "https://github.com/verona238/finish_r_project"

PAL <- list(
  pos     = "#14538a",
  neg     = "#c0635e",
  neutral = "#9bb8cc",
  ink     = "#1c2733",
  muted   = "#8a93a0"
)

EMO_COLORS <- c(
  "радость"      = "#d9a441",
  "доверие"      = "#4f9d77",
  "предвкушение" = "#3f78b5",
  "удивление"    = "#7fb6dd",
  "печаль"       = "#9aa3af",
  "страх"        = "#5d6b7a",
  "гнев"         = "#c0635e",
  "отвращение"   = "#3a4049"
)

# ---- Резервные встроенные словари ---------------------------------------
# Они нужны только для устойчивости приложения: если внешний лексикон недоступен,
# приложение не падает, а честно показывает источник «fallback».
POS_RU <- c(
  "хорош", "отличн", "прекрасн", "любл", "любов", "радост", "рад", "успе",
  "надежд", "замечательн", "великолепн", "счаст", "добр", "восхит", "лучш",
  "удовольств", "прият", "улыб", "побед", "достиж", "вдохнов", "благодар",
  "нрав", "восторг", "уверен", "перспектив", "рост", "выгод", "эффективн"
)

NEG_RU <- c(
  "плох", "ужасн", "проблем", "страх", "потер", "грусть", "груст", "ошиб",
  "ненавист", "печал", "разочаров", "худш", "боль", "кризис", "провал",
  "сложн", "трудн", "опасн", "угроз", "недостат", "жаль", "сожал",
  "разруш", "паден", "убыт", "конфликт", "риск", "сбой"
)

POS_EN <- c(
  "good", "great", "excellent", "love", "joy", "success", "hope", "wonderful",
  "amazing", "happy", "best", "positive", "win", "growth", "benefit", "improve"
)

NEG_EN <- c(
  "bad", "terrible", "problem", "fear", "loss", "sad", "error", "hate",
  "worst", "pain", "crisis", "fail", "difficult", "danger", "negative", "risk"
)

STOP_RU <- c(
  "и", "в", "во", "не", "что", "он", "на", "я", "с", "со", "как", "а", "то",
  "все", "она", "так", "его", "но", "да", "ты", "к", "у", "же", "вы", "за",
  "бы", "по", "только", "ее", "её", "мне", "было", "вот", "от", "меня",
  "еще", "ещё", "нет", "о", "из", "ему", "когда", "даже", "ну", "ли", "уже",
  "или", "ни", "быть", "был", "него", "до", "вас", "там", "потом", "себя",
  "ей", "они", "тут", "где", "есть", "для", "мы", "тебя", "их", "чем", "была",
  "без", "чего", "раз", "себе", "под", "будет", "тогда", "кто", "этот", "того",
  "потому", "этого", "какой", "ним", "здесь", "этом", "один", "мой", "тем",
  "чтобы", "нее", "неё", "были", "куда", "всех", "при", "два", "об", "другой",
  "после", "над", "эти", "нас", "про", "всего", "них", "эту", "моя", "свою",
  "этой", "перед", "том", "такой", "им", "более", "всю", "между", "это", "также",
  "однако", "возможны"
)

STOP_EN <- c(
  "the", "a", "an", "and", "or", "but", "is", "are", "was", "were", "of", "to",
  "in", "on", "for", "with", "as", "by", "at", "it", "this", "that", "i", "you",
  "he", "she", "they", "we", "be", "have", "has", "had", "do", "not", "no", "so",
  "if", "then", "than", "too", "very", "can", "will", "just"
)

EMO_RU <- list(
  "радость" = c(
    "радост", "рад", "счаст", "весел", "ликов", "восторг", "восхищ", "наслажд",
    "удовольств", "улыб", "смех", "праздник", "любл", "любов", "прекрасн",
    "замечательн", "отличн", "успе", "побед", "вдохнов"
  ),
  "доверие" = c(
    "довер", "надёжн", "надежн", "верност", "преданн", "честн", "поддержк",
    "опор", "уважен", "искрен", "друж", "союзник", "спокой", "уверен",
    "благодар", "забот"
  ),
  "предвкушение" = c(
    "ожида", "предвкуш", "надежд", "планир", "стремл", "мечт", "перспектив",
    "будущ", "нетерпен", "готов"
  ),
  "удивление" = c(
    "удивл", "поражён", "поражен", "изумл", "неожида", "внезап", "шок",
    "невероятн", "сюрприз", "вдруг"
  ),
  "печаль" = c(
    "печал", "груст", "тоск", "уныл", "скорб", "одиночеств", "потер", "утрат",
    "разочаров", "жаль", "сожал", "слёз", "слез", "плач"
  ),
  "страх" = c(
    "страх", "страшн", "боя", "испуг", "тревог", "паник", "ужас", "опасен",
    "опасн", "угроз", "беспоко", "нервн", "кошмар"
  ),
  "гнев" = c(
    "гнев", "зл", "ярост", "бешен", "раздраж", "негодован", "возмущ",
    "ненавист", "агресс", "конфликт", "ссор", "обид", "разъяр"
  ),
  "отвращение" = c(
    "отвращ", "омерз", "брезг", "тошнот", "гадк", "мерзк", "презрен",
    "неприязн", "противн"
  )
)

EMO_EN_FALLBACK <- list(
  "радость" = c(
    "joy", "happy", "glad", "delight", "pleasure", "cheer", "smile", "laugh",
    "love", "wonderful", "great", "success", "enjoy", "fun", "excit"
  ),
  "доверие" = c(
    "trust", "reliable", "honest", "faith", "support", "respect", "sincere",
    "secure", "confident", "grateful", "loyal"
  ),
  "предвкушение" = c(
    "anticipat", "expect", "hope", "plan", "ready", "eager", "await", "upcoming", "soon"
  ),
  "удивление" = c(
    "surprise", "amaze", "astonish", "unexpected", "sudden", "shock", "incredible", "wow"
  ),
  "печаль" = c(
    "sad", "grief", "sorrow", "lonely", "loss", "lose", "disappoint", "regret",
    "cry", "tears", "miserable", "unhappy", "gloom"
  ),
  "страх" = c(
    "fear", "afraid", "scare", "anxi", "panic", "terror", "danger", "threat",
    "worry", "nervous", "nightmare", "dread"
  ),
  "гнев" = c(
    "anger", "angry", "rage", "fury", "furious", "irritat", "outrage", "hate",
    "aggress", "annoy"
  ),
  "отвращение" = c(
    "disgust", "gross", "nasty", "revolt", "awful", "horrible", "despise", "repuls", "sick"
  )
)

POS_EMOTIONS <- c("радость", "доверие", "предвкушение", "удивление")
NEG_EMOTIONS <- c("печаль", "страх", "гнев", "отвращение")
NRC_EMOTION_COLS <- c("anger", "anticipation", "disgust", "fear", "joy", "sadness", "surprise", "trust")

# ---- Стандартные tidytext-лексиконы для английского текста ---------------
load_tidy_lexicon <- function(lexicon) {
  tryCatch(
    tidytext::get_sentiments(lexicon) |>
      as_tibble() |>
      mutate(word = tolower(as.character(word))),
    error = function(e) {
      message("Лексикон tidytext не загружен: ", lexicon, ". Ошибка: ", e$message)
      tibble()
    }
  )
}

TIDY_BING     <- load_tidy_lexicon("bing")
TIDY_AFINN    <- load_tidy_lexicon("afinn")
TIDY_LOUGHRAN <- load_tidy_lexicon("loughran")
TIDY_NRC      <- load_tidy_lexicon("nrc")

translate_nrc_emotion <- function(x) {
  dplyr::recode(
    x,
    "anger"        = "гнев",
    "anticipation" = "предвкушение",
    "disgust"      = "отвращение",
    "fear"         = "страх",
    "joy"          = "радость",
    "sadness"      = "печаль",
    "surprise"     = "удивление",
    "trust"        = "доверие",
    "positive"     = "positive",
    "negative"     = "negative",
    .default = x
  )
}

# ---- Русский NRC из подготовленного CSV ----------------------------------
# Файл должен лежать рядом с app.R: finish_r_project/app.R и finish_r_project/nrc_russian.csv
NRC_RU_PATH <- "nrc_russian.csv"

load_nrc_ru_external <- function(path = NRC_RU_PATH) {
  empty <- tibble(word = character(), emotion = character())
  if (!file.exists(path)) return(empty)
  
  raw <- tryCatch(
    utils::read.csv(
      path,
      stringsAsFactors = FALSE,
      check.names = FALSE,
      fileEncoding = "UTF-8"
    ),
    error = function(e) NULL
  )
  
  if (is.null(raw) || nrow(raw) == 0) return(empty)
  if (!("Russian (ru)" %in% names(raw)) || !all(NRC_EMOTION_COLS %in% names(raw))) {
    return(empty)
  }
  
  raw |>
    rename(word = `Russian (ru)`) |>
    select(word, all_of(NRC_EMOTION_COLS)) |>
    mutate(word = tolower(as.character(word))) |>
    filter(!is.na(word), nchar(word) > 1) |>
    pivot_longer(
      cols = all_of(NRC_EMOTION_COLS),
      names_to = "emotion",
      values_to = "assoc"
    ) |>
    filter(assoc == 1) |>
    mutate(emotion = translate_nrc_emotion(emotion)) |>
    distinct(word, emotion)
}

NRC_RU_EXTERNAL <- load_nrc_ru_external()
message("Русский NRC загружен: ", nrow(NRC_RU_EXTERNAL), " строк")

nrc_ru_status <- if (nrow(NRC_RU_EXTERNAL) > 0) {
  paste0("Русский NRC загружен: ", format(nrow(NRC_RU_EXTERNAL), big.mark = " "), " строк.")
} else {
  "Русский NRC не найден или не прочитан; будет использован fallback-словарь."
}

# ---- Вспомогательные функции анализа ------------------------------------
empty_emotions <- function() {
  tibble(emotion = names(EMO_RU), value = 0)
}

scores_from_sentiment_lexicon <- function(token_tbl, lex_tbl) {
  value <- rep(0, nrow(token_tbl))
  if (nrow(lex_tbl) == 0) return(value)
  
  joined <- token_tbl |>
    left_join(lex_tbl, by = "word")
  
  score_tbl <- joined |>
    filter(sentiment %in% c("positive", "negative")) |>
    mutate(score = if_else(sentiment == "positive", 1, -1)) |>
    group_by(position) |>
    summarise(score = sum(score), .groups = "drop")
  
  if (nrow(score_tbl) > 0) value[score_tbl$position] <- score_tbl$score
  value
}

scores_from_afinn <- function(token_tbl, lex_tbl) {
  value <- rep(0, nrow(token_tbl))
  if (nrow(lex_tbl) == 0) return(value)
  
  score_tbl <- token_tbl |>
    left_join(lex_tbl, by = "word") |>
    mutate(value = tidyr::replace_na(value, 0)) |>
    group_by(position) |>
    summarise(score = sum(value), .groups = "drop")
  
  if (nrow(score_tbl) > 0) value[score_tbl$position] <- score_tbl$score
  value
}

scores_from_prefix_fallback <- function(tok_lc, is_en) {
  pos_set <- if (is_en) POS_EN else POS_RU
  neg_set <- if (is_en) NEG_EN else NEG_RU
  
  vapply(tok_lc, function(t) {
    is_pos <- any(startsWith(t, pos_set))
    is_neg <- any(startsWith(t, neg_set))
    if (is_pos && !is_neg) 1 else if (is_neg && !is_pos) -1 else 0
  }, numeric(1))
}

scores_and_emotions_from_fallback_nrc <- function(tok_lc, is_en) {
  emo_lex <- if (is_en) EMO_EN_FALLBACK else EMO_RU
  
  value <- vapply(tok_lc, function(t) {
    emo_hit <- names(emo_lex)[
      vapply(emo_lex, function(stems) any(startsWith(t, stems)), logical(1))
    ]
    sum(emo_hit %in% POS_EMOTIONS) - sum(emo_hit %in% NEG_EMOTIONS)
  }, numeric(1))
  
  emo_counts <- vapply(
    emo_lex,
    function(stems) {
      sum(vapply(tok_lc, function(t) any(startsWith(t, stems)), logical(1)))
    },
    integer(1)
  )
  
  list(
    value = value,
    emotions = tibble(emotion = names(emo_counts), value = as.numeric(emo_counts))
  )
}

# ---- Основная функция анализа -------------------------------------------
# English: Bing / AFINN / Loughran–McDonald / NRC из tidytext.
# Русский: NRC из nrc_russian.csv или резервный словарь основ.
analyze_text <- function(text, lexicon = "nrc", rm_stop = TRUE,
                         to_lower = TRUE, lang = "Русский") {
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  
  raw <- if (to_lower) tolower(text) else text
  tokens <- unlist(str_extract_all(raw, "[\\p{L}]+"))
  tokens <- tokens[nchar(tokens) > 1]
  if (length(tokens) == 0) return(NULL)
  
  is_en <- lang == "English"
  stop_w <- if (is_en) STOP_EN else STOP_RU
  tok_lc <- tolower(tokens)
  total_words <- length(tokens)
  uniq_words <- length(unique(tok_lc))
  
  if (rm_stop) tok_lc <- tok_lc[!(tok_lc %in% stop_w)]
  if (length(tok_lc) == 0) return(NULL)
  
  token_tbl <- tibble(position = seq_along(tok_lc), word = tok_lc)
  value <- rep(0, length(tok_lc))
  emotions <- empty_emotions()
  lexicon_source <- "не определён"
  
  if (is_en && lexicon == "bing" && nrow(TIDY_BING) > 0) {
    lexicon_source <- "tidytext: Bing"
    value <- scores_from_sentiment_lexicon(token_tbl, TIDY_BING)
    
  } else if (is_en && lexicon == "afinn" && nrow(TIDY_AFINN) > 0) {
    lexicon_source <- "tidytext: AFINN"
    value <- scores_from_afinn(token_tbl, TIDY_AFINN)
    
  } else if (is_en && lexicon == "loughran" && nrow(TIDY_LOUGHRAN) > 0) {
    lexicon_source <- "tidytext: Loughran–McDonald"
    value <- scores_from_sentiment_lexicon(token_tbl, TIDY_LOUGHRAN)
    
  } else if (is_en && lexicon == "nrc" && nrow(TIDY_NRC) > 0) {
    lexicon_source <- "tidytext: NRC"
    joined <- token_tbl |>
      left_join(TIDY_NRC, by = "word")
    
    pol_tbl <- joined |>
      filter(sentiment %in% c("positive", "negative")) |>
      mutate(score = if_else(sentiment == "positive", 1, -1)) |>
      group_by(position) |>
      summarise(score = sum(score), .groups = "drop")
    
    if (nrow(pol_tbl) > 0) value[pol_tbl$position] <- pol_tbl$score
    
    emo_tbl <- joined |>
      filter(sentiment %in% NRC_EMOTION_COLS) |>
      mutate(emotion = translate_nrc_emotion(sentiment)) |>
      count(emotion, name = "value")
    
    emotions <- empty_emotions() |>
      select(emotion) |>
      left_join(emo_tbl, by = "emotion") |>
      mutate(value = tidyr::replace_na(value, 0))
    
  } else if (!is_en && lexicon == "nrc" && nrow(NRC_RU_EXTERNAL) > 0) {
    lexicon_source <- "русский NRC: nrc_russian.csv"
    emo_tbl_long <- token_tbl |>
      left_join(NRC_RU_EXTERNAL, by = "word")
    
    emo_tbl <- emo_tbl_long |>
      filter(!is.na(emotion)) |>
      count(emotion, name = "value")
    
    emotions <- empty_emotions() |>
      select(emotion) |>
      left_join(emo_tbl, by = "emotion") |>
      mutate(value = tidyr::replace_na(value, 0))
    
    score_tbl <- emo_tbl_long |>
      filter(!is.na(emotion)) |>
      mutate(score = case_when(
        emotion %in% POS_EMOTIONS ~ 1,
        emotion %in% NEG_EMOTIONS ~ -1,
        TRUE ~ 0
      )) |>
      group_by(position) |>
      summarise(score = sum(score), .groups = "drop")
    
    if (nrow(score_tbl) > 0) value[score_tbl$position] <- score_tbl$score
    
  } else if (!is_en && lexicon == "ru_fallback") {
    lexicon_source <- "резервный русский словарь основ"
    value <- scores_from_prefix_fallback(tok_lc, is_en = FALSE)
    
  } else if (lexicon == "nrc") {
    lexicon_source <- if (is_en) "резервный английский NRC" else "резервный русский NRC"
    fb <- scores_and_emotions_from_fallback_nrc(tok_lc, is_en)
    value <- fb$value
    emotions <- fb$emotions
    
  } else {
    lexicon_source <- if (is_en) "резервный английский словарь" else "резервный русский словарь"
    value <- scores_from_prefix_fallback(tok_lc, is_en)
  }
  
  polarity <- ifelse(value > 0, "pos", ifelse(value < 0, "neg", "neu"))
  n_pos <- sum(polarity == "pos")
  n_neg <- sum(polarity == "neg")
  n_ton <- max(n_pos + n_neg, 1)
  max_abs <- if (lexicon == "afinn") 5 else max(max(abs(value)), 1)
  index <- sum(value) / (max_abs * n_ton)
  
  label <- if (index > 0.05) {
    "Позитивная"
  } else if (index < -0.05) {
    "Негативная"
  } else {
    "Нейтральная"
  }
  
  label_color <- if (index > 0.05) {
    PAL$pos
  } else if (index < -0.05) {
    PAL$neg
  } else {
    PAL$muted
  }
  
  nb <- min(10L, length(value))
  grp <- pmin(ceiling(seq_along(value) / (length(value) / nb)), nb)
  dynamics <- tibble(
    block = seq_len(nb),
    net = as.numeric(tapply(value, factor(grp, levels = seq_len(nb)), sum))
  )
  dynamics$net[is.na(dynamics$net)] <- 0
  
  top_words <- tibble(word = tok_lc, pol = polarity) |>
    filter(pol != "neu") |>
    count(word, pol, name = "n") |>
    group_by(pol) |>
    slice_max(n, n = 6, with_ties = FALSE) |>
    ungroup()
  
  list(
    lexicon = lexicon,
    lexicon_source = lexicon_source,
    label = label,
    label_color = label_color,
    index = index,
    total_words = total_words,
    uniq_words = uniq_words,
    n_pos = n_pos,
    n_neg = n_neg,
    dynamics = dynamics,
    top_words = top_words,
    emotions = emotions
  )
}

# ---- UI-вспомогательные функции -----------------------------------------
empty_plot <- function(msg = "Введите текст и нажмите «Анализировать»") {
  ggplot() +
    annotate("text", x = 0, y = 0, label = msg, colour = PAL$muted, size = 4.4) +
    theme_void()
}

base_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      plot.margin = margin(4, 8, 4, 4),
      axis.title = element_text(colour = PAL$muted, size = 11),
      axis.text = element_text(colour = PAL$muted)
    )
}

stat_card <- function(label, value, sub, accent) {
  div(
    class = "stat-card",
    style = paste0("--accent:", accent, ";"),
    div(class = "stat-label", label),
    div(class = "stat-value", value),
    div(class = "stat-sub", sub)
  )
}

panel_head <- function(title, sub = NULL, hint = NULL) {
  tagList(
    div(
      class = "panel-title",
      span(title),
      if (!is.null(hint)) {
        bslib::tooltip(span(class = "help-badge", "?"), hint, placement = "top")
      }
    ),
    if (!is.null(sub)) div(class = "panel-sub", sub)
  )
}

read_uploaded_text <- function(file) {
  ext <- tolower(tools::file_ext(file$name))
  
  if (ext == "csv") {
    tab <- tryCatch(
      utils::read.csv(
        file$datapath,
        stringsAsFactors = FALSE,
        fileEncoding = "UTF-8",
        check.names = FALSE
      ),
      error = function(e) NULL
    )
    
    # Если CSV с точкой с запятой, пробуем read.csv2().
    if (!is.null(tab) && ncol(tab) == 1) {
      tab2 <- tryCatch(
        utils::read.csv2(
          file$datapath,
          stringsAsFactors = FALSE,
          fileEncoding = "UTF-8",
          check.names = FALSE
        ),
        error = function(e) NULL
      )
      if (!is.null(tab2) && ncol(tab2) > 1) tab <- tab2
    }
    
    if (is.null(tab)) return(NULL)
    char_cols <- tab[, vapply(tab, is.character, logical(1)), drop = FALSE]
    if (ncol(char_cols) == 0) return(paste(unlist(tab), collapse = " "))
    return(paste(unlist(char_cols), collapse = " "))
  }
  
  tryCatch(
    paste(readLines(file$datapath, warn = FALSE, encoding = "UTF-8"), collapse = " "),
    error = function(e) NULL
  )
}

app_theme <- bs_theme(
  version = 5,
  bg = "#eef0f2",
  fg = PAL$ink,
  primary = PAL$pos,
  "border-radius" = "10px"
)

# ---- Интерфейс -----------------------------------------------------------
ui <- page_fluid(
  theme = app_theme,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"
    ),
    tags$style(HTML("
      * { font-family: 'Inter', system-ui, sans-serif; }
      body { background: #eef0f2; }
      .container-fluid { padding: 0; }

      .app-header { background:#171a21; color:#fff; padding:14px 28px;
        display:flex; align-items:center; justify-content:space-between; }
      .app-header .brand { display:flex; align-items:center; gap:12px;
        font-weight:800; letter-spacing:.09em; font-size:13.5px; text-transform:uppercase; }
      .brand-mark { width:22px; height:22px; border:2px solid #fff; border-radius:50%;
        display:flex; align-items:center; justify-content:center; }
      .brand-mark span { width:7px; height:7px; background:#fff; border-radius:50%; }

      .body-wrap { padding:22px 28px; }

      .bslib-sidebar-layout > .sidebar { background:#fff; border:1px solid #e3e6ea;
        border-radius:10px; }
      .sidebar-title { font-size:17px; font-weight:700; margin:2px 0 14px;
        padding-bottom:12px; border-bottom:1px solid #eef0f2; }
      .ctrl-label { font-size:12.5px; font-weight:600; color:#46505c; margin:14px 0 6px; }
      .ctrl-note { font-size:11px; color:#8a93a0; margin-top:4px; line-height:1.4; }
      .btn-analyze { background:#171a21; color:#fff; width:100%; border:none;
        padding:12px; font-weight:700; letter-spacing:.07em; text-transform:uppercase;
        font-size:12.5px; border-radius:8px; margin-top:18px; }
      .btn-analyze:hover { background:#262b35; color:#fff; }

      .about-card { background:#fff; border:1px solid #e3e6ea; border-radius:10px;
        padding:18px 22px; margin-bottom:18px; }
      .about-text { font-size:13px; line-height:1.6; color:#525c68; margin-top:4px; }
      .about-text b { color:#1c2733; }
      .about-text ul { margin:6px 0 0 0; padding-left:20px; }

      .stat-card { background:#fff; border:1px solid #e3e6ea;
        border-left:4px solid var(--accent,#14538a); border-radius:8px;
        padding:14px 16px; height:100%; }
      .stat-label { font-size:10px; letter-spacing:.1em; text-transform:uppercase;
        color:#8a93a0; font-weight:600; }
      .stat-value { font-size:26px; font-weight:800; line-height:1.15; margin:7px 0 3px;
        word-break:break-word; }
      .stat-sub { font-size:11px; color:#9aa3af; }

      .panel-card { background:#fff; border:1px solid #e3e6ea; border-radius:10px;
        padding:18px 20px; height:100%; }
      .panel-card .card-body { padding:0; }
      .panel-title { font-size:16px; font-weight:700; color:#1c2733;
        display:flex; align-items:center; gap:8px; }
      .help-badge { display:inline-flex; align-items:center; justify-content:center;
        width:16px; height:16px; border-radius:50%; background:#e3e6ea; color:#5a6470;
        font-size:11px; font-weight:700; cursor:help; }
      .help-badge:hover { background:#cdd4db; }
      .panel-sub { font-size:11.5px; color:#9aa3af; margin:2px 0 12px; }

      .app-footer { background:#fff; border-top:1px solid #e3e6ea;
        padding:18px 28px; display:flex; align-items:center;
        justify-content:space-between; flex-wrap:wrap; gap:16px; margin-top:6px; }
      .foot-head { font-size:13px; font-weight:700; }
      .foot-sub { font-size:11.5px; color:#9aa3af; }
      .foot-links { display:flex; align-items:center; gap:14px; flex-wrap:wrap; }
      .foot-repo { background:#171a21; color:#fff !important; border-radius:7px;
        padding:9px 14px; font-size:11.5px; font-weight:600; text-decoration:none;
        text-transform:uppercase; letter-spacing:.06em; }
      .foot-pub { border:1px solid #e3e6ea; border-radius:7px; padding:9px 14px;
        font-size:11.5px; color:#9aa3af; }
    "))
  ),
  
  useShinyjs(),
  
  div(
    class = "app-header",
    div(
      class = "brand",
      div(class = "brand-mark", span()),
      "Анализ эмоциональной тональности"
    )
  ),
  
  div(
    class = "body-wrap",
    
    div(
      class = "about-card",
      div(class = "panel-title", "О приложении"),
      div(
        class = "about-text",
        HTML(
          "Приложение выполняет <b>словарный анализ эмоциональной тональности</b> текста
          на русском и английском языках. Введите текст или загрузите файл (.txt/.csv),
          выберите язык и лексикон, при необходимости уберите стоп-слова — и получите
          общую тональность, динамику по блокам текста, распределение восьми эмоций и
          топ слов, влияющих на оценку.<br>
          Для <b>английского текста</b> используются стандартные лексиконы из <b>tidytext</b>:
          <ul><li><b>Bing</b> — делит слова на позитивные и негативные;</li>
          <li><b>AFINN</b> — задаёт числовую оценку от −5 до +5;</li>
          <li><b>Loughran–McDonald</b> — лексикон для финансовых и деловых текстов;</li>
          <li><b>NRC</b> — распределяет слова по эмоциям: радость, доверие, страх, гнев и др.</li></ul>
          Для <b>русского текста</b> используется подготовленный командой файл
          <b>nrc_russian.csv</b>, который должен лежать рядом с app.R. Если внешний файл или
          внешний лексикон недоступен, приложение показывает резервный fallback-источник,
          чтобы не падать на shinyapps.io."
        )
      ),
      div(
        class = "about-text",
        style = "margin-top:10px;",
        HTML(
          "<b>Ограничение метода.</b> Словарный подход хорошо подходит для быстрой и
          воспроизводимой визуализации, но хуже учитывает контекст, отрицания и сарказм.
          Поэтому результаты следует интерпретировать как приблизительную оценку
          эмоциональной окраски текста."
        )
      )
    ),
    
    layout_sidebar(
      border = FALSE,
      fillable = FALSE,
      sidebar = sidebar(
        width = 320,
        padding = 18,
        div(class = "sidebar-title", "Параметры анализа"),
        
        div(class = "ctrl-label", "Введите текст для анализа"),
        textAreaInput(
          "text",
          NULL,
          rows = 6,
          placeholder = "Вставьте текст или абзац...",
          width = "100%"
        ),
        uiOutput("word_hint"),
        
        div(class = "ctrl-label", "…или загрузите файл (.txt / .csv)"),
        fileInput(
          "file",
          NULL,
          accept = c(".txt", ".csv"),
          buttonLabel = "Обзор…",
          placeholder = "Файл не выбран"
        ),
        
        div(class = "ctrl-label", "Язык текста"),
        radioButtons(
          "lang",
          NULL,
          inline = TRUE,
          choices = c("Русский", "English"),
          selected = "Русский"
        ),
        
        div(class = "ctrl-label", "Выберите лексикон"),
        uiOutput("lexicon_ui"),
        div(class = "ctrl-note", textOutput("lexicon_note", inline = TRUE)),
        
        div(
          style = "margin-top:14px;",
          checkboxInput("rm_stop", "Удалять стоп-слова", TRUE),
          checkboxInput("to_lower", "Приводить к нижнему регистру", TRUE)
        ),
        
        actionButton("analyze", "Анализировать", class = "btn-analyze")
      ),
      
      uiOutput("stat_row"),
      
      div(
        style = "margin-top:18px;",
        layout_columns(
          col_widths = c(6, 6),
          card(
            class = "panel-card",
            panel_head(
              "Динамика тональности по тексту",
              "сумма тональности по блокам",
              hint = "Текст делится на блоки по порядку слов; высота столбца — сумма тональности слов в блоке."
            ),
            plotOutput("dyn_plot", height = 250)
          ),
          card(
            class = "panel-card",
            panel_head(
              "Распределение эмоций",
              "доступно при выборе NRC",
              hint = "Сколько слов текста относится к каждой из восьми эмоций модели NRC."
            ),
            plotOutput("emo_plot", height = 250)
          )
        )
      ),
      
      div(
        style = "margin-top:18px;",
        card(
          class = "panel-card",
          panel_head(
            "Топ слов по вкладу в тональность",
            hint = "Самые частые позитивные и негативные слова текста."
          ),
          plotOutput("top_plot", height = 300)
        )
      )
    )
  ),
  
  div(
    class = "app-footer",
    div(
      div(class = "foot-head", "Команда six seven"),
      div(class = "foot-sub", "Назаровская Вероника · Безрукова Анастасия · Итыгилова Юмжит"),
      div(class = "foot-sub", "НИУ ВШЭ · Компьютерный анализ текста")
    ),
    div(
      class = "foot-links",
      tags$a(
        class = "foot-repo",
        href = GITHUB_URL,
        target = "_blank",
        "Репозиторий на GitHub"
      ),
      div(class = "foot-pub", "Опубликовано: shinyapps.io")
    )
  )
)

# ---- Сервер --------------------------------------------------------------
server <- function(input, output, session) {
  observeEvent(input$file, {
    req(input$file)
    txt <- read_uploaded_text(input$file)
    if (!is.null(txt) && nzchar(trimws(txt))) {
      updateTextAreaInput(session, "text", value = txt)
    }
  })
  
  word_count <- reactive({
    length(unlist(str_extract_all(input$text %||% "", "[\\p{L}]+")))
  })
  
  observe({
    toggleState("analyze", condition = word_count() >= MIN_WORDS)
  })
  
  output$word_hint <- renderUI({
    n <- word_count()
    ok <- n >= MIN_WORDS
    col <- if (ok) "#2f8f5b" else PAL$muted
    div(
      style = paste0("font-size:11.5px; margin-top:4px; color:", col, ";"),
      if (ok) {
        paste0("Слов: ", n, " — можно анализировать")
      } else {
        paste0("Слов: ", n, " из ", MIN_WORDS, " (минимум для анализа)")
      }
    )
  })
  
  output$lexicon_ui <- renderUI({
    if (identical(input$lang, "English")) {
      selectInput(
        "lexicon",
        NULL,
        width = "100%",
        selected = "bing",
        choices = c(
          "Bing, tidytext" = "bing",
          "AFINN, tidytext" = "afinn",
          "Loughran–McDonald, tidytext" = "loughran",
          "NRC, tidytext" = "nrc"
        )
      )
    } else {
      selectInput(
        "lexicon",
        NULL,
        width = "100%",
        selected = "nrc",
        choices = c(
          "Русский NRC из nrc_russian.csv" = "nrc",
          "Резервный русский словарь основ" = "ru_fallback"
        )
      )
    }
  })
  
  output$lexicon_note <- renderText({
    if (identical(input$lang, "English")) {
      "Для English используются стандартные лексиконы tidytext. Если один из них недоступен, приложение переключится на fallback."
    } else {
      nrc_ru_status
    }
  })
  
  res <- reactive({
    chosen_lexicon <- input$lexicon %||% if (identical(input$lang, "English")) "bing" else "nrc"
    analyze_text(
      input$text,
      chosen_lexicon,
      input$rm_stop,
      input$to_lower,
      input$lang
    )
  }) |>
    bindEvent(input$analyze, ignoreNULL = FALSE)
  
  output$stat_row <- renderUI({
    r <- res()
    if (is.null(r)) {
      cards <- list(
        stat_card("Общая тональность", "—", "нажмите «Анализировать»", PAL$muted),
        stat_card("Источник лексикона", "—", "", PAL$ink),
        stat_card("Позитивных слов", "—", "", PAL$pos),
        stat_card("Негативных слов", "—", "", PAL$neg)
      )
    } else {
      pct <- function(n) round(n / r$total_words * 100, 1)
      cards <- list(
        stat_card(
          "Общая тональность",
          span(style = paste0("color:", r$label_color, ";"), r$label),
          paste0("индекс ", sprintf("%+.2f", r$index)),
          r$label_color
        ),
        stat_card(
          "Источник лексикона",
          r$lexicon_source,
          paste0(
            "всего слов: ", format(r$total_words, big.mark = " "),
            "; уникальных: ", r$uniq_words
          ),
          PAL$ink
        ),
        stat_card("Позитивных слов", r$n_pos, paste0(pct(r$n_pos), "% от текста"), PAL$pos),
        stat_card("Негативных слов", r$n_neg, paste0(pct(r$n_neg), "% от текста"), PAL$neg)
      )
    }
    do.call(layout_columns, c(list(col_widths = c(3, 3, 3, 3)), cards))
  })
  
  output$dyn_plot <- renderPlot({
    r <- res()
    if (is.null(r)) return(empty_plot())
    
    ggplot(r$dynamics, aes(block, net, fill = net >= 0)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 0, colour = "#cfd6dd") +
      scale_fill_manual(values = c("TRUE" = PAL$pos, "FALSE" = PAL$neg), guide = "none") +
      scale_x_continuous(
        breaks = r$dynamics$block,
        labels = paste0("Блок ", r$dynamics$block)
      ) +
      labs(x = NULL, y = "тональность блока") +
      base_theme() +
      theme(
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(size = 9, angle = 0)
      )
  })
  
  output$emo_plot <- renderPlot({
    r <- res()
    if (is.null(r)) return(empty_plot())
    if (r$lexicon != "nrc") return(empty_plot("Доступно при выборе NRC"))
    
    e <- dplyr::filter(r$emotions, value > 0)
    if (nrow(e) == 0) return(empty_plot("Эмоциональные слова не найдены"))
    
    ggplot(e, aes(value, reorder(emotion, value), fill = emotion)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      scale_fill_manual(values = EMO_COLORS) +
      labs(x = "число слов", y = NULL) +
      base_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12, colour = PAL$ink)
      )
  })
  
  output$top_plot <- renderPlot({
    r <- res()
    if (is.null(r)) return(empty_plot())
    
    tw <- r$top_words
    if (nrow(tw) == 0) return(empty_plot("Тональные слова не найдены"))
    
    tw$pol <- factor(tw$pol, c("pos", "neg"), c("ПОЗИТИВНЫЕ", "НЕГАТИВНЫЕ"))
    
    ggplot(tw, aes(n, reorder(word, n), fill = pol)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      facet_wrap(~pol, scales = "free_y", ncol = 2) +
      scale_fill_manual(values = c("ПОЗИТИВНЫЕ" = PAL$pos, "НЕГАТИВНЫЕ" = PAL$neg)) +
      labs(x = NULL, y = NULL) +
      base_theme() +
      theme(
        panel.grid.major.y = element_blank(),
        axis.text.y = element_text(size = 12, colour = PAL$ink),
        strip.text = element_text(face = "bold", hjust = 0, colour = PAL$muted, size = 12)
      )
  })
}

shinyApp(ui, server)