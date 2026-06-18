library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)
library(tidyr)

`%||%` <- function(x, y) if (is.null(x)) y else x

MIN_WORDS <- 5  # минимум слов, при котором кнопка анализа становится активной

GITHUB_URL <- "https://github.com/verona238/finish_r_project"

PAL <- list(
  pos     = "#14538a",
  neg     = "#c0635e",
  neutral = "#9bb8cc",
  ink     = "#1c2733",
  muted   = "#8a93a0"
)

EMO_COLORS <- c(
  "радость"      = "#d9a441", "доверие"   = "#4f9d77",
  "предвкушение" = "#3f78b5", "удивление" = "#7fb6dd",
  "печаль"       = "#9aa3af", "страх"     = "#5d6b7a",
  "гнев"         = "#c0635e", "отвращение" = "#3a4049"
)

# ---- Встроенные словари тональности (основы слов, сравнение по префиксу) ----
POS_RU <- c("хорош","отличн","прекрасн","любл","любов","радост","рад","успе","надежд",
            "замечательн","великолепн","счаст","добр","восхит","лучш","удовольств","прият",
            "улыб","побед","достиж","вдохнов","благодар","нрав","восторг","уверен",
            "перспектив","рост","выгод","эффективн")
NEG_RU <- c("плох","ужасн","проблем","страх","потер","грусть","груст","ошиб","ненавист",
            "печал","разочаров","худш","боль","кризис","провал","сложн","трудн","опасн",
            "угроз","недостат","жаль","сожал","разруш","паден","убыт","конфликт","риск","сбой")
POS_EN <- c("good","great","excellent","love","joy","success","hope","wonderful","amazing",
            "happy","best","positive","win","growth","benefit","improve")
NEG_EN <- c("bad","terrible","problem","fear","loss","sad","error","hate","worst","pain",
            "crisis","fail","difficult","danger","negative","risk")

# Сильные слова — вес 3 во «взвешенном» режиме (подмножество словарей выше)
POS_STRONG_RU <- c("прекрасн","великолепн","восхит","восторг","счаст","любл","любов",
                   "успе","побед","замечательн","отличн","вдохнов")
NEG_STRONG_RU <- c("ужасн","ненавист","кризис","провал","разруш","угроз")
POS_STRONG_EN <- c("excellent","wonderful","amazing","love","best","success")
NEG_STRONG_EN <- c("terrible","hate","worst","crisis")

STOP_RU <- c("и","в","во","не","что","он","на","я","с","со","как","а","то","все","она","так",
             "его","но","да","ты","к","у","же","вы","за","бы","по","только","ее","мне","было",
             "вот","от","меня","еще","нет","о","из","ему","когда","даже","ну","ли","уже","или",
             "ни","быть","был","него","до","вас","там","потом","себя","ей","они","тут","где",
             "есть","для","мы","тебя","их","чем","была","без","чего","раз","себе","под","будет",
             "тогда","кто","этот","того","потому","этого","какой","ним","здесь","этом","один",
             "мой","тем","чтобы","нее","были","куда","всех","при","два","об","другой","после",
             "над","эти","нас","про","всего","них","эту","моя","свою","этой","перед","том",
             "такой","им","более","всю","между","это","её","также","однако","возможны")
STOP_EN <- c("the","a","an","and","or","but","is","are","was","were","of","to","in","on","for",
             "with","as","by","at","it","this","that","i","you","he","she","they","we","be",
             "have","has","had","do","not","no","so","if","then","than","too","very","can",
             "will","just")

# ---- Эмоциональный словарь (8 эмоций модели NRC) -------------------------
EMO_RU <- list(
  "радость"      = c("радост","рад","счаст","весел","ликов","восторг","восхищ","наслажд",
                     "удовольств","улыб","смех","праздник","любл","любов","прекрасн",
                     "замечательн","отличн","успе","побед","вдохнов"),
  "доверие"      = c("довер","надёжн","надежн","верност","преданн","честн","поддержк","опор",
                     "уважен","искрен","друж","союзник","спокой","уверен","благодар","забот"),
  "предвкушение" = c("ожида","предвкуш","надежд","планир","стремл","мечт","перспектив",
                     "будущ","нетерпен","готов"),
  "удивление"    = c("удивл","поражён","поражен","изумл","неожида","внезап","шок",
                     "невероятн","сюрприз","вдруг"),
  "печаль"       = c("печал","груст","тоск","уныл","скорб","одиночеств","потер","утрат",
                     "разочаров","жаль","сожал","слёз","слез","плач"),
  "страх"        = c("страх","страшн","боя","испуг","тревог","паник","ужас","опасен","опасн",
                     "угроз","беспоко","нервн","кошмар"),
  "гнев"         = c("гнев","зл","ярост","бешен","раздраж","негодован","возмущ","ненавист",
                     "агресс","конфликт","ссор","обид","разъяр"),
  "отвращение"   = c("отвращ","омерз","брезг","тошнот","гадк","мерзк","презрен","неприязн",
                     "противн")
)
EMO_EN <- list(
  "радость"      = c("joy","happy","glad","delight","pleasure","cheer","smile","laugh","love",
                     "wonderful","great","success","enjoy","fun","excit"),
  "доверие"      = c("trust","reliable","honest","faith","support","respect","sincere","secure",
                     "confident","grateful","loyal"),
  "предвкушение" = c("anticipat","expect","hope","plan","ready","eager","await","upcoming","soon"),
  "удивление"    = c("surprise","amaze","astonish","unexpected","sudden","shock","incredible","wow"),
  "печаль"       = c("sad","grief","sorrow","lonely","loss","lose","disappoint","regret","cry",
                     "tears","miserable","unhappy","gloom"),
  "страх"        = c("fear","afraid","scare","anxi","panic","terror","danger","threat","worry",
                     "nervous","nightmare","dread"),
  "гнев"         = c("anger","angry","rage","fury","furious","irritat","outrage","hate","aggress","annoy"),
  "отвращение"   = c("disgust","gross","nasty","revolt","awful","horrible","despise","repuls","sick")
)

POS_EMOTIONS <- c("радость","доверие","предвкушение","удивление")
NEG_EMOTIONS <- c("печаль","страх","гнев","отвращение")

# Опциональный русский NRC из CSV
NRC_RU_PATH <- "nrc_russian.csv"

translate_nrc_emotion <- function(x) {
  dplyr::recode(x,
                "anger" = "гнев", "anticipation" = "предвкушение", "disgust" = "отвращение",
                "fear" = "страх", "joy" = "радость", "sadness" = "печаль",
                "surprise" = "удивление", "trust" = "доверие")
}

load_nrc_ru_external <- function(path = NRC_RU_PATH) {
  empty <- tibble(word = character(), emotion = character())
  if (!file.exists(path)) return(empty)
  raw <- tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                    fileEncoding = "UTF-8"),
    error = function(e) NULL)
  if (is.null(raw) || nrow(raw) == 0) return(empty)
  
  emo_cols <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust")
  if (!("Russian (ru)" %in% names(raw)) || !all(emo_cols %in% names(raw))) return(empty)
  
  raw |>
    rename(word = `Russian (ru)`) |>
    select(word, all_of(emo_cols)) |>
    mutate(word = tolower(as.character(word))) |>
    filter(!is.na(word), nchar(word) > 1) |>
    pivot_longer(cols = all_of(emo_cols), names_to = "emotion", values_to = "assoc") |>
    filter(assoc == 1) |>
    mutate(emotion = translate_nrc_emotion(emotion)) |>
    distinct(word, emotion)
}

NRC_RU_EXTERNAL <- load_nrc_ru_external()

# Итоговый эмоциональный словарь: внешний NRC (если есть) или встроенный.
get_emo_lex <- function(is_en) {
  if (!is_en && nrow(NRC_RU_EXTERNAL) > 0) {
    split(NRC_RU_EXTERNAL$word, NRC_RU_EXTERNAL$emotion)
  } else if (is_en) {
    EMO_EN
  } else {
    EMO_RU
  }
}

# ---- Анализ текста -------------------------------------------------------
# lexicon: "binary" (±1), "weighted" (−3…+3), "nrc" (полярность по эмоциям).
analyze_text <- function(text, lexicon = "binary", rm_stop = TRUE,
                         to_lower = FALSE, lang = "Русский") {
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  
  raw <- if (to_lower) tolower(text) else text
  tokens <- unlist(str_extract_all(raw, "[\\p{L}]+"))
  tokens <- tokens[nchar(tokens) > 1]
  if (length(tokens) == 0) return(NULL)
  
  is_en   <- lang == "English"
  emo_lex <- get_emo_lex(is_en)
  stop_w  <- if (is_en) STOP_EN else STOP_RU
  
  tok_lc      <- tolower(tokens)
  total_words <- length(tokens)
  uniq_words  <- length(unique(tok_lc))
  
  if (rm_stop) {
    keep   <- !(tok_lc %in% stop_w)
    tokens <- tokens[keep]; tok_lc <- tok_lc[keep]
  }
  if (length(tok_lc) == 0) return(NULL)
  
  # Наборы основ для определения полярности зависят от выбранного словаря
  if (lexicon == "nrc") {
    pos_set <- unlist(emo_lex[POS_EMOTIONS], use.names = FALSE)
    neg_set <- unlist(emo_lex[NEG_EMOTIONS], use.names = FALSE)
  } else {
    pos_set <- if (is_en) POS_EN else POS_RU
    neg_set <- if (is_en) NEG_EN else NEG_RU
  }
  pos_strong <- if (is_en) POS_STRONG_EN else POS_STRONG_RU
  neg_strong <- if (is_en) NEG_STRONG_EN else NEG_STRONG_RU
  
  # Полярность и знаковый вес каждого токена
  cls <- lapply(tok_lc, function(t) {
    is_pos <- any(startsWith(t, pos_set))
    is_neg <- any(startsWith(t, neg_set))
    if (is_pos && !is_neg) {
      w <- if (lexicon == "weighted" && any(startsWith(t, pos_strong))) 3 else 1
      list(pol = "pos", val = w)
    } else if (is_neg && !is_pos) {
      w <- if (lexicon == "weighted" && any(startsWith(t, neg_strong))) 3 else 1
      list(pol = "neg", val = -w)
    } else {
      list(pol = "neu", val = 0)
    }
  })
  polarity <- vapply(cls, `[[`, character(1), "pol")
  value    <- vapply(cls, `[[`, numeric(1),   "val")
  
  n_pos <- sum(polarity == "pos")
  n_neg <- sum(polarity == "neg")
  n_ton <- max(n_pos + n_neg, 1)
  
  index <- if (lexicon == "weighted") sum(value) / (3 * n_ton)
  else (n_pos - n_neg) / n_ton
  
  label <- if (index >  0.05) "Позитивная"
  else if (index < -0.05) "Негативная"
  else "Нейтральная"
  label_color <- if (index > 0.05) PAL$pos
  else if (index < -0.05) PAL$neg
  else PAL$muted
  
  # Динамика: знаковый вес по блокам текста (до 10 блоков)
  nb  <- min(10L, length(value))
  grp <- pmin(ceiling(seq_along(value) / (length(value) / nb)), nb)
  dynamics <- tibble(
    block = seq_len(nb),
    net   = as.numeric(tapply(value, factor(grp, levels = seq_len(nb)), sum))
  )
  dynamics$net[is.na(dynamics$net)] <- 0
  
  # Топ слов по вкладу в тональность
  top_words <- tibble(word = tok_lc, pol = polarity) |>
    filter(pol != "neu") |>
    count(word, pol, name = "n") |>
    group_by(pol) |>
    slice_max(n, n = 6, with_ties = FALSE) |>
    ungroup()
  
  # Распределение эмоций: число слов, отнесённых к каждой из 8 эмоций
  emo_counts <- vapply(emo_lex, function(stems)
    sum(vapply(tok_lc, function(t) any(startsWith(t, stems)), logical(1))),
    integer(1))
  emotions <- tibble(emotion = names(emo_counts), value = as.numeric(emo_counts))
  
  list(
    lexicon = lexicon, label = label, label_color = label_color, index = index,
    total_words = total_words, uniq_words = uniq_words,
    n_pos = n_pos, n_neg = n_neg,
    dynamics = dynamics, top_words = top_words, emotions = emotions
  )
}

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
      axis.title  = element_text(colour = PAL$muted, size = 11),
      axis.text   = element_text(colour = PAL$muted)
    )
}

stat_card <- function(label, value, sub, accent) {
  div(class = "stat-card", style = paste0("--accent:", accent, ";"),
      div(class = "stat-label", label),
      div(class = "stat-value", value),
      div(class = "stat-sub", sub))
}

panel_head <- function(title, sub = NULL, hint = NULL) {
  tagList(
    div(class = "panel-title",
        span(title),
        if (!is.null(hint))
          tooltip(span(class = "help-badge", "?"), hint, placement = "top")),
    if (!is.null(sub)) div(class = "panel-sub", sub)
  )
}

read_uploaded_text <- function(file) {
  ext <- tolower(tools::file_ext(file$name))
  if (ext == "csv") {
    tab <- tryCatch(
      utils::read.csv(file$datapath, stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8", check.names = FALSE),
      error = function(e) NULL)
    if (is.null(tab)) return(NULL)
    char_cols <- tab[, vapply(tab, is.character, logical(1)), drop = FALSE]
    if (ncol(char_cols) == 0) return(paste(unlist(tab), collapse = " "))
    return(paste(unlist(char_cols), collapse = " "))
  }
  tryCatch(
    paste(readLines(file$datapath, warn = FALSE, encoding = "UTF-8"), collapse = " "),
    error = function(e) NULL)
}

app_theme <- bs_theme(
  version = 5,
  bg = "#eef0f2", fg = PAL$ink, primary = PAL$pos,
  "border-radius" = "10px"
)

ui <- page_fluid(
  theme = app_theme,
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap"),
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
      .stat-value { font-size:29px; font-weight:800; line-height:1.15; margin:7px 0 3px; }
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
      .foot-links { display:flex; align-items:center; gap:14px; }
      .foot-repo { background:#171a21; color:#fff !important; border-radius:7px;
        padding:9px 14px; font-size:11.5px; font-weight:600; text-decoration:none;
        text-transform:uppercase; letter-spacing:.06em; }
      .foot-pub { border:1px solid #e3e6ea; border-radius:7px; padding:9px 14px;
        font-size:11.5px; color:#9aa3af; }
    "))
  ),
  
  useShinyjs(),
  
  div(class = "app-header",
      div(class = "brand",
          div(class = "brand-mark", span()),
          "Анализ эмоциональной тональности")
  ),
  
  div(class = "body-wrap",
      
      div(class = "about-card",
          div(class = "panel-title", "О приложении"),
          div(class = "about-text", HTML(
            "Приложение выполняет <b>словарный анализ эмоциональной тональности</b> текста
             на русском и английском языках. Введите текст или загрузите файл (.txt/.csv),
             выберите словарь и язык, при необходимости уберите стоп-слова — и получите
             общую тональность, динамику по блокам текста, распределение восьми эмоций и
             топ слов, влияющих на оценку.<br>
             Метод основан на сопоставлении слов со словарями основ:
             <ul><li><b>Бинарный</b> — каждое тональное слово ±1;</li>
             <li><b>Взвешенный</b> — учитывает силу слова (−3…+3);</li>
             <li><b>Эмоциональный (NRC)</b> — распределяет слова по восьми эмоциям и по ним
             определяет полярность.</li></ul>"
          )),
          div(class = "about-text", style = "margin-top:8px;", HTML(
            "Наведите курсор на значок <b>?</b> рядом с заголовком графика — появится
             краткое пояснение, что показывает диаграмма."
          )),
          div(class = "about-text", style = "margin-top:10px;", HTML(
            "<b>Преимущество нейросетевого подхода.</b> Точнее всего тональность определяет
             трансформерная модель (например, <i>rubert-tiny2-russian-sentiment</i>): она
             оценивает предложение целиком и учитывает контекст, порядок слов, отрицания и
             сарказм. Словарь же видит только отдельные слова: фразу «я вообще не рад» он по
             слову «рад» относит к позитивным, тогда как нейросеть прочитала бы её как
             негативную. В этой версии нейросеть не используется — она требует Python и не
             укладывается в бесплатный тариф shinyapps.io, — но с ней результат был бы заметно лучше."
          ))
      ),
      
      layout_sidebar(
        border = FALSE, fillable = FALSE,
        sidebar = sidebar(
          width = 320, padding = 18,
          div(class = "sidebar-title", "Параметры анализа"),
          
          div(class = "ctrl-label", "Введите текст для анализа"),
          textAreaInput("text", NULL, rows = 6,
                        placeholder = "Вставьте текст или абзац...", width = "100%"),
          uiOutput("word_hint"),
          
          div(class = "ctrl-label", "…или загрузите файл (.txt / .csv)"),
          fileInput("file", NULL, accept = c(".txt", ".csv"),
                    buttonLabel = "Обзор…", placeholder = "Файл не выбран"),
          
          div(class = "ctrl-label", "Выберите словарь"),
          selectInput("lexicon", NULL, width = "100%",
                      choices = c("Бинарный (±1 за слово)"        = "binary",
                                  "Взвешенный (−3…+3)"            = "weighted",
                                  "Эмоциональный, NRC (8 эмоций)" = "nrc")),
          div(class = "ctrl-note", "Словари на основах слов (рус./англ.)."),
          
          div(style = "margin-top:14px;",
              checkboxInput("rm_stop",  "Удалять стоп-слова", TRUE),
              checkboxInput("to_lower", "Приводить к нижнему регистру", FALSE)),
          
          div(class = "ctrl-label", "Язык текста"),
          radioButtons("lang", NULL, inline = TRUE,
                       choices = c("Русский", "English")),
          
          actionButton("analyze", "Анализировать", class = "btn-analyze")
        ),
        
        uiOutput("stat_row"),
        
        div(style = "margin-top:18px;",
            layout_columns(
              col_widths = c(6, 6),
              card(class = "panel-card",
                   panel_head("Динамика тональности по тексту",
                              "сумма тональности по блокам",
                              hint = "Текст делится на блоки по порядку слов; высота столбца — сумма тональности слов в блоке (синий — позитив, красный — негатив). Показывает, как настроение меняется по ходу текста."),
                   plotOutput("dyn_plot", height = 250)),
              card(class = "panel-card",
                   panel_head("Распределение эмоций",
                              "доступно при выборе эмоционального словаря (NRC)",
                              hint = "Сколько слов текста относится к каждой из восьми эмоций модели NRC. Работает в режиме «Эмоциональный (NRC)»."),
                   plotOutput("emo_plot", height = 250))
            )
        ),
        
        div(style = "margin-top:18px;",
            card(class = "panel-card",
                 panel_head("Топ слов по вкладу в тональность",
                            hint = "Самые частые позитивные (синие) и негативные (красные) слова текста. Длина столбца — частота слова."),
                 plotOutput("top_plot", height = 300))
        )
      )
  ),
  
  div(class = "app-footer",
      div(
        div(class = "foot-head", "Команда six seven"),
        div(class = "foot-sub", "Назаровская Вероника · Безрукова Анастасия · Итыгилова Юмжит"),
        div(class = "foot-sub", "НИУ ВШЭ · Компьютерный анализ текста")),
      div(class = "foot-links",
          tags$a(class = "foot-repo", href = GITHUB_URL, target = "_blank",
                 "Репозиторий на GitHub"),
          div(class = "foot-pub", "Опубликовано: shinyapps.io"))
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$file, {
    req(input$file)
    txt <- read_uploaded_text(input$file)
    if (!is.null(txt) && nzchar(trimws(txt)))
      updateTextAreaInput(session, "text", value = txt)
  })
  
  word_count <- reactive({
    length(unlist(str_extract_all(input$text %||% "", "[\\p{L}]+")))
  })
  
  observe({
    toggleState("analyze", condition = word_count() >= MIN_WORDS)
  })
  
  output$word_hint <- renderUI({
    n  <- word_count()
    ok <- n >= MIN_WORDS
    col <- if (ok) "#2f8f5b" else PAL$muted
    div(style = paste0("font-size:11.5px; margin-top:4px; color:", col, ";"),
        if (ok) paste0("Слов: ", n, " — можно анализировать")
        else    paste0("Слов: ", n, " из ", MIN_WORDS, " (минимум для анализа)"))
  })
  
  res <- reactive({
    analyze_text(input$text, input$lexicon, input$rm_stop,
                 input$to_lower, input$lang)
  }) |> bindEvent(input$analyze)
  
  output$stat_row <- renderUI({
    r <- res()
    if (is.null(r)) {
      cards <- list(
        stat_card("Общая тональность", "—", "нажмите «Анализировать»", PAL$muted),
        stat_card("Всего слов", "—", "", PAL$ink),
        stat_card("Позитивных слов", "—", "", PAL$pos),
        stat_card("Негативных слов", "—", "", PAL$neg))
    } else {
      pct <- function(n) round(n / r$total_words * 100, 1)
      cards <- list(
        stat_card("Общая тональность",
                  span(style = paste0("color:", r$label_color, ";"), r$label),
                  paste0("индекс ", sprintf("%+.2f", r$index)), r$label_color),
        stat_card("Всего слов", format(r$total_words, big.mark = " "),
                  paste0("уникальных: ", r$uniq_words), PAL$ink),
        stat_card("Позитивных слов", r$n_pos,
                  paste0(pct(r$n_pos), "% от текста"), PAL$pos),
        stat_card("Негативных слов", r$n_neg,
                  paste0(pct(r$n_neg), "% от текста"), PAL$neg))
    }
    do.call(layout_columns, c(list(col_widths = c(3, 3, 3, 3)), cards))
  })
  
  output$dyn_plot <- renderPlot({
    r <- res(); if (is.null(r)) return(empty_plot())
    ggplot(r$dynamics, aes(block, net, fill = net >= 0)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 0, colour = "#cfd6dd") +
      scale_fill_manual(values = c("TRUE" = PAL$pos, "FALSE" = PAL$neg), guide = "none") +
      scale_x_continuous(breaks = r$dynamics$block,
                         labels = paste0("Блок ", r$dynamics$block)) +
      labs(x = NULL, y = "тональность блока") +
      base_theme() +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(size = 9, angle = 0))
  })
  
  output$emo_plot <- renderPlot({
    r <- res(); if (is.null(r)) return(empty_plot())
    if (r$lexicon != "nrc")
      return(empty_plot("Доступно при выборе эмоционального словаря (NRC)"))
    e <- dplyr::filter(r$emotions, value > 0)
    if (nrow(e) == 0) return(empty_plot("Эмоциональные слова не найдены"))
    ggplot(e, aes(value, reorder(emotion, value), fill = emotion)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      scale_fill_manual(values = EMO_COLORS) +
      labs(x = "число слов", y = NULL) +
      base_theme() +
      theme(panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 12, colour = PAL$ink))
  })
  
  output$top_plot <- renderPlot({
    r <- res(); if (is.null(r)) return(empty_plot())
    tw <- r$top_words
    if (nrow(tw) == 0) return(empty_plot("Тональные слова не найдены"))
    tw$pol <- factor(tw$pol, c("pos", "neg"), c("ПОЗИТИВНЫЕ", "НЕГАТИВНЫЕ"))
    ggplot(tw, aes(n, reorder(word, n), fill = pol)) +
      geom_col(width = 0.6, show.legend = FALSE) +
      facet_wrap(~pol, scales = "free_y", ncol = 2) +
      scale_fill_manual(values = c("ПОЗИТИВНЫЕ" = PAL$pos, "НЕГАТИВНЫЕ" = PAL$neg)) +
      labs(x = NULL, y = NULL) +
      base_theme() +
      theme(panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 12, colour = PAL$ink),
            strip.text  = element_text(face = "bold", hjust = 0,
                                       colour = PAL$muted, size = 12))
  })
}

shinyApp(ui, server)
