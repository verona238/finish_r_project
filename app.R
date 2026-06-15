library(shiny)
library(bslib)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(stringr)
library(tibble)

MIN_WORDS <- 5  # минимум слов, при котором кнопка анализа становится активной

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

#Встроенный мини-лексикон
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

# Эмоциональный словарь (8 эмоций модели NRC). Хранятся основы слов —
# сопоставление по префиксу, чтобы покрывать словоформы. Ключи совпадают
# с EMO_COLORS, чтобы цвета и подписи были согласованы.
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

analyze_text <- function(text, lexicon = "Bing", rm_stop = TRUE,
                         to_lower = FALSE, lang = "Русский") {
  if (is.null(text) || !nzchar(trimws(text))) return(NULL)
  
  raw <- if (to_lower) tolower(text) else text
  tokens <- unlist(str_extract_all(raw, "[\\p{L}]+"))
  tokens <- tokens[nchar(tokens) > 1]
  if (length(tokens) == 0) return(NULL)
  
  pos_lex <- if (lang == "English") POS_EN else POS_RU
  neg_lex <- if (lang == "English") NEG_EN else NEG_RU
  stop_w  <- if (lang == "English") STOP_EN else STOP_RU
  
  tok_lc      <- tolower(tokens)
  total_words <- length(tokens)
  uniq_words  <- length(unique(tok_lc))
  
  if (rm_stop) {
    keep   <- !(tok_lc %in% stop_w)
    tokens <- tokens[keep]; tok_lc <- tok_lc[keep]
  }
  if (length(tok_lc) == 0) return(NULL)
  
  polarity <- vapply(tok_lc, function(t) {
    if (any(startsWith(t, pos_lex)))      "pos"
    else if (any(startsWith(t, neg_lex))) "neg"
    else                                  "neu"
  }, character(1), USE.NAMES = FALSE)
  
  n_pos <- sum(polarity == "pos")
  n_neg <- sum(polarity == "neg")
  n_neu <- sum(polarity == "neu")
  index <- (n_pos - n_neg) / max(n_pos + n_neg, 1)
  
  label <- if (index >  0.05) "Позитивная"
  else if (index < -0.05) "Негативная"
  else "Нейтральная"
  label_color <- if (index > 0.05) PAL$pos
  else if (index < -0.05) PAL$neg
  else PAL$muted
  
  # Динамика
  score <- ifelse(polarity == "pos", 1L, ifelse(polarity == "neg", -1L, 0L))
  nb    <- min(10L, length(score))
  grp   <- pmin(ceiling(seq_along(score) / (length(score) / nb)), nb)
  dynamics <- tibble(
    block = seq_len(nb),
    net   = as.numeric(tapply(score, factor(grp, levels = seq_len(nb)), sum))
  )
  dynamics$net[is.na(dynamics$net)] <- 0
  
  # Топ слов по вкладу в тональность
  top_words <- tibble(word = tok_lc, pol = polarity) |>
    filter(pol != "neu") |>
    count(word, pol, name = "n") |>
    group_by(pol) |>
    slice_max(n, n = 6, with_ties = FALSE) |>
    ungroup()
  
  # Распределение эмоций: число слов текста, отнесённых к каждой из 8 эмоций.
  # Слово может относиться к нескольким эмоциям (как и в модели NRC).
  emo_lex <- if (lang == "English") EMO_EN else EMO_RU
  emo_counts <- vapply(emo_lex, function(stems)
    sum(vapply(tok_lc, function(t) any(startsWith(t, stems)), logical(1))),
    integer(1))
  emotions <- tibble(emotion = names(emo_counts),
                     value   = as.numeric(emo_counts))
  
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

panel_head <- function(title, sub = NULL) {
  tagList(
    div(class = "panel-title", title),
    if (!is.null(sub)) div(class = "panel-sub", sub)
  )
}

app_theme <- bs_theme(
  version = 5,
  bg = "#eef0f2", fg = PAL$ink, primary = PAL$pos,
  "border-radius" = "10px"
)

GITHUB_URL <- "https://github.com/your-team/sentiment-app"

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

      /* Шапка */
      .app-header { background:#171a21; color:#fff; padding:14px 28px;
        display:flex; align-items:center; justify-content:space-between; }
      .app-header .brand { display:flex; align-items:center; gap:12px;
        font-weight:800; letter-spacing:.09em; font-size:13.5px; text-transform:uppercase; }
      .brand-mark { width:22px; height:22px; border:2px solid #fff; border-radius:50%;
        display:flex; align-items:center; justify-content:center; }
      .brand-mark span { width:7px; height:7px; background:#fff; border-radius:50%; }

      /* Основная область */
      .body-wrap { padding:22px 28px; }

      /* Сайдбар */
      .bslib-sidebar-layout > .sidebar { background:#fff; border:1px solid #e3e6ea;
        border-radius:10px; }
      .sidebar-title { font-size:17px; font-weight:700; margin:2px 0 14px;
        padding-bottom:12px; border-bottom:1px solid #eef0f2; }
      .ctrl-label { font-size:12.5px; font-weight:600; color:#46505c;
        margin:14px 0 6px; }
      .ctrl-note { font-size:11px; color:#aab1bb; margin-top:4px; }
      .btn-analyze { background:#171a21; color:#fff; width:100%; border:none;
        padding:12px; font-weight:700; letter-spacing:.07em; text-transform:uppercase;
        font-size:12.5px; border-radius:8px; margin-top:18px; }
      .btn-analyze:hover { background:#262b35; color:#fff; }

      /* Карточки-метрики */
      .stat-card { background:#fff; border:1px solid #e3e6ea;
        border-left:4px solid var(--accent,#14538a); border-radius:8px;
        padding:14px 16px; height:100%; }
      .stat-label { font-size:10px; letter-spacing:.1em; text-transform:uppercase;
        color:#8a93a0; font-weight:600; }
      .stat-value { font-size:29px; font-weight:800; line-height:1.15; margin:7px 0 3px; }
      .stat-sub { font-size:11px; color:#9aa3af; }

      /* Панели с графиками */
      .panel-card { background:#fff; border:1px solid #e3e6ea; border-radius:10px;
        padding:18px 20px; height:100%; }
      .panel-card .card-body { padding:0; }
      .panel-title { font-size:16px; font-weight:700; color:#1c2733; }
      .panel-sub { font-size:11.5px; color:#9aa3af; margin:2px 0 12px; }

      /* Подвал */
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
  
  # Шапка
  div(class = "app-header",
      div(class = "brand",
          div(class = "brand-mark", span()),
          "Анализ эмоциональной тональности")
  ),
  
  # Тело
  div(class = "body-wrap",
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
          
          div(class = "ctrl-label", "Выберите лексикон"),
          selectInput("lexicon", NULL, width = "100%",
                      choices = c("Bing (positive / negative)" = "Bing",
                                  "AFINN" = "AFINN", "NRC" = "NRC",
                                  "Loughran" = "Loughran")),
          div(class = "ctrl-note", "Доступно: Bing · AFINN · NRC · Loughran"),
          
          div(style = "margin-top:14px;",
              checkboxInput("rm_stop",  "Удалять стоп-слова", TRUE),
              checkboxInput("to_lower", "Приводить к нижнему регистру", FALSE)),
          
          div(class = "ctrl-label", "Язык текста"),
          radioButtons("lang", NULL, inline = TRUE,
                       choices = c("Русский", "English")),
          
          actionButton("analyze", "Анализировать", class = "btn-analyze")
        ),
        
        # Метрики
        uiOutput("stat_row"),
        
        # Динамика + эмоции
        div(style = "margin-top:18px;",
            layout_columns(
              col_widths = c(6, 6),
              card(class = "panel-card",
                   panel_head("Динамика тональности по тексту",
                              "net sentiment по блокам"),
                   plotOutput("dyn_plot", height = 250)),
              card(class = "panel-card",
                   panel_head("Распределение эмоций",
                              "доступно при выборе лексикона NRC"),
                   plotOutput("emo_plot", height = 250))
            )
        ),
        
        # Топ слов
        div(style = "margin-top:18px;",
            card(class = "panel-card",
                 panel_head("Топ слов по вкладу в тональность"),
                 plotOutput("top_plot", height = 300))
        )
      )
  ),
  
  # Подвал
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
  
  # Загрузка файла подставляется в поле ввода
  observeEvent(input$file, {
    req(input$file)
    txt <- tryCatch(
      paste(readLines(input$file$datapath, warn = FALSE, encoding = "UTF-8"),
            collapse = " "),
      error = function(e) NULL)
    if (!is.null(txt)) updateTextAreaInput(session, "text", value = txt)
  })
  
  # Подсчёт слов в текущем вводе
  word_count <- reactive({
    length(unlist(str_extract_all(input$text %||% "", "[\\p{L}]+")))
  })
  
  # Кнопка анализа активна только при достаточном числе слов
  observe({
    toggleState("analyze", condition = word_count() >= MIN_WORDS)
  })
  
  # Подсказка-счётчик под полем ввода
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
  
  # Динамика тональности
  output$dyn_plot <- renderPlot({
    r <- res(); if (is.null(r)) return(empty_plot())
    ggplot(r$dynamics, aes(block, net, fill = net >= 0)) +
      geom_col(width = 0.7) +
      geom_hline(yintercept = 0, colour = "#cfd6dd") +
      scale_fill_manual(values = c("TRUE" = PAL$pos, "FALSE" = PAL$neg),
                        guide = "none") +
      scale_x_continuous(breaks = r$dynamics$block,
                         labels = paste0("Блок ", r$dynamics$block)) +
      labs(x = NULL, y = "net sentiment") +
      base_theme() +
      theme(panel.grid.major.x = element_blank(),
            axis.text.x = element_text(size = 9, angle = 0))
  })
  
  # Распределение эмоций (NRC)
  output$emo_plot <- renderPlot({
    r <- res(); if (is.null(r)) return(empty_plot())
    if (r$lexicon != "NRC")
      return(empty_plot("Доступно при выборе лексикона NRC"))
    e <- dplyr::filter(r$emotions, value > 0)
    if (nrow(e) == 0) return(empty_plot("Тональные слова не найдены"))
    ggplot(e, aes(value, reorder(emotion, value), fill = emotion)) +
      geom_col(width = 0.65, show.legend = FALSE) +
      scale_fill_manual(values = EMO_COLORS) +
      labs(x = NULL, y = NULL) +
      base_theme() +
      theme(panel.grid.major.y = element_blank(),
            axis.text.y = element_text(size = 12, colour = PAL$ink))
  })
  
  # Топ слов
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