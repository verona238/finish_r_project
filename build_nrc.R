# Скрипт готовит словарь nrc_russian.csv для приложения из официального NRC.
# Запускается 1 раз, кладет CSV в папку проекта рядом с app.R
# Приложение читает широкий CSV (Russian (ru) + 8 колонок эмоций в нижнем
# регистре) и сопоставляет слова по основам через startsWith()
# Поэтому слова здесь приводятся к основам SnowballC

#install.packages(c("readr", "dplyr", "stringr", "SnowballC"))

library(readr)
library(dplyr)
library(stringr)
library(SnowballC)

# Официальный файл NRC с переводами
nrc_source <- "NRC-Emotion-Lexicon-ForVariousLanguages.txt"

# Читаем сначала как TSV, при неудаче — как CSV.
nrc_raw <- suppressWarnings(read_tsv(nrc_source, show_col_types = FALSE,
                                     guess_max = 100000))
if (ncol(nrc_raw) < 3) {
  nrc_raw <- read_csv(nrc_source, show_col_types = FALSE, guess_max = 100000)
}

nm <- names(nrc_raw)

# Находим колонку с русским переводом
ru_col <- nm[str_detect(str_to_lower(nm), "russian")][1]
if (is.na(ru_col)) stop("Не нашел колонку с русским переводом (Russian).")

# Находим 8 колонок эмоций без учета регистра
want <- c("anger", "anticipation", "disgust", "fear",
          "joy", "sadness", "surprise", "trust")
emo_src <- vapply(want, function(w) {
  hit <- nm[str_to_lower(nm) == w]
  if (length(hit) == 0) NA_character_ else hit[1]
}, character(1))
if (any(is.na(emo_src))) {
  stop("Не нашел колонки эмоций: ",
       paste(want[is.na(emo_src)], collapse = ", "))
}

# Берем русское слово + флаги эмоций и приводим имена к нижнему регистру
df <- nrc_raw[, c(ru_col, unname(emo_src))]
names(df) <- c("word", want)

nrc_russian <- df |>
  mutate(
    word = str_to_lower(word),
    across(all_of(want), ~ coalesce(suppressWarnings(as.integer(.x)), 0L))
  ) |>
  filter(!is.na(word), nchar(word) > 1) |>
  # приводим к основам, чтобы приложение ловило словоформы по префиксу
  mutate(word = wordStem(word, language = "russian")) |>
  filter(nchar(word) > 1) |>
  # одна основа может прийти из нескольких слов — объединяем флаги
  group_by(word) |>
  summarise(across(all_of(want), ~ as.integer(max(.x))), .groups = "drop") |>
  # оставляем основы хотя бы с одной эмоцией
  filter(rowSums(across(all_of(want))) > 0) |>
  # формат, который читает приложение (нижний регистр колонок)
  transmute(
    `English (en)` = NA_character_,
    `Russian (ru)` = word,
    anger, anticipation, disgust, fear, joy, sadness, surprise, trust
  )

write_csv(nrc_russian, "nrc_russian.csv")