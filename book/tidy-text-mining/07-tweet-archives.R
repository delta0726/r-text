# ***********************************************************************************************
# Title   : Rによるテキストマイニング
# Chapter : 7 ケーススタディ：Twitterアーカイブの比較
# Date    : 2022/08/10
# Page    : P129 - P148
# URL     : https://github.com/dgrtwo/tidy-text-mining
# ***********************************************************************************************


# ＜概要＞
# - ツイッターのデータで文字数カウントを中心として分析を行う


# ＜目次＞
# 0 準備
# 1 データ準備
# 2 単語の出現頻度
# 3 使用している単語の比較
# 4 使用している単語の変化
# 5 いいねとリツイート


# 0 準備 -----------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidytext)
library(lubridate)
library(stringr)
library(scales)
library(broom)


# データロード
tweets_julia <- read_csv("data/tweets_julia.csv")
tweets_dave <- read_csv("data/tweets_dave.csv")


# 1 データ準備 -----------------------------------------------------------------

# データ結合
tweets <-
  tweets_julia %>%
    mutate(person = "Julia") %>%
    bind_rows(mutate(tweets_dave, person = "David")) %>%
    mutate(timestamp = ymd_hms(timestamp))

# プロット作成
# --- ヒストグラム
tweets %>%
  ggplot(aes(x = timestamp, fill = person)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  facet_wrap(~person, ncol = 1)


# 2 単語の出現頻度 ----------------------------------------------------------

# 整理データの作成
# --- 正規表現で不要な文字を削除
# --- ストップワードを削除
tidy_tweets <- 
  tweets %>%
    filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_remove_all(text, "&amp;|&lt;|&gt;")) %>%
    unnest_tokens(word, text, token = "tweets") %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words$word, "'"),
           str_detect(word, "[a-z]"))

# 全レコードのカウント
# --- personごと
total_tweets <-
  tidy_tweets %>%
    group_by(person) %>%
    summarise(total = n()) %>%
    ungroup()

# 文字数カウント
frequency <-
  tidy_tweets %>%
    group_by(person) %>%
    count(word, sort = TRUE) %>%
    left_join(total_tweets, by = "person") %>%
    mutate(freq = n / total)

# データ確認
frequency %>% print()

# データ変換
# --- ワイド型
frequency <-
  frequency %>%
    select(person, word, freq) %>%
    spread(person, freq) %>%
    arrange(Julia, David)

# プロット作成
# --- 散布図（Julia vs David）
# --- 45度線上の単語は使用頻度が等しいことを示す
frequency %>%
  ggplot(aes(x = Julia, y = David)) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
    geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    geom_abline(color = "red")


# 3 使用している単語の比較 ----------------------------------------------------------

# データ抽出
tidy_tweets <-
  tidy_tweets %>%
    filter(timestamp >= as.Date("2016-01-01"),
           timestamp < as.Date("2017-01-01"))

# 対数オッズ比の計算
# --- David vs Julia
word_ratios <-
  tidy_tweets %>%
    filter(!str_detect(word, "^@")) %>%
    count(word, person) %>%
    group_by(word) %>%
    filter(sum(n) >= 10) %>%
    ungroup() %>%
    spread(person, n, fill = 0) %>%
    mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
    mutate(logratio = log(David / Julia)) %>%
    arrange(desc(logratio))

# プロット作成
# --- 対数オッズ比が正/負で分けて上位ワードを抽出
word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (David/Julia)") +
  scale_fill_discrete(name = "", labels = c("David", "Julia"))


# 4 使用している単語の変化 ------------------------------------------------------

# データ集計
# --- 日ごとに文字数カウントを集計
words_by_time <-
  tidy_tweets %>%
    filter(!str_detect(word, "^@")) %>%
    mutate(time_floor = floor_date(timestamp, unit = "1 month")) %>%
    count(time_floor, person, word) %>%
    group_by(person, time_floor) %>%
    mutate(time_total = sum(n)) %>%
    group_by(person, word) %>%
    mutate(word_total = sum(n)) %>%
    ungroup() %>%
    rename(count = n) %>%
    filter(word_total > 30)

# ネストデータの作成
nested_data <-
  words_by_time %>%
    group_by(word, person) %>%
    nest()

# モデル構築
# --- ネストごとに一般化線形モデルを構築
nested_models <-
  nested_data %>%
    mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, .,
                                    family = "binomial")))

# データ確認
nested_models %>% print()

# 回帰係数の抽出
slopes <-
  nested_models %>%
    mutate(models = map(models, tidy)) %>%
    unnest(cols = "models") %>%
    filter(term == "time_floor") %>%
    mutate(adjusted.p.value = p.adjust(p.value))

# データ抽出
# --- 統計的有意なレコードのみを抽出
top_slopes <- slopes %>% filter(adjusted.p.value < 0.05)
top_slopes %>% print()

# プロット作成
# --- 高頻度単語の時系列推移
# --- 上からDavid, Julia
words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "David") %>%
  ggplot(aes(x = time_floor, y = count / time_total, color = word)) +
  geom_line(size = 1.3) +
  facet_grid(~person) +
  labs(x = NULL, y = "Word frequency")

words_by_time %>%
  inner_join(top_slopes, by = c("word", "person")) %>%
  filter(person == "Julia") %>%
  ggplot(aes(x = time_floor, y = count/time_total, color = word)) +
  geom_line(size = 1.3) +
  facet_grid(~person) +
  labs(x = NULL, y = "Word frequency")


# 5 いいねとリツイート -------------------------------------------------------------

# データ不整合のため省略
