# ***********************************************************************************************
# Title   : Rによるテキストマイニング
# Chapter : 3 if-idf
# Date    : 2022/08/01
# Page    : P35 - P51
# URL     : https://github.com/dgrtwo/tidy-text-mining
# ***********************************************************************************************


# ＜概要＞
# - tf-idfとはtfとidfの積を計算することで、普段あまり使われない単語の頻度をを協調する手法
#   --- tfとは文書内での単語の出現頻度の観点における重要度（ストップワードを除去）
#   --- idfとは一般的にはあまり出現しない単語を出現する単語よりも重視する重要度


# ＜目次＞
# 0 準備
# 1 単語出現頻度の確認
# 2 ジップの法則
# 3 bind_tf_idf関数
# 4 物理学書のコーパス


# 0 準備 ------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(gutenbergr)
library(stringr)


# 1 単語出現頻度の確認 ------------------------------------------------------

# トークンのカウント
book_words <-
  austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word, sort = TRUE)

# データ集計
# --- Bookごとの総文字数
total_words <-
  book_words %>%
    group_by(book) %>%
    summarize(total = sum(n))

# データ結合
# --- トークンのカウントにBookごとの総文字数を結合
book_words <-
  book_words %>%
    left_join(total_words) %>%
    mutate(tf = n / total)

# 確認
book_words %>% print()

# プロット作成
# --- 特定の単語が極端に多く、多くの単語の頻度は少ない
# --- 一般的にロングテールになる
book_words %>%
  ggplot(aes(x = tf, fill = book)) +
    geom_histogram(bins = 30, show.legend = FALSE) +
    xlim(NA, 0.0009) +
    facet_wrap(~book, ncol = 2, scales = "free_y")


# 2 ジップの法則 -------------------------------------------------------------

# TFの算出
freq_by_rank <-
  book_words %>%
    group_by(book) %>%
    mutate(rank = row_number(),
           tf = n / total) %>%
    ungroup()

# 確認
freq_by_rank %>% print()

# プロット作成
# --- 単語出現順位とtfの関係は反比例となる（ジップの法則）
# --- X軸とY軸ともに対数スケール
freq_by_rank %>%
  ggplot(aes(x = rank, y = tf, color = book)) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()

# データ抽出
# --- ランクの中間部分
rank_subset <-
  freq_by_rank %>%
    filter(between(rank, 10, 500))

# 線形回帰
# --- 傾きが-1に近くなる
lm(log10(tf) ~ log10(rank), data = rank_subset)

# プロット作成
# --- 線形回帰の結果を重ね書き
freq_by_rank %>%
  ggplot(aes(x = rank, y = tf, color = book)) +
  geom_abline(intercept = -0.62, slope = -1.1,
              color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) +
  scale_x_log10() +
  scale_y_log10()


# 3 bind_tf_idf関数 -----------------------------------------------------------

# TF-IDFの計算
# --- 極端に出現頻度の高い単語はidfが0となる（tf-idfも0となる）
book_tf_idf <-
  book_words %>%
    bind_tf_idf(term = word, document = book, n = n)

# 確認
book_tf_idf %>% print()

# 並び替え
# --- tf_idfが大きい重要性の高いキーワード
# --- 全て重要な登場人物の名前
book_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))

# プロット作成
# --- 書籍ごとの上位15単語のtf-idfを表示
book_tf_idf %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  ggplot(aes(x = fct_reorder(word, tf_idf), y = tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~book, ncol = 2, scales = "free") +
  labs(x = "tf-idf", y = NULL) +
  coord_flip()


# 4 物理学書のコーパス --------------------------------------------------------

# データ取得
load("data/physics.rda")

# 単語ごとのカウント
physics_words <-
  physics %>%
    unnest_tokens(word, text) %>%
    count(author, word, sort = TRUE)

# 確認
physics_words %>% print()

# tf-idfの算出
plot_physics <-
  physics_words %>%
    bind_tf_idf(word, author, n) %>%
    mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan",
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))

# プロット作成
# --- 書籍ごとの上位15単語
plot_physics %>%
  group_by(author) %>%
  slice_max(tf_idf, n = 15) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(x = word, y = tf_idf, fill = author)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~author, ncol = 2, scales = "free") +
  coord_flip()


# 単語検索
# --- 意味のある単語かをチェック
physics %>%
  filter(str_detect(text, "_k_")) %>%
  select(text)

physics %>%
  filter(str_detect(text, "RC")) %>%
  select(text)

# ストップワード
# --- 実行結果から独自に作成
mystopwords <-
  tibble(word = c("eq", "co", "rc", "ac", "ak", "bn",
                  "fig", "file", "cg", "cb", "cm",
                  "ab", "_k", "_k_", "_x"))

# データ除外
# --- ストップワードを削除
physics_words <-
  physics_words %>%
    anti_join(mystopwords, by = "word")


# プロットデータ作成
plot_physics <-
  physics_words %>%
    bind_tf_idf(word, author, n) %>%
    mutate(word = str_remove_all(word, "_")) %>%
    group_by(author) %>%
    slice_max(tf_idf, n = 15) %>%
    ungroup() %>%
    mutate(word = reorder_within(word, tf_idf, author)) %>%
    mutate(author = factor(author, levels = c("Galilei, Galileo",
                                              "Huygens, Christiaan",
                                              "Tesla, Nikola",
                                              "Einstein, Albert")))

# プロット作成
plot_physics %>%
  ggplot(aes(x = word, y = tf_idf, fill = author)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~author, ncol = 2, scales = "free") +
    scale_x_reordered() +
    coord_flip()
