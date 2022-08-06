# ***********************************************************************************************
# Title   : Rによるテキストマイニング
# Chapter : 4 単語間の関係：nグラムと相関
# Date    : 2022/08/07
# Page    : P53 - P79
# URL     : https://github.com/dgrtwo/tidy-text-mining
# ***********************************************************************************************


# ＜概要＞
# - 単語ごとの共起に着目することで、単語間の関係性を意識したテキスト分析が行えるようになる


# ＜目次＞
# 0 準備
# 1 nグラムによるトークン化
# 2 nグラムデータの前処理
# 3 バイグラム分析
# 4 センチメント分析にコンテキストを反映させるためのバイグラム活用
# 5 バイグラムのネットワーク可視化
# 6 他のテキストのバイグラム可視化
# 7 単語のペアワイズ出現頻度と相関


# 0 準備 ------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidytext)
library(janeaustenr)
library(igraph)
library(ggraph)
library(stringr)
library(widyr)


# 1 nグラムによるトークン化 --------------------------------------------------------

# ＜ポイント＞
# - nグラムとは任意の文字列や文書を連続したn個の連続する単位


# nグラムに分割
austen_bigrams <-
  austen_books() %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    drop_na()

# 確認
austen_bigrams %>% print()


# 2 nグラムデータの前処理 -------------------------------------

# ＜ポイント＞
# - nグラムの上位カウントにはストップワードを含むペアが上位に挙がる
#   - ストップワードを含むnグラムを除外する


# データ集計
# --- nグラム単位でカウント
# --- ストップワードを含むものが上位に挙がる
austen_bigrams %>%
  count(bigram, sort = TRUE)

# nグラムを分割
# --- スペースごとに分割
bigrams_separated <-
  austen_bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

# ストップワードの除去
# --- 分割したword1/word2に対してストップワードが含まれているか確認
bigrams_filtered <-
  bigrams_separated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

# カウント
bigram_counts <-
  bigrams_filtered %>%
    count(word1, word2, sort = TRUE)

# nグラムの再作成
# --- ストップワード除去後
bigrams_united <-
  bigrams_filtered %>%
    unite(bigram, word1, word2, sep = " ")

# 確認
bigrams_united %>% print()


# 参考：nグラムが3つの場合
austen_books() %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  drop_na() %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)


# 3 バイグラム分析 ---------------------------------------------------------------

# ＜ポイント＞
# - 1行に1バイグラム分析の形式はテキストの予備分析で役に立つ
#   --- メリット：個別の単語では分からない情報が見えてくることがある
#   --- デメリット：バイグラムは出現頻度が下がるので重要度を判定しにくくなる


# nグラムの単語カウント
# --- word2が"street"のレコード
bigrams_filtered %>%
  filter(word2 == "street") %>%
  count(book, word1, sort = TRUE)

# tf-idfの算出
bigram_tf_idf <-
  bigrams_united %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf_idf))

# 確認
bigram_tf_idf %>% print()

# プロット作成
# --- 登場人物の名前が上位に挙がっている
bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  group_by(book) %>%
  slice_max(tf_idf, n = 10) %>%
  ungroup() %>%
  mutate(bigram = reorder(bigram, tf_idf)) %>%
  ggplot(aes(x = bigram, y = tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ book, ncol = 2, scales = "free") +
  labs(x = "tf-idf of bigram", y = NULL) +
  coord_flip()


# 4 センチメント分析にコンテキストを反映させるためのバイグラム活用 ----------------

# nグラムの単語カウント
# --- word1が"not"のレコード
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

# センチメント辞書の取得
# --- Afinn
AFINN <- get_sentiments("afinn")

# センチメントスコアを結合してカウント
# --- word1が"not"のword2に対してセンチメントスコアを算出
not_words <-
  bigrams_separated %>%
    filter(word1 == "not") %>%
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word2, value, sort = TRUE)

# 確認
not_words %>% print()

# プロット作成
not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")

# センチメントを反転させる単語
# --- 少なくとも4つある
negation_words <- c("not", "no", "never", "without")

# 単語カウント
# --- 上記の4単語を排除
negated_words <-
  bigrams_separated %>%
    filter(word1 %in% negation_words) %>%
    inner_join(AFINN, by = c(word2 = "word")) %>%
    count(word1, word2, value, sort = TRUE)

# プロット作成
negated_words %>%
  mutate(contribution = n * value,
         word2 = reorder(paste(word2, word1, sep = "__"), contribution)) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 12, with_ties = FALSE) %>%
  ggplot(aes(word2, contribution, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free") +
  scale_x_discrete(labels = function(x) gsub("__.+$", "", x)) +
  xlab("Words preceded by negation term") +
  ylab("Sentiment value * # of occurrences") +
  coord_flip()


# 5 バイグラムのネットワーク可視化 -----------------------------------------------------

# ＜ポイント＞
# - バイグラムは関係性データの1つ（今回は2つのワードの関係性）
#   --- ネットワーク・グラフで可視化すると見やすい
#   --- バイグラムのグラフはマルコフ連鎖の可視化を意味する（直前の単語のみに影響を受ける）

# データ作成
# --- ストップワード除去後のカウント
bigram_counts2 <-
  austen_books() %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    drop_na() %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)

# グラフデータの作成
# --- igraphオブジェクトに変換
bigram_graph <-
  bigram_counts %>%
    filter(n > 20) %>%
    graph_from_data_frame()

# 確認
bigram_graph %>% print()
bigram_graph %>% class()

# グラフ作成
# --- {igpraph}にもプロット関数はあるが{ggraph}の方が秀逸
set.seed(2017)
bigram_graph %>%
  ggraph(layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

# プロット再作成
# --- 矢印の追加
# --- 各種装飾の追加
set.seed(2020)
bigram_graph %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = grid::arrow(type = "closed", length = unit(.15, "inches")),
                 end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()


# 6 他のテキストのバイグラム可視化 -------------------------------------------------

# 関数作成
# --- nグラムのカウントデータの作成
# --- ストップワード除去後のnグラムを使用
count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

# 関数定義
# --- ネットワーク可視化
visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


# テキストデータの取得
load("data/kjv.rda")

# カウントデータの作成
# --- ストップワード除去後のバイグラム
kjv_bigrams <-

# プロット作成
kjv %>%
  count_bigrams() %>%
  filter(n > 40,
         !str_detect(word1, "\\d"),
         !str_detect(word2, "\\d")) %>%
  visualize_bigrams()


# 7 単語のペアワイズ出現頻度と相関 -------------------------------------------------------

# データ取得
austen_section_words <-
  austen_books() %>%
    filter(book == "Pride & Prejudice") %>%
    mutate(section = row_number() %/% 10) %>%
    filter(section > 0) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word)

# 確認
austen_section_words %>% print()

# 共起する単語をカウント
# --- セクションごとにカウント
word_pairs <-
  austen_section_words %>%
    pairwise_count(word, section, sort = TRUE)

# 特定単語でフィルタ
word_pairs %>%
  filter(item1 == "darcy")

# ペアワイズ相関の算出
word_cors <-
  austen_section_words %>%
    group_by(word) %>%
    filter(n() >= 20) %>%
    pairwise_cor(word, section, sort = TRUE)

# データ確認
word_cors %>% print()


word_cors %>%
  filter(item1 == "pounds")

# プロット作成
word_cors %>%
  filter(item1 %in% c("elizabeth", "pounds", "married", "pride")) %>%
  group_by(item1) %>%
  top_n(6) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip()

# ネットワーク作成
set.seed(2016)
word_cors %>%
  filter(correlation > .15) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()
