# ***********************************************************************************************
# Title   : Rによるテキストマイニング
# Chapter : 8 ケーススタディ：NASAメタデータのマイニング
# Date    : 2022/08/16
# Page    : P149 - P180
# URL     : https://github.com/dgrtwo/tidy-text-mining
# ***********************************************************************************************


# ＜概要＞
# - NASAデータセットのメタデータをテキスト分析フローに基づいて分析する


# ＜目次＞
# 0 準備
# 1 データセットの確認と整理
# 2 整理テキストデータの作成
# 3 タイトルと説明文のワードネットワーク
# 4 キーワードのワードネットワーク
# 5 説明フィールドのtf-idfの計算
# 6 トピックモデルの準備
# 7 トピックモデルの構築
# 8 トピックモデルの解釈


# 0 準備 ---------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidytext)
library(widyr)
library(igraph)
library(ggraph)
library(topicmodels)


# データロード
load("data/metadata.rda")


# 1 データセットの確認と整理 -----------------------------------------------------

# データ概要
metadata$dataset %>% class()
metadata$dataset %>% names()

# 基本項目の確認
# --- タイトル、説明、キーワード
metadata$dataset$title %>% class()
metadata$dataset$description %>% class()
metadata$dataset$keyword %>% class()


# タイトル
nasa_title <-
  tibble(id = metadata$dataset$`_id`$`$oid`,
         title = metadata$dataset$title)

# 説明
nasa_desc <-
  tibble(id = metadata$dataset$`_id`$`$oid`,
         desc = metadata$dataset$description)

# キーワード
# --- リスト形式なのでunnest()が必要
nasa_keyword <-
  tibble(id = metadata$dataset$`_id`$`$oid`,
         keyword = metadata$dataset$keyword) %>%
  unnest(keyword)


# 2 整理テキストデータの作成 --------------------------------------------------------

# タイトルの整理データ作成
# --- ストップワードを削除
nasa_title <-
  nasa_title %>%
    unnest_tokens(word, title) %>%
    anti_join(stop_words)

nasa_desc <-
  nasa_desc %>%
    unnest_tokens(word, desc) %>%
    anti_join(stop_words)

# 上位単語の確認
nasa_title %>% count(word, sort = TRUE)
nasa_desc %>% count(word, sort = TRUE)

# カスタムのストップワード
my_stopwords <-
  tibble(word = c(as.character(1:10),
                  "v1", "v03", "l2", "l3", "l4", "v5.2.0",
                  "v003", "v004", "v005", "v006", "v7"))

# カスタムのストップワード削除
nasa_title <- nasa_title %>% anti_join(my_stopwords, by = "word")
nasa_desc <- nasa_desc %>% anti_join(my_stopwords, by = "word")

# 上位キーワードの確認
# --- 大文字と小文字が混在する
nasa_keyword %>%
  group_by(keyword) %>%
  count(sort = TRUE)

# キーワードを全て大文字に変換
nasa_keyword <-
  nasa_keyword %>%
    mutate(keyword = toupper(keyword))


# 3 タイトルと説明文のワードネットワーク --------------------------------------------------

# 共起数のカウント
# --- タイトル(Title)
title_word_pairs <-
  nasa_title %>%
    pairwise_count(word, id, sort = TRUE, upper = FALSE)

# 共起数のカウント
# --- 説明(Description)
desc_word_pairs <-
  nasa_desc %>%
    pairwise_count(word, id, sort = TRUE, upper = FALSE)

# ネットワークによる可視化
# --- タイトル(Title)
set.seed(1234)
title_word_pairs %>%
  filter(n >= 250) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "cyan4") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# ネットワークによる可視化
# --- 説明(Description)
set.seed(1234)
desc_word_pairs %>%
  filter(n >= 5000) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "darkred") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


# 4 キーワードのワードネットワーク ----------------------------------------------------

# 共起数のカウント
# --- キーワード
keyword_pairs <-
  nasa_keyword %>%
    pairwise_count(keyword, id, sort = TRUE, upper = FALSE)

# ネットワークによる可視化
# --- キーワード(Keyword)
set.seed(1234)
keyword_pairs %>%
  filter(n >= 700) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()

# ペアワイズ相関の算出
# --- キーワード間の相関
keyword_cors <-
  nasa_keyword %>%
    group_by(keyword) %>%
    filter(n() >= 50) %>%
    pairwise_cor(keyword, id, sort = TRUE, upper = FALSE)

# 確認
# --- 相関1は常に共起していることを示す
keyword_cors %>% print()

# ネットワークによる可視化
# --- ペアワイズ相関の高いキーワード
set.seed(1234)
keyword_cors %>%
  filter(correlation > .6) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
  geom_node_point(size = 5) +
  geom_node_text(aes(label = name), repel = TRUE,
                 point.padding = unit(0.2, "lines")) +
  theme_void()


# 5 説明フィールドのtf-idfの計算 --------------------------------------------------------

# tf-idfの計算
desc_tf_idf <-
  nasa_desc %>%
    count(id, word, sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(term = word, document = id, n = n)

# 上位単語の抽出
desc_tf_idf %>% arrange(-tf_idf)

# データ結合
# --- 説明のtf-idfとキーワード
desc_tf_idf <-
  desc_tf_idf %>%
    full_join(nasa_keyword, by = "id")

# プロット
desc_tf_idf %>%
  filter(!near(tf, 1)) %>%
  filter(keyword %in% c("SOLAR ACTIVITY", "CLOUDS",
                        "SEISMOLOGY", "ASTROPHYSICS",
                        "HUMAN HEALTH", "BUDGET")) %>%
  arrange(desc(tf_idf)) %>%
  group_by(keyword) %>%
  distinct(word, keyword, .keep_all = TRUE) %>%
  slice_max(tf_idf, n = 15, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>%
  ggplot(aes(tf_idf, word, fill = keyword)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~keyword, ncol = 3, scales = "free") +
  labs(title = "Highest tf-idf words in NASA metadata description fields",
       caption = "NASA metadata from https://data.nasa.gov/data.json",
       x = "tf-idf", y = NULL)


# 6 トピックモデルの準備 ----------------------------------------------------------

# ストップワードの定義
my_stop_words <-
  bind_rows(stop_words,
            tibble(word = c("nbsp", "amp", "gt", "lt",
                            "timesnewromanpsmt", "font",
                            "td", "li", "br", "tr", "quot",
                            "st", "img", "src", "strong",
                            "http", "file", "files",
                            as.character(1:12)),
                   lexicon = rep("custom", 30)))

# 単語カウント
# --- 説明
word_counts <-
  nasa_desc %>%
    anti_join(my_stop_words, by = "word") %>%
    count(id, word, sort = TRUE) %>%
    ungroup()

# データ変換
# --- トピックモデル用
desc_dtm <-
  word_counts %>%
    cast_dtm(document = id, term = word, value = n)

# 確認
desc_dtm %>% print()
desc_dtm %>% class()


# 7 トピックモデルの構築 ---------------------------------------------------------

# モデル構築
desc_lda <- desc_dtm %>% LDA(k = 24, control = list(seed = 1234))

# 確認
desc_lda %>% print()
desc_lda %>% class()


# 8 トピックモデルの解釈 ---------------------------------------------------------

# データ整理
tidy_lda <- desc_lda %>% tidy()
tidy_lda %>% print()

# 上位トピックの抽出
top_terms <-
  tidy_lda %>%
    group_by(topic) %>%
    slice_max(beta, n = 10, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(topic, -beta)

# 確認
top_terms %>% print()

# プロット作成
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  group_by(topic, term) %>%
  arrange(desc(beta)) %>%
  ungroup() %>%
  ggplot(aes(beta, term, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  labs(title = "Top 10 terms in each LDA topic",
       x = expression(beta), y = NULL) +
  facet_wrap(~topic, ncol = 4, scales = "free")


lda_gamma <- desc_lda %>% tidy(matrix = "gamma")

# ヒストグラム作成
# --- 全体
lda_gamma %>%
  ggplot(aes(gamma)) +
  geom_histogram(alpha = 0.8) +
  scale_y_log10() +
  labs(title = "Distribution of probabilities for all topics",
       y = "Number of documents", x = expression(gamma))

# ヒストグラム作成
# --- トピックごと
lda_gamma %>%
  ggplot(aes(gamma, fill = as.factor(topic))) +
  geom_histogram(alpha = 0.8, show.legend = FALSE) +
  facet_wrap(~topic, ncol = 4) +
  scale_y_log10() +
  labs(title = "Distribution of probability for each topic",
       y = "Number of documents", x = expression(gamma))


# キーワード結合
lda_gamma <-
  lda_gamma %>%
    full_join(nasa_keyword, by = c("document" = "id"))

# 上位単語のカウント
top_keywords <-
  lda_gamma %>%
    filter(gamma > 0.9) %>%
    count(topic, keyword, sort = TRUE)

# プロット作成
top_keywords %>%
  group_by(topic) %>%
  slice_max(n, n = 5, with_ties = FALSE) %>%
  ungroup %>%
  mutate(keyword = reorder_within(keyword, n, topic)) %>%
  ggplot(aes(n, keyword, fill = as.factor(topic))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Top keywords for each LDA topic",
       x = "Number of documents", y = NULL) +
  scale_y_reordered() +
  facet_wrap(~topic, ncol = 4, scales = "free")
