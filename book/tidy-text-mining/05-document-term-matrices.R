# ***********************************************************************************************
# Title   : Rによるテキストマイニング
# Chapter : 5 未整理データ形式の変換
# Date    : 2022/08/08
# Page    : P81 - P103
# URL     : https://github.com/dgrtwo/tidy-text-mining
# ***********************************************************************************************


# ＜概要＞
# - DTM(文書-単語行列)はテキストマイニングの捜査対象として最も広く使われているデータ構造



# ＜目次＞
# 0 準備
# 1 DocumentTermMatrixオブジェクトの整理
# 2 dfmオブジェクトの整理
# 3 整理データの行列へのキャスト
# 4 メタデータを持つコーパスオブジェクトの整理
# 5 株式に関する記事のマイニング


# 0 準備 ------------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidytext)
library(stringr)
library(tm)
library(Hmisc)
library(Matrix)
library(janeaustenr)


# 1 DocumentTermMatrixオブジェクトの整理 ------------------------------------------------

# データロード
# --- AP通信の記事コレクション
data("AssociatedPress", package = "topicmodels")

# データ確認
# --- Document Term Matrixオブジェクト
# --- 99％疎になっている
AssociatedPress %>% glimpse()
AssociatedPress %>% class()
AssociatedPress %>% print()


# 文書内の単語にアクセス
# --- tidy()を適用すると整理データフレームのフォーマットに変換される
ap_td <- AssociatedPress %>% tidy()
ap_td %>% print()

# センチメントデータの結合
ap_sentiments <-
  ap_td %>%
    inner_join(get_sentiments("bing"), by = c(term = "word"))

# データ確認
ap_sentiments %>% print()

# プロット作成
# --- Negativeの場合はカウント数をマイナスにする
ap_sentiments %>%
  count(sentiment, term, wt = count) %>%
  ungroup() %>%
  filter(n >= 200) %>%
  mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
  mutate(term = reorder(term, n)) %>%
  ggplot(aes(n, term, fill = sentiment)) +
  geom_col() +
  labs(x = "Contribution to sentiment", y = NULL)


# 2 dfmオブジェクトの整理 -----------------------------------------------------------

# データロード
data("data_corpus_inaugural", package = "quanteda")

# データ確認
# --- "corpus" "character"
data_corpus_inaugural %>% class()
data_corpus_inaugural %>% print()

# データ構造
data_corpus_inaugural %>% glimpse()
data_corpus_inaugural %>% list.tree(2)


# データ変換
inaug_dfm <- quanteda::dfm(data_corpus_inaugural, verbose = FALSE)
inaug_dfm %>% print()
inaug_dfm %>% class()

# 整理データに変換
inaug_td <- inaug_dfm %>% tidy()
inaug_td %>% print()

# tf-idfを計算
inaug_tf_idf <-
  inaug_td %>%
    bind_tf_idf(term, document, count) %>%
    arrange(desc(tf_idf))

# 確認
inaug_tf_idf %>% print()

# プロット作成
# --- 各演説のtf-idfの上位単語を表示
inaug_tf_idf %>%
  filter(document %in% c("1933-Roosevelt", "1861-Lincoln", "1961-Kennedy", "2009-Obama")) %>%
  group_by(document) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, tf_idf, document)) %>%
  ggplot(aes(x = term, y = tf_idf, fill = document)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ document, scales = "free") +
  coord_flip() +
  scale_x_reordered() +
  labs(x = NULL,
       y = "tf-idf")


year_term_counts <-
  inaug_td %>%
    extract(document, "year", "(\\d+)", convert = TRUE) %>%
    complete(year, term, fill = list(count = 0)) %>%
    group_by(year) %>%
    mutate(year_total = sum(count))

# プロット
# --- キーワードの推移
year_term_counts %>%
  filter(term %in% c("god", "america", "foreign", "union", "constitution", "freedom")) %>%
  ggplot(aes(year, count / year_total)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~ term, scales = "free_y") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(y = "% frequency of word in inaugural address")


# 3 整理データの行列へのキャスト ----------------------------------------------------------

# データロード
# --- AP通信の記事コレクション
data("AssociatedPress", package = "topicmodels")

# 整理データの作成
ap_td <- AssociatedPress %>% tidy()

# DTM形式に変換
ap_td_dtm <- ap_td %>% cast_dtm(document, term, count)
ap_td_dtm %>% class()
ap_td_dtm %>% print()

# dfmオブジェクトに変換
ap_td_dfm <- ap_td %>% cast_dfm(document, term, count)
ap_td_dfm %>% class()
ap_td_dfm %>% print()

# 単純に疎データに変換
ap_td_sparse <- ap_td %>% cast_sparse(document, term, count)
ap_td_sparse %>% class()
ap_td_sparse %>% dim()


# 練習：テキストデータのDTMに変換
austen_dtm <-
  austen_books() %>%
    unnest_tokens(word, text) %>%
    count(book, word) %>%
    cast_dtm(book, word, n)

# 確認
austen_dtm %>% print()


# 4 メタデータを持つコーパスオブジェクトの整理 --------------------------------------------

# ＜ポイント＞
# - コーパスとはトークン化する前の文字コレクションを格納するためのデータ構造
#   --- コーパスはテキスト本文に加えてメタデータ(属性データ)を格納する
#   --- tmオブジェクトが知られている


# データロード
data("acq")

# データ確認
acq %>% print()
acq %>% class()

# データ構造
# --- 1つめの文書
acq[[1]] %>% class()
acq[[1]] %>% print()
acq[[1]] %>% names()
acq[[1]] %>% glimpse()


# 整理データに変換
acq_td <- acq %>% tidy()
acq_td %>% print()

# ストップワードの除去
acq_tokens <-
  acq_td %>%
    select(-places) %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word")

# most common words
acq_tokens %>%
  count(word, sort = TRUE)

# tf-idfの算出
acq_tokens %>%
  count(id, word) %>%
  bind_tf_idf(word, id, n) %>%
  arrange(desc(tf_idf))


# 5 株式に関する記事のマイニング ----------------------------------------------------

# ＜ポイント＞
# - コーパスオブジェクトはテキストデータの取り込み用パッケージで使われる出力形式
#   --- tidy()による整理データへの変換で整理データへの分析が簡単になる


# データロード
load("data/stock_articles.rda")

# データ確認
stock_articles %>% print()


# 整理データに変換
stock_tokens <-
  stock_articles %>%
    mutate(corpus = map(corpus, tidy)) %>%
    unnest(cols = (corpus)) %>%
    unnest_tokens(word, text) %>%
    select(company, datetimestamp, word, id, heading)

# データ確認
stock_tokens %>% print()

# tf-idfの計算
stock_tf_idf <-
  stock_tokens %>%
    count(company, word) %>%
    filter(!str_detect(word, "\\d+")) %>%
    bind_tf_idf(word, company, n) %>%
    arrange(-tf_idf)

# プロット作成
# --- tf-idfが上位のキーワード
stock_tf_idf %>%
  group_by(company) %>%
  top_n(8, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(word, tf_idf, fill = company)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ company, scales = "free") +
  coord_flip() +
  labs(x = NULL, y = "tf-idf")


# センチメント分析
# --- Afinn辞書
stock_tokens_afinn <-
  stock_tokens %>%
    anti_join(stop_words, by = "word") %>%
    count(word, id, sort = TRUE) %>%
    inner_join(get_sentiments("afinn"), by = "word")

# プロット作成
# --- 上位キーワード
stock_tokens_afinn %>%
  group_by(word) %>%
  summarise(contribution = sum(n * value)) %>%
  slice_max(abs(contribution), n = 12) %>%
  ungroup() %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, word)) +
  geom_col() +
  labs(x = "Frequency of word * AFINN value", y = NULL)


# センチメント分析
# --- Loughran辞書
stock_tokens_loughran <-
  stock_tokens %>%
    count(word) %>%
    inner_join(get_sentiments("loughran"), by = "word")

# プロット作成
# --- 上位キーワード
stock_tokens_loughran %>%
  group_by(sentiment) %>%
  slice_max(n, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free") +
  labs(x = "Frequency of this word in the recent financial articles", y = NULL)


stock_sentiment_count <-
  stock_tokens %>%
    inner_join(get_sentiments("loughran"), by = "word") %>%
    count(sentiment, company) %>%
    spread(sentiment, n, fill = 0)

stock_sentiment_count %>%
  mutate(score = (positive - negative) / (positive + negative)) %>%
  mutate(company = reorder(company, score)) %>%
  ggplot(aes(score, company, fill = score > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Positivity score among 20 recent news articles", y = NULL)