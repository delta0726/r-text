# ***********************************************************************************************
# Title   : Rによるテキストマイニング
# Chapter : 9 ケーススタディ：Usenetテキストの分析
# Date    : 2022/08/18
# Page    : P181 - P202
# URL     : https://github.com/dgrtwo/tidy-text-mining
# ***********************************************************************************************



# ＜目次＞
# 0 準備
# 1 テキストの前処理
# 2 ニュースグループに含まれる単語
# 3 ペアワイズ相関とネットワーク可視化
# 4 トピックモデリング
# 5 ニュースグループごとのセンチメント分析
# 6 単語ごとのセンチメント分析
# 7 メッセージごとのセンチメント分析


# 0 準備 -------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidytext)
library(stringr)
library(widyr)
library(ggraph)
library(igraph)
library(topicmodels)


# データロード
load("data/raw_text.rda")


# 1 テキストの前処理 ---------------------------------------------------------------

# データ確認
raw_text %>% print()
raw_text %>% glimpse()

# ニュースグループのカウント
raw_text %>% group_by(newsgroup) %>% tally()

# プロット作成
# ----  ニュースグループごとのid数
raw_text %>%
  group_by(newsgroup) %>%
  summarize(messages = n_distinct(id)) %>%
  ggplot(aes(messages, newsgroup)) +
  geom_col() +
  labs(y = NULL)


# テキストのクリーニング
# --- cumsum()とstr_detect()を組み合わせて対象単語を抽出する
# --- 正規表現による除外
cleaned_text <-
  raw_text %>%
    group_by(newsgroup, id) %>%
    filter(cumsum(text == "") > 0,
           cumsum(str_detect(text, "^--")) == 0) %>%
    ungroup() %>%
    filter(str_detect(text, "^[^>]+[A-Za-z\\d]") | text == "",
           !str_detect(text, "writes(:|\\.\\.\\.)$"),
           !str_detect(text, "^In article <"),
           !id %in% c(9704, 9985))


# 2 ニュースグループに含まれる単語 -------------------------------------------------

# 整理データ作成
# --- ストップワードの除去
usenet_words <-
  cleaned_text %>%
    unnest_tokens(word, text) %>%
    filter(str_detect(word, "[a-z']$"),
           !word %in% stop_words$word)

# カウント
# --- 全体
usenet_words %>% count(word, sort = TRUE)

# カウント
# # --- ニュースグループごと
words_by_newsgroup <-
  usenet_words %>%
    count(newsgroup, word, sort = TRUE) %>%
    ungroup()

# tf-idfの計算
tf_idf <-
  words_by_newsgroup %>%
    bind_tf_idf(word, newsgroup, n) %>%
    arrange(desc(tf_idf))

# プロット作成
tf_idf %>%
  filter(str_detect(newsgroup, "^sci\\.")) %>%
  group_by(newsgroup) %>%
  top_n(12, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%
  ggplot(aes(tf_idf, word, fill = newsgroup)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ newsgroup, scales = "free") +
  labs(x = "tf-idf", y = NULL)


# 関数定義
# --- プロット作成
# --- ニュースグループごとのtf-idfの上位単語
plot_tf_idf <- function(d) {
  d %>%
    group_by(newsgroup) %>%
    top_n(10, tf_idf) %>%
    mutate(word = reorder(word, tf_idf)) %>%
    ggplot(aes(tf_idf, word, fill = newsgroup)) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ newsgroup, scales = "free") +
    labs(x = "tf-idf", y = NULL)
}

# プロット作成
tf_idf %>%
  filter(str_detect(newsgroup, "^rec\\.")) %>%
  plot_tf_idf()


# 3 ペアワイズ相関とネットワーク可視化 ----------------------------------------------

# ペアワイズ相関
newsgroup_cors <-
  words_by_newsgroup %>%
    pairwise_cor(newsgroup, word, n, sort = TRUE)

# 確認
newsgroup_cors %>% print()

# ネットワーク作成
set.seed(2017)
newsgroup_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 6, color = "lightblue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()


# 4 トピックモデリング ---------------------------------------------------------------

# 単語抽出
# --- 出現頻度が50回以上
word_sci_newsgroups <-
  usenet_words %>%
    filter(str_detect(newsgroup, "^sci")) %>%
    group_by(word) %>%
    mutate(word_total = n()) %>%
    ungroup() %>%
    filter(word_total > 50)

# DTM形式に変換
sci_dtm <-
  word_sci_newsgroups %>%
    unite(document, newsgroup, id) %>%
    count(document, word) %>%
    cast_dtm(document, word, n)

# モデル構築
sci_lda <- LDA(sci_dtm, k = 4, control = list(seed = 2016))

# プロット作成
# --- 単語頻度
sci_lda %>%
  tidy() %>%
  group_by(topic) %>%
  top_n(8, beta) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

# プロット作成
sci_lda %>%
  tidy(matrix = "gamma") %>%
  separate(document, c("newsgroup", "id"), sep = "_") %>%
  mutate(newsgroup = reorder(newsgroup, gamma * topic)) %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot() +
  facet_wrap(~ newsgroup) +
  labs(x = "Topic",
       y = "# of messages where this was the highest % topic")


# 5 ニュースグループごとのセンチメント分析 -----------------------------------------------

# センチメント集計
# --- ニュースグループごと
newsgroup_sentiments <-
  words_by_newsgroup %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(newsgroup) %>%
    summarize(value = sum(value * n) / sum(n)) %>%
    ungroup()

# プロット作成
# --- センチメント
newsgroup_sentiments %>%
  mutate(newsgroup = reorder(newsgroup, value)) %>%
  ggplot(aes(value, newsgroup, fill = value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Average sentiment value", y = NULL)


# 6 単語ごとのセンチメント分析 --------------------------------------------------------

# センチメント集計
# --- 単語ごと
contributions <-
  usenet_words %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(word) %>%
    summarize(occurences = n(),
              contribution = sum(value))

# プロット作成
# --- センチメント
contributions %>%
  slice_max(abs(contribution), n = 25) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  labs(y = NULL)

top_sentiment_words <-
  words_by_newsgroup %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    mutate(contribution = value * n / sum(n))

top_sentiment_words %>%
  filter(str_detect(newsgroup, "^(talk|alt|misc)")) %>%
  group_by(newsgroup) %>%
  slice_max(abs(contribution), n = 12) %>%
  ungroup() %>%
  mutate(newsgroup = reorder(newsgroup, contribution),
         word = reorder_within(word, contribution, newsgroup)) %>%
  ggplot(aes(contribution, word, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  scale_y_reordered() +
  facet_wrap(~ newsgroup, scales = "free") +
  labs(x = "Sentiment value * # of occurrences", y = NULL)


# 7 メッセージごとのセンチメント分析 ----------------------------------------------------

sentiment_messages <-
  usenet_words %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(newsgroup, id) %>%
    summarize(sentiment = mean(value),
              words = n()) %>%
    ungroup() %>%
    filter(words >= 5)


sentiment_messages <-
  usenet_words %>%
    inner_join(afinn, by = "word") %>%
    group_by(newsgroup, id) %>%
    summarize(sentiment = mean(value),
              words = n()) %>%
    ungroup() %>%
    filter(words >= 5)


sentiment_messages %>%
  arrange(desc(sentiment))


print_message <- function(group, message_id) {
  result <- cleaned_text %>%
    filter(newsgroup == group, id == message_id, text != "")

  cat(result$text, sep = "\n")
}

print_message("rec.sport.hockey", 53560)



sentiment_messages %>%
  arrange(sentiment)

print_message("rec.sport.hockey", 53907)


usenet_bigrams <- cleaned_text %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2)


usenet_bigram_counts <- usenet_bigrams %>%
  count(newsgroup, bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")



negate_words <- c("not", "without", "no", "can't", "don't", "won't")

usenet_bigram_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(get_sentiments("afinn"), by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = "Sentiment value * # of occurrences",
       y = "Words preceded by a negation")


negate_words <- c("not", "without", "no", "can't", "don't", "won't")

usenet_bigram_counts %>%
  filter(word1 %in% negate_words) %>%
  count(word1, word2, wt = n, sort = TRUE) %>%
  inner_join(afinn, by = c(word2 = "word")) %>%
  mutate(contribution = value * n) %>%
  group_by(word1) %>%
  slice_max(abs(contribution), n = 10) %>%
  ungroup() %>%
  mutate(word2 = reorder_within(word2, contribution, word1)) %>%
  ggplot(aes(contribution, word2, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ word1, scales = "free", nrow = 3) +
  scale_y_reordered() +
  labs(x = "Sentiment value * # of occurrences",
       y = "Words preceded by a negation")
