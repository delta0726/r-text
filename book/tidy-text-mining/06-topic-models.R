# ***********************************************************************************************
# Title   : Rによるテキストマイニング
# Chapter : 6 トピックモデリング
# Date    : 2022/08/08
# Page    : P105 - P127
# URL     : https://github.com/dgrtwo/tidy-text-mining
# ***********************************************************************************************


# ＜概要＞
# - トピックモデリングは数値データのクラスタリングに似た方法で文書を教師なしデータにより分類する
# - 潜在的ディリクレ配分法(LDA)は特に使われるトピックモデリング手法


# ＜目次＞
# 0 準備
# 1 LDAとは
# 2 単語のトピック確率
# 3 文書のトピック確率
# 4 図書館荒らし：準備
# 5 図書館荒らし：章を対象とするLDA
# 6 図書館荒らし：文書ごとの分類
# 7 図書館荒らし：単語ごとの分類
# 8 LDAの他の実装


# 0 準備 -----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidytext)
library(topicmodels)
library(stringr)
library(scales)
library(mallet)


# 1 LDAとは ---------------------------------------------------------------------------

# ＜ポイント＞
# - LDAは個々の文書を形作るトピックの組み合わせを明らかにしながら、個々のトピックに関連する言葉の組み合わせを探す
# - LDAは2つの原則に基づいている
#   --- 全ての文書はトピックの組み合わせ
#   --- 全てのトピックは単語の組み合わせ


# データロード
data("AssociatedPress")

# データ確認
AssociatedPress %>% print()
AssociatedPress %>% class()

# モデル構築
# --- 2トピックのLDAモデル
# --- 単語とトピックがどのように結びつき、トピックと文書がどのように結びつくかをまとめたオブジェクトを返す
ap_lda <- AssociatedPress %>% LDA(k = 2, control = list(seed = 1234))

# 確認
ap_lda %>% print()
ap_lda %>% class()
ap_lda %>% glimpse()


# 2 単語のトピック確率 -----------------------------------------------------------

# ＜ポイント＞
# - topicmodelオブジェクトからtidy()により各トピックごとの単語の出現確率であるベータを抽出する


# 整理データに変換
# --- 1行/1単語/1トピックの形式にデータ変換している
# --- topicごとに同じ単語が出現している
ap_topics <- ap_lda %>% tidy(matrix = "beta")
ap_topics %>% print()

# 上位キーワードの抽出
# --- トピックごと
ap_top_terms <-
  ap_topics %>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

# プロット作成
# --- "new"や"people"など両方のトピックに共通して含まれる単語がある（ハードクラスタリング）
ap_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# 対数比の算出
# --- トピック1とトピック2のベータの差に注目
beta_spread <-
  ap_topics %>%
    mutate(topic = paste0("topic", topic)) %>%
    spread(topic, beta) %>%
    filter(topic1 > .001 | topic2 > .001) %>%
    mutate(log_ratio = log2(topic2 / topic1))

# プロット作成
# --- 2つのトピックで出現頻度の差が最も大きい単語
beta_spread %>%
  group_by(direction = log_ratio > 0) %>%
  top_n(10, abs(log_ratio)) %>%
  ungroup() %>%
  mutate(term = reorder(term, log_ratio)) %>%
  ggplot(aes(log_ratio, term)) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 1", y = NULL)


# 3 文書のトピック確率 ---------------------------------------------------------

# ＜ポイント＞
# - LDAはトピックと単語だけでなく、文書をトピックの組み合わせとしてモデリングする


# 整理データに変換
# --- 各文書のトピック出現確率である文書-トピック確率(γ)を調べることができる
ap_documents <- ap_lda %>% tidy(matrix = "gamma")
ap_documents %>% print()

# 出現回数の確認
# --- ドキュメント6のみ抽出
AssociatedPress %>%
  tidy() %>%
  filter(document == 6) %>%
  arrange(desc(count))


# 4 図書館荒らし：準備 -------------------------------------------------------------

# ＜ポイント＞
# - 4つの書籍のページがバラバラになった状態からの復元をトピックモデルによるクラスタリングにより実現する


# データロード
load("data/books.rda")

# 確認
books %>% print()


# 書籍データの作成
by_chapter <-
  books %>%
    group_by(title) %>%
    mutate(chapter = cumsum(str_detect(text, regex("^chapter ", ignore_case = TRUE)))) %>%
    ungroup() %>%
    filter(chapter > 0) %>%
    unite(document, title, chapter)

# 整理データに変換
by_chapter_word <-
  by_chapter %>%
    unnest_tokens(word, text)

# 単語カウント
word_counts <-
  by_chapter_word %>%
    anti_join(stop_words) %>%
    count(document, word, sort = TRUE) %>%
    ungroup()

# データ確認
word_counts %>% print()


# 5 図書館荒らし：章を対象とするLDA -------------------------------------------------

# データ変換
# --- LDAを実行するためDocumentTermMatrixに変換する
chapters_dtm <-
  word_counts %>%
    cast_dtm(document, word, n)

# データ確認
chapters_dtm %>% print()
chapters_dtm %>% class()

# モデル構築
# --- 4トピックのLDAモデル
chapters_lda <- LDA(chapters_dtm, k = 4, control = list(seed = 1234))
chapters_lda %>% print()


# 単語-トピック確率の算出
# --- beta
chapter_topics <- chapters_lda %>% tidy(matrix = "beta")
chapter_topics %>% print()

# トピックごとの上位単語を抽出
top_terms <-
  chapter_topics %>%
    group_by(topic) %>%
    top_n(5, beta) %>%
    ungroup() %>%
    arrange(topic, -beta)

# データ確認
top_terms %>% print()

# プロット作成
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()


# 6 図書館荒らし：文書ごとの分類 -------------------------------------------------

# 文書-トピック確率の算出
chapters_gamma <- chapters_lda %>% tidy(matrix = "gamma")
chapters_gamma %>% print()

# データ加工
# --- タイトルとチャプターを分離
chapters_gamma <-
  chapters_gamma %>%
    separate(document, c("title", "chapter"), sep = "_", convert = TRUE)

# プロット作成
# --- 文書ごとにトピックの出現頻度を確認
# --- それぞれのトピックに対する純度を示す
chapters_gamma %>%
  mutate(title = reorder(title, gamma * topic)) %>%
  ggplot(aes(x = factor(topic), y = gamma)) +
  geom_boxplot() +
  facet_wrap(~ title) +
  labs(x = "topic", y = expression(gamma))

# トピック抽出
# --- タイトルとチャプターごと
chapter_classifications <-
  chapters_gamma %>%
    group_by(title, chapter) %>%
    slice_max(gamma) %>%
    ungroup()

# 確認
chapter_classifications %>% print()


book_topics <-
  chapter_classifications %>%
    count(title, topic) %>%
    group_by(title) %>%
    top_n(1, n) %>%
    ungroup() %>%
    transmute(consensus = title, topic)

chapter_classifications %>%
  inner_join(book_topics, by = "topic") %>%
  filter(title != consensus)


# 7 図書館荒らし：単語ごとの分類 -------------------------------------------------

# 単語分類の取得
# --- 各文書のどの単語がどのトピックに分類されるかを確認
assignments <- chapters_lda %>% augment(data = chapters_dtm)

# データ整理
assignments <-
  assignments %>%
    separate(document, c("title", "chapter"),
             sep = "_", convert = TRUE) %>%
    inner_join(book_topics, by = c(".topic" = "topic"))

# データ確認
assignments %>% print()

# プロット作成
# --- 混合行列の作成（正解データと照らし合わせて作成）
assignments %>%
  count(title, consensus, wt = count) %>%
  mutate(across(c(title, consensus), ~str_wrap(., 20))) %>%
  group_by(title) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(consensus, title, fill = percent)) +
  geom_tile() +
  scale_fill_gradient2(high = "darkred", label = percent_format()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        panel.grid = element_blank()) +
  labs(x = "Book words were assigned to",
       y = "Book words came from",
       fill = "% of assignments")

# 分類ミスの抽出
wrong_words <-
  assignments %>%
    filter(title != consensus)

# 上位単語の確認
wrong_words %>%
  count(title, consensus, term, wt = count) %>%
  ungroup() %>%
  arrange(desc(n))


word_counts %>%
  filter(word == "flopson")


# 8 LDAの他の実装 -----------------------------------------------------------

# create a vector with one string per chapter
collapsed <-
  by_chapter_word %>%
    anti_join(stop_words, by = "word") %>%
    mutate(word = str_replace(word, "'", "")) %>%
    group_by(document) %>%
    summarize(text = paste(word, collapse = " "))

# create an empty file of "stopwords"
file.create(empty_file <- tempfile())
docs <- mallet.import(collapsed$document, collapsed$text, empty_file)

mallet_model <- MalletLDA(num.topics = 4)
mallet_model$loadDocuments(docs)
mallet_model$train(100)


# word-topic pairs
mallet_model %>% tidy()

# document-topic pairs
mallet_model %>% tidy(, matrix = "gamma")

# column needs to be named "term" for "augment"
term_counts <- word_counts %>% rename(term = word)
augment(mallet_model, term_counts)
