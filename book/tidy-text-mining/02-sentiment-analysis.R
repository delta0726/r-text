# ***********************************************************************************************
# Title   : Rによるテキストマイニング
# Chapter : 2 整理データを使ったセンチメント分析
# Date    : 2022/08/01
# Page    : P15 - P33
# URL     : https://github.com/dgrtwo/tidy-text-mining
# ***********************************************************************************************


# ＜概要＞
# - テキストを個別の単語の組み合わせと考えて、テキスト全体の感情内容は個別の単語の感情内容の総和と考える
#   --- 整理データに｢センチメント辞書｣を導入して分析する


# ＜目次＞
# 0 準備
# 1 センチメント辞書
# 2 内部結合を使ったセンチメント分析
# 3 センチメント辞書の比較
# 4 センチメント辞書のPos/Neg比率
# 5 Pos/Negワードのイメージ
# 6 ストップワードの除去とワードクラウド
# 7 単語を超えた単位での評価


# 0 準備 ----------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidytext)
library(textdata)
library(janeaustenr)
library(reshape2)
library(wordcloud)


# 1 センチメント辞書 ----------------------------------------------------------

# ＜ポイント＞
# - テキストのセンチメントを評価するためのテキスト辞書が多数存在する
#   --- {tidytext}では複数の辞書(lexicon)が含まれている
#   --- それぞれデータフレームとして管理されている

# ＜辞書の限界＞
# - 辞書ベースの方法はユニグラムのみに依存する
#   --- 英単語の多くは感情的な意味を持たないので収録される単語数は限られる
#   --- ｢not good｣などの修飾語を反映することができない点に注意


# センチメント辞書
# --- get_sentiments("bing")と同じ
sentiments %>% print()
sentiments %>% group_by(sentiment) %>% tally()

# センチメント辞書の取得
# --- AFINN
afinn <- get_sentiments("afinn")
afinn %>% group_by(value) %>% tally()

# センチメント辞書の取得
# --- AFINN
bing <- get_sentiments("bing")
bing %>% group_by(sentiment) %>% tally()

# センチメント辞書の取得
# --- AFINN
nrc <- get_sentiments("nrc")
nrc %>% group_by(sentiment) %>% tally()

# センチメント辞書の取得
# --- Loughran
loughran <- get_sentiments("loughran")
loughran %>% group_by(sentiment) %>% tally()


# 2 内部結合を使ったセンチメント分析 ---------------------------------------------

# ＜ポイント＞
# - 内部結合(inner_join)でトークンをセンチメント辞書を突合してセンチメントを割り当てる
# - センチメントごとにカウントして文章全体のセンチメントを評価する
#   --- sentiment = positive - negative


# トークンデータの作成
tidy_books <-
  austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text,
                                       regex("^chapter [\\divxlc]",
                                             ignore_case = TRUE)))) %>%
    ungroup() %>%
    unnest_tokens(word, text)

# センチメント辞書
# --- joyのセンチメントを持つ単語のみを抽出
nrc_joy <-
  get_sentiments("nrc") %>%
    filter(sentiment == "joy")

# センチメント分析
# --- トークンをセンチメント辞書と突合
tidy_books %>%
  filter(book == "Emma") %>%
  inner_join(nrc_joy, by = "word") %>%
  count(word, sort = TRUE)


# センチメントの算出
# --- bing辞書を使用
# --- sentiment = positive - negative
jane_austen_sentiment <-
  tidy_books %>%
      inner_join(get_sentiments("bing"), by = "word") %>%
      count(book, index = linenumber %/% 80, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)

# プロット作成
# --- ジェーン・オースティンの小説の感情の流れ
jane_austen_sentiment %>%
    ggplot(aes(x = index, y = sentiment, fill = book)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~book, ncol = 2, scales = "free_x")


# 3 センチメント辞書の比較 -------------------------------------------------------

# ＜ポイント＞
# - どのセンチメント辞書が目的に適しているかを判断する必要がある


# データ取得
pride_prejudice <-
  tidy_books %>%
    filter(book == "Pride & Prejudice")

# センチメント分析
# --- AFINN
afinn <-
  pride_prejudice %>%
    inner_join(get_sentiments("afinn")) %>%
    group_by(index = linenumber %/% 80) %>%
    summarise(sentiment = sum(value)) %>%
    mutate(method = "AFINN")

# データ結合
# --- センチメント辞書との突合（bing / nrc）
bing <-
  pride_prejudice %>%
    inner_join(get_sentiments("bing")) %>%
    mutate(method = "Bing et al.")

nrc <-
  pride_prejudice %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive",
                                         "negative"))) %>%
    mutate(method = "NRC")

# センチメント分析
# --- bing & nrc
bing_and_nrc <-
  bing %>%
    bind_rows(nrc) %>%
    count(method, index = linenumber %/% 80, sentiment) %>%
    spread(sentiment, n, fill = 0) %>%
    mutate(sentiment = positive - negative) %>%
    select(index, sentiment, method)

# プロット作成
# --- 行ごとのセンチメントの推移
# --- 数値水準は異なるが全体の変化は類似している
afinn %>%
  bind_rows(bing_and_nrc) %>%
  ggplot(aes(index, sentiment, fill = method)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~method, ncol = 1, scales = "free_y")


# 4 センチメント辞書のPos/Neg比率 ----------------------------------------------

# ＜ポイント＞
# - 辞書によってポジティブ/ネガティブの割合が異なる
#   --- 辞書自体の格納ワード以外の大きな原因の1つ


# NRC
# --- 59:41
get_sentiments("nrc") %>%
  filter(sentiment %in% c("positive", "negative")) %>%
  count(sentiment) %>%
  mutate(pct = n / sum(n) * 100)

# Bing
# --- 71:29
get_sentiments("bing") %>%
  count(sentiment) %>%
  mutate(pct = n / sum(n) * 100)


# 5 Pos/Negワードのイメージ -----------------------------------------------

# ＜ポイント＞
# - ポジティブとネガティブの上位ワードを目視で確認するのは間違いを避けるのに有効
#   --- 今回は"miss"というネガティブワードがPositiveの上位に入っている（実は女性のmiss）

# ワードカウント
# --- センチメント辞書に突合後
bing_word_counts <-
  tidy_books %>%
    inner_join(get_sentiments("bing")) %>%
    count(word, sentiment, sort = TRUE) %>%
    ungroup()

# 確認
bing_word_counts

# プロット
# --- センチメントごとの上位ワード
# --- ポジティブワードにネガティブのイメージのある"miss"が大量に含まれている（実は女性のmiss）
# --- ストップワードに加えることで異常値処理を実施
bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "Contribution to sentiment",
       y = NULL)


# 6 ストップワードの除去とワードクラウド ----------------------------------------------

# ストップワードの拡張
addtional_stop_words <- tibble(word = "miss", lexicon = "custom")
custom_stop_words <- stop_words %>% bind_rows(addtional_stop_words)

# 確認
custom_stop_words %>% tail()

# ワードクラウドの作成
# --- デフォルトのストップワードで除去後
tidy_books %>%
  anti_join(stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))

# ワードクラウドの作成
# --- カスタムのストップワードで除去後
tidy_books %>%
  anti_join(custom_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))


tidy_books %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("gray20", "gray80"),
                   max.words = 100)


# 7 単語を超えた単位での評価 --------------------------------------------------


# センテンスデータの取得
p_and_p_sentences <-
  tibble(text = prideprejudice) %>%
    unnest_tokens(sentence, text, token = "sentences")

# 確認
p_and_p_sentences$sentence[2]


austen_chapters <-
  austen_books() %>%
    group_by(book) %>%
    unnest_tokens(chapter, text, token = "regex",
                  pattern = "Chapter|CHAPTER [\\dIVXLC]") %>%
    ungroup()

austen_chapters %>%
  group_by(book) %>%
  summarise(chapters = n())

# センチメント辞書
# --- ネガティブワードのみ
bingnegative <-
  get_sentiments("bing") %>%
    filter(sentiment == "negative")

# 文字数カウント
# --- チャプターごと
wordcounts <-
  tidy_books %>%
    group_by(book, chapter) %>%
    summarize(words = n()) %>%
    ungroup()

# ポジティブワード
# --- 各章ごと
tidy_books %>%
  semi_join(bingnegative) %>%
  group_by(book, chapter) %>%
  summarize(negativewords = n()) %>%
  left_join(wordcounts, by = c("book", "chapter")) %>%
  mutate(ratio = negativewords / words) %>%
  filter(chapter != 0) %>%
  top_n(1) %>%
  ungroup()
