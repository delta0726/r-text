# ***********************************************************************************************
# Title   : Rによるテキストマイニング
# Chapter : 1 整理テキスト形式
# Date    : 2023/01/17
# Page    : P1 - P14
# URL     : https://github.com/dgrtwo/tidy-text-mining
# ***********************************************************************************************


# ＜概要＞
# - {tidytext}はトークンの処理に整理テキスト形式の概念を持ち込むための仕組みが提供されている
# - テキストのトークン処理と、トークンの頻度による文書類似度比較を行う


# ＜用語整理＞
# - ｢整理テキスト形式｣とは、1行に1つのトークンからなる表を定義して｢整理データ｣の概念でデータ処理をすること
# - ｢トークン｣とは、テキストの単位として意味のあるもの（たとえば単語、センテンス、段落）
# - ｢トークン化｣とは、テキストをトークンに分割する処理


# ＜目次＞
# 0 準備
# 1 文章をtidyデータに変換する
# 2 ジェーン・オースティンの作品をtidyデータに変換
# 3 ストップワードの削除


# 0 準備 ---------------------------------------------------------------------------

# ライブラリ
library(tidyverse)
library(tidytext)
library(stringr)
library(janeaustenr)
library(gutenbergr)
library(scales)


# 1 文章をtidyデータに変換する --------------------------------------------------------

# ＜ポイント＞
# - テキストを分析するにあたり、トークン単位にデータ変換する必要がある
# - トークンは分析のために有用な単位のこと（ほとんどの場合は単語単位）
# - トークン化によりテキストをtidyデータとして扱うことができるようになる


# ベクトル形式の文章
# --- 文節ごとにベクトルとして格納
text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

# データフレームに変換
text_df <- tibble(line = 1:4, text = text)

# トークン単位に変換
# --- ストップワード(句読点、記号など)は除外される
# --- テキスト列以外はそのまま残される
# --- デフォルトではトークンを小文字に変換する
text_df %>%
  unnest_tokens(output = word, input = text, token = "words", to_lower = TRUE)


# 2 ジェーン・オースティンの作品をtidyデータに変換 ---------------------------------

# ＜ポイント＞
# - {janeaustenr}に含まれる小説のテキストデータを整理形式に変換する


# データ確認
# --- Text：文章
# --- Book：タイトル
austen_books()
austen_books() %>% slice(100:200)

# データ集計
# --- 6編に分かれている
austen_books() %>% group_by(book) %>% tally()

# データ変換
# --- 行ナンバーを追加（編ごと）
# --- チャプターを追加（正規表現で"chapter"を抽出）
original_books <-
  austen_books() %>%
    group_by(book) %>%
    mutate(linenumber = row_number(),
           chapter = cumsum(str_detect(text,
                                       regex("^chapter [\\divxlc]",
                                             ignore_case = TRUE)))) %>%
    ungroup()

# データ確認
original_books %>% print()

# データ集計
# --- Chapterごとのセンテンス数
# --- Chapterは62個に分かれている
original_books %>%
  group_by(chapter) %>%
  tally()

# トークンに変換
# --- {tokenizers}を使ってデータフレームをトークンに分割している（デフォルトは単語）
tidy_books <-
  original_books %>%
    unnest_tokens(output = word, input = text, token = "words", to_lower = TRUE)

# データ確認
tidy_books %>% print()


# 3 ストップワードの削除 --------------------------------------------------

# ＜ポイント＞
# - ストップワードとは｢the｣｢or｣｢to｣など単独では意味をなさない単語のことをいう
#   --- テキスト分析では多くの場合、ストップワードは削除する


# 上位単語を確認
# --- ストップワードが上位に並んでいる
tidy_books %>% count(word, sort = TRUE)

# ストップワード一覧の取得
# --- {tidytext}で管理されている
data(stop_words)

# 確認
stop_words %>% print()

# ストップワードの削除
tidy_books_ex_stopword <-
  tidy_books %>%
    anti_join(stop_words, by = "word")

# 上位単語を確認
# --- ストップワードが削除された
tidy_books_ex_stopword %>% count(word, sort = TRUE)

# プロット作成
# --- 上位単語のカウント（出現が600回以上）
tidy_books_ex_stopword %>%
  count(word, sort = TRUE) %>%
  filter(n > 600) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)


# 4 単語の出現頻度 -------------------------------------------------------------------

# ＜ポイント＞
# - テキストマイニングでは｢単語の頻度の確認｣や｢文書ごとの単語出現頻度の比較｣を行う
#   --- 以下では上位単語の頻度で文書の類似度を測定する


# データロード
# --- H.G.ウェルズの作品
# --- ブロンテ姉妹の作品
# --- ストップワード辞書
load("data/hgwells.rda")
load("data/bronte.rda")
data(stop_words)

# データ確認
hgwells %>% print()
bronte %>% print()

# tidyデータの作成
# --- トークン化とストップワード削除
tidy_austen <-
  original_books %>%
    unnest_tokens(output = word, input = text) %>%
    anti_join(stop_words, by = "word") %>%
    mutate(author = "Jane_Austen") %>%
    select(word, author)

tidy_hgwells <-
  hgwells %>%
    unnest_tokens(output = word, input = text) %>%
    anti_join(stop_words, by = "word") %>%
    mutate(author = "H_G_Wells") %>%
    select(word, author)

tidy_bronte <-
  bronte %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words, by = "word") %>%
    mutate(author = "Bront_Sisters") %>%
    select(word, author)

# 単語カウント
# --- 上位にいずれも｢time｣｢eyes｣｢hand｣を含んでいる
tidy_austen %>% count(word, sort = TRUE) %>% print()
tidy_hgwells %>% count(word, sort = TRUE) %>% print()
tidy_bronte %>% count(word, sort = TRUE) %>% print()

# 頻度計算
frequency <-
  tidy_austen %>%
    bind_rows(tidy_hgwells) %>%
    bind_rows(tidy_bronte) %>%
    mutate(word = str_extract(word, "[a-z']+")) %>%
    count(author, word) %>%
    group_by(author) %>%
    mutate(proportion = n / sum(n)) %>%
    select(-n) %>%
    spread(author, proportion) %>%
    gather(author, proportion, Bront_Sisters:H_G_Wells)

# プロット作成
frequency %>%
  ggplot(aes(x = proportion, y = Jane_Austen,
              color = abs(Jane_Austen - proportion))) +
  geom_abline(color = "gray40", lty = 2) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  scale_color_gradient(limits = c(0, 0.001),
                       low = "darkslategray4", high = "gray75") +
  facet_wrap(~author, ncol = 2) +
  theme(legend.position="none") +
  labs(y = "Jane Austen", x = NULL)


# 相関テスト
cor.test(data = frequency[frequency$author == "Bront_Sisters",], ~ proportion + Jane_Austen)
cor.test(data = frequency[frequency$author == "H_G_Wells",], ~ proportion + Jane_Austen)
