library(readr)
library(dplyr)
library(lubridate)


# Чтение и подготовка данных

df_users <- read_csv2("train/users.csv", 
                      col_types = cols(
                        chb = "c",
                        age = "c",
                        gender = "c",
                        chit_type = "c"
                      ))

df_users[df_users == "отсутствует"] <- NA
df_users$age <- as.integer(df_users$age)


df_items <- read_csv2("train/items.csv") %>% 
  rowwise %>% 
  mutate(author = paste(unlist(strsplit(gsub("[-(),0-9]", "", author), " ")), collapse = " "))
colnames(df_items)

df_train_trans <- read_csv2("train/train_transactions_extended.csv",
                            col_types = cols(
                              chb = "c",
                              sys_numb = col_character(),
                              date_1 = col_date(format = ""),
                              is_real = col_character(),
                              type = col_character(),
                              source = col_character(),
                              is_printed = col_logical()
                            ))


min(df_train_trans$date_1)
max(df_train_trans$date_1)

# # Убираем дубликаты Читатель-Документ
# df_train_trans <- df_train_trans %>% 
#   group_by(chb, sys_numb) %>% 
#   summarise(date_1 = min(date_1))

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Выбор и оценка методики ----

# Для оценки методики будем предсказывать выбор читателя за три последних известных месяца
# Hазбиваем полный тренировочный датасет на две части

train_date <- ymd("2022-01-01")

# Тренировочный датасет для оценок
df_train_trans_train <- df_train_trans %>% 
  filter(date_1 < train_date)

# Тестовый датасет для оценок. 
# Убираем книги, которые были уже взяты раньше, в тренировочном датасете.
# Убираем дубликаты.
df_train_trans_test <- df_train_trans %>% 
  filter(date_1 >= train_date) %>% 
  select(chb, sys_numb) %>% 
  distinct %>% 
  left_join(df_train_trans_train %>% 
              select(chb, sys_numb) %>% 
              mutate(is_used = T), 
            by = c("chb", "sys_numb")) %>% 
  filter(is.na(is_used)) %>% 
  select(-is_used)


# Функция вычисления метрики
calc_metrics <- function(df_predict, df_real) {
  df_all <- df_real %>% 
    select(chb, sys_numb) %>% 
    distinct %>% 
    mutate(is_real = T) %>% 
    full_join(df_predict %>% 
                select(chb, sys_numb) %>% 
                distinct %>% 
                mutate(is_predict = T))
  
  
  recomend_cnt <- sum(!is.na(df_all$is_predict))
  relev_cnt <- sum(!is.na(df_all$is_predict) & !is.na(df_all$is_real))
  real_cnt <- sum(!is.na(df_all$is_real))
  precision <- relev_cnt / recomend_cnt
  recall <- relev_cnt / real_cnt
  
  if (precision + recall == 0)
    return(0)
  else
    return(2*(precision * recall) / (precision + recall))
}

calc_metrics(df_train_trans_train, df_train_trans_test)
calc_metrics(df_train_trans_test, df_train_trans_test)


# Подход №1. ----
# Простейший вариант - каждому читателю порекомендуем 20 самых популярных книг

# Формируем датасет, на котором будем проводить вычисления

# df_train_trans_oper <- df_train_trans_train  # Для оценки методики - часть датасета
df_train_trans_oper <- df_train_trans  # Для финальных вычислений - полный датасет

# Убираем выбросы - все записи, относящиеся к количеству книг, превышающему 0.95 квантиль.
# 
# # Оценим, сколько книг взял каждый читатель
# df_train_trans_grp_chb <- df_train_trans_oper %>% 
#   count(chb)
# 
# head(sort(df_train_trans_grp_chb$n, decreasing = T), 50)
# 
# q_95 <- quantile(df_train_trans_grp_chb$n, 0.95)
# 
# # Отсеиваем выбросы
# df_train_trans_oper <- df_train_trans_oper %>% 
#   group_by(chb) %>% 
#   mutate(cnt_book = n()) %>% 
#   ungroup %>% 
#   filter(cnt_book < q_95)


# Выбираем 80 самых популярных книг, небольшим запасом. Так как далее потребуется удалить те книги, 
# которые читатель уже брал ранее
df_most_popular <- df_train_trans_oper %>% 
  count(sys_numb) %>% 
  arrange(desc(n)) %>% 
  head(80)

# Каждому пользователю - 80 самых популярных книг
df_result_1 <- df_users %>% 
  select(chb) %>% 
  full_join(df_most_popular %>% select(sys_numb, n), by = character())

# Удаляем то, что было взято раньше
df_result_1 <- df_result_1 %>% 
  left_join(df_train_trans_oper %>% 
              select(chb, sys_numb) %>% 
              mutate(is_used = T), 
            by = c("chb", "sys_numb")) %>% 
  filter(is.na(is_used)) %>% 
  select(-is_used)

# Оставляем не более 20 рекомендаций, в порядке убывания популярности книги

df_result_1 <- df_result_1 %>% 
  group_by(chb) %>% 
  arrange(desc(n)) %>% 
  slice(1:20) %>% 
  ungroup %>% 
  select(chb, sys_numb) %>% 
  arrange(chb)

# Проверим качество рекомендаций

calc_metrics(df_result_1, df_train_trans_test)

# Сохраняем результат
write_csv2(df_result_1, "result_1.csv")
# Score на публичном датасете: Score = 0.000480

# rm(df_result)
# rm(df_most_popular)

# Подход №2. ----
# Предлагаем книги тех авторов, которые читатель уже читал ранее

df_train_trans_oper <- df_train_trans_train  # Для оценки методики - часть датасета
# df_train_trans_oper <- df_train_trans  # Для финальных вычислений - полный датасет

# Строим для каждого пользователя список авторов, которых он читал ранее, 
# подсчитывая сколько раз были взяты книги каждого автора.
df_user_author <- df_train_trans_oper %>% 
  select(chb, sys_numb) %>% 
  inner_join(df_items %>% select(sys_numb, author), by = "sys_numb") %>% 
  select(chb, author) %>% 
  count(chb, author, name = "cnt_author")

# Оставляем только содержательные записи
df_user_author <- df_user_author %>% 
  filter(!is.na(author)) %>% 
  filter(author != "none") %>% 
  filter(author != "отсутствует")

# Для каждой книги подсчитываем, как часто её брали
df_items_book_cnt <- df_items %>% 
  left_join(df_train_trans_oper %>% 
              count(sys_numb, name = "sys_numb_cnt"),
            by = "sys_numb")

df_items_book_cnt$sys_numb_cnt[is.na(df_items_book_cnt$sys_numb_cnt)] <- 0

# Добавляем все книги авторов, которых читатель читал ранее. 
# И оставляем только те книги, которые читатель не читал ранее
df_user_author_book <- df_user_author %>% 
  inner_join(df_items_book_cnt %>% 
               select(sys_numb, author, sys_numb_cnt), 
             by  = "author") %>% 
  left_join(df_train_trans_oper %>% 
              select(chb, sys_numb) %>% 
              mutate(is_used = T), 
            by = c("chb", "sys_numb")) %>% 
  filter(is.na(is_used)) %>% 
  select(-is_used)


# Оставляем для каждого читателя не более 20 рекомендаций
# При этом приоритет у наиболее читаемых читателем авторов, для каждого автора - наиболее популярные книги
df_user_author_book_cut <- df_user_author_book %>% 
  group_by(chb) %>% 
  arrange(desc(cnt_author)) %>% 
  # arrange(desc(cnt_author), desc(sys_numb_cnt)) %>% 
  slice(1:20) %>% 
  ungroup

df_result_2 <- df_user_author_book_cut %>% 
  select(chb, sys_numb) %>% 
  arrange(chb)

# Без sys_numb_cnt - лучше

nrow(df_result_2 %>% count(chb) %>% filter(n < 20))

calc_metrics(df_result_2, df_train_trans_test)
# Score = 0.019120
# 0.003124487

write_csv2(df_result_2, "result_2.csv")

# rm(df_user_author, df_items_book_cnt, df_user_author_book, df_user_author_book_cut, df_result)


# Подход №3. ----
# Предлагаем книги, которые чаще всего берут вместе с теми, которые были взяты читателем ранее

df_train_trans_oper <- df_train_trans_train  # Для оценки методики - часть датасета
#df_train_trans_oper <- df_train_trans  # Для финальных вычислений - полный датасет
# 
# # Оценим, сколько книг взял каждый читатель
# df_train_trans_grp_chb <- df_train_trans_oper %>% 
#   count(chb)
# 
# head(sort(df_train_trans_grp_chb$n, decreasing = T), 50)
# 
# q_95 <- quantile(df_train_trans_grp_chb$n, 0.95)


df_train_trans_oper <- df_train_trans_oper %>%
  select(chb, sys_numb) %>%
  distinct

# Отсеиваем выбросы
df_train_trans_oper_short <- df_train_trans_oper %>% 
  group_by(chb) %>% 
  mutate(cnt_book = n()) %>% 
  ungroup %>% 
  filter(cnt_book < 80)


# Строим датасет, связывающий книги, которые брал один читатель.
# Для каждой книги оставляем не более 20 соответствий с учетом приоритета, заданного количества раз,
# с которым встречается данная связь
df_book_rel <- df_train_trans_oper_short %>% 
  select(chb, sys_numb1 = sys_numb) %>% 
  inner_join(df_train_trans_oper_short %>% select(chb, sys_numb2 = sys_numb)) %>% 
  filter(sys_numb1 != sys_numb2) %>% 
  count(sys_numb1, sys_numb2, name = "cnt_rel") %>% 
  group_by(sys_numb1) %>% 
  arrange(desc(cnt_rel)) %>% 
  slice(1:20) %>% 
  ungroup


# Для каждого читателя составляем список книг, которые связаны с теми книгами, которые он читал ранее.
# Если в рекомендации попадает одна и та же книга несколько раз, то для нее кол-во связей суммируются
df_user_rel <- df_train_trans_oper %>% 
  inner_join(df_book_rel, by = c("sys_numb" = "sys_numb1")) %>% 
  select(-sys_numb) %>% 
  group_by(chb, sys_numb2) %>% 
  summarise(cnt_rel = sum(cnt_rel), .groups = "drop") %>% 
  select(chb, sys_numb = sys_numb2, cnt_rel)

# Удаляем те книги, которые читатель уже читал
df_user_rel <- df_user_rel %>% 
  left_join(df_train_trans_oper %>% 
              select(chb, sys_numb) %>% 
              mutate(is_used = T), 
            by = c("chb", "sys_numb")) %>% 
  filter(is.na(is_used)) %>% 
  select(-is_used)

# Оставляем только 20 книг, приоритет имеют книги с наиболее "сильными" связями
df_user_rel_cut <- df_user_rel %>% 
  group_by(chb) %>% 
  arrange(desc(cnt_rel)) %>% 
  slice(1:20) %>% 
  ungroup

df_result_3 <- df_user_rel_cut %>% 
  select(chb, sys_numb) %>% 
  arrange(chb)

calc_metrics(df_result_3, df_train_trans_test)

write_csv2(df_result_3, "result_3.csv")

# rm(df_book_rel, df_user_rel, df_user_rel_cut, df_result)


# Подход 4. КомбинацияПодхода1, Подхода2 и подхода 3


df_result_4_2 <- df_result_2 %>% mutate(res_type = 2)

df_result_4_3 <- df_result_3 %>% 
  left_join(df_result_2 %>% mutate(is_res_2 = T)) %>% 
  filter(is.na(is_res_2)) %>% 
  select(-is_res_2) %>% 
  mutate(res_type = 3)

df_result_4 <- rbind(df_result_4_2, df_result_4_3)

df_result_4_1 <- df_result_1 %>% 
  mutate(res_type = 4) %>% 
  left_join(df_result_4 %>% mutate(is_res_4 = T)) %>% 
  filter(is.na(is_res_4)) %>% 
  select(-is_res_4)
  
df_result_4 <- rbind(df_result_4, df_result_4_1) %>% 
  group_by(chb) %>% 
  arrange(res_type) %>% 
  slice(1:20) %>% 
  ungroup %>% 
  select(chb, sys_numb) %>% 
  arrange(chb)


write_csv2(df_result_4, "result_4.csv")

