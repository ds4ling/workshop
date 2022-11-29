

## @knitr create_vocab_table

# Create table -----------------------------------------------------------------
vocab_sample <- vocab_data %>%
  sample_n(., size = 12) %>% 
  select(., x = ages, y = vocab) %>%
  mutate(., x = x %>% round(., 2),
            y = (y / 1000) %>% round(., 2), 
            Vocab_sample = 1:length(x),
            z_x = ((x - mean(x)) / sd(x)) %>% round(., 2),
            z_y = ((y - mean(y)) / sd(y)) %>% round(., 2),
           `(z_x)(z_y)`  = (z_x * z_y) %>% round(., 2)) %>% 
  select(., Vocab_sample, x:`(z_x)(z_y)`)

sample_sums <- colSums(vocab_sample)
sample_means <- colMeans(vocab_sample)
sample_sds <- map_dbl(vocab_sample, sd)


vocab_table1 <- vocab_sample %>% 
  mutate(Vocab_sample = as.character(Vocab_sample)) %>% 
  add_row(.,  Vocab_sample = NA) %>% 
  add_row(.,  Vocab_sample = 'Sum', 
              x = sample_sums[2], 
              y = sample_sums[3], 
              z_x = sample_sums[4] %>% round, 
              z_y = sample_sums[5] %>% round, 
             `(z_x)(z_y)` = sample_sums[6]) %>% 
  add_row(.,  Vocab_sample = 'Mean', 
              x = sample_means[2] %>% round(., 2), 
              y = sample_means[3] %>% round(., 2), 
              z_x = sample_means[4] %>% round(., 2), 
              z_y = sample_means[5] %>% round(., 2)) %>% 
  add_row(.,  Vocab_sample = 'SD', 
              x = sample_sds[2] %>% round(., 2), 
              y = sample_sds[3] %>% round(., 2), 
              z_x = sample_sds[4] %>% round(., 2), 
              z_y = sample_sds[5] %>% round(., 2), 
             `(z_x)(z_y)` = NA)

vocab_table2 <- vocab_sample %>% 
  mutate(Vocab_sample = as.character(Vocab_sample)) %>% 
  add_row(.,  Vocab_sample = NA) %>% 
  add_row(.,  Vocab_sample = 'Sum', 
              x = sample_sums[2], 
              y = sample_sums[3], 
              z_x = sample_sums[4] %>% round, 
              z_y = sample_sums[5] %>% round, 
             `(z_x)(z_y)` = sample_sums[6]) %>% 
  add_row(.,  Vocab_sample = 'Mean', 
              x = sample_means[2] %>% round(., 2), 
              y = sample_means[3] %>% round(., 2), 
              z_x = sample_means[4] %>% round(., 2), 
              z_y = sample_means[5] %>% round(., 2)) %>% 
  add_row(.,  Vocab_sample = 'SD', 
              x = sample_sds[2] %>% round(., 2), 
              y = sample_sds[3] %>% round(., 2), 
              z_x = sample_sds[4] %>% round(., 2), 
              z_y = sample_sds[5] %>% round(., 2), 
             `(z_x)(z_y)` = NA) %>% 
  mutate(., Vocab_sample = cell_spec(Vocab_sample, "html", 
                            color = if_else(is.na(Vocab_sample), "white", "black")), 
             z_x = cell_spec(z_x, "html", 
                            color = if_else(is.na(z_x), "white", "black")), 
             z_y = cell_spec(z_y, "html", 
                            color = if_else(is.na(z_y), "white", "black")),
            `(z_x)(z_y)` = cell_spec(`(z_x)(z_y)`, "html", 
                            color = if_else(is.na(`(z_x)(z_y)`), "white", "black"))) 







