library(rethinking)
library(flextable)
tibble(draw    = 1:3,
       marbles = 4) %>% 
  mutate(possibilities = marbles ^ draw) %>% 
  flextable()
(
  d <-
    tibble(position = c((1:4^1) / 4^0, 
                        (1:4^2) / 4^1, 
                        (1:4^3) / 4^2),
           draw     = rep(1:3, times = c(4^1, 4^2, 4^3)),
           fill     = rep(c("b", "w"), times = c(1, 3)) %>% 
             rep(., times = c(4^0 + 4^1 + 4^2)))
)
d %>% 
  mutate(denominator = ifelse(draw == 1, .5,
                              ifelse(draw == 2, .5 / 4,
                                     .5 / 4^2))) %>% 
  mutate(position = position - denominator) %>%
  ggplot(aes(x = position, y = draw, fill = fill)) +
  geom_point(shape = 21, size = 3) +
  scale_fill_manual(values  = c("navy", "white")) +
  scale_y_continuous(breaks = 1:3) +
  theme(legend.position = "none",
        panel.grid.minor = element_blank())
# these will connect the dots from the first and second draws
(
  lines_1 <-
    tibble(x    = rep(1:4, each = 4),
           xend = ((1:4^2) / 4),
           y    = 1,
           yend = 2) %>% 
    mutate(x    = x - 0.5,
           xend = xend - 0.5 / 4^1)
)
# these will connect the dots from the second and third draws
(
  lines_2 <-
    tibble(x    = rep((1:4^2) / 4, each = 4),
           xend = (1:4^3) / (4^2),
           y    = 2,
           yend = 3) %>% 
    mutate(x    = x - 0.5 / 4^1,
           xend = xend - 0.5 / 4^2)
)
d %>% 
  ggplot(aes(x = position, y = draw)) +
  geom_segment(data = lines_1,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_segment(data = lines_2,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               linewidth = 1/3) +
  geom_point(aes(fill = fill),
             shape = 21, size = 3) +
  scale_fill_manual(values  = c("navy", "white")) +
  coord_polar() +
  scale_y_continuous(breaks = 1:3) +
  theme_bw()+
  theme(legend.position = "none",
        panel.grid.minor = element_blank())

# building a model

globe_qa_18 <- quap(
  alist(
    w ~ dbinom(9*2, p), #binomial likelihood
    p ~ dunif(0, 1)       # uniform prior
  ),
  data = list(w = 6 * 2)
)

globe_qa_36 <- quap(
  alist(
    w ~ dbinom(9*4, p), #binomial likelihood
    p ~ dunif(0, 1)       # uniform prior
  ),
  data = list(w = 6 * 4)
)
n_grid <- 100

# wrangle
tibble(w = c(6, 12, 24),
       n = c(9, 18, 36),
       s = c(.16, .11, .08)) %>% 
  expand_grid(p_grid = seq(from = 0, to = 1, length.out = n_grid)) %>% 
  mutate(prior = 1,
         m     = .67)  %>%
  mutate(likelihood = dbinom(w, size = n, prob = p_grid)) %>%
  mutate(unstd_grid_posterior = likelihood * prior,
         unstd_quad_posterior = dnorm(p_grid, m, s)) %>%
  group_by(w) %>% 
  mutate(grid_posterior = unstd_grid_posterior / sum(unstd_grid_posterior),
         quad_posterior = unstd_quad_posterior / sum(unstd_quad_posterior),
         n              = str_c("n = ", n)) %>% 
  mutate(n = factor(n, levels = c("n = 9", "n = 18", "n = 36"))) %>% 
  
  # plot
  ggplot(aes(x = p_grid)) +
  geom_line(aes(y = grid_posterior)) +
  geom_line(aes(y = quad_posterior),
            color = "grey50") +
  labs(x = "proportion water",
       y = "density") +
  theme(panel.grid = element_blank()) +
  facet_wrap(~ n, scales = "free")
