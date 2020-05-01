## Code prepared by Ullrika and Dmytro

library(dplyr)
library(ggplot2)
library(ggstance)
library(purrr)
library(tidyr)
library(tidybayes)
library(cowplot)
library(RColorBrewer)
library(gganimate)
library(hrbrthemes)


theme_set(theme_ipsum_rc(grid_col = "grey95"))
#####
## Plotting a subjective probablity distribution
# If we use sampling

df.par <- data.frame(param = rlnorm(10000,2,0.5))

df.par %>%
  ggplot(aes(y = "",  x = param)) +
  stat_intervalh()+
  scale_color_brewer()+
  labs(
    x = "Parameter",
    y = ""
  )

df.par %>%
  ggplot(aes(y = "",  x = param)) +
  stat_pointintervalh() + 
  scale_color_brewer()+
  labs(
    x = "Parameter",
    y = ""
  )

df.par %>%
  ggplot(aes(y = "",  x = param)) +
  stat_gradientintervalh() +
  scale_color_brewer() +
  labs(
    x = "Parameter",
    y = ""
  )

df.par %>%
  ggplot(aes(y = "",  x = param)) +
  geom_halfeyeh() +
  coord_cartesian(expand = FALSE) +
  scale_color_brewer() +
  labs(
    x = "Parameter",
    y = "pdf for epistemic uncertainty"
  )

#####
## Plotting a subjective probablity distribution
# If we have the exact parametric distribution
df_param <- data.frame(qs=c(0.001, seq(0.01, 0.99, by=0.01), 0.999)) %>%
  mutate(vals = qlnorm(qs, 2, 0.5),
         ds = dlnorm(vals, 2, 0.5))

p_pdf <- df_param %>%
  ggplot(aes(x=vals, y=ds)) +
  geom_line(size=1, alpha=0.2, color="grey50")+
  coord_cartesian(expand = FALSE) +
  labs(
    x = "Parameter",
    y = "pdf for epistemic uncertainty"
  )
p_pdf

## generate data 

#####
# Plotting a two-dimensional probability distribution 
#Generate data for plotting
ndraws <- 20 ## number of spaghetti straws
sample_param_df <- data.frame(lambda = rlnorm(ndraws,2,0.5))
# 2 d plots
df_ale <- sample_param_df  %>% 
  mutate(q=map2(x=lambda, ~tibble(qs=c(0.001, seq(0.01, 0.99, by=0.01), 0.999),
                                   vals=qexp(qs, rate = .x),
                                   ds=dexp(vals, rate = .x)))
  ) %>% unnest(q)

#something is giving an error

p_pdf <- df_ale %>%
  mutate(grp = .iter) %>% 
  ggplot(aes(group=grp, x=vals, y=ds)) +
  geom_line(data=. %>% select(-.iter), size=1, alpha=0.2, color="grey50")+
  coord_cartesian(expand = FALSE) +
  labs(
    title = "The spaghetti plot",
    x = "Varible",
    y = "pdf for aleatory uncertainty"
  )
p_pdf



### didnt change below this point
p_cdf <- df_ale %>% 
  mutate(grp = .iter) %>% 
  ggplot(aes(group=grp, x=vals, y=qs)) +
  geom_line(data=. %>% select(-.iter), size=1, alpha=0.2, color="grey50")+
  stat_ecdf(data = df, aes(x = x, y = NULL, group=NULL), pad = TRUE) + ## adds the empirical cdf for data
  coord_cartesian(expand = FALSE) +
  labs(
    title = "The spaghetti plot",
    x = "X",
    y = "cdf for aleatory uncertainty"
  )
p_cdf

#####
## Plot 2dim with animations
p_pdf_anime <- df_ale %>% 
  mutate(grp = .iter) %>% 
  ggplot(aes(group=grp, x=vals, y=ds)) +
  geom_line(data=. %>% select(-.iter), size=1, alpha=0.2, color="grey50")+
  geom_line(size=1, color="firebrick")+
  coord_cartesian(expand = FALSE) +
  transition_manual(.iter) +
  labs(
    title = "The spaghetti plot",
    x = "X",
    y = "pdf for aleatory uncertainty"
  )

p_pdf_anime
anim_save(p_pdf_anime,file='pdfanime.gif')

p_cdf_anime <- df_ale %>% 
  mutate(grp = .iter) %>% 
  ggplot(aes(group=grp, x=vals, y=qs)) +
  geom_line(data=. %>% select(-.iter), size=1, alpha=0.2, color="grey50")+
  geom_line(size=1, color="firebrick")+
  coord_cartesian(expand = FALSE) +
  transition_manual(.iter) +
  labs(
    title = "The spaghetti plot",
    x = "X",
    y = "pdf for aleatory uncertainty"
  )

p_cdf_anime