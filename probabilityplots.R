## Code prepared by Ullrika and Dmytro

library(tidyverse)
library(tidybayes)
#install.packages("hrbrthemes")
#hrbrthemes::import_roboto_condensed()
library(hrbrthemes)


theme_set(theme_ipsum_rc(grid_col = "grey95"))

#####
## Plotting a subjective probablity distribution
# If we have the exact parametric distribution
df_param <- data.frame(qs=c(0.001, seq(0.01, 0.99, by=0.01), 0.999)) %>%
  mutate(vals_prior = qlnorm(qs,1, 1),
         ds_prior = dlnorm(vals_prior, 1, 1),
         vals_post = qlnorm(qs, 2, 0.5),
         ds_post = dlnorm(vals_post, 2, 0.5))

param_pdf <- df_param %>%
  ggplot(aes(x=vals_prior, y=ds_prior)) +
  geom_line(size=1, alpha=0.2, color="grey50")+
  geom_line(aes(x = vals_post, y = ds_post), size=1, alpha=0.2, color="darkred")+
  coord_cartesian(expand = FALSE) +
  labs(
    title = "a)",
    x = "Parameter",
    y = "pdf"
  )
param_pdf


#####
# Plot a predictive distribution
niter = 10000
sample_param_df <- data.frame(param = rlnorm(niter, 2, 0.5))

df_predictive <- sample_param_df  %>% 
  mutate(vals_predictive=rexp(niter, rate = 1/param))

p_predictive <- df_predictive %>%
  ggplot(aes(x=vals_predictive,y = "")) +
  geom_halfeyeh() +
  #geom_density(size=1, alpha=0.2, color="grey50")+  ## make a density plot of this using tidybayes
 coord_cartesian(expand = FALSE, xlim = c(0,100)) +
  labs(
    title = "b)",
    x = "Variable",
    y = "pdf"
  ) 
p_predictive

# Plot a two-dimensional probability distribution 
ndraws <- 20 ## number of spaghetti straws
df_sample_param_spaghetti <- tibble::tibble(param=sample_param_df[sample.int(niter,ndraws),])
#something is giving an error

df_spaghetti <- df_sample_param_spaghetti %>% 
  mutate(.iter=1:n(),
    q=map(param, ~tibble(qs=c(0.001, seq(0.01, 0.99, by=0.01), 0.999),
                                   vals=qexp(qs, rate = 1/.x),
                                   ds=dexp(vals, rate = 1/.x)))
  ) %>% unnest(q)


p_spaghetti <- df_spaghetti %>%
  mutate(grp=.iter) %>% 
  ggplot(aes(group=grp, x=vals, y=ds)) +
  geom_line(data=. %>% select(-.iter), size=1, alpha=0.2, color="grey50")+
  xlim(0,100) +
  labs(
    title = "c)",
    x = "Variable",
    y = "pdf"
  )
p_spaghetti


threshold = 1
# Plot uncertainty in derived parameter (here the 90% percentile)
p_derived <- sample_param_df  %>% 
#  mutate(vals_derived=qexp(0.9, rate = param)) %>% 
   mutate(vals_derived=1-pexp(threshold, rate = 1/param)) %>% 
  ggplot(aes(x=vals_derived,y=""))+
  geom_halfeyeh() +
  #geom_density()+
  coord_cartesian(expand = FALSE, xlim=c(0,1)) +
    labs(title="d)",
       x="Derived parameter",
       y="pdf")

p_derived


