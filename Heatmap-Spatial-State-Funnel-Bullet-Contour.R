#R Heat Map
library(dplyr)
library(ggplot2)


ds <- diamonds %>%
  dplyr::group_by(cut,color)%>%
  dplyr::tally()

pl <- ggplot(data = ds,aes(x = cut, y = color, fill = n))
pl <- pl + geom_tile()
pl <- pl + theme_minimal()
pl <- pl + scale_fill_gradient(low="white", high="blue")
pl <- pl + labs(title = "Heatmap")
pl <- pl + labs(x ="Cut of diamonds", y = "Color of diamonds")
pl <- pl + labs(subtitle = "Cut and color of diamonds")
pl <- pl + labs(caption = paste0("n=", prettyNum(sum(ds$n), big.mark = ",")))

pl


#R Spatial Chart
library(tidyverse)
library(maps)
library(mapproj)
library(dbplyr)

?map_data()

world_tbl <- map_data("world") %>%
  as_tibble()

world_tbl

world_base <- world_tbl %>%
  ggplot() +
  geom_map(
    aes(long, lat, map_id = region),
    map = world_tbl,
    color = "gray80", fill = "gray30", size = 0.3
  )

world_base

world_base + 
  coord_map("ortho", orientation = c(39, -98, 0))


#state map
usa_tbl <- map_data("state") %>% as_tibble()
usa_tbl %>%
  ggplot(aes(long, lat, map_id = region)) +
  geom_map(
    map = usa_tbl,
    color = "gray80", fill = "gray30", size = 0.3
  ) +
  coord_map("ortho", orientation = c(39, -98,0))
  
usa_tbl

republican_voting_tbl <- maps::votes.repub %>%
  as_tibble(rownames = "state") %>%
  select(state, '1976') %>%
  rename(repub_prop = '1976') %>%
  mutate(repub_prop = repub_prop / 100) %>%
  mutate(state = str_to_lower(state))

republican_voting_tbl


usa_voting_tbl <- usa_tbl %>%
  left_join(republican_voting_tbl, by = c("region" = "state"))

usa_voting_tbl

usa_voting_tbl %>%
  ggplot(aes(long, lat, group = subregion)) +
  geom_map(
    aes(map_id = region),
    map = usa_tbl,
    color = "gray80", fill = "gray30", size = 0.3
  ) +
  coord_map("ortho", orientation = c(39, -98, 0)) +
  geom_polygon(aes(group = group, fill = repub_prop), color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                      midpoint = 0.5, labels = scales::percent) +
  theme_minimal() +
  labs(
      title = "Voting Republican in 1976",
      x = "", y = "", fill = ""
  ) +
  theme(
    plot.title = element_text(size = 26, face = "bold", color = "red3"),
    legend.position = "bottom"
  )






#R Funnel or Violin

library(ggplot2)
library(dplyr)


comorbidity <- c('asthma', 'cardio', 'diabetes', 'downsyn', 'hematologic',
                'immuno','neurological', 'obesity', 'pneumopathy',
                'puerperium', 'renal')


cases <-       c(145, 2768, 2369, 36, 179,289,198, 168, 44, 318, 14)


deaths <-   c(42, 1069 , 914 ,13, 63,  127, 62,  81,  6 ,137 ,7)

df <- data.frame(comorbidity, cases, deaths)
df

df2 <- df%>%
  dplyr::mutate(DeathRatebyComorb =  deaths/cases
                , OverallDeathRate = sum(deaths)/sum(cases)
                , se = sqrt( OverallDeathRate * ((1- OverallDeathRate) /cases))
                , lcl95 = OverallDeathRate   - (1.96* se)
                , ucl95 = OverallDeathRate   + (1.96* se)
                , lcl99.7 = OverallDeathRate - (3* se)
                , ucl99.7 = OverallDeathRate + (3* se)

  )
df2                
                
pl <- ggplot(data = df2, aes(x = cases, group =OverallDeathRate))

pl <- pl + geom_smooth(aes(y =lcl95),se = FALSE,linetype ="solid",color = "red", size = 0.5)
pl <- pl + geom_smooth(aes(y =ucl95),se = FALSE, linetype ="solid",color = "red", size = 0.5)

pl <- pl + geom_smooth(aes(y =lcl99.7),se = FALSE, linetype ="solid",color = "blue", size = 0.5)
pl <- pl + geom_smooth(aes(y =ucl99.7),se = FALSE, linetype ="solid",color = "blue", size = 0.5)

pl <- pl + geom_smooth(aes(y =OverallDeathRate),se = FALSE, color = "blue")

pl <- pl + geom_point(aes(y =DeathRatebyComorb), color ="red")

pl <- pl + geom_text(aes(y =DeathRatebyComorb,label=comorbidity),color = "blue",size = 3,hjust= -0.2, vjust= -0.3)


pl <- pl + geom_label(aes(x = 1500, y = .5, label ="red  line shows 95% CI"))
pl <- pl + geom_label(aes(x = 1500, y = .25, label ="blue line shows 97.5% CI"))

pl <- pl +  theme_classic()
pl <- pl + scale_x_continuous(breaks = seq(0, 4000, by=500 ))
pl <- pl + scale_y_continuous(labels = scales::percent)
pl <- pl + labs(title ="Funnel plot showing the mortality rates for various comorbidities")
pl <- pl + labs(x ="Covid cases")
pl <- pl + labs(y= "Mortality rate %")
pl


#R Bullet Chart
library(tidyverse)
tibble(
  name = "Tesla",
  quant_value = 75,
  qualitative = 100
) %>% 
  ggplot(aes(x = quant_value, y = name)) +
  geom_col(aes(x = qualitative), fill = "grey") +
  geom_col(width = 0.5, fill = "black") +
  coord_cartesian(ylim = c(0.3, 1.7)) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())


ex_df <- tibble(
  name = rep("Tesla", 2),
  group = c("Qualitative", "Measure"),
  value = c(100, 75),
  width = c(0.9, 0.5)
)
ex_df %>% 
  ggplot(aes(x = value, y = name, fill = group)) +
  geom_col(width = ex_df$width) +
  coord_cartesian(ylim = c(0.3, 1.7)) +
  scale_fill_manual(values = c("black", "grey")) +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

ex_df <- bind_rows(
  tibble(
    name = rep("Model X", 2),
    group = c("Qualitative", "Measure"),
    color = c("grey", "black"),
    value = c(100, 75),
    width = c(0.9, 0.5),
    target = rep(82, 2),
    ymin = rep(0.7, 2),
    ymax = rep(1.3, 2)
  ),
  tibble(
    name = rep("Model 3", 2),
    group = c("Qualitative", "Measure"),
    color = c("grey", "black"),
    value = c(88, 64),
    width = c(0.9, 0.5),
    target = rep(77, 2),
    ymin = rep(1.7, 2),
    ymax = rep(2.3, 2)
  )
)

ex_df %>% 
  ggplot(aes(x = value, y = name, fill = color)) +
  geom_col(width = c(0.9, 0.5, 0.9, 0.5)) +
  geom_linerange(
    aes(x = target, ymin = ymin, ymax = ymax),
    size = 2, color = "red"
  ) +
  coord_cartesian(ylim = c(0.3, 2.7)) +
  scale_fill_identity() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())



#R Contour
library(ggplot2)
library(tidyverse)
theme_set(theme_light())

#Dataset

set.seed(123)

a <- data.frame(x=rnorm(n = 20000, mean = 10, sd = 1.2),
                y=rnorm(n = 20000, mean = 10, sd =1.2),
                group=rep("A", 20000))
b <- data.frame(x=rnorm(20000, 14.5, 1.2),
                y=rnorm(20000, 14.5, 1.2),
                group=rep("B", 20000))
c <- data_frame(x=rnorm(20000, 9.5, 1.5),
                y=rnorm(20000, 15.5, 1.5),
                group=rep("C", 20000))
data <- rbind(a,b,c)
str(data)


data %>%
  ggplot(aes(x = x,y = y, color = group)) +
  geom_point(size = 1, alpha = 0.5)


data %>%
  ggplot(aes(x=x, y=y) ) +
  geom_density_2d()

data %>%
  ggplot(aes(x=x, y=y) ) +
  geom_density_2d_filled(contour_var = "density")







