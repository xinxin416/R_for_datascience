
#install.packages("tidyverse")
#install.packages("ggridges")
library(tidyverse)
library(ggridges)
#library(ggstatsplot) ## lack package v8

penguins <- read.csv("./data/penguins.csv",header = TRUE)
head(penguins)
summary(penguins)
dim(penguins)

### 质控
## 多少个NA值，及其分布
penguins %>% filter_all(any_vars(is.na(.)))
penguins %>% summarise_all(~sum(is.na(.)))

penguin <- penguins %>% filter(!is.na(sex)) ## 所有sex为NA的，包括了其他字段为na的观测
## 或者
penguins %>% drop_na() %>% dim()
penguin %>% summarise(across(everything(),~sum(is.na(.))))

## 1. 每种类型企鹅有多少只？
penguin %>% group_by(species) %>% summarise(ncount = n())
penguin %>% group_by(island) %>% summarise(ncount = n())
## 或者
penguin %>% count(species, sort = TRUE)
penguin %>% count(island, sort = TRUE)


## 2. 每种类型企鹅各种属性的均值和分布？
penguin %>% select(-c(island,sex,year)) %>% group_by(species) %>% 
  summarize(across(everything(),mean))
## 或者
penguin %>% select(species,bill_length_mm:body_mass_g) %>% 
  group_by(species) %>% 
  summarise(across(where(is.numeric),mean))

### 可视化,山脊图，能比较性别差异
penguin %>% select(species,bill_length_mm:sex) %>% 
  pivot_longer(-c(species,sex), names_to = "measures", values_to = "value") %>% 
  ggplot(aes(x = value,y = species, fill = sex)) +
  geom_density_ridges(alpha = 0.5) +
  facet_wrap(vars(measures),scales = "free")

### 盒形图,能辨别总体趋势
penguin %>% select(species,bill_length_mm:sex) %>% 
  pivot_longer(-c(species,sex), names_to = "measures", values_to = "value") %>% 
  ggplot(aes(x = species,y = value,fill = species)) +
  geom_boxplot() +
  facet_wrap(vars(measures),scales = "free")


### 3. 嘴峰长度和深度的关联？
penguin %>%
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(aes(color = species,shape = species,size = body_mass_g)) +
  geom_smooth(method = "lm", aes(color = species)) +
  geom_smooth(method ="lm")

mod1 <- with(penguin,lm(bill_length_mm~bill_depth_mm+species))
print(mod1)
anova(mod1)

### 4. 体重与翅膀长度的关联？
penguin %>% 
  group_by(species,island,sex) %>% 
  ggplot(aes(x = body_mass_g,y = reorder(species,-body_mass_g),color = species)) +
  geom_jitter() +
  stat_summary(fun = mean,geom = "point",size = 5,alpha = 1)

mod2 <- with(penguin,lm(body_mass_g~flipper_length_mm+species))
print(mod2)
anova(mod2)

#### 5. 嘴峰长度与嘴峰深度的比例？
penguin %>% mutate(ratio = bill_length_mm/bill_depth_mm) %>% 
  select(species,ratio) %>% 
  group_by(species) %>% 
  summarise(mean = mean(ratio))

### 可视化
penguin %>% mutate(ratio = bill_length_mm/bill_depth_mm) %>% 
  select(species,ratio) %>% 
  ggplot(aes(x = ratio, y = species,fill = species)) +
  geom_density_ridges()


#### 6. 不同种类的宝宝，体重具有显著性差异？
penguin %>% 
  ggplot(aes(x = species, y = body_mass_g , fill = species)) +
  geom_boxplot()+
  geom_jitter()

### 单因素方差 分析，avo，整体方差,存在显著性，则继续多重比较
penguin %>% stats::aov(body_mass_g~species,data = .) %>% summary()

### 多重比较
penguin %>% stats::aov(body_mass_g~species,data = .) %>% 
  TukeyHSD(which = "species") %>% 
  broom::tidy()

### 可视化
#install.packages("ggpubr")
library("ggpubr")
my_comparisons <- list(c("Adelie","Chinstrap"), c("Gentoo", "Chinstrap"),c("Adelie", "Gentoo"))
                       
compare_means(weight ~ group, data = PlantGrowth,method="kruskal.test")                      

penguin %>% 
  ggboxplot( x = "species", y = "body_mass_g",color = "species",
          palette = "jco", order = c("Adelie", "Chinstrap", "Gentoo"),
          ylab = "body_mass_g", xlab = "species")+
  stat_compare_means(label.y = 6500,method = "anova")+
  stat_compare_means(comparisons = my_comparisons,label.y = c(5000,6000,5500))

#### 7. 这体征中哪个因素对性别影响最大
## 建立模型，首先归一化各个属性，然后编码0,1
fun_scale <- function(x){
  (x-mean(x))/sd(x)
}

d <- penguin %>% 
  select(sex,species,bill_length_mm:body_mass_g) %>% 
  mutate(
    across(where(is.numeric), fun_scale)  ### 函数可以是函数名，或者purrr形式或者列表
  ) %>% 
  mutate(males = if_else(sex == "male",1,0))

head(d)


### sex 变量为二元，构建逻辑回归
mod4 <- glm(males~1 + species+bill_length_mm+bill_depth_mm+flipper_length_mm+body_mass_g,
                          family = "binomial",data = d)
print(mod4)
summary(mod4)


penguin %>% 
  mutate(predic = predict(mod4),
         fitted = fitted(mod4)) %>% 
  head()

## 计算每个变量的平均边际效应
#install.packages("margins")
library(margins)

logit_mod1_m <- mod4 %>% 
  margins() %>% 
  summary() %>% 
  as_tibble()

logit_mod1_m

logit_mod1_m %>%
  ggplot(aes(
    x = reorder(factor, AME),
    y = AME, ymin = lower, ymax = upper
  )) +
  geom_hline(yintercept = 0, color = "gray80") +
  geom_pointrange() +
  coord_flip() +
  labs(x = NULL, y = "Average Marginal Effect")

install.packages("ggeffects")
library(ggeffects)
ggpredict(mod4, terms = "bill_length_mm") 

## mode2
#install.packages("brms")
library(brms)  ## lack package V8


penguins %>%
  group_by(species) %>%
  modelr::data_grid(flipper_length_mm) %>%
  tidybayes::add_fitted_draws(brms_mod3, n = 100) %>%
  ggplot() +
  geom_point(
    data = penguins,
    aes(flipper_length_mm, bill_length_mm, color = species, shape = species)
  ) +
  geom_line(aes(flipper_length_mm, .value, group = interaction(.draw, species), color = species), alpha = 0.1)

install.packages("tidybayes")



