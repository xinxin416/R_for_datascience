### 构建随机函数

library(tidyverse)

### 1. 构建随机数据，rnorm，dnorm，并与实际比较
x <- rnorm(1000,mean = 0, sd = 1)
plot(x)

rand.df <- tibble(x = seq(150,200,by = 0.5), y = dnorm(x, mean = 175, sd = 15)) ## 此处y 值有学问
rand.df %>% 
  ggplot() +
  geom_line(aes(x,y)) 

tibble(x = rnorm(1000, mean = 0, sd = 1)) %>% 
  ggplot() +
  geom_density(aes(x))+
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "red")

### 公式表述，y=4+3.2x ,考虑到可能随机误差，设置相关参数
beta_0 <- 4
beta_1 <- 3.2
epsilon <- rnorm(n = 1000, mean = 0, sd = 1)

sim_normal <- tibble(
  # x_vals = runif(1000, 0, 10)
  x_vals = seq(from = 0, to = 5, length.out = 1000),
  y_vals = beta_0 + beta_1 * x_vals + epsilon
)

sim_normal %>% head()

### 或者更简洁方式，总的误差存在于Y值，
tibble(a = runif(1000,0,5), b = 4 + rnorm(1000, mean = 3.2 * a,sd = 1) ) %>% 
  ggplot()+
  geom_point(aes(a,b))

### 蒙特卡洛抽样、多元高斯分布模拟。暂定

### 2.抽样分布
true.mean = 175.7
true.sd = 15.19

pop.dens <- tibble( height = seq(100,250,0.5),  ## 100,250 范围
        density = dnorm(height, mean = true.mean, sd = true.sd)) 

ggplot(pop.dens) +
geom_line(aes(height, density ))+
geom_vline(xintercept = true.mean,color = "red")+
geom_vline(xintercept = true.mean-true.sd,color = "blue",linetype = "dashed")+
geom_vline(xintercept = true.mean+true.sd,color = "blue",linetype = "dashed")
 
## 完成一次抽样 
sample.a <- tibble( height = rnorm(30,mean = true.mean,sd = true.sd))
sample.a %>% 
  ggplot(aes(x = height)) +
  geom_histogram(aes(y = stat(density)),fill = "steelblue", alpha = 0.7, bins = 10) +
  geom_line(data = pop.dens,aes(x = height, y = density))

## 统计均值、方差
sample.a %>% 
  summarise( a.mean = mean(height), a.sd = sd(height))

### 完成2500 次抽样，就散均值、方差，自定义函数
func.sample <- function(nums,mu,sigma){
  sample.x <- rnorm(nums,mean = mu,sd = sigma)
  tibble(
    sample.size = nums,
    sample.mean = mean(sample.x),
    sample.sd = sd(sample.x)
  )
}

func.sample(30,true.mean,true.sd)

### purrr rerun 函数，运行2500次
sample.of.30 <- purrr::rerun(2500,func.sample(30,true.mean,true.sd)) %>% 
  dplyr::bind_rows()  ## rbind, 返回一个矩阵。bind_rows 返回相同的输入数据格式，形同do.call(rbind,dfs)

head(sample.of.30)
  
sample.of.30 %>% 
  ggplot() +
  geom_histogram(aes(x = sample.mean, y = stat(density)),bins = 30, fill = "red",alpha = .5)+
  geom_vline(xintercept = true.mean, color = 'red', linetype = 'dashed')

### 同时绘制一次抽样样本、样本均值、真实值分布
sample.of.30 %>% 
  ggplot() +
  geom_histogram(aes(x = sample.mean, y = stat(density)),bins = 50, fill = "firebrick",alpha = .5)+
  geom_histogram(data = sample.a, aes(x = height, y = stat(density)), bins = 10,fill = "steelblue",alpha = .7)+
  geom_line(data = pop.dens, aes(x = height, y = density),alpha=0.5,size = 1.2)+
  geom_vline(xintercept = true.mean, color = 'red',linetype = 'dashed')+
  xlim(125,225)

func.sample(30,true.mean,true.sd) ## 再次抽样仍落在样本均值区间

### 增大抽样样本量，比较样本均值差异
sample.of.50 <- purrr::rerun(2500,func.sample(50,true.mean,true.sd)) %>% 
  dplyr::bind_rows()  ## rbind, 返回一个矩阵。bind_rows 返回相同的输入数据格式，形同do.call(rbind,dfs)

sample.of.100 <- purrr::rerun(2500,func.sample(100,true.mean,true.sd)) %>% 
  dplyr::bind_rows()  ## rbind, 返回一个矩阵。bind_rows 返回相同的输入数据格式，形同do.call(rbind,dfs)

sample.of.250 <- purrr::rerun(2500,func.sample(250,true.mean,true.sd)) %>% 
  dplyr::bind_rows()  ## rbind, 返回一个矩阵。bind_rows 返回相同的输入数据格式，形同do.call(rbind,dfs)

sample.of.500 <- purrr::rerun(2500,func.sample(500,true.mean,true.sd)) %>% 
  dplyr::bind_rows()  ## rbind, 返回一个矩阵。bind_rows 返回相同的输入数据格式，形同do.call(rbind,dfs)

sample.nums <- bind_rows(
  sample.of.30,
  sample.of.50,
  sample.of.100,
  sample.of.250,
  sample.of.500
) %>% mutate(sz = factor(sample.size))
head(sample.nums)

sample.nums %>% 
  ggplot() +
    geom_histogram(aes(x = sample.mean, y = stat(density),fill = sz),bins = 25, alpha = .5)+
    geom_vline(xintercept = true.mean, color = 'red',linetype = 'dashed')+
    facet_wrap(vars(sz),nrow = 1)+
    scale_fill_brewer(palette = "Set1")+
    labs(
      title = "Distribution of mean heights for samples of varying size",
      x = "Sample means", y = "Density"
    )

### 比较抽样的平均值的标准差和理论平均值的标准差
df.sample.sd.means <- sample.nums %>% 
  group_by(sample.size) %>% 
  summarize(
    sd.of.means = sd(sample.mean),
    mean.of.means = mean(sample.mean)) 
df.sample.sd.means

df.theory.sample.sd <- tibble(
  size = seq(10,500,10),
  sd.means = true.sd/sqrt(size)
)
head(df.theory.sample.sd)

df.sample.sd.means %>% 
  ggplot(aes(x = sample.size, y = sd.of.means))+
  geom_point()+
  geom_line(data = df.theory.sample.sd, aes(x = size, y = sd.means),color = 'red')+
  labs(
    x = "Sample size", y = "Std Error of Mean",
    title = "平均值标准误差随样本大小变化（理论值和模拟值对比）"
  )

df.sample.sd.sd <- sample.nums %>% 
  group_by(sample.size) %>% 
  summarize(
    sd.of.sd = sd(sample.sd),
    mean.of.sd = mean(sample.sd)) 
df.sample.sd.sd  ### 随着样本量的增大，抽样方差的方差也越小，不确定性越小

