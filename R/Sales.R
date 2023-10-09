library(fpp3)
library(tidyverse)
library(readxl)

sales <- read_excel('data/raw/Sales.xlsx')

sales <- sales %>% 
  mutate(Month = yearmonth(date)) %>% 
  mutate(DV = DV/1000) %>% 
  as_tsibble(index = Month)

autoplot(sales, DV)
sales %>% 
  gg_season(DV, labels = 'both')+
  labs(y = 'тонн',

              title = 'Сезонный график продаж ДВ')

# Пермь -------------------------------------------------------------------


perm <- read_excel('data/raw/perm.xlsx')

perm <- perm %>% 
  mutate(Month = yearmonth(date)) %>% 
  mutate(sales = sales / 1000) %>% 
  as_tsibble(index=Month)

# Общий график продаж -----------------------------------------------------

autoplot(perm, sales)+
  labs(title = 'Продажи ДВ в Перми 2016-2023',
       subtitle= 'Продажи со склада',
  y = 'Продажи, тонн',
  x = 'Месяц [1M]')

# Сезонность --------------------------------------------------------------

perm %>% gg_season(sales, period= 'year')+
  theme(legend.position = 'none')+
  labs(y='тонн', title='Продажи ДВ со склада Перми')


perm %>% 
  gg_subseries(sales)+
  labs(
    y = 'тонн',
    title = 'Продажи'
  )


# вся Пермь ---------------------------------------------------------------

perm_sales <- read_excel('data/raw/ps.xlsx')

perm_sales <- perm_sales %>% 
  mutate(Month = yearmonth(date)) %>% 
  mutate(sales = sales / 1000) %>% 
  as_tsibble(key=sku, index=Month)
autoplot(perm_sales, sales)

# new plot ----------------------------------------------------------------

perm_s <- perm_sales %>% 
  group_by(sku) %>% 
  summarise(sales = sum(sales))
perm_s

autoplot(perm_sales, sales)

# new plot ----------------------------------------------------------------

perm_s %>% 
  gg_subseries(sales)+
  labs(
    y = 'тонн',
    title = 'Продажи'
  )


# new plot ----------------------------------------------------------------

perm_s %>% gg_season(sales, period= 'year', labels='right')+
  theme(legend.position = 'none')+
  labs(y='тонн', title='Продажи со склада Перми')
  
perm_s %>% 
  filter(year(Month) >= 2020) %>% 
  gg_season(sales, period= 'year', labels='right')+
  theme(legend.position = 'none')+
  labs(y='тонн', title='Продажи со склада Перми')

# графики подрядов --------------------------------------------------------

perm_s %>% 
  gg_subseries(sales)+
  labs(y='Продажи тонн',
       title = 'Продажи Перми')

# диаграмма рассеяния -----------------------------------------------------

perm_s %>% 
  filter(year(Month) >= 2020) %>% 
  autoplot(sales)+
  labs(y= 'тонн',
       title='Продажи Перми за пару лет')

perm_s %>%
  filter(year(Month) >= 2020) %>% 
  ggplot(aes(x=Month, y=sales))+
  geom_line()+
  facet_grid(vars(sku), scales='free_y')

perm_s %>% 
  pivot_wider(values_from=sales, names_from=sku) %>% 
  GGally::ggpairs(columns=2:6)

# график лагов ------------------------------------------------------------

recent_production <- perm_s %>% 
  filter(sku == 'dv')
recent_production %>% 
  gg_lag(sales, geom = 'point')+
  labs(x='lag(Beer, k) ')


# автокорелляция ----------------------------------------------------------

recent_production %>% ACF(sales, lag_max = 36)
recent_production %>% 
  ACF(sales, lag_max = 6) %>% 
  autoplot() + labs(title = 'Продажи ДВ в Перми')
