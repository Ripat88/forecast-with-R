library(fpp3)
library(tidyverse)
library(readxl)
df <- read_excel('data/raw/ch2.xlsx')
y <- tsibble(
  Year = 2018:2023,
  Observation = c(24, 19, 20, 31, 19, 18),
  index = Year
)
df2 <- read_excel('data/raw/2.xlsx')
df2 %>% 
  mutate(Date = yearmonth(Date)) %>% 
  as_tibble(index=Month)

pbs <- PBS
pbs %>% 
  filter(ATC2 == 'A10') %>% 
  select(Month, Concession, Type, Cost) %>% 
  summarise(TotalC = sum(Cost)) %>% 
  mutate(Cost = TotalC/1e6) -> a10
a10
autoplot(a10, TotalC)
melsyd_economy <- ansett %>% 
  filter(Airports == 'MEL-SYD', Class == 'Economy') %>% 
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers)+
           labs(title = 'Эконом-класс авиакомпании Ансетт',
                subtitle = 'Мельбурн-Сидней',
                y = "Пассажиры (тыс.)",
                x = 'Неделя [1w]')
a10 %>% 
  gg_season(Cost, labels = 'both')+
  labs(y = '$(mln)',
       title = 'Сезонный график: продажи препаратов')
melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
  labs(title = "Эконом-класс авиакомпании Ansett",
       subtitle = "Мельбурн-Сидней",
       y = "Пассажиры (тыс.)",
        x = "Неделя [1W]")

# new plot ----------------------------------------------------------------


vic_elec %>% gg_season(Demand, period= 'day')+
  theme(legend.position = 'none')+
  labs(y='MWh', title='Спрос на электроэнергию: Виктория')


# new plot ----------------------------------------------------------------

vic_elec %>% gg_season(Demand, period='week')+
  theme(legend.position = 'none')+
  labs(y='MWh', title='Спрос на электроэнергию: Виктория')


# new plot ----------------------------------------------------------------

vic_elec %>% gg_season(Demand, period='year')+
  theme(legend.position = 'none')+
  labs(y='MWh', title='Спрос на электроэнергию: Виктория')

a <- vic_elec
a10 %>% 
  gg_subseries(Cost)+
  labs(
    y = '$',
    title = 'Sales'
  )


# новая тема --------------------------------------------------------------

holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))
holidays
x <- tourism
autoplot(holidays, Trips) +
  labs(y = "Поездки с ночевкой (тыс.)",
       title = "Внутренние праздничные дни в Австралии")

# new lesson --------------------------------------------------------------

gg_season(holidays, Trips)+
  labs(y='Поездки с ночевкой (тыс.)',
       title = 'Внутренние праздничные дни в Австралии')

holidays %>% 
  gg_subseries(Trips)+
  labs(y='Поездки с ночевкой (тыс.)',
       title = 'Внутренние праздничные дни в Австралии')

# диаграмма рассеяния -----------------------------------------------------

vic_elec %>% 
  filter(year(Time) == 2014) %>% 
  autoplot(Demand)+
  labs(y='ГВт',
       title='Получасовой спрос на  электроэнергию: Виктория')

vic_elec %>% 
  filter(year(Time) == 2014) %>% 
  autoplot(Temperature)+
  labs(y='Градусы Цельсия',
       title='Получасовой спрос на  электроэнергию: Виктория')

vic_elec %>% 
  filter(year(Time) == 2014) %>% 
  ggplot(aes(x=Temperature, y=Demand))+
  geom_point()+
  labs(x='Температура (градусы Цельсия',
       y='Спрос на электроэнергию (ГВт')

# Корреляция --------------------------------------------------------------

visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))
visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Австралийский внутренний туризм",
       y= "Поездки с ночевкой (тыс.)")

visitors %>% 
  pivot_wider(values_from=Trips,names_from=State) %>%
  GGally::ggpairs(columns = 2:9)


# 2.7 Графики лагов -------------------------------------------------------

recent_production <- aus_production %>% 
  filter(year(Quarter) >= 2000)
recent_production %>% 
  gg_lag(Beer, geom = 'point')+
  labs(x='lag(Beer, k) ')

recent_production1 <- aus_production

recent_production <- aus_production %>% 
  gg_lag(Beer, geom = 'point')+
  labs(x='lag(Beer, k) ')

# Автокорреляция ----------------------------------------------------------

recent_production1 %>% ACF(Beer, lag_max = 9)

recent_production1 %>% 
  ACF(Beer) %>% 
  autoplot() + labs(title = 'Производство пива в Австралии')


# 2.9 Белый шум -----------------------------------------------------------

set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "Белый шум", y = "")
