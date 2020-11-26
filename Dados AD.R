install.packages("devtools")
library('devtools')
devtools::install_github("pedrodrocha/cbloldataR")
library(cbloldataR)
library('tidyverse')
install.packages('xlsx')
library('xlsx')

#Crio uma tabela com todos os jogos do segundo Split de 2020
games_second <- getData_games(Year = 2020, Split = c("Split 2","Split 2 Playoffs"))

#Crio uma tabela que me retorna todos os campeões picados no CBLOL
#, segundo split de 2020 e seus status
Bot <- getData_playersChampion(Role = "Bot", 
                               Year = 2020, 
                               Split = c("Split 2",
                                         "Split 2 Playoffs"))
micao_regular <- Bot [c(21:29),]
micao_playoff <- Bot [c(85:89),]
micao <- bind_rows(micao_playoff,micao_regular)

m <- micao %>% filter(kda != 0) %>% summarise_each(funs(mean))

m["Micao"]<-c("Micao")


brtt_regular <- Bot [c(1:11),]
brtt_playoff <- Bot [c(90:93),]
brtt <- bind_rows(brtt_playoff, brtt_regular)

b <- brtt %>% filter(kda != 0) %>% summarise_each(funs(mean))

b["Brtt"]<-c("Brtt")

bvoy <- Bot [c(38:44),]

bvoy <- bvoy %>% filter(kda != 0) %>% summarise_each(funs(mean))

bvoy['Bvoy'] <- c("Bvoy")

finals <- bind_rows(m, b, bvoy)

finals["Jogadores"] <- c("Micao", "Brtt", "Bvoy") 

ggplot(finals, aes(y = kda, x = Jogadores, fill = Jogadores)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  ggtitle('KDA dos Finalistas de melhor AD') +
  xlab("Jogadores") + 
  ylab("KDA") + 
  coord_cartesian(ylim = c(1.0,8.0))

ggplot(finals, aes(y = cs_m, x = Jogadores, fill = Jogadores)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  ggtitle('CS por minuto dos Finalistas de melhor AD') +
  xlab("Jogadores") + 
  ylab("CS por Minuto") +
  coord_cartesian(ylim = c(1.0,10.0))

ggplot(finals, aes(y = g_m, x = Jogadores, fill = Jogadores)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  ggtitle('Gold por minuto dos Finalistas de melhor AD') +
  xlab("Jogadores") + 
  ylab("Gold por Minuto") +
  coord_cartesian(ylim = c(1,500))




