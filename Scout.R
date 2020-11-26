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
champion <- getData_champion(Role = "Mid",
                             Year = 2020, 
                             Split = c("Split 2","Split 2 Playoffs"))

#Crio uma tabela que me retorna com quais campeões os players jogaram na Jungle
jg <- getData_playersChampion(Role = "Jungle", 
                                            Year = 2020, 
                                            Split = c("Split 2",
                                                      "Split 2 Playoffs"))

#Me retorna todos os campeões jogados pelo Shini durante o split
shini_regular <- jg [c(52:59),]
shini_playoff <- jg [c(85:90),]

shini <- bind_rows(shini_playoff,shini_regular)

ggplot(shini, aes(y = g, x = champion, fill = champion)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  ggtitle('Campeões jogados durante o Split - Shini') +
  xlab("Campeões jogados") + 
  ylab("Quantidade de Partidas jogadas")

write.xlsx(shini, file = "Campeões Jogados Pelo Shini Durante o Split.xlsx")
getwd()

#Me retorna todos os campeões jogados pelo Tay durante o split
top <- getData_playersChampion(Role = "Top", 
                              Year = 2020, 
                              Split = c("Split 2",
                                        "Split 2 Playoffs"))

tay_regular <- top [c(62:73),]
tay_playoff <- top [c(85:89),]
tay <- bind_rows(tay_playoff,tay_regular)

ggplot(tay, aes(y = g, x = champion, fill = champion)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  ggtitle('Campeões jogados durante o Split - Tay') +
  xlab("Campeões jogados") + 
  ylab("Quantidade de Partidas jogadas")

write.xlsx(tay, file = "Campeões Jogados Pelo Tay Durante o Split.xlsx")

#Me retorna todos os campeões jogados pelo Envy durante o split
Mid <- getData_playersChampion(Role = "Mid", 
                               Year = 2020, 
                               Split = c("Split 2",
                                         "Split 2 Playoffs"))

envy_regular <- Mid [c(50:58),]
envy_playoff <- Mid [c(84:90),]
envy <- bind_rows(envy_playoff,envy_regular)

ggplot(envy, aes(y = g, x = champion, fill = champion)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  ggtitle('Campeões jogados durante o Split - Envy') +
  xlab("Campeões jogados") + 
  ylab("Quantidade de Partidas jogadas")

write.xlsx(envy, file = "Campeões Jogados Pelo Envy Durante o Split.xlsx")

#Me retorna todos os campeões jogados pelo Micao durante o split
Bot <- getData_playersChampion(Role = "Bot", 
                               Year = 2020, 
                               Split = c("Split 2",
                                         "Split 2 Playoffs"))

micao_regular <- Bot [c(21:29),]
micao_playoff <- Bot [c(85:89),]
micao <- bind_rows(micao_playoff,micao_regular)

ggplot(micao, aes(y = g, x = champion, fill = champion)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  ggtitle('Campeões jogados durante o Split - Micao') +
  xlab("Campeões jogados") + 
  ylab("Quantidade de Partidas jogadas")

write.xlsx(micao, file = "Campeões Jogados Pelo Micao Durante o Split.xlsx")

#Me retorna todos os campeões jogados pelo RedBert durante o split
Support <- getData_playersChampion(Role = "Support", 
                               Year = 2020, 
                               Split = c("Split 2",
                                         "Split 2 Playoffs"))

rb_regular <- Support [c(38:47),]
rb_playoff <- Support [c(81:84),]
rb <- bind_rows(rb_playoff,rb_regular)

ggplot(rb, aes(y = g, x = champion, fill = champion)) +
  geom_bar(stat = "identity", fill = gray(.3)) +
  theme_classic(base_size = 18) +
  ggtitle('Campeões jogados durante o Split - RedBert') +
  xlab("Campeões jogados") + 
  ylab("Quantidade de Partidas jogadas")

write.xlsx(rb, file = "Campeões Jogados Pelo RedBert Durante o Split.xlsx")

#Retorna uma tabela que mostra todos os jogos que a intz foi Red Side
INTZ_g_red <- games[games$red == "INTZ", ]
write.xlsx(INTZ_g_red, file = "Jogos INTZ - Red Side.xlsx")

#Retorna uma tabela que mostra todos os jogos que a intz foi Blue Side
INTZ_g_blue <- games[games$blue == "INTZ", ]
write.xlsx(INTZ_g_blue, file = "Jogos INTZ - Blue Side.xlsx")

#Retorna todos os Bans No Blue Side
s <- strsplit(INTZ_g_blue$ban_blue, split = ",")
bans_b <- data.frame(blue = rep(INTZ_g_blue$blue, sapply(s, length)),
                     ban_blue = unlist(s))

ggplot(bans_b) +
  aes(x = ban_blue) +
  geom_bar(fill = gray(.3)) +
  labs(x = "Campeões Banidos", y = "Quantidade de Bans", title = "Bans Blue Side") +
  theme(axis.text.x = element_text(angle = 90))

#Retorna todos os Bans no Red Side
s_red <- strsplit(INTZ_g_red$ban_red, split = ",")
bans_r <- data.frame(red = rep(INTZ_g_red$red, sapply(s_red, length)),
                     ban_red = unlist(s_red))

ggplot(bans_r) +
  aes(x = ban_red) +
  geom_bar(fill = gray(.3)) +
  labs(x = "Campeões Banidos", y = "Quantidade de Bans", title = "Bans Red Side") +
  theme(axis.text.x = element_text(angle = 90))

#Retorna Todos os Picks do Blue Side 
s_p_b <- strsplit(INTZ_g_blue$pick_blue, split = ",")
pick_b <- data.frame(blue = rep(INTZ_g_blue$blue, sapply(s_p_b, length)),
                     pick_blue = unlist(s_p_b))

ggplot(pick_b) +
  aes(x = pick_blue) +
  geom_bar(fill = gray(.3)) +
  labs(x = "Campeões Pickados", y = "Quantidade de Picks", title = "Picks Blue Side") +
  theme(axis.text.x = element_text(angle = 90))

#Retorna Todos os Picks do Red Side
s_p_red <- strsplit(INTZ_g_red$pick_red, split = ",")
pick_r <- data.frame(red = rep(INTZ_g_red$red, sapply(s_p_red, length)),
                     pick_red = unlist(s_p_red))

ggplot(pick_r) +
  aes(x = pick_red) +
  geom_bar(fill = gray(.3)) +
  labs(x = "Campeões Pickados", y = "Quantidade de Picks", title = "Picks Red Side") +
  theme(axis.text.x = element_text(angle = 90))

#Retorna um grafico com vitoria e derrotas Blue side da INTZ
ggplot(INTZ_g_blue) +
  aes(x = winner) +
  geom_bar(fill = gray(.3)) +
  labs(x = "Ganhadores Blue side", y = "Quantidade de Vitorias", 
       title = "Quantidade de Vitorias no Blue Side") +
  theme_minimal()

#Retorna um grafico com vitoria e derrotas Red side da INTZ
ggplot(INTZ_g_red) +
  aes(x = winner) +
  geom_bar(fill = gray(.3)) +
  labs(x = "Ganhadores Red side", y = "Quantidade de Vitorias", 
       title = "Quantidade de Vitorias no Red Side") +
  theme_minimal()

