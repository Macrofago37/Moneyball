######Moneyball exercise

#Packages
x <- c("Lahman", "tidyverse", "broom", "reshape2", "lpSolve")
(function(x){
  sapply(x, function(x) if(!x %in% installed.packages()){
    install.packages(x, dependencies = T)
  })
  sapply(x, library, character.only=T)
})(x)



# Teams metrics per game
Teams_61_99 <-Teams %>%  filter(yearID %in% 1961:1969) %>% mutate(
                                BB_game=BB/G,
                                R_game=R/G,
                                HR_game=HR/G,
                                Singles_game=(H-X2B-X3B-HR)/G,
                                Doubles_game= X2B/G,
                                Triples_game= X3B/G)

#### Uma argumentação para incluir base on balls como um critério importante para runs
#Singles
Teams_61_99 %>%  lm(R_game~ Singles_game, .) %>% tidy() %>% .[2,2]

##BB
Teams_61_99 %>% summarise(cor=cor(R_game, BB_game, use = "complete.obs"))
Teams_61_99 %>%  lm(R~BB, .) %>% tidy() %>% .[2,2]
##Entretanto, BB estão associadas com HR
cor(Teams_61_99$HR, Teams_61_99$BB, use="complete.obs")
#Assim, ela precisa ser corrigida por HR
Teams_61_99 %>%  lm(R~BB+HR,.) %>% tidy() 
#Mas o efeito dela continua, mesmo sendo corrigida por HR, como pode ser observado no gráfico
Teams_61_99 %>% mutate(HR_game=round(HR_game,1)) %>% 
  select(HR_game, BB_game, R_game) %>% 
  filter(HR_game>=.4 & HR_game<=1.2) %>% #Para remover os extremos e não produzir NAs
  group_by(HR_game) %>% do(tidy(lm(R_game~BB_game,. ), conf.int=T)) %>% 
  filter(term =="BB_game") %>% arrange(desc(estimate)) %>% 
  select(HR_game, estimate, conf.low, conf.high) %>% 
  ggplot(aes(HR_game, estimate, ymin=conf.low, ymax=conf.high)) +
  geom_errorbar()+geom_point()
#conforme pode ser observado os intervalos de confiança se sobrepoe e o slope não muda




#Criando um modelo para predizer
fit <- Teams_61_99 %>% lm(R_game~ HR_game + Singles_game + Doubles_game + Triples_game + BB_game, .) 
fit%>% summary()
#O modelo prediz com 92% de certeza a produção de Runs pela equipe

#Para verificar o modelo, ele será usado para predizer o desempenho de 2002
Teams_2002 <- Teams %>% filter(yearID==2002) %>% mutate(
  BB_game=BB/G,
  R_game=R/G,
  HR_game=HR/G,
  Singles_game=(H-X2B-X3B-HR)/G,
  Doubles_game= X2B/G,
  Triples_game= X3B/G)

Teams_2002 %>% mutate(R_hat=predict(fit,.)) %>% 
  ggplot(aes(R_hat, R_game, label=teamID))+
  geom_point()+
  geom_text(nudge_x = 0.1, cex=2)+
  geom_abline()
#O modelo faz um bom trabalho para 2002!

# A formula seria como:
# R_hat= -2.769 + 0.371* BB + 0.519* singles + 0.771 * doubles + 1.24 * triples + 1.443 *HR

#No entanto, o modelo leva em conta estatística por equipes. Estamos interessados nos jogadores
#Estatística por jogos por jogadores não leva em conta o tempo que eles jogaram
#Uma estatística mais realista é frequência de vezes que eles apareceram na base (PA)
Player_PA <- Batting %>% filter(yearID==2002) %>% 
  group_by(teamID) %>% summarise(pa_per_game= sum(AB+BB)/max(G)) %>% 
  pull(pa_per_game) %>% mean()

players <- Batting %>% filter(yearID %in% 1997:2001) %>% 
  group_by(playerID) %>% 
  mutate(PA=BB+ AB) %>% 
  summarise(G=sum(PA) / Player_PA, #Possivelmente o Player_PA poderia ser substituido pela média dos jogadores entre 1997 e 2001
            BB_game= sum(BB)/G,
            Singles_game=sum(H-X2B-X3B-HR)/G,
            Doubles_game=sum(X2B)/G,
            Triples_game=sum(X3B)/G,
            HR_game=sum(HR)/G,
            AVG=sum(H)/ sum(AB),
            PA=sum(PA)) %>% 
  filter(PA >=1000) %>% #Para remover rookies ou jogadores que jogaram muito pouco
  select(-G) %>% 
  mutate(R_hat=predict(fit, newdata=.))


#Tem bastante variação no desempenho dos jogadores
qplot(R_hat, data = players, binwidth = 0.5, color = I("blue"))



#Adicionando o salário dos jogadores
players <-  Salaries %>%
  filter(yearID==2002) %>%
  select(playerID, salary) %>% 
  right_join(players, by="playerID")
  

#Adicionando a posição
#Os jogadores jogam em diferentes posições,
#assim eles serão filtrados pela posição que mais jogadores

position_names <- 
  paste0("G_", c("p","c","1b","2b","3b","ss","lf","cf","rf", "dh"))

tmp <- Appearances %>% 
  filter(yearID==2002) %>% 
  group_by(playerID) %>% 
  summarise(across(position_names, sum)) %>% 
  ungroup()

pos <- tmp %>% 
  select(position_names) %>% apply(., 1, which.max)

#Adc a base players
players <- tibble(playerID= tmp$playerID,
                  POS= position_names[pos]) %>% 
  mutate(POS=str_to_upper(str_remove(POS, "G_"))) %>% 
  filter(POS != "P") %>% 
  right_join(players, by="playerID") %>% 
  filter(!is.na) %>% 
  filter(!is.na(POS) & !is.na(salary))


#Adc nome dos jogadores
players <- Master %>% select(playerID, nameFirst, nameLast, debut) %>% 
  mutate(debut= as.Date(debut)) %>% 
  right_join(players, by="playerID")


#Top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>% 
  arrange(desc(R_hat)) %>% top_n(10) %>%
  knitr::kable() %>% kableExtra::kable_styling(bootstrap_options = "hover")

players %>% ggplot(aes(salary, R_hat, colour=POS))+
  geom_point()+
  scale_x_log10()







####Testando o efeito sophomore slump

PlayerInfo <- Fielding %>% group_by(playerID) %>% 
  arrange(desc(G)) %>% 
    slice(1) %>%  #Para selecionar a informação da temporada que ele mais jogou
    ungroup() %>% left_join(Master, by="playerID") %>% 
    select(playerID, nameFirst, nameLast, POS)

#Cria uma tabela com apenas os ROY ganhadores  
ROY <- AwardsPlayers %>% filter(awardID== "Rookie of the Year") %>% 
  left_join(PlayerInfo, by="playerID") %>% 
  rename(rookie_year= yearID) %>% 
  right_join(Batting, by="playerID") %>% 
  mutate(AVG= H/AB) %>% 
  filter(POS != "P")

#Mantem apenas os ROY que tiveram uma segunda temporada
ROY <- ROY %>% 
   filter(yearID== rookie_year | yearID== rookie_year+1) %>%  # O | acaba funcionando como um "e" satisfaça condição e depois a condição b
   group_by(playerID) %>% 
   mutate(rookie= ifelse(yearID== min(yearID), "rookie", "sophomore")) %>% 
   filter(n()==2) %>% #Para selecionar apenas o ano rookie e sophomore o n quer dizer que dois por grupo são selecionados
   ungroup() %>% 
   select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

#Para ter uma coluna com a AVG do ano rookie e do 2ano
ROY_spread <- ROY %>% pivot_wider(names_from=rookie,values_from = AVG) %>% arrange(desc(rookie))

#Redução média na performance nos jogadores que reduziram a performance
ROY_spread %>% summarise(mean_reduction= mean(sophomore - rookie <=0))




