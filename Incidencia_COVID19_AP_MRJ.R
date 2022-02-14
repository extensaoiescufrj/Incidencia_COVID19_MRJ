#############################
#
# Universidade Federal do Rio de Janeiro - UFRJ
# Instituto de Estudos em Saúde Coletiva - IESC
# Projeto de extensão "Apoio às ações de vigilância epidemiológica no enfrentamento da epidemia de COVID-19"
# Título: Análise comparativa dos casos confirmados por COVID-19 no município do Rio de Janeiro 
# por Área Programática (AP)
# Autores: Mariana Costa, Matheus Matos, Lana Meijinhos, Paula Pungartnik e Gabriela Almeida
# Orientadores: Antônio José Leal Costa, Ana Paula Razal e Natália Paiva
#
############################

########## 1º - CHAMANDO OS PACOTES QUE VAMOS UTILIZAR NA BIBLIOTECA

library(tidyverse) # manipular base de dados
library(readxl) # importar dados em excel
library(PHEindicatormethods) # calcular taxas brutas e padronizadas


########## 2º - SELECIONADO O DIRETORIO
# session > set working directory



########## 3º - IMPORTANDO A BASE DE DADOS  

# Casos confirmados de covid-19 no municipio do Rio de Janeiro (MRJ)
# Disponivel em: https://www.data.rio/documents/PCRJ::dados-individuais-dos-casos-confirmados-de-covid-19-no-munic%C3%ADpio-do-rio-de-janeiro-2/about
covid_mrj <- read.csv("db_PainelRioCovid_2909.csv", sep=",", na.strings=c(""," ", "NA")) 


########## 4º - TAXA BRUTA DE INCIDENCIA (TBI) DO MRJ

# Guardando o numero de casos de Covid-19 do MRJ em um vetor
casos.mrj <- c(482364) 

# Guardando a população do MRJ em um vetor
# Estamos utilizando a população dobrada, pois o estudo se trata de casos acumulados de 2020 e 2021
# Ainda não temos a população de 2021 por sexo, faixa etária e AP
pop.total.mrj <- c(13678693) 

# Juntando os casos do MRJ com a pop do MRJ no mesmo objeto (TBI.mrj) para calcular a TBI
TBI.mrj <- bind_cols(casos.mrj, pop.total.mrj) 

# Renomeando as colunas
colnames(TBI.mrj)[1:2]<-c('casos', 'popMRJ' ) 

# Calculando a TBI utilizando a função "phe_rate" do pacote "PHEindicatormethods"
# A taxa será calculada em uma nova coluna chamada 'TBI' no mesmo objeto criado anteriormente 'TBI.mrj'
TBI.mrj$TBI <- phe_rate(TBI.mrj, casos, popMRJ, type = "full", confidence = 0.95, multiplier = 100.000) 

# value = a TBI
# lowercl e uppercl = intervalo de confiança
# confidence = 95% de confiança


########## 5º - TAXA BRUTA DE INCIDENCIA POR AP

# Criando objeto com os casos de Covid-19 por AP e nomeando como 'casos.ap'
casos.ap <- covid_mrj %>%
  group_by(ap_residencia_estadia) %>% 
  tally

# Excluindo linha de ignorado
casos.ap <- casos.ap[(-11),] 

# Importando banco de dados com a população do MRJ por AP
pop.ap <- read_excel("populacao_MRJ.xlsx",  sheet = "AP",
                           range = "A01:B11") 

# Juntando os casos por AP (casos.ap) com a pop por AP (pop.ap) no objeto nomeado de'TBI.ap'
TBI.ap <- bind_cols(casos.ap, pop.ap) 

# Excluindo coluna indesejada
TBI.ap <- TBI.ap[, -c(3)] 

# Calculando a TBI por AP utilizando a função "phe_rate" do pacote "PHEindicatormethods" 
# A taxa será calculada em uma nova coluna chamada 'TBI' no mesmo objeto criado anteriormente 'TBI.ap'
TBI.ap$TBI <- phe_rate(TBI.ap, n, POP, type = "full", confidence = 0.95, multiplier = 100.000)


########## 6º - TAXA BRUTA DE INCIDENCIA POR SEXO

# Renomeando as categorias da variavel sexo para 'Feminino' e 'Masculino'
# Tudo que iniciar com F ou f recebe Feminino
# Tudo que iniciar om M ou m recebe Masculino
covid_mrj$sexo[str_sub(covid_mrj$sexo, 1,1) == "F" | str_sub(covid_mrj$sexo, 1,1) == "f"] <- "Feminino"
covid_mrj$sexo[str_sub(covid_mrj$sexo, 1,1) == "M" | str_sub(covid_mrj$sexo, 1,1) == "m"] <- "Masculino"
covid_mrj$sexo[covid_mrj$sexo != "Feminino" & covid_mrj$sexo != "Masculino"] <- "Ignorado"

# Verificar
table(covid_mrj$sexo)

# Criando objeto com os casos de Covid-19 por sexo e nomeando de 'casos.sexo'
casos.sexo <- covid_mrj %>% 
  group_by(sexo) %>%
  tally

# Excluindo linha de ignorados
casos.sexo <- casos.sexo[(-2),] 

# Importando banco de dados com a população do MRJ por sexo
pop.sexo <- read_excel("populacao_MRJ.xlsx",  sheet = "Sexo",
                       range = "A01:B03") 

# Juntando os casos por sexo (casos.sexo) com a pop por sexo (pop.sexo) no objeto nomeado de 'TBI.sexo'
TBI.sexo <- bind_cols(casos.sexo, pop.sexo) 

# Excluindo coluna indesejada
TBI.sexo <- TBI.sexo[, -c(3)] 

# Calculando a TBI utilizando a função "phe_rate" do pacote "PHEindicatormethods" 
# A taxa será calculada em uma nova coluna chamada 'TBI' no mesmo objeto criado anteriormente 'TBI.sexo'
TBI.sexo$TBI <- phe_rate(TBI.sexo, n, pop, type = "full", confidence = 0.95, multiplier = 100.000)


########## 7º - TAXA BRUTA DE INCIDENCIA POR FAIXA ETARIA

# Vericando a variavel 'faixa_etaria'
summary.factor(covid_mrj$faixa_etaria)

# Temos 3530 valores NA'S
# colocando NA's na categoria "Ignorado"
covid_mrj$faixa_etaria[is.na(covid_mrj$faixa_etaria)] <- "Ignorado" 

# Recodificando os intervalos das faixas etárias
covid_mrj <- covid_mrj %>%
  mutate(faixa_etaria = recode(faixa_etaria, `De 0 a 9` = "De 0 a 19"),
         faixa_etaria = recode(faixa_etaria, `De 10 a 19` = "De 0 a 19"),
         faixa_etaria = recode(faixa_etaria, `De 20 a 29` = "De 20 a 39"),
         faixa_etaria = recode(faixa_etaria, `De 30 a 39` = "De 20 a 39"),
         faixa_etaria = recode(faixa_etaria, `De 40 a 49` = "De 40 a 59"),
         faixa_etaria = recode(faixa_etaria, `De 50 a 59` = "De 40 a 59"),
         faixa_etaria = recode(faixa_etaria, `De 90 a 99` = "De 90 ou +"),
         faixa_etaria = recode(faixa_etaria, `Maior de 100` = "De 90 ou +")) 

# Verificar
summary.factor(covid_mrj$faixa_etaria)

# Criando objeto com os casos de Covid-19 por faixa etaria e nomeando de 'casos.fxetaria'
casos.fxetaria <- covid_mrj %>% 
  group_by(faixa_etaria) %>%
  tally

# Excluindo linha de ignorado
casos.fxetaria <- casos.fxetaria[(-8),] 

# Importando banco de dados da população do MRJ por faixa etaria
pop.fxetaria <- read_excel("populacao_MRJ.xlsx",  sheet = "faixa etaria",
                           range = "A01:B08")

# Juntando OS casos por faixa etaria (casos.gxetaria) com a pop por faixa etaria (pop.fxetaria)
# no objeto nomeado de 'TBI.fxetaria'
TBI.fxetaria <- bind_cols(casos.fxetaria, pop.fxetaria)

# Excluindo coluna indesejada
TBI.fxetaria <- TBI.fxetaria[, -c(3)] 

# Calculando a TBI utilizando a função "phe_rate" do pacote "PHEindicatormethods" 
# A taxa será calculada em uma nova coluna chamada 'TBI' no mesmo objeto criado anteriormente 'TBI.fxetaria'
TBI.fxetaria$TBI <- phe_rate(TBI.fxetaria, n, POP, type = "full", confidence = 0.95, multiplier = 100.000)


########## 8º - TAXA PADRONIZADA DE INCIDENCIA (TPI) POR AP

# Classificando a variavel AP de residencia como factor
covid_mrj$ap_residencia_estadia <- factor(covid_mrj$ap_residencia_estadia)

# Criando objeto com os casos de Covid-19 por faixa etaria e AP e nomeando de 'casos.ap.fxetaria'
casos.ap.fxetaria<- covid_mrj %>%
  group_by(ap_residencia_estadia, faixa_etaria, .drop=FALSE) %>%
  tally

# Excluindo linhas de ignorado
casos.ap.fxetaria <- casos.ap.fxetaria[c(-8,-16, -24,-32, -40,-48,-56,-64,-72,-80,-81,-82,-83,
                                  -84,-85,-86,-87,-88),] 


# Importando banco de dados com a população do MRJ por AP e faixa etaria
pop.ap.fxetaria <- read_excel("populacao_MRJ.xlsx",  sheet = "AP e fxetaria") 

# Juntando os casos (casos.ap.fxetaria) com a pop (pop.ap.fxetaria) no objeto nomeado de 'TPI.aux'
TPI.aux <- bind_cols(casos.ap.fxetaria, pop.ap.fxetaria)

# Verificar se estao na mesma ordem
View(TPI.aux) # sim, estao

# Excluindo colunas indesejadas
TPI.aux <- TPI.aux[, -c(4,5)] 

# Criando vetor com a populacao padrao por faixa etaria (MRJ em 2020) para usar no calculo da TPI 
popmrj <- c(1520197, 2157159, 1860127, 684377, 389762, 173382, 46338)

names(popmrj)<- c('0 a 19', '20 a 39', '40 a 59', '60 a 69', '70 a 79', '80 a 89' ,'90 ou mais')

# Repetindo os dados 10 vezes pq são 10 Áreas Programáticas
# O vetor ficará com a mesma quantidade de linhas (70) do objeto 'TPI.aux', desse jeito podemos juntar os dados
popmrj <- rep(popmrj, 10) 

# Incluindo os dados da pop padrao (popmrj) em 'TPI.aux'
TPI.aux$popmrj <- bind_cols(popmrj)

# Renomeando as colunas
colnames(TPI.aux)[1:5]<-c('AP', 'Faixa etaria', 'Casos', 'pop', 'popMRJ' ) 

# Calculando a TPI utilizando a função "phe_dsr" do pacote "PHEindicatormethods" e nomeando o objeto de 'TPI.ap'
TPI.ap <- TPI.aux %>%
  group_by(AP)%>% # 
  phe_dsr(x = Casos, n = pop, type = "standard", 
          stdpop = popMRJ, stdpoptype = "field")


########## 9º - GRAFICOS

# Os gráficos foram realizados com o pacote 'ggplot2' que está incluso no pacote 'tidyverse'

# Criando um objeto 'tema' para padronizar o layout dos graficos
# tema do grafico (theme_classic), fonte (family), tamanho (size) e cor (black) dos elementos do grafico 

tema <- theme_classic() +
  theme(plot.caption = element_text(family="serif", size=7, color = "black"),
        axis.title=element_text(family="serif", size=15, color = "black"),
        axis.text.x=element_text(family="serif", size=15, color="black"),
        axis.text.y=element_text(family="serif", size=15, color="black"),
        axis.title.y=element_blank())

# Gráfico da TBI por sexo

# As TBI geradas acima foram colocadas em uma planilha do excel para gerar os graficos
# Importando base de dados com a TBI por sexo
graf.sexo <- read_excel("grafico_TBI.xlsx",  sheet = "Sexo",
                           range = "A01:B03")


fig.TBI.sexo <- graf.sexo %>%
  ggplot(aes(Sexo, TBI)) +
  geom_col(fill = "#F9844A", colour = "#F3722C", width = .55, alpha = 0.80) +
  geom_label(aes(label = TBI), size = 5, family = "serif") +
  labs(x = "Sexo", y = "Taxa Bruta de Incidência (100 mil/hab.)", caption = "Fonte: CIEVS/SMS e IPP") +
  coord_flip() +
  tema


fig.TBI.sexo

# Salvar o grafico como png
# Esse comando salva o ultimo gráfico que foi rodado
ggsave("fig.tbi.sexo.png", device = "png", width=10, height =5, dpi = 300)

# Gráfico da TBI por faixa etária

# importando dados com a taxa bruta por faixa etaria
graf.fxetaria <- read_excel("grafico_TBI.xlsx",  sheet = "Fxetaria",
                        range = "A01:B08")

fig.TBI.fxetaria <- graf.fxetaria %>%
  ggplot(aes(Faixa_etaria, TBI)) +
  geom_col(fill = "#F9844A", colour = "#F3722C", width = .65, alpha = 0.80) +
  geom_label(aes(label = TBI), size = 5, family = "serif") +
  scale_y_continuous(breaks = c(0, 1000, 2000, 3000, 4000, 5000)) +
  labs(x = "", y = "Taxa de Incidência Específica por Idade (100 mil/hab.)", caption = "Fonte: CIEVS/SMS e IPP") +
  coord_flip() +
  tema

fig.TBI.fxetaria

# salver o grafico como png
ggsave("fig.tbi.idade.png", device = "png", width=10, height =5, dpi = 300)

