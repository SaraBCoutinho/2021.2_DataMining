library(tidyverse)
library(skimr)
library(patchwork)
library(MASS)

#Pré-processamento ---

#aquisiçao dos dados ---

raw_data <- read.table("data/microdados_enade_2019.txt",
                       header = TRUE, 
                       sep=";", 
                       dec = ",",
                       colClasses=c(DS_VT_ACE_OFG="character",DS_VT_ACE_OCE="character"))

skim(raw_data)

#limpeza dos dados ---

raw_data <- raw_data %>% 
  dplyr::select(CO_IES, CO_CATEGAD, CO_ORGACAD, CO_GRUPO, CO_CURSO, CO_MODALIDADE, CO_MUNIC_CURSO, CO_UF_CURSO, CO_REGIAO_CURSO,
         NU_IDADE, TP_SEXO, ANO_FIM_EM, ANO_IN_GRAD, CO_TURNO_GRADUACAO, TP_INSCRICAO_ADM, TP_INSCRICAO,
         NT_GER, starts_with("QE_I")) %>% 
  dplyr::select(c(1:43))

  skim(raw_data) %>% 
  dplyr::select(skim_variable, n_missing, complete_rate) %>% 
  filter(n_missing > 0)

  raw_data <- raw_data %>% 
  mutate(ANO_IN_GRAD = ifelse(is.na(ANO_IN_GRAD), round(mean(ANO_IN_GRAD)), ANO_IN_GRAD)) %>% 
  na.omit()

dim(raw_data)

#detecçao de outliers ---
p <- raw_data %>% 
  ggplot(aes(x = "", y = ANO_IN_GRAD)) +
  geom_boxplot() + 
  labs(x = "ANO_IN_GRAD", y = "Ano") +
  coord_flip()

q <- raw_data %>% 
  ggplot(aes(x = "", y = ANO_FIM_EM)) +
  geom_boxplot() + 
  labs(x = "ANO_FIM_GRAD", y = "Ano") +
  coord_flip()

r <- raw_data %>% 
  ggplot(aes(x = "", y = NU_IDADE)) +
  geom_boxplot() + 
  labs(x = "NU_IDADE", y = "Anos") + 
  coord_flip()

p / q / r

ano_in_grad_boxplot <- boxplot(raw_data$ANO_IN_GRAD)

ano_fim_em_boxplot <- boxplot(raw_data$ANO_FIM_EM)

nu_idade_boxplot <- boxplot(raw_data$NU_IDADE)

raw_data <- raw_data %>% 
  filter(ANO_IN_GRAD >= ano_in_grad_boxplot$stats[[1]], ANO_IN_GRAD <= ano_in_grad_boxplot$stats[[5]]) %>% 
  filter(ANO_FIM_EM >= ano_fim_em_boxplot$stats[[1]], ANO_FIM_EM <= ano_fim_em_boxplot$stats[[5]]) %>% 
  filter(NU_IDADE >= nu_idade_boxplot$stats[[1]], NU_IDADE <= nu_idade_boxplot$stats[[5]])

dim(raw_data)

#transformaçao de dados ---
raw_data <- raw_data %>%
  rename(IES = CO_IES,
         CATEGADM = CO_CATEGAD,
         ORGANIZACAO = CO_ORGACAD,
         GRUPO = CO_GRUPO,
         MODALIDADE = CO_MODALIDADE,
         UF = CO_UF_CURSO,
         REGIAO = CO_REGIAO_CURSO,
         IDADE = NU_IDADE,
         SEXO = TP_SEXO,
         TURNO = CO_TURNO_GRADUACAO,
         ESTADO_CIVIL = QE_I01,
         RACA = QE_I02,
         BRASILEIRO = QE_I03,
         ESCOLARIDADE_PAI = QE_I04,
         ESCOLARIDADE_MAE = QE_I05,
         MORA_SOZINHO = QE_I06,
         TEM_RENDA = QE_I09,
         TRABALHA = QE_I10,
         RECEBEU_BOLSA_FINANCIAMENTO = QE_I11,
         RECEBEU_AUXILIO_PERMANENCIA = QE_I12,
         RECEBEU_BOLSA_ACADEMICA = QE_I13,
         EXTERIOR = QE_I14,
         INCLUSAO_SOCIAL = QE_I15,
         ENSINO_MEDIO = QE_I17,
         LIVROS = QE_I22,
         HORAS_EXTRAS = QE_I23,
         IDIOMA_ESTRANGEIRO = QE_I24,
         MOTIVO_ESCOLHA_CURSO = QE_I25,
         MOTIVO_ESCOLHA_IES = QE_I26) %>% 
  mutate(CATEGADM = ifelse(CATEGADM == 1 || CATEGADM == 2 || CATEGADM == 3, "Publica",
                           ifelse(CATEGADM == 4 || CATEGADM == 5, "Privada", "Especial")),
         ORGANIZACAO = ifelse(ORGANIZACAO == 10019 || ORGANIZACAO == 10020, "CentroUniversitario",
                              ifelse(ORGANIZACAO == 10022, "Faculdade",
                                     ifelse(ORGANIZACAO == 10026, "Instituto", "Universidade"))),
         MODALIDADE = ifelse(MODALIDADE == 0, "EaD", "Presencial"),
         REGIAO = ifelse(REGIAO == 1, "Norte",
                         ifelse(REGIAO == 2, "Nordeste",
                                ifelse(REGIAO == 3, "Sudeste",
                                       ifelse(REGIAO == 4, "Sul", "CentroOeste")))),
         TURNO = ifelse(TURNO == 1, "Matutino",
                        ifelse(TURNO == 2, "Vespertino", 
                               ifelse(TURNO == 3, "Integral", "Noturno"))),
         GRUPO = ifelse(GRUPO == 4003 || GRUPO == 5710 || GRUPO == 5806 || GRUPO == 5814 || GRUPO == 5902 || GRUPO == 6002 || GRUPO == 6008 || GRUPO == 6208 || GRUPO == 6307 || GRUPO == 6405, "Engenharia",
                        ifelse(GRUPO == 69 || GRUPO == 90 || GRUPO == 91 || GRUPO == 92 || GRUPO == 95 || GRUPO == 6410, "Tecnologia",
                               ifelse(GRUPO == 6 || GRUPO == 12 || GRUPO == 19 || GRUPO == 23 || GRUPO == 27 || GRUPO == 28 || GRUPO == 36 || GRUPO == 55 || GRUPO == 3501, "Saúde", "Outros"))),
         ESTADO_CIVIL = ifelse(ESTADO_CIVIL == "A", "Solteiro", 
                               ifelse(ESTADO_CIVIL == "B", "Casado", 
                                      ifelse(ESTADO_CIVIL == "C", "Separado", 
                                             ifelse(ESTADO_CIVIL == "D", "Viúvo", "Outros")))),
         RACA = ifelse(RACA == "A", "Branca",
                       ifelse(RACA == "B", "Preta",
                              ifelse(RACA == "C", "Amarela",
                                     ifelse(RACA == "D", "Parda", 
                                            ifelse(RACA == "E", "Indígena", "Não quero declarar"))))),
         BRASILEIRO = ifelse(BRASILEIRO == "A" || BRASILEIRO == "B", "Sim", "Não"),
         ESCOLARIDADE_PAI = ifelse(ESCOLARIDADE_PAI == "A", "Nenhuma",
                                   ifelse(ESCOLARIDADE_PAI == "E" || ESCOLARIDADE_PAI == "F", "Superior", "Escola")),
         ESCOLARIDADE_MAE = ifelse(ESCOLARIDADE_MAE == "A", "Nenhuma",
                                   ifelse(ESCOLARIDADE_MAE == "E" || ESCOLARIDADE_MAE == "F", "Superior", "Escola")),
         MORA_SOZINHO = ifelse(MORA_SOZINHO == "A", "Sim", "Não"),
         TEM_RENDA = ifelse(TEM_RENDA == "A" || TEM_RENDA == "B", "Não", "Sim"),
         TRABALHA = ifelse(TRABALHA == "A", "Não", "Sim"),
         RECEBEU_BOLSA_FINANCIAMENTO = ifelse(RECEBEU_BOLSA_FINANCIAMENTO == "A" || RECEBEU_BOLSA_FINANCIAMENTO == "B", "Não", "Sim"),
         RECEBEU_AUXILIO_PERMANENCIA = ifelse(RECEBEU_AUXILIO_PERMANENCIA == "A", "Não", "Sim"),
         RECEBEU_BOLSA_ACADEMICA = ifelse(RECEBEU_BOLSA_ACADEMICA == "A", "Não", "Sim"),
         EXTERIOR = ifelse(EXTERIOR == "A", "Não", "Sim"),
         INCLUSAO_SOCIAL = ifelse(INCLUSAO_SOCIAL == "A", "Não", "Sim"),
         ENSINO_MEDIO = ifelse(ENSINO_MEDIO == "A" || ENSINO_MEDIO == "D", "Pública", 
                               ifelse(ENSINO_MEDIO == "B" || ENSINO_MEDIO == "E", "Privada", "Exterior")),
         LIVROS = ifelse(LIVROS == "A", "Não", "Sim"),
         HORAS_EXTRAS = ifelse(HORAS_EXTRAS == "A", "Não", "Sim"),
         IDIOMA_ESTRANGEIRO = ifelse(IDIOMA_ESTRANGEIRO == "E", "Não", "Sim"),
         MOTIVO_ESCOLHA_CURSO = ifelse(MOTIVO_ESCOLHA_CURSO == "A" || MOTIVO_ESCOLHA_CURSO == "C" || MOTIVO_ESCOLHA_CURSO == "D", "Profissional",
                                       ifelse(MOTIVO_ESCOLHA_CURSO == "B" || MOTIVO_ESCOLHA_CURSO == "E", "Familiar", "Outros")),
         MOTIVO_ESCOLHA_IES = ifelse(MOTIVO_ESCOLHA_IES == "C" || MOTIVO_ESCOLHA_IES == "D" || MOTIVO_ESCOLHA_IES == "E", "Acesso",
                                     ifelse(MOTIVO_ESCOLHA_IES == "A" || MOTIVO_ESCOLHA_IES == "B", "Custo", "Outros"))) %>% 
  dplyr::select(-c("CO_CURSO", "CO_MUNIC_CURSO", "IES", "UF", "TP_INSCRICAO", "TP_INSCRICAO_ADM", "QE_I07", "QE_I08", "QE_I16", "QE_I18", "QE_I19", "QE_I20", "QE_I21"))


  summary(raw_data$NT_GER)


  raw_data <- raw_data %>% 
  rename(DESEMPENHO = NT_GER) %>% 
  mutate(DESEMPENHO = ifelse(DESEMPENHO < 34.30, "Baixo",
                             ifelse(DESEMPENHO > 55.70, "Alto", "Desconsiderar"))) %>% 
  filter(DESEMPENHO != "Desconsiderar") %>% 
  mutate(DESEMPENHO = factor(DESEMPENHO))

dim(raw_data)


#seleçao ---
raw_data <- raw_data %>% 
  dplyr::select(-c("CATEGADM", 
            "ORGANIZACAO", 
            "GRUPO", 
            "MODALIDADE", 
            "BRASILEIRO",
            "TEM_RENDA",
            "RECEBEU_BOLSA_FINANCIAMENTO",
            "ENSINO_MEDIO",
            "MOTIVO_ESCOLHA_CURSO",
            "MOTIVO_ESCOLHA_IES"))

initial <- glm(DESEMPENHO ~ ., 
               data = raw_data,
               family = binomial)

predictors <- stepAIC(initial, direction = "both")