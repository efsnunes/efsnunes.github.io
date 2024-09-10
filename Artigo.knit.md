---
title: "Aplicação de Técnicas de Otimização de Fluxo de Redes para Minimizar Custos Associados a Desastres e Inundações"
subtitle: "Foco em um Centro de Distribuição para atender Abrigos"
author: "Edison Fernando da Silva Nunes"
date: "2024-09-07"
output: 
  pdf_document:
    keep_tex: true
    latex_engine: xelatex
header-includes:
      -\usepackage{setspace}
      -\usepackage{ragged2e}
      -\justifying
      -\setlength{\parindent}{1em}
      -\setstretch{1.5}
      -\renewcommand{\normalsize}{\fontsize{12}{14}\selectfont}
editor_options: 
  markdown: 
    wrap: 72
    
---



1. INTRODUÇÃO

A logística humanitária refere-se à organização e coordenação das atividades logísticas envolvidas em operações de ajuda humanitária.
É um campo especializado que lida com os desafios enfrentados pelos
trabalhadores humanitários, como a entrega rápida de suprimentos
essenciais em áreas remotas ou afetadas por desastres. Os desastres
resultam de eventos adversos, naturais ou provocados pelo homem, que
impactam ecossistemas vulneráveis, causando danos humanos, materiais
e/ou ambientais, além de prejuízos econômicos e sociais.
Especificamente, desastres naturais têm consequências drásticas para
sociedades, regiões e países, resultando em vítimas fatais, feridos,
desabrigados, cidades destruídas e altos custos de reconstrução. No
Brasil, um levantamento realizado pelo Centro de Estudos e Pesquisas
sobre Desastres (CEPED, 2013) no período de 1991 a 2012 registrou
38.996 desastres naturais, com destaque para os anos de 2010, 2011 e
2012, que concentraram 78% dos desastres registrados. A cada ano,
centenas de desastres naturais, como terremotos, enchentes e
inundações, ocorrem em todo o mundo. Diversas atividades de
logística humanitária, como o transporte de suprimentos, a gestão de
estoques, a tecnologia da informação e comunicação, a distribuição
eficiente de alimentos, água e abrigos, bem como serviços de saúde e
gestão de resíduos, são necessárias. Também pode incluir outras
organizações e agências governamentais, a fim de garantir uma
resposta eficaz. A logística humanitária enfrenta muitos desafios,
como a falta de infraestrutura básica em áreas afetadas, condições
climáticas e ambientais adversas, logística precária e questões de
segurança. Além disso, há dificuldades em garantir a equidade e o
acesso igualitário aos recursos entre as populações afetadas. Nesse
contexto, o desastre natural ocorrido durante o mês de maio de 2024
na região sul do Brasil, no estado do Rio Grande do Sul, devido à
chuvas intensas e consequentes inundações, afetou gravemente as
regiões dos vales dos rios Taquari, Caí, Pardo, Jacuí, Sinos e
Gravataí, além de Porto Alegre pelo rio Guaíba e regiões de Pelotas
e Rio Grande pela Lagoa dos Patos. As inundações devastaram áreas
urbanas e rurais, resultando em danos significativos como
desabrigados e desalojados com grande prejuízo material. Ao todo,
478 municípios foram afetados, com uma população de 2.398.255
pessoas impactadas. As inundações resultaram em 806 feridos, 31
desaparecidos e 182 óbitos confirmados. A partir da coleta de dados
de monitoramento já realizados e pela produção prévia de mapas do
município de Rio Grande-RS, que indicavam os locais possivelmente
afetados pela elevação das águas da Lagoa dos Patos, foi possível
tomadas de decisões mais assertivas e maior previsibilidade das
ações. Ao longo dos anos, a logística humanitária avançou
consideravelmente, com o uso de tecnologias avançadas, como drones e
análise de dados, para melhorar a eficiência e a eficácia das
operações. Também foi desenvolvida uma rede global de organizações e
agências especializadas em logística humanitária para o
compartilhamento de conhecimentos e recursos. A logística
humanitária desempenha um papel vital na resposta a emergências e
desastres, garantindo que os recursos essenciais cheguem às pessoas
que mais precisam. Além disso, desempenha um papel importante na
recuperação e reconstrução pós-desastre, ajudando a restabelecer
infraestruturas básicas e a fornecer apoio contínuo às comunidades
afetadas.

    
2. JUSTIFICATIVA

A crescente frequência e severidade dos desastres naturais, como as
inundações recentes no Rio Grande do Sul em maio de 2024, evidenciam a
necessidade urgente de desenvolver e implementar estratégias eficazes de
logística humanitária. Esses eventos sublinham a importância de uma
resposta rápida e coordenada para salvar vidas, minimizar danos e apoiar
a recuperação das comunidades afetadas. No entanto, os desafios impostos
por infraestrutura precária e condições adversas frequentemente
dificultam a entrega eficaz de ajuda humanitária. A aplicação de
técnicas de otimização de fluxo de redes em operações de socorro pode
aprimorar significativamente a distribuição de recursos e serviços de
emergência. Essas técnicas não apenas melhoram a alocação eficiente de
suprimentos críticos, como também reduzem os custos operacionais
envolvidos, garantindo que os recursos limitados sejam utilizados da
melhor forma possível. Tecnologias avançadas e uma colaboração estreita
entre organizações são essenciais para enfrentar os desafios logísticos
em situações de desastre, aumentando a eficácia das operações de
emergência. Este estudo visa fornecer ferramentas e conhecimentos
práticos para gestores e tomadores de decisão, promovendo maior
resiliência e capacidade de resposta em situações de emergência. Ao
implementar técnicas de otimização e estratégias logísticas avançadas,
espera-se melhorar a prontidão e a eficiência das respostas a desastres,
fortalecendo a capacidade das comunidades de lidar com os impactos de
eventos extremos e facilitando a recuperação pós-desastre.

3. OBJETIVOS

   3.1. Objetivo Geral

   Aplicar técnicas de otimização de fluxo de redes para minimizar o
   tempo de atendimento à vítimas de desastres de inundações para o
   município de Rio Grande - RS, melhorando a eficiência na
   distribuição de recursos e na alocação de serviços de emergência.

   3.2. Objetivos Específicos

   1.  Mapear a região identificando locais com menor e maior risco de
   inundações;

   2.  Desenvolver um modelo de otimização para a localização de abrigos às
   vítimas de desastres provocados por inundações no município de Rio
   Grande - RS;

   3.  Desenvolver um modelo de roteamento para a entrega dos principais
   suprimentos (alimentos, remédios e roupas).

   4.  Realizar simulações para os dois modelos apontando a solução ótima.

   5.  Propor recomendações para estratégias de logística humanitária
   baseadas nos resultados obtidos.

4. METODOLOGIA

O trabalho proposto, segundo (Pizzolato, 2012), seguirá a metodologia
básica da Pesquisa Operacional, passando pela identificação do problema,
a formulação de um modelo matemático utilizando hipóteses
simplificadoras, a resolução do modelo, a validação dos resultados e
posteriormente o oferecimento de propostas para implementação. Exemplos
de formulações matemáticas para o problema, sobretudo na busca por
respostas, para minimizar danos causados por desastre natural, que
constitui o tema principal deste trabalho encontram-se relacionados
abaixo.

   4.1. Tipos de Metodologia
    
   4.1. Coleta de Dados
    
   Dados meteorológicos, relatórios dos últimos
   desastres e informações sobre infraestrutura do município serão
   coletados para análise.


    4.1.1. O modelo da P-Mediana

    Para o número ótimo e localização dos abrigos, pretende-se utilizar o
    modelo das P-Medianas. Nesse modelo p-medianas procura-se minimizar a
    distância média, ou seja, minimizar a soma das menores distâncias dentro
    de uma região, determinando a localização ideal de (p) instalações
    (abrigos) de forma que a demanda total desta área seja atendida de
    maneira eficiente. Esses problemas são comumente aplicados em contextos
    de planejamento urbano, logística de distribuição, e situações de
    resposta a emergências, onde a meta é reduzir o tempo ou o custo de
    transporte entre os centros de distribuição e os abrigos. A solução do
    problema envolve escolher (p) locais de um conjunto de possíveis locais
    de abrigos para que a soma das distâncias ponderadas (baseadas na
    demanda) dos centros de distribuição até as instalações mais próximas
    seja minimizada. Sejam N={1,...,n} o conjunto de pontos de demanda;

$$
i \in \mathbb{N}
$$ 

      um determinado cliente ou vértice; 

$$
j \in \mathbb{N}
$$ 
      uma instalação em potencial ou mediana; p o número de instalações de
      serviço ou medianas a serem localizadas; 
$$
w_i
$$
      o peso ou importância de cliente 
$$
i
$$; 

$$
[d_{ij}]_{n \times n}
$$ 
      a matriz simétrica de distâncias de cada cliente $$i$$ à instalação
$$
j
$$ 
      , com 
$$
d_{ii}=0, \forall i; [x_{ij}]_{n \times n} 
$$ 
      a matriz de alocação de cada cliente $$i$$; onde 
$$
x_{ij}=1
$$ 
      se o cliente 
$$
i
$$
      é alocado à instalação 
$$
j
$$ 
      e
$$
x_{ij}=0
$$
      , caso contrário; 
$$
x_{jj}=1
$$ 
      indica que 
$$
j
$$ 
      é uma mediana e 
$$
x_{jj}=0
$$
      em caso contrário. Então, o modelo da p-mediana é apresentado da
      seguinte forma:

$$
Min \quad  z=\sum_{i \in \mathbb{N}} \sum_{j \in \mathbb{N}} w_{i}d_{ij}x_{ij}    (1)
$$ 
Sujeito a 

$$
\sum_{j \in \mathbb{N}}x_{ij}=1; i \in \mathbb{N}
$$

$$
\sum_{j \in \mathbb{N}}x_{jj}=p
$$ 

$$
x_{ij} \leq x_{jj}; i,j \in \mathbb{N}
$$

$$
x_{ij} \in \mathbb{0,1}; i,j \in \mathbb{N}
$$
onde a função objetivo (1) indica a minimização das distâncias
ponderadas entre os clientes e os opostos que oferecem serviços; as
restrições em (2) indicam que cada cliente $$i$$ é alocado a somente uma instalação $$j$$ ; a restrição (3) garante que somente 
$$
p
$$
instalações oferecem o serviço proposto; as restrições em (4) afirmam
que um cliente somente é atendido num local onde existe uma instalação
que oferece o serviço, e as restrições em (5) impõem variáveis de
decisão binárias. Cabe lembrar que pode existir uma lista prévia de
pontos candidatos a serem escolhidos como mediana; nesse caso o modelo
acima sofre pequenas modificações e recebe o nome tradicional de
problema de localização de uma planta simples ou em inglês $$ simple $$
$$ plant $$ $$ lOcation $$ $$ model $$ $$(SPLP)$$ Observamos ainda que,
em lugar de localização, o modelo acima pode ser interpretado como
modelo de zoneamento, em que se busca dividir o espaço em $$
p$$ zonas. Nessa ótica de zoneamento, o modelo de p-mediana pode ser
aplicadoao problema de classificação de um conjunto de padrões,
conhecido como $$cluster$$ $$analysis$$ (Xavier e Xavier, 2011), que busca agrupar elementos com padrões semelhantes em certo $$ cluster $$ e com padrões diferentes em $$ clusters $$ distintos

4.2. Desenvolvimento do Modelo

Serão utilizados modelos matemáticos para
a localização dos abrigos e para a entrega de suprimentos. Estes modelos
serão alimentado com variáveis como: quantidades de suprimentos,
veículos disponíveis, rotas de transporte e demanda por serviços de
emergência.

4.2.2. O Modelo para o Roteamento de Veículos

Os problemas de roteamento de veículos (PRV) consistem basicamente em
determinar rotas para realizar algum tipo de serviço, de maneira que o
custo seja mínimo. Resolver um PRV significa procurar a forma de
distribuir a um ou mais veículos uma determinada lista de compromissos
de entrega, associados a determinados pontos, devendo os veículos
retornar ao ponto de origem ao final do trabalho. A formulação
matemática para um Problema de Roteamento de Veículos (PRV) é
apresentada abaixo:
Minimizar

$$
z=\sum_{i,j}(c_{ij}\sum_{k}x_{ijk})
$$
Sujeito a:
$$
\sum_{k}y_{ik}=1 \quad \text{para} \quad  i=2, \dots,n
$$
$$
\sum_{k}y_{ik}=1\quad \text{para} \quad  i=1
$$
$$
\sum_{i}q_{i}y_{ik} \leq Q_{k} \quad \text{para} \quad  k=1, \dots,m
$$
$$
\sum_{j}x_{ijk} \leq\sum x_{jix}=y_{ik} \quad \text{para}\quad i=1,\dots,n\quad k=1,\dots,m
$$
$$
y_{ik} \in \{0,1\}\quad\text{para}\quad i=1,\dots,n\quad\text{para}\quad k=1,\dots,m
$$
$$
x_{ijk} \in \{0,1\}\quad\text{para}\quad i,j=1,\dots,n\quad\text{para}\quad k=1,\dots,m
$$
##Onde: Definir cada termo
Formulação matemática

Seja 
𝑁={1,...,𝑛}
N={1,...,n} o conjunto de pontos de demanda, onde:

𝑖∈𝑁i∈N representa um ponto de demanda (cliente ou vértice);

𝑗∈𝑁j∈N é um local potencial para um abrigo;

$$d_{ij}$$  é a distância entre o ponto de demanda 
𝑖e o local j;

$$x_{j}$$
  é uma variável binária que indica se um abrigo será instalado no local j (1 se sim, 0 caso contrário);

$$y_{ij}  é uma variável binária que indica se o ponto i será atendido pelo abrigo j.

##

4.3. Simulação e Análise 

Os modelos serão simulados usando dados reais
das inundações de 2024 no município de Rio Grande - RS, focando na
eficiência de distribuição e na redução de custos operacionais. Testes
de sensibilidade serão realizados para avaliar diferentes cenários.

5.  RESULTADOS ESPERADOS

##Aqui precisa um pouco de mais blá, blá, .... que iremos construir. 
Melhoria na alocação de abrigos: Espera-se que o modelo de P-Mediana sugira um conjunto de locais estratégicos para a instalação de abrigos, considerando a minimização da distância média entre os pontos de demanda e os abrigos. Isso deve reduzir significativamente o tempo de resposta durante desastres, permitindo que as vítimas sejam acolhidas de forma mais rápida e eficiente. A localização estratégica dos abrigos é crucial em cenários de inundações, onde o tempo de resposta pode ser determinante para salvar vidas e mitigar o impacto dos desastres. Além disso, a distribuição eficiente dos abrigos contribui para a otimização de recursos, uma vez que evita sobrecarga em determinados locais e garante que todos os abrigos recebam um fluxo equilibrado de vítimas.

Este projeto busca contribuir para a logística humanitária ao aplicar modelos de P-Medianas e técnicas de roteamento, focando na diminuição dos tempos de atendimento às vítimas de inundações no município de Rio Grande – RS. A aplicação desses modelos não só auxilia na escolha do local mais próximo que servirá de abrigo, mas também no abastecimento adequado de cada abrigo com suprimentos essenciais, como alimentos, medicamentos e água. Além disso, espera-se que o estudo possa oferecer insights valiosos sobre como aprimorar a alocação de recursos em cenários futuros, utilizando técnicas de otimização para criar uma rede de resposta rápida e eficiente. A utilização de modelos matemáticos e algoritmos de roteamento também permitirá identificar as melhores rotas de distribuição de suprimentos, minimizando o tempo de transporte e os custos operacionais envolvidos, resultando em um sistema de resposta mais ágil e menos oneroso.

6.  CRONOGRAMA

\begin{table}[ht]
\centering
\begin{tabular}{|p{3cm}|p{1.5cm}|p{1.5cm}|p{1.5cm}|p{1.5cm}|p{1.5cm}|p{1.5cm}|p{1.5cm}|p{1.5cm}|}
\hline
Etapas & 1º trim 2024 & 2º trim 2024 & 3º trim 2024 & 4º trim 2024 & 1º trim 2025 & 2º trim 2025 & 3º trim 2025 & 4º trim 2025 \\ \hline
   & B1   & C1   & D1   & E1   & F1   & G1   & H1   & I1   \\ \hline
Revisão Bibliográfica   & B2   & C2   & D2   & E2   & F2   & G2   & H2   & I2   \\ \hline
Submissão de artigo da
disciplina de Seminários
em Ambientometria& B3   & C3   & D3   & E3   & F3   & G3   & H3   & I3   \\ \hline
Estudos dos Modelos   & B4   & C4   & D4   & E4   & F4   & G4   & H4   & I4   \\ \hline
Desenvolvimento dos
Modelos& B5   & C5   & D5   & E5   & F5   & G5   & H5   & I5   \\ \hline
Conclusões   & B6   & C6   & D6   & E6   & F6   & G6   & H6   & I6   \\ \hline
\end{tabular}
\caption{Sua tabela de 9 colunas e 6 linhas}
\end{table}

