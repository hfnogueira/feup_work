# Guia de Apresentação da Defesa
## Previsão da Produção Vinícola com Aprendizagem Automática Baseada em Regras
### Hugo Filipe Queiróz Nogueira — MECD, FEUP — junho de 2026

> **Nota:** Este guia refere em alguns pontos um filtro de suporte de "15%". Face à errata, o valor correto é **20%** (`carenr_min_sup = 0.2`). Ao apresentar, diz **20%** (uma regra cobre ≥18 dos 89 anos no RDD). Ver `07_ERRATA_imprimir_e_levar.md`.

---

# PARTE 1 — GUIÃO SLIDE A SLIDE

---

## Slide 1 — Título & Gancho de Abertura
**Objetivo:** Preparar o cenário e captar a atenção do júri nos primeiros 30 segundos.

**Conteúdo recomendado:**
- Título da tese (abreviado): *"Podem as Regras Explicar e Prever a Produção Vinícola?"*
- O teu nome, programa, orientadores, data
- Uma imagem de abertura marcante: uma vinha do Douro, ou um gráfico de série temporal de produção a mostrar o declínio dramático do Vinho Verde ao lado da subida do Douro

**Visual a incluir:**
> Figuras 3.1 + 3.2 lado a lado: as duas séries temporais de produção. Contam de imediato a história da divergência estrutural — uma região a subir 80%, a outra a descer 65% — sem uma única palavra de explicação.

**Notas do orador:**
> Introduz a imagem antes do título. "As duas linhas que veem aqui representam 80 anos de produção vinícola em duas regiões portuguesas vizinhas. Uma tem vindo a subir há um século. A outra colapsou 65% em três décadas. Mesmo país, mesma zona climática — mas histórias estruturalmente muito diferentes. A minha tese pergunta: podem regras interpretáveis de aprendizagem automática explicar *e* prever o que impulsiona a produção acima ou abaixo da tendência em cada região?"

---

## Slide 2 — O Problema de Investigação
**Objetivo:** Enunciar a lacuna científica e porque importa. Um minuto para justificar toda a tese.

**Conteúdo recomendado:**
- A produção vinícola é sensível ao clima E impulsionada por fatores estruturais
- As abordagens de ML existentes (Random Forest, LSTM) preveem bem mas são caixas pretas
- Os peritos de domínio — agrónomos, cooperativas, órgãos de política — precisam de regras *legíveis*
- Nenhum trabalho anterior aplica descoberta de regras distribucionais interpretáveis a registos de produção vinícola de várias décadas
- Lacuna de investigação numa frase: **"A combinação de séries temporais longas + saída de regras interpretáveis + validação preditiva rigorosa nunca foi tentada antes."**

**Visual a incluir:**
> Tabela 2.1 (mapa da literatura): uma tabela simples a mostrar que nenhum trabalho anterior pontua "sim" nas cinco dimensões (interpretável + alvo contínuo + dados de longo prazo + duas regiões contrastantes + validação rigorosa).

**Notas do orador:**
> "A aprendizagem automática foi amplamente aplicada ao rendimento vinícola. Random forests, gradient boosting, deep learning — todos preveem razoavelmente bem. Mas não explicam. Um agrónomo não consegue agir sobre um valor SHAP. O que os profissionais precisam é de uma afirmação como: 'Se a água no solo antes da vindima ficar entre 74 e 101 mm, e os dias húmidos pós-floração forem menos de 3, a produção tende a ficar 120 mil hectolitros acima da tendência.' Isso é um achado acionável e auditável. Nenhum trabalho publicado aplicou este tipo de descoberta de regras distribucionais interpretáveis a dados de produção vinícola de várias décadas. É essa a lacuna que esta tese preenche."

---

## Slide 3 — Dois Objetivos, Quatro Questões de Investigação
**Objetivo:** Orientar o júri sobre a estrutura dupla da tese e montar o arco narrativo.

**Conteúdo recomendado:**
- **Objetivo 1 (Descoberta de Regras):** Descobrir regras agroclimáticas interpretáveis que expliquem anos de produção acima/abaixo da tendência — *alcançado*
- **Objetivo 2 (Validação Preditiva):** Avaliar se essas regras batem uma baseline ingénua em teste walk-forward rigoroso — *não alcançado no agregado*
- Quatro questões de investigação:
  - RQ1: Que variáveis climáticas distinguem anos acima/abaixo da tendência?
  - RQ2: São essas variáveis sinais genuínos dentro da era ou artefactos de pertença de era?
  - RQ3: Três algoritmos diferentes convergem nas mesmas características?
  - RQ4: As regras descobertas melhoram a previsão fora da amostra?
- **Antecipa a reviravolta:** "O achado cientificamente mais interessante não foi que a previsão falhou — foi *porque* falhou."

**Visual a incluir:**
> Um layout limpo de duas colunas: Objetivo 1 (✓ Alcançado) vs Objetivo 2 (✗ Não alcançado no agregado / ◑ Achado condicional no RDD). Simples, memorável, honesto.

**Notas do orador:**
> "A tese persegue dois objetivos complementares aplicados aos mesmos dados. O Objetivo 1 pergunta se o CarenR — um algoritmo de descoberta de subgrupos distribucional — consegue encontrar regras climáticas estatisticamente validadas e legíveis. O Objetivo 2 pergunta se essas regras são úteis num cenário de previsão real. A resposta ao Objetivo 1 é sim. A resposta ao Objetivo 2 é em larga medida não. Mas essa assimetria — a descoberta de regras a ter sucesso onde a previsão falha — não é uma falha da metodologia. Revela-se um dos achados mais informativos de todo o estudo."

---

## Slide 4 — Dados, Características & Desenho Experimental
**Objetivo:** Dar ao júri confiança no rigor da montagem. É o slide da credibilidade.

**Conteúdo recomendado:**
- Dois conjuntos de dados: RDD (89 anos, 1934–2022), RVV (80 anos, 1942–2021)
- 59 características agroclimáticas por região, ancoradas na fenologia da vinha (temperatura, precipitação, água no solo, LAI, dias de calor/geada — ano corrente e anterior)
- Três algoritmos comparados: CarenR (distribucional), RIPPER (classificação), M5Rules (regressão por partes)
- Validação walk-forward de janela expansível: 18 cenários (RDD) / 16 cenários (RVV)
- Todo o pré-processamento dentro de cada fold — sem fuga de dados
- **Baseline primária: Naive_Median** (prevê o resíduo mediano de treino; minimiza o MAE entre todos os preditores constantes)

**Visual a incluir:**
> Figura 3.4: o diagrama walk-forward de janela expansível. Esta única figura explica todo o desenho de validação. Mostra-a. Os membros do júri vão escrutinar sobretudo o método de avaliação — este diagrama antecipa a maioria das perguntas de metodologia.

**Notas do orador:**
> "Os dados abrangem 80 a 89 anos de registos meteorológicos diários. Foram agregados em 59 características anuais ancoradas nas datas fenológicas da vinha — portanto uma característica como 'dias húmidos pós-floração' ajusta-se à data de floração real de cada ano, não a uma janela de calendário fixa. Três algoritmos de aprendizagem de regras foram comparados, cada um com um critério de otimização fundamentalmente diferente. O protocolo de avaliação é um desenho walk-forward de janela expansível — o modelo é sempre treinado nos anos até ao tempo t e avaliado no ano t+1 em diante. Todo o pré-processamento, incluindo destendência e discretização de características, é feito dentro de cada fold de treino para evitar fuga. A referência primária é a Naive_Median — que prevê sempre o resíduo mediano de treino. Qualquer modelo que a bata extraiu genuinamente informação climática."

---

## Slide 5 — O Arcabouço das Duas Decisões (A Contribuição Conceptual)
**Objetivo:** Introduzir a contribuição intelectual mais original da tese. É o slide de que o júri se vai lembrar.

**Conteúdo recomendado:**
- Erro de previsão = **Decisão 1** + **Decisão 2**
- **Decisão 1:** Com que precisão a tendência de treino extrapola? Medida pelo MAE da Naive_Median. É o "piso" abaixo do qual nenhum modelo pode melhorar sem melhor modelação da tendência.
- **Decisão 2:** Consegue um modelo baseado em regras bater a mediana de treino? Requer resíduos estruturados e não triviais para explorar.
- **A tensão fundamental:** Um melhor ajuste da tendência (Decisão 1) produz resíduos menores, mas resíduos menores contêm menos sinal para indução de regras (Decisão 2). Melhorar um degrada o outro.
- **Exemplo concreto:** A destendência LOESS reduz a Naive_Median do RVV de 140 para 90 mhl (−36%, melhor Decisão 1), mas elimina todas as regras CarenR (0 regras disparam — zero Decisão 2). Quanto mais flexível a destendência, menos sinal resta.

**Visual a incluir:**
> Um diagrama personalizado (desenha-o): duas caixas ("Decisão 1: Qualidade da Tendência" e "Decisão 2: Sinal Climático"), ligadas por uma seta de tensão, com o MAE da Naive_Median a rotular a Decisão 1 e "MAE do Modelo − MAE da Naive_Median" a rotular a Decisão 2. Acrescenta o exemplo LOESS como destaque.

**Notas do orador:**
> "Este arcabouço é, creio, a contribuição mais transferível da tese. Qualquer problema de previsão agroclimática com uma tendência estrutural não-estacionária enfrenta o mesmo dilema. A Decisão 1 é sobre a qualidade da tendência — quão bem o teu modelo de destendência extrapola para o período de teste. A Decisão 2 é sobre a extração de sinal — quanta variação interanual estruturada resta após a destendência, e se um modelo a consegue explorar. Estas duas exigências puxam em direções opostas. Torna a destendência mais flexível e reduzes o erro da Decisão 1, mas retiras o próprio sinal residual de que a indução de regras precisa. Esta tensão não é um problema resolúvel a esforçar-se mais; requer uma abordagem diferente — especificamente, destendência estrutural usando covariáveis não climáticas para separar mudanças da indústria de efeitos do clima."

---

## Slide 6 — Resultados da Descoberta de Regras: O Que o CarenR Encontrou (Objetivo 1)
**Objetivo:** Mostrar a carga científica. Estes são os resultados reais, interpretáveis, agronomicamente significativos e validados.

**Conteúdo recomendado:**
- **Douro (RDD):** 48 regras; temas dominantes: água no solo na vindima (54% das regras), dias húmidos pós-floração (44%), dias de stress térmico. Regra de topo: pós-floração seca + pós-maturação seca → **+121 mhl acima da tendência** (suporte 28%, p=0,004). Regra 4: regra multi-anual → **+192 mhl** (maior desvio no Douro).
- **Vinho Verde (RVV):** 20 regras; 16 das 20 apontam ABAIXO da tendência (assimetria inversa). Características dominantes: dias de stress térmico na maturação (50% das regras). Apenas 1 regra acima da tendência → **+311 mhl** (maior desvio absoluto em qualquer das regiões).
- **Inversão direcional:** A mesma característica (água no solo na vindima) prevê ACIMA da tendência no Douro e ABAIXO da tendência no Vinho Verde — o CarenR deteta estrutura distribucional específica da região, não uma correlação espúria global.
- **Validação LOESS:** As características de topo do RDD mantêm 90–125% da correlação após remoção agressiva da tendência de era → sinal genuíno dentro da era, não artefacto de pertença de era. (Exceção: LAI na vindima no RVV — apenas 15% de retenção → impulsionada por era.)

**Visual a incluir:**
> Tabelas 4.1 e 4.2 lado a lado (top 5 regras por região), reduzidas às quatro colunas mais importantes (Condições, Direção, Δ Mediana, Suporte). São as tabelas visualmente mais impactantes da tese. Destaca a Regra 4 (RDD, +192 mhl) e a Regra 4 (RVV, +311 mhl).

**Notas do orador:**
> "O CarenR descobriu 48 regras para o Douro e 20 para o Vinho Verde, todas a passar um limiar de significância KS e um filtro de suporte mínimo de 20%. As regras do Douro convergem numa história agroclimática clara: condições secas pós-floração — menos de 3,3 dias húmidos — combinadas com défice moderado de água no solo na vindima. Esta combinação favorece bagos concentrados, menor pressão de doença e produção acima da tendência. A regra mais poderosa envolve um mecanismo multi-anual: água no solo moderada pré-vindima combinada com condições secas pós-vindima no ano anterior, captando o mecanismo de reserva de carryover da vinha. Produz um desvio de +192 mil hectolitros acima da tendência. No Vinho Verde, a assimetria inverte-se: 16 das 20 regras apontam abaixo da tendência. O clima atlântico significa que condições frescas e húmidas — que no Douro sinalizariam risco de seca — no Vinho Verde são simplesmente normais, e os anos abaixo da tendência caracterizam-se pela *ausência* de condições quentes. O Objetivo 1 é claramente alcançado."

---

## Slide 7 — Validação Entre Algoritmos (A Evidência de Robustez)
**Objetivo:** Antecipar a objeção "talvez seja só um artefacto do CarenR".

**Conteúdo recomendado:**
- O RIPPER (critério de classificação) e o M5Rules (critério MSE) selecionam independentemente as **mesmas famílias de variáveis** que o CarenR
- Três características confirmadas pelos três algoritmos: água no solo na vindima, família LAI, temperatura de verão do ano anterior
- A concordância entre algoritmos significa que o sinal é real — três critérios de otimização fundamentalmente diferentes apontam para as mesmas variáveis
- Uma divergência a mencionar: o M5Rules atribui um declive *negativo* à água no solo na vindima no global, enquanto o CarenR identifica uma zona *positiva* acima da tendência em défice moderado — porque o M5Rules não consegue modelar a interação não-linear sem termos de interação explícitos
- Exatidão CV do RIPPER: 42% (RDD), 28% (RVV) — classificação fraca, mas a seleção de variáveis é significativa

**Visual a incluir:**
> Tabela 4.9 (concordância de características entre algoritmos): uma tabela compacta de três colunas a mostrar em que características o CarenR, o RIPPER e o M5Rules concordam, com classificações de confiança. As características "3/3 ALTA universal" são o destaque.

**Notas do orador:**
> "Um membro cético do júri pode perguntar: poderão estas regras ser simplesmente artefactos do algoritmo de pesquisa particular do CarenR? A comparação entre algoritmos fornece a evidência mais forte contra essa preocupação. O RIPPER, que pergunta 'que condições climáticas melhor separam os três tercis de produção', e o M5Rules, que pergunta 'que característica permite a regressão linear mais exata', ambos identificam independentemente a água no solo na vindima, o LAI e a temperatura de verão do ano anterior. Três perguntas diferentes, três algoritmos diferentes, a mesma resposta. Essa convergência é a evidência de que estas características carregam estrutura genuína dos dados, não ruído algorítmico."

---

## Slide 8 — Validação Preditiva: O Ranking Completo de Modelos (Objetivo 2)
**Objetivo:** Apresentar o resultado honesto e completo. A transparência aqui constrói credibilidade.

**Conteúdo recomendado:**
- **Resultado RDD:** A Naive_Median fica em 1.º com 184 mhl. O melhor CarenR (Eta2) fica em 2.º com 189 mhl — uma diferença de 5 mhl (3%). Wilcoxon p=0,468: não significativo. RIPPER (279 mhl) e Árvore de Decisão (250 mhl) são os piores.
- **Resultado RVV:** A Naive_Median fica em 1.º com 140 mhl. O melhor CarenR (Sup_FS) fica em 4.º com 172 mhl — uma diferença de 22%. Achado confirmatório: quando o CarenR *dispara* regras no RVV, essas regras são significativamente *piores* do que a baseline ingénua (p=0,004). O CarenR vence em 0 de 16 cenários do RVV.
- **A comparação-chave:** CarenR vs Naive_Median, não CarenR vs RIPPER. A pergunta relevante não é "qual aprendiz de regras é melhor" mas "algum aprendiz de regras bate não fazer nada com dados climáticos?"

**Visual a incluir:**
> Figuras 5.1 e 5.2 (gráficos de barras de MAE para RDD e RVV). São os resumos visuais mais claros do Objetivo 2. Mostra-as lado a lado. O gráfico de barras comunica de imediato que a Naive_Median está no topo e todos os aprendizes de regras estão à sua direita (piores).

**Notas do orador:**
> "Nenhum modelo bate consistentemente a baseline Naive_Median no agregado em qualquer das regiões. No Douro, a melhor variante CarenR fica 5 mil hectolitros atrás — uma diferença de 3% que não é estatisticamente significativa. No Vinho Verde, a diferença cresce para 22%, e um teste estatístico confirmatório mostra que quando o CarenR dispara regras, essas regras prejudicam ativamente a previsão em vez de ajudarem. Este é um resultado preditivo negativo, e quero ser claro que a tese o reporta honestamente e sem suavizar. O Objetivo 2 não é alcançado. A pergunta importante não é que falhou, mas porquê."

---

## Slide 9 — Porque a Previsão Falhou: Composição por Era e a Explicação Estrutural
**Objetivo:** Transformar o resultado negativo no achado intelectualmente mais interessante.

**Conteúdo recomendado:**
- **Confundimento por era:** Os dados de treino abrangem eras pré e pós-1990 com níveis de produção estruturalmente diferentes. As regras descobertas em dados de treino de eras mistas aprendem pertença de era, não sinal climático.
- **Achado RDD equilibrado por era (exploratório):** Nos cenários 10–14, assim que a proporção pós-1990 atinge 29% dos dados de treino, o CarenR bate a Naive_Median em *todos os 5 cenários consecutivos* (margens 17–32 mhl). Binomial p=0,031. É um achado post-hoc exploratório — anotado claramente, não sobre-interpretado.
- **O RVV não tem janela de era recuperável:** A quebra estrutural foi tão grande e rápida (−65% impulsionado pela adesão à UE, abandono de terras, mudança do sistema de condução) que nenhuma relação clima-produção estável sobrevive entre eras. Acrescentar mais dados da era moderna piora o RVV, não melhora.
- **A destendência estrutural é o caminho em frente:** Se a área plantada, a quota do Porto e o número de produtores certificados fossem usados como covariáveis de destendência explícitas, os resíduos climáticos seriam mais limpos. É a recomendação principal para trabalho futuro.

**Visual a incluir:**
> Figura 5.3 (RDD por cenário: CarenR Eta2 vs Naive_Median) anotada com "Cedo: confundido por era (CarenR perde)" e "Era equilibrada: CarenR vence 5/5". Esta figura conta toda a história da composição por era numa imagem.
> Tabela 6.1 (tabela de divergência estrutural RDD vs RVV): mostra o contraste sistemático na severidade do confundimento por era.

**Notas do orador:**
> "A análise por cenário revela estrutura que o MAE ponderado agregado esconde. Nos cenários iniciais, o conjunto de treino é fortemente dominado por dados pré-1990. O CarenR aprende regras que refletem em parte pertença de era — a era mais antiga tinha produção mais baixa por razões estruturais, não climáticas. Estas regras disparam mal em anos de teste modernos. Mas assim que o conjunto de treino atinge uma proporção pós-1990 de cerca de 29%, o CarenR vence em todos os cinco cenários consecutivos, por margens de 17 a 32 mil hectolitros. Quero ser explícito: este é um achado post-hoc exploratório. Identifiquei estes cenários depois de observar o padrão, não antes. Gera uma hipótese testável, não um resultado confirmado. Para o Vinho Verde, não aparece tal recuperação. A quebra estrutural foi demasiado grande e rápida para uma destendência linear separar o sinal climático da mudança estrutural. É por isto que o arcabouço das duas decisões é a contribuição mais importante: dá um diagnóstico preciso do *porquê* de a previsão falhar e do *que* precisaria de mudar para a corrigir."

---

## Slide 10 — O Contraste Douro vs Vinho Verde
**Objetivo:** Enquadrar a experiência comparativa como uma análise de sensibilidade desenhada, não um acidente.

**Conteúdo recomendado:**
- As duas regiões foram escolhidas como uma *experiência comparativa natural*: o mesmo pipeline, os mesmos algoritmos, histórias estruturais diferentes
- **RDD:** Aumento gradual +80%; a destendência linear alcança estacionaridade; confundimento por era moderado; o CarenR vence em 5/18 cenários; a característica de topo (água no solo na vindima) mantém 125% de correlação sob LOESS
- **RVV:** Quebra estrutural (−65%, não-linear); a destendência linear deixa não-estacionaridade residual; confundimento por era severo; o CarenR vence 0/16 cenários; as características de topo (LAI, água no solo) colapsam para 15–16% sob LOESS — **artefactos impulsionados por era**
- A inversão direcional da mesma característica entre regiões é um ponto de validação do método: o CarenR deteta estrutura específica da região, não correlações espúrias globais
- **Lição:** O mesmo algoritmo, a mesma engenharia de características, o mesmo protocolo de validação produz resultados qualitativamente diferentes conforme a história estrutural. Esta comparação define as condições sob as quais a descoberta de regras distribucionais funciona.

**Visual a incluir:**
> Tabela 6.1 (explicação estrutural da divergência RDD vs RVV). Três linhas: tendência de produção, confundimento por era, desempenho do CarenR. Clara e concisa.

**Notas do orador:**
> "O contraste entre o Douro e o Vinho Verde não é uma fraqueza metodológica — é uma característica do desenho experimental. Ao correr pipelines idênticos em duas regiões estruturalmente diferentes, a tese identifica o que determina se a previsão baseada em regras tem sucesso. A resposta não é o algoritmo. A resposta é a história estrutural da série de produção. A tendência ascendente gradual e gerível do Douro permite que uma destendência linear isole resíduos climáticos genuínos. A quebra estrutural catastrófica do Vinho Verde — impulsionada pela política agrícola da UE, não pelo clima — contamina o sinal climático de forma tão severa que nenhuma abordagem baseada em regras a consegue superar sem covariáveis estruturais. Este achado não seria visível num estudo de região única."

---

## Slide 11 — Contribuições Científicas
**Objetivo:** Enunciar as contribuições de forma clara e defensável. Os membros do júri vão desafiar cada uma.

**Conteúdo recomendado:**
Cinco contribuições, ordenadas por originalidade:

1. **Arcabouço de Decomposição em Duas Decisões** — separa o erro de previsão em qualidade da tendência e extração de sinal climático; ferramenta de diagnóstico transferível para qualquer problema de previsão agroclimática com não-estacionaridade estrutural.

2. **Primeira avaliação walk-forward sistemática de descoberta de subgrupos distribucional para previsão vinícola** — 17 modelos, 18/16 cenários, todo o pré-processamento dentro do fold, testes estatísticos explícitos a distinguir achados confirmatórios de exploratórios.

3. **Análise de sensibilidade sistemática de 11 variantes CarenR** — demonstra que a seleção ótima de características é específica do problema (η² no RDD, Sup_FS no RVV); motiva a avaliação estruturada de variantes como prática padrão.

4. **Análise de composição por era** — caracteriza não só *se* a previsão baseada em regras funciona, mas *sob que condições de dados* funciona (limiar de 29% de era moderna no RDD; exploratório).

5. **Validação entre algoritmos** — o RIPPER e o M5Rules confirmam independentemente as seleções de características do CarenR, fornecendo evidência de estrutura genuína dos dados.

**Visual a incluir:**
> Uma lista numerada limpa. Sem figura. As contribuições falam por si.

**Notas do orador:**
> "A tese faz cinco contribuições. A mais fundamental é a decomposição em duas decisões: este arcabouço explica precisamente porque um algoritmo pode descobrir padrões válidos a partir de dados históricos e ao mesmo tempo falhar em melhorar a previsão fora da amostra. É a ferramenta analítica que torna o resultado negativo do Objetivo 2 interpretável em vez de meramente dececionante. A segunda contribuição é o próprio arcabouço de validação — 17 modelos, protocolo walk-forward rigoroso, classificação explícita de achados confirmatórios versus exploratórios. Quero ser transparente quanto a isto: reporto os p-values com o seu estatuto. O resultado equilibrado por era no Douro é exploratório, identificado post-hoc, e não pode ser tratado como confirmado sem replicação prospetiva."

---

## Slide 12 — Limitações, Trabalho Futuro & Encerramento
**Objetivo:** Mostrar maturidade intelectual. Reconhecer os limites honestamente; apontar em frente de forma construtiva.

**Conteúdo recomendado:**

**Limitações:**
- A destendência estrutural é a limitação central: o tempo é a única covariável de destendência; as mudanças estruturais de produção (área plantada, quota do Porto) confundem-se com efeitos do clima
- O pequeno número de cenários de teste (18/16) limita a potência estatística — o achado RDD post-hoc (n=5) tem potência insuficiente para conclusões confirmatórias
- Agregados anuais de região única — nenhuma análise sub-regional ou espacial possível

**Trabalho futuro (três propostas específicas e acionáveis):**
1. **Destendência estrutural:** Usar área plantada + quota do Porto + número de produtores certificados como covariáveis de destendência explícitas. Dados disponíveis no IVV/IVDP. Resolveria diretamente a tensão Decisão 1/Decisão 2.
2. **Validação prospetiva:** Pré-registar a hipótese equilibrada por era (limiar de 29% no RDD) e testá-la à medida que se acumulam anos adicionais da era moderna.
3. **Exceptional Model Mining:** Aplicar o arcabouço EMM para detetar subgrupos onde a *relação de regressão clima-produção* é excecional — captando efeitos de interação que o teste KS de distribuição marginal não apanha.

**Frase de encerramento:**
> "A aprendizagem automática baseada em regras ainda não consegue prever de forma fiável a produção vinícola portuguesa. Mas consegue explicá-la — com regras validadas, auditáveis e agronomicamente significativas em que três algoritmos independentes concordam. Compreender *porque* a previsão falha revelou-se tão valioso como a própria previsão."

**Visual a incluir:**
> As duas séries temporais de produção de novo (Figuras 3.1+3.2), emparelhadas com uma das regras de topo do CarenR exibida como uma frase em linguagem natural. Fecha o círculo a partir da abertura.

---

# PARTE 2 — NOTAS DETALHADAS DO ORADOR (Por Slide, Prontas a Ler)

As notas do orador acima na Parte 1 estão prontas a ler. As notas de cada slide foram concebidas para:
- Soar conversacionais (não lidas de um slide)
- Demorar 50–70 segundos
- Encadear naturalmente no slide seguinte

**Frases de transição:**
- Slide 1→2: "Deixem-me começar com o problema que esta tese foi concebida para resolver."
- Slide 2→3: "Essa lacuna na literatura moldou dois objetivos concretos."
- Slide 3→4: "Para perseguir esses objetivos, precisei de dados e de um arcabouço de avaliação rigoroso."
- Slide 4→5: "Antes de mostrar resultados, quero introduzir o arcabouço analítico que os torna interpretáveis."
- Slide 5→6: "Com esse arcabouço montado, deixem-me mostrar como são as regras."
- Slide 6→7: "Estas regras parecem convincentes. Mas como sabemos que não são artefactos de um algoritmo?"
- Slide 7→8: "A validação entre algoritmos confirma que o sinal existe. Agora a pergunta difícil: as regras conseguem prever?"
- Slide 8→9: "A previsão falhou no agregado. A pergunta é porquê — e a resposta é mais informativa do que um resultado positivo teria sido."
- Slide 9→10: "Esse confundimento por era explica ambas as regiões, mas de formas muito diferentes."
- Slide 10→11: "Esta comparação afia as contribuições do trabalho."
- Slide 11→12: "Essas contribuições vêm com limitações que quero abordar diretamente."

---

# PARTE 3 — RECOMENDAÇÕES DE DESENHO VISUAL

## Estética geral
- Usa um template limpo e minimal com fundo branco ou cinzento muito claro
- Uma cor de destaque: um vermelho-púrpura profundo (referência ao vinho, mas subtil) para cabeçalhos de secção e destaques-chave
- Evita slides carregados de pontos — usa o espaço visual intencionalmente
- Tipo de letra: uma serifada para os títulos dos slides (Georgia ou Garamond — registo académico), sem serifa para o corpo (Calibri ou Helvetica)

## Notas de desenho específicas por slide

**Slides 1, 12 (abertura e encerramento):** Usa as Figuras 3.1 + 3.2 como fundo de largura total ou gráfico central grande. Deixa as duas curvas de produção fazer a narrativa visual.

**Slide 2 (Problema de Investigação):** Mostra a Tabela 2.1 (mapa da literatura) como uma tabela limpa de 5 colunas. Usa cor para destacar as células vazias (onde nenhum trabalho anterior pontua "sim") — isto torna a lacuna de investigação visualmente óbvia.

**Slide 4 (Desenho walk-forward):** A Figura 3.4 deve ser grande — pelo menos 60% da área do slide. É a figura de metodologia mais importante. Rotula claramente os segmentos "treino" e "teste". Acrescenta um destaque: "Todo o pré-processamento dentro desta janela."

**Slide 5 (Arcabouço das Duas Decisões):** Cria um diagrama personalizado de duas caixas. Não uses nenhuma figura da tese aqui — desenha-o de novo, simplificado. A seta de tensão entre a Decisão 1 e a Decisão 2 é a carga conceptual. Acrescenta um pequeno destaque com os números do LOESS: "LOESS: Decisão 1 −36%, Decisão 2 → 0 regras."

**Slide 6 (Descoberta de Regras):** Mostra as Tabelas 4.1 e 4.2 lado a lado, reduzidas a 4 colunas (Condições, Direção, Δ mhl, Suporte). Destaca a Regra 4 (RDD +192 mhl) e a Regra 4 (RVV +311 mhl) com fundo colorido. São os "números-herói".

**Slide 8 (Rankings de MAE):** Figuras 5.1 e 5.2 lado a lado. Acrescenta uma linha vertical tracejada vermelha na barra da Naive_Median em cada gráfico para enfatizar visualmente a baseline. Rotula as barras do CarenR explicitamente.

**Slide 9 (Composição por Era):** Figura 5.3 anotada com duas zonas entre parênteses: "Confundido por era (1–9)" e "Equilibrado por era (10–14, todas vitórias CarenR)". A anotação visual torna o padrão inegável.

**Slide 11 (Contribuições):** Uma lista numerada sem figura. Cada contribuição numa frase. Considera um layout de duas colunas com a contribuição à esquerda e uma linha de evidência à direita.

## O que NÃO fazer
- Não coloques os passos completos do algoritmo CarenR num slide
- Não mostres a tabela completa da análise de sensibilidade das 11 variantes — referencia-a brevemente
- Não mostres equações matemáticas a menos que o júri peça especificamente
- Não coloques a tabela completa das 48 regras do RDD num slide — as 5 de topo chegam

---

# PARTE 4 — PERGUNTAS PROVÁVEIS DO JÚRI E RESPOSTAS FORTES

### Q1: "Porquê usar o CarenR especificamente? Porque não um aprendiz de regras mais simples?"

**Resposta forte:** "O CarenR é motivado por duas propriedades que os aprendizes de regras mais simples não têm. Primeiro, opera sobre a distribuição-alvo contínua usando o teste de Kolmogorov-Smirnov — não requer discretizar a variável resposta, o que evita perda de informação. Segundo, identifica explicitamente desvios distribucionais, não apenas diferenças de média. Uma característica que desloca a distribuição dos resíduos de produção sem separar limpamente três classes de tercis será detetada pelo CarenR mas perdida pelo RIPPER. É precisamente a situação do LAI na vindima no Vinho Verde — as três variantes CarenR identificam-no como sinal dominante, enquanto o RIPPER e o M5Rules não. A comparação entre algoritmos valida que é um sinal genuíno, não um artefacto do CarenR."

---

### Q2: "Reportas um achado p=0,031 para a janela RDD equilibrada por era. Isso não é escolher a dedo?"

**Resposta forte:** "É um desafio justo e importante. Quero ser completamente explícito: sim, este achado é post-hoc e exploratório. Identifiquei a janela dos cenários 10–14 depois de observar o padrão nos dados, não antes da análise. A tese rotula-o como 'post-hoc exploratório' em todas as tabelas e parágrafos onde aparece, e afirma explicitamente que gera uma hipótese testável em vez de confirmar uma. Um teste binomial em n=5 cenários, identificado post-hoc, não cumpre o padrão de um achado científico confirmatório. O que faz é motivar um teste prospetivo pré-registado: à medida que se acumulam anos adicionais da era moderna nas próximas épocas, a hipótese do limiar de 29% pode ser testada em dados novos e não vistos. Trato-o exatamente como o que é — não um resultado, mas uma direção de investigação."

---

### Q3: "Porque não usar um conjunto de dados espacial ou sub-regional adequado em vez de agregados regionais?"

**Resposta forte:** "Os dados agregados regionais foram os dados disponíveis para 80–89 anos. Dados sub-regionais ou ao nível da parcela com registos fenológicos consistentes ao longo deste período não existem para estas regiões. A tese é explícita quanto a isto como limitação de âmbito na Secção 7.3. A vantagem teórica dos agregados regionais é que captam o sinal de produção sobre o qual as cooperativas, o IVDP e os órgãos de política de facto agem — a produção anual total, não parcelas individuais. Dito isto, a limitação é real: a heterogeneidade espacial dentro de cada região não é captada, e um futuro conjunto de dados com dados meteorológicos em grelha afiaria consideravelmente o sinal distribucional. Está listado como uma direção concreta de trabalho futuro."

---

### Q4: "A Naive_Median vence sempre. Isso não é evidência de que o clima é irrelevante para a produção?"

**Resposta forte:** "Não — e esta distinção é central à tese. A Naive_Median vencer significa que o sinal climático é real mas insuficiente em magnitude para superar o ruído de produção de ano para ano num teste de previsão retido. A validação LOESS no Objetivo 1 mostra que as características de topo mantêm 90–125% da sua correlação com os resíduos de produção após remoção agressiva da tendência de era — o sinal é genuíno. O |r| ≈ 0,47 para as características de topo significa que o clima explica cerca de 22% da variância dos resíduos. Os restantes 78% são ruído de fatores fora do conjunto de características — doenças da vinha, logística, agregação de medições. Numa série com coeficiente de variação ~47%, esse patamar de ruído é demasiado alto para a previsão baseada em regras bater consistentemente a mediana. Isto não é irrelevância do clima; é uma propriedade do sistema estudado à agregação anual regional."

---

### Q5: "Porquê 11 variantes CarenR? Parece garimpagem de dados."

**Resposta forte:** "As 11 variantes foram desenhadas como uma análise de sensibilidade estruturada, não uma pesquisa post-hoc pelo melhor resultado. Variam ao longo de três dimensões ortogonais: critério de seleção de características (η², Pearson |r|, limiar duro), se um filtro de suporte é aplicado, e como as previsões das regras são agregadas. A decisão de avaliar todas as variantes em simultâneo — em vez de selecionar uma a priori — foi tomada precisamente para evitar esta objeção. O achado-chave da análise de sensibilidade é em si uma contribuição: a configuração ótima é específica do problema. O η² funciona melhor no Douro porque deteta associações distribucionais não-lineares. O Sup_FS funciona melhor no Vinho Verde porque o seu mecanismo de recurso conservador evita disparar regras espúrias impulsionadas por era. Um investigador que se comprometa com uma variante a priori perde este insight. A tese recomenda a avaliação estruturada de variantes como prática padrão por esta razão."

---

### Q6: "Qual é o valor prático das regras se não conseguem melhorar a previsão?"

**Resposta forte:** "As regras têm três usos práticos que não requerem bater uma previsão de referência. Primeiro, fornecem descrições agroclimáticas validadas e legíveis que os agrónomos podem usar para avaliação de colheita e comunicação de risco — 'as condições deste ano correspondem ao padrão seco pós-floração associado a produção acima da tendência em 28% dos anos históricos'. Segundo, identificam que variáveis climáticas são genuinamente informativas, o que é valioso para gestão de rega, decisões de controlo de copado e tarifação de seguros. Terceiro, quantificam a magnitude do desvio distribucional — '+192 mil hectolitros acima da tendência' é uma estimativa, não apenas um sinal direcional. A distinção entre 'o clima afeta a produção' e 'o sinal climático é forte o suficiente para bater a baseline na previsão' é real, mas ambas as metades são cientificamente significativas."

---

### Q7: "Consideraste métodos de ensemble ou stacking?"

**Resposta forte:** "Sim — o CarenR_Dist_Stack é uma das 11 variantes. Usa as previsões de regras do CarenR como entradas de um modelo linear de segundo estágio. Ficou em 14.º de 17 no RDD e 8.º de 17 no RVV — pior do que o CarenR simples ponderado por suporte. A razão provável é que o stacking requer variância suficiente nas previsões do primeiro estágio para ajustar um modelo de segundo estágio significativo; com apenas 18/16 cenários walk-forward, a camada de stacking sobreajusta aos resíduos de treino em vez de aprender uma combinação estável. A variante CarenR_Dist_Conf — que pondera as regras pela sua significância estatística KS — ficou em 6.º no RDD e 3.º no RVV. Ambas as variantes confirmam que o método de agregação de previsões importa menos do que a seleção de características."

---

# PARTE 5 — PONTOS FRACOS E COMO OS DEFENDER

### Fraqueza 1: O achado central é negativo (Objetivo 2 não alcançado)
**Como defender:** "Um resultado negativo confirmado num arcabouço de validação rigoroso é um achado científico, não uma falha. A literatura está cheia de estudos que reivindicam resultados positivos de previsão a partir de desenhos de avaliação inadequados — validação cruzada aleatória em séries temporais, sem controlo de fuga de dados, sem correção para testes múltiplos. Esta tese usa a avaliação metodologicamente correta, reporta o resultado negativo honestamente, e explica *porque* o resultado é negativo em termos mecanísticos. O arcabouço das duas decisões que explica a falha é uma contribuição que emerge *do* resultado negativo. Um resultado positivo não teria motivado essa decomposição analítica."

### Fraqueza 2: Achado post-hoc exploratório (p=0,031, n=5)
**Como defender:** Já coberto na Q2. Enfatiza a rotulagem, o pequeno tamanho de amostra e a proposta de validação prospetiva. Não tentes defendê-lo como achado confirmatório — isso pioraria as coisas.

### Fraqueza 3: Apenas duas regiões; não se generaliza amplamente
**Como defender:** "O desenho comparativo de duas regiões foi deliberado. Duas regiões com histórias estruturais contrastantes permitem à tese distinguir achados específicos da metodologia de achados específicos do domínio. Um estudo de região única não o consegue. As condições sob as quais o arcabouço tem sucesso (tendência gradual, confundimento por era moderado) e falha (quebra estrutural, confundimento por era severo) são identificadas precisamente porque ambos os casos estão presentes. A generalização a outras regiões requer aplicar a mesma avaliação estruturada, não assumir o mesmo resultado."

### Fraqueza 4: 59 características podem introduzir problemas de testes múltiplos
**Como defender:** "O CarenR aplica um limiar de significância KS e um filtro de suporte de 20% dentro da sua indução de regras. O filtro de suporte é a salvaguarda prática contra regras espúrias que cobrem apenas 2–3 observações. A validação LOESS fornece uma verificação independente de que as características de topo mantêm sinal genuíno dentro da era. A convergência entre algoritmos — três algoritmos com diferentes espaços de pesquisa a identificar as mesmas características — é a salvaguarda mais forte contra falsas descobertas. Uma característica que aparece em 54% das regras E é selecionada pelo RIPPER E aparece nos coeficientes do M5Rules não é um artefacto de testes múltiplos."

### Fraqueza 5: O CarenR-Supervised foi o pior — isso não desacredita o arcabouço?
**Como defender:** "O CarenR_Supervised usa discretização supervisionada por fold — as fronteiras dos bins são otimizadas no mesmo fold usado para avaliar a qualidade da regra. É uma forma de fuga do alvo no passo de engenharia de características. O seu fracasso consistente é um *achado metodológico*, não um descrédito do arcabouço: demonstra precisamente onde a fuga entra e confirma que a discretização em frequência igual não-supervisionada usada por todas as outras variantes é a abordagem correta. O resultado é reproduzível, consistente em ambas as regiões, e fornece uma recomendação prática clara: não usar discretização supervisionada na indução de regras agroclimáticas."

### Fraqueza 6: Os resultados do RVV são fortemente negativos — isso significa que o CarenR só é aplicável a um tipo de região?
**Como defender:** "O resultado do RVV não é que o CarenR é inaplicável. É que o CarenR não consegue superar a não-estacionaridade estrutural que uma simples destendência linear não separa do sinal climático. A análise LOESS mostra que, com remoção adequada da tendência de era, a Naive_Median do RVV cai 36% — *há* variação estrutural para explicar. O problema é que remover essa variação estrutural também remove os resíduos climáticos. A destendência estrutural usando covariáveis não climáticas — área plantada, número de produtores, rácios de sistema de condução — daria ao CarenR resíduos mais limpos para trabalhar. O resultado do RVV define o problema e motiva a solução."

---

# PARTE 6 — O GUIÃO FALADO DE 15 MINUTOS

*Este guião foi concebido para ser ensaiado e adaptado. O tempo aproximado é mostrado entre parênteses. Fala naturalmente; não leias literalmente, mas usa isto como base de preparação.*

---

**[SLIDE 1 — 0:00–1:00]**

"Antes de introduzir o meu título, quero que olhem para estas duas linhas. [aponta para as Figuras 3.1+3.2] Representam 80 anos de produção vinícola anual em duas regiões vizinhas do norte de Portugal — o Douro e o Vinho Verde. Uma praticamente duplicou ao longo do período de estudo. A outra declinou 65% em apenas quatro décadas. Mesmo país, mesma zona climática geral, mas trajetórias estruturalmente muito diferentes. A minha tese coloca uma pergunta enganadoramente simples: podem regras interpretáveis de aprendizagem automática *explicar* e *prever* o que impulsiona a produção acima ou abaixo da tendência em cada região? A resposta, afinal, é mais matizada — e mais interessante — do que um simples sim ou não."

---

**[SLIDE 2 — 1:00–2:00]**

"O rendimento vinícola é notoriamente difícil de modelar. É sensível à temperatura, precipitação, água no solo, desenvolvimento do copado e efeitos de carryover multi-anuais da fisiologia da vinha. A aprendizagem automática moderna lida muito bem com tudo isto — random forests, gradient boosting, deep learning conseguem todos prever com razoável exatidão. O problema é a interpretabilidade. Um agrónomo não consegue agir sobre um valor SHAP. O que os profissionais precisam é de algo que possam ler, contestar e aplicar — uma afirmação como: 'Se a água no solo antes da vindima ficar abaixo de um certo limiar, e os dias húmidos pós-floração forem menos de três, a produção tende a ficar 120 mil hectolitros acima da tendência num ano em cada quatro.' Nenhum trabalho publicado aplicou este tipo de descoberta de regras distribucionais interpretáveis a registos regionais de produção vinícola de várias décadas. É essa a lacuna que esta tese preenche."

---

**[SLIDE 3 — 2:00–3:00]**

"A tese persegue dois objetivos complementares. O Objetivo 1 é a descoberta de regras: consegue o CarenR — um algoritmo de descoberta de subgrupos distribucional — extrair regras agroclimáticas estatisticamente validadas e legíveis a partir de 80–89 anos de dados? O Objetivo 2 é a validação preditiva: essas regras melhoram as previsões numa avaliação walk-forward rigorosa? Posso dizer-vos já que o Objetivo 1 tem sucesso e o Objetivo 2 não tem sucesso no agregado. Mas quero ser claro: a assimetria entre estes dois resultados não é uma falha. Revela-se o achado cientificamente mais informativo de todo o estudo — e gera um arcabouço que explica quando e porque a previsão baseada em regras pode ter sucesso."

---

**[SLIDE 4 — 3:00–4:00]**

"Deixem-me descrever brevemente a montagem experimental, porque o rigor metodológico é o que torna os resultados credíveis. Dois conjuntos de dados: o Douro, 89 anos de 1934 a 2022; o Vinho Verde, 80 anos de 1942 a 2021. A partir de registos meteorológicos diários criei 59 características agroclimáticas por região — temperatura, precipitação, água no solo, índice de área foliar, contagens de dias de calor e geada — todas ancoradas em datas fenológicas da vinha, de modo que uma característica como 'dias húmidos pós-floração' se adapta à data de floração real de cada ano. Três algoritmos de aprendizagem de regras foram comparados: o CarenR para regras distribucionais, o RIPPER para regras de classificação, e o M5Rules para regressão por partes. [aponta para a Figura 3.4] O desenho de validação é um protocolo walk-forward de janela expansível — o modelo é sempre treinado no passado e avaliado no que veio a seguir. Crucialmente, todo o pré-processamento — destendência, discretização — é feito dentro de cada fold de treino para evitar fuga. A referência primária é a Naive_Median: o modelo que prevê sempre o resíduo mediano de treino. Qualquer aprendiz de regras que a bata extraiu genuinamente informação climática."

---

**[SLIDE 5 — 4:00–5:15]**

"Antes de mostrar resultados, quero introduzir o arcabouço analítico que os torna interpretáveis. Chamo-lhe a decomposição em duas decisões. Qualquer pipeline de previsão agroclimática envolve dois problemas sequenciais. Decisão 1: quão bem a tendência do período de treino extrapola para os anos de teste? Isto é medido pelo MAE da Naive_Median — o teu piso de erro. Um melhor ajuste da tendência baixa este piso. Decisão 2: consegue um modelo baseado em regras prever os resíduos melhor do que zero? Isto requer resíduos estruturados para explorar. Eis a tensão fundamental: uma destendência mais flexível reduz o erro da Decisão 1, mas ao mesmo tempo retira o sinal residual de que a indução de regras depende. Posso mostrar-vos isto concretamente. No Vinho Verde, aplicar a destendência flexível LOESS reduz o MAE da Naive_Median de 140 para 90 mil hectolitros — uma melhoria de 36% na Decisão 1. Mas ao mesmo tempo elimina todas as regras CarenR — zero regras disparam em todos os 16 cenários de validação. Melhorar a tendência mata o sinal. Esta tensão não pode ser resolvida com a montagem atual. Requer destendência estrutural — usar covariáveis não climáticas para separar mudanças da indústria de efeitos do clima. Voltarei a isto."

---

**[SLIDE 6 — 5:15–6:30]**

"Agora, as regras em si. [aponta para as Tabelas 4.1+4.2] O CarenR descobriu 48 regras para o Douro e 20 para o Vinho Verde, todas a passar significância estatística e um filtro de suporte de 20%. As regras do Douro convergem numa história agroclimática clara. A condição dominante — a aparecer em 54% de todas as 48 regras — é défice moderado de água no solo na vindima. Combina isso com condições secas pós-floração e stress térmico limitado, e obténs um perfil de produção consistentemente acima da tendência. A regra mais forte é um mecanismo multi-anual: água no solo moderada pré-vindima combinada com condições secas pós-vindima no ano *anterior*, captando o mecanismo de reserva de hidratos de carbono da vinha. Essa regra está associada a um desvio de +192 mil hectolitros acima da tendência — num ano em cada cinco no registo histórico. No Vinho Verde, a história está estruturalmente invertida: 16 das 20 regras apontam *abaixo* da tendência. O clima atlântico significa que as condições associadas a anos abaixo da tendência são frescas e húmidas — a ausência das condições quentes e secas que beneficiam o Douro. A única regra acima da tendência no Vinho Verde é a mais poderosa de qualquer das regiões: uma combinação de abrolhamento quente do ano anterior e floração amena do ano corrente que produz +311 mil hectolitros acima da tendência. Estas são agronomicamente significativas, direcionalmente consistentes e validadas: as características de topo do Douro mantêm 90–125% da sua correlação com a produção após remoção agressiva da tendência de era. O Objetivo 1 é alcançado."

---

**[SLIDE 7 — 6:30–7:15]**

"Uma preocupação-chave com qualquer saída algorítmica é se os padrões são genuínos ou artefactos. A validação entre algoritmos aborda isto diretamente. [aponta para a Tabela 4.9] O RIPPER, usando ganho de informação para separar três tercis de produção, e o M5Rules, usando minimização do erro quadrático para regressão, ambos identificam independentemente a água no solo na vindima, a família de características LAI e a temperatura de verão do ano anterior como as variáveis mais informativas. Três perguntas diferentes, três algoritmos diferentes, a mesma resposta. Essa convergência — sob critérios de otimização inteiramente diferentes — é a evidência mais forte de que estas características refletem estrutura genuína dos dados. Um detalhe importante: o M5Rules atribui um coeficiente *negativo* à água no solo na vindima no global, enquanto o CarenR identifica uma zona acima da tendência em défice *moderado*. Esta aparente contradição é resolvida pela não-linearidade: o M5Rules não consegue modelar um efeito de limiar sem termos de interação explícitos, portanto o coeficiente linear global absorve co-ocorrências confundidas. A estrutura condicional do CarenR capta o que o modelo linear não consegue."

---

**[SLIDE 8 — 7:15–8:15]**

"Objetivo 2. [aponta para as Figuras 5.1+5.2] Nenhum modelo baseado em regras bate consistentemente a Naive_Median em qualquer das regiões. No Douro, a melhor variante CarenR — o CarenR Eta2 — fica em segundo com 189 mil hectolitros de MAE ponderado versus 184 da baseline. Uma diferença de 3%, não estatisticamente significativa. No Vinho Verde, a diferença cresce para 22%, e um teste estatístico confirmatório mostra que quando o CarenR dispara regras, essas regras são significativamente *piores* do que a baseline ingénua — p=0,004. O CarenR vence em zero de dezasseis cenários do Vinho Verde. O RIPPER é o pior em ambas as regiões, confirmando que as fronteiras de classificação duras são particularmente inadequadas para este conjunto de dados pequeno e de alto ruído. Reporto estes resultados sem suavizar. O Objetivo 2 não é alcançado. A pergunta é porquê."

---

**[SLIDE 9 — 8:15–9:30]**

"A análise por cenário revela a resposta. [aponta para a Figura 5.3] No Douro, quando represento o MAE do CarenR contra a Naive_Median cenário a cenário, emerge um padrão. Nos cenários iniciais, onde os dados de treino são dominados por observações pré-1990, o CarenR perde para a baseline consistentemente. Mas a partir do cenário 10 — assim que os dados pós-1990 atingem aproximadamente 29% do conjunto de treino — o CarenR vence em cinco cenários consecutivos, por margens de 17 a 32 mil hectolitros. Quero ser transparente: este é um achado post-hoc exploratório. Identifiquei o limiar de 29% depois de observar o padrão, não antes. O p-value binomial de 0,031 em cinco cenários não constitui um resultado confirmatório. O que faz é gerar uma hipótese testável: à medida que se acumulam anos adicionais da era moderna, este limiar pode ser validado prospetivamente. Para o Vinho Verde, não aparece tal recuperação. Acrescentar mais dados da era moderna piora o Vinho Verde. A quebra estrutural foi demasiado grande e rápida para a destendência linear a desemaranhar. As regras do CarenR no Vinho Verde tornam-se cada vez mais afinadas a padrões específicos de era que não generalizam. [aponta para a Tabela 6.1] Esta tabela capta a explicação estrutural: a tendência gradual do Douro permite confundimento por era modesto; a quebra estrutural do Vinho Verde cria um confundimento severo que elimina a vantagem do sinal climático."

---

**[SLIDE 10 — 9:30–10:15]**

"O contraste entre as duas regiões não é uma inconsistência inconveniente — é uma característica do desenho experimental. Ao correr o pipeline idêntico em duas regiões estruturalmente diferentes, a tese identifica o que determina se a descoberta de regras distribucionais tem sucesso. Não é o algoritmo. É a história estrutural da série de produção. Quando a mudança estrutural é gradual e gerível, a destendência linear isola resíduos climáticos genuínos e o CarenR consegue encontrá-los. Quando a mudança estrutural é abrupta e impulsionada por política, nenhuma sofisticação algorítmica supera o sinal contaminado. Esta comparação seria invisível num estudo de região única. A inversão direcional — onde a mesma característica, água no solo na vindima, prevê acima da tendência no Douro e abaixo da tendência no Vinho Verde — é um ponto de validação do método: o CarenR está a detetar estrutura distribucional específica da região, não uma correlação espúria global."

---

**[SLIDE 11 — 10:15–11:15]**

"Deixem-me resumir as contribuições. Primeira: o arcabouço de decomposição em duas decisões, que separa o erro de previsão em qualidade da tendência e extração de sinal climático e fornece um diagnóstico de porque a previsão baseada em regras falha mesmo quando existe sinal genuíno. Este arcabouço é transferível para qualquer problema de previsão agroclimática com não-estacionaridade estrutural. Segunda: a primeira avaliação walk-forward sistemática de descoberta de subgrupos distribucional para previsão vinícola — 17 modelos, protocolo rigoroso, classificação explícita de achados confirmatórios versus exploratórios. Terceira: uma análise de sensibilidade estruturada de 11 variantes a mostrar que a seleção ótima de características é específica do problema — a seleção agressiva funciona em regimes com sinal forte, a seleção conservadora que preserva o recurso funciona em regimes com sinal fraco ou contaminado. Quarta: análise de composição por era que caracteriza não só se a previsão baseada em regras funciona, mas sob que condições de dados funciona. Quinta: validação entre algoritmos a mostrar que três algoritmos independentes convergem nas mesmas famílias de características."

---

**[SLIDE 12 — 11:15–12:30 + margem para Perguntas]**

"Três limitações. Primeira: o passo de destendência usa o tempo como única covariável, confundindo a variação climática com mudanças estruturais da indústria. É a limitação central e o alvo principal para trabalho futuro. Segunda: 18 e 16 cenários são um número pequeno para testes estatísticos; o achado post-hoc tem potência insuficiente. Terceira: os agregados anuais regionais impedem a análise espacial.

O caminho em frente tem três passos concretos. Incorporar covariáveis estruturais — área plantada, quota do Porto, número de produtores certificados — como variáveis de destendência explícitas. Pré-registar a hipótese RDD equilibrada por era e testá-la prospetivamente. Explorar o Exceptional Model Mining, que identifica subgrupos onde a *relação de regressão* clima-produção é excecional, em vez de onde a distribuição marginal se desloca.

Quero encerrar com o achado central numa frase. [regressa ao slide de abertura com as séries temporais de produção] A aprendizagem automática baseada em regras ainda não consegue prever de forma fiável a produção vinícola portuguesa. Mas consegue explicá-la — com regras validadas, auditáveis e agronomicamente significativas em que três algoritmos independentes concordam. E compreender precisamente porque a previsão falha revelou-se tão cientificamente valioso como a própria previsão teria sido. Obrigado."

---

*[Pausa. Contacto visual. Aguarda as perguntas.]*

---

# APÊNDICE — DISTRIBUIÇÃO DO TEMPO

| Slide | Conteúdo | Tempo alvo |
|-------|----------|------------|
| 1 | Título + gancho | 1:00 |
| 2 | Problema de investigação | 1:00 |
| 3 | Objetivos + RQ | 1:00 |
| 4 | Dados + metodologia | 1:15 |
| 5 | Arcabouço das duas decisões | 1:15 |
| 6 | Resultados da descoberta de regras | 1:15 |
| 7 | Validação entre algoritmos | 0:45 |
| 8 | Resultados da validação preditiva | 1:00 |
| 9 | Porque a previsão falhou | 1:15 |
| 10 | Contraste RDD vs RVV | 0:45 |
| 11 | Contribuições | 1:00 |
| 12 | Limitações + encerramento | 1:15 |
| **Total** | | **~13:00** |

*Deixa 2 minutos de margem para variação natural de ritmo. O guião corre aproximadamente 13 minutos a um ritmo de fala constante.*

---

*Guia preparado em modo Cowork — junho de 2026*
*Baseado na tese completa: "Previsão da Produção Vinícola com Aprendizagem Automática Baseada em Regras", Hugo Nogueira, MECD FEUP*
