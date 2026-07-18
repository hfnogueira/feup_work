# Tese Anotada — Guia de Leitura (pontos de maior risco)

**Previsão da Produção Vinícola com Aprendizagem Automática Baseada em Regras** · Hugo Nogueira · MECD, FEUP

Guia de estudo que segue a estrutura da tese. Para cada ponto de maior risco: **[Afirmação]** o que a tese diz · **[P]** pergunta(s) provável(is) do júri · **[R]** resposta curta · **[Ver]** onde está a resposta completa nos outros documentos.

Documentos de apoio: `01_preparacao_arguente_azevedo` (arguente), `02_critica_e_ataques_do_juri` (lista de alvos), `03_Perguntas_e_Respostas` (respostas modelo), `06_100_perguntas_treino` (treino), `07_ERRATA` (correções), `08_como_o_caren_funciona` (algoritmo).

---

## Resumo (Abstract)

**1. "A descoberta funciona; a previsão não bate a mediana."**
- **[P]** O título diz "previsão", mas a previsão falha. O título é honesto?
- **[R]** A previsão é o objeto de estudo, não uma promessa de sucesso. O achado central — *descobrir ≠ prever* — só é possível porque testei a previsão a sério. Reformulo o resultado nulo como diagnóstico, não como fracasso.
- **[Ver]** 03 Q&A P1; 10 script slide 20.

**2. "|r| ≈ 0,47; o clima explica ~22% da variância dos resíduos."**
- **[P]** Se a correlação existe, porque não prevê?
- **[R]** A descoberta só precisa que o sinal *exista* (KS deteta o desvio); a previsão precisa que ele *domine*. Com CV≈47%, 78% da variância é ruído — abaixo do que qualquer método bate a mediana fora da amostra.
- **[Ver]** 03 Q&A P1/P3; 08 secção "dados pequenos".

**3. [ERRATA] "LAI/copado no RVV."**
- **[P]** O LAI domina as regras do RVV?
- **[R]** Não. A família LAI aparece em **4/20** regras; os dominantes são dias de stress térmico (até 50%) e água no solo (25%). Corrigido no resumo, Tabela 4.9, Apêndice C, Cap.2/4. Está na errata.
- **[Ver]** 07 ERRATA item 6.

---

## Capítulo 1 — Introdução

**4. Beneficiário e motivação (produção total regional).**
- **[P]** A produção total anual regional é de facto útil para decisões? Porquê não rendimento por hectare?
- **[R]** É a quantidade sobre a qual cooperativas, IVDP e planeamento agem. Rendimento/ha remove o efeito de área na origem — concedo que é a extensão óbvia — mas a produção total é a variável de decisão e a série de área não está disponível para todo o horizonte.
- **[Ver]** 02 crítica #1 (quase certa); 03 Q&A P20.

---

## Capítulo 3 — Métodos (o mais escrutinado)

**5. Destendência linear, o tempo como única covariável (Decisão 1).**
- **[P]** Porquê destendência só com o tempo? Porquê linear e não LOESS como principal?
- **[R]** A OLS linear é simples, transparente e aplicável dentro do fold sem afinação. O LOESS é usado como *teste de esforço*, não como destendência de produção — precisamente para caracterizar o que a linear deixa escapar. É a raiz da tensão das duas decisões (Cap.6).
- **[Ver]** 03 Q&A W9; 10 script slide 21.

**6. Discretização em quartis de frequência igual.**
- **[P]** Porquê frequência igual e não largura igual ou supervisionada (Fayyad–Irani)?
- **[R]** Frequência igual garante ~22 obs por bin, de que o teste KS precisa com n pequeno. Testei a supervisionada (CarenR_Supervised) — foi a **pior** variante nas duas regiões: fuga do alvo. Resultado negativo replicável.
- **[Ver]** 03 Q&A P7; 08 Passo 1.

**7. Pesquisa por níveis (Apriori), não beam search. [ERRATA]**
- **[P]** Como é que o CAREN pesquisa? (pergunta de quadro provável — Azevedo)
- **[R]** Geração de candidatos por níveis com poda de suporte por antimonotonia — exaustiva sobre os itemsets frequentes, não uma beam heurística — depois o teste KS como segundo filtro. Comprimento 3–4. **Saber desenhar o reticulado.**
- **[Ver]** 01 Parte 4 (quadro); 08 Passo 2; 07 ERRATA item 3.

**8. Suporte ≥ 20%, p ≤ 0,10. [ERRATA: era 15% em alguns sítios]**
- **[P1]** Porquê p ≤ 0,10 e não 0,05? **[P2]** Milhares de subgrupos sem correção múltipla — quantas das 48 regras são falsas descobertas?
- **[R]** 0,10 é um compromisso de potência com n=80–89 (0,05 esvazia o conjunto em vários folds); as regras enfrentam um segundo filtro mais duro (MAE fora da amostra). Contra falsas descobertas: suporte 20%, revalidação LOESS, convergência de 3 algoritmos, e a própria validação walk-forward. Concedo permutação/FDR como extensão.
- **[Ver]** 03 Q&A P3/P4/P5; 07 ERRATA item 4.

**9. Teste KS (comparação de ECDFs).**
- **[P]** O KS é assintótico; com um subgrupo de ~18 anos os p-values são válidos? O conjunto de referência inclui o subgrupo.
- **[R]** Subgrupos pequenos mas referência de 60–89; a sobreposição torna o teste *conservador* (D subestimado, não inflacionado). Resíduos contínuos → poucos empates. Mais uma razão para tratar as regras como exploratórias.
- **[Ver]** 03 Q&A P2/P5; 08 Passo 3.

**10. Walk-forward de janela expansível, 18/16 cenários, pré-processamento dentro do fold.**
- **[P]** Os cenários partilham a maioria dos anos de teste — o Wilcoxon é válido? Há fuga?
- **[R]** Sem fuga: tudo é calculado no fold de treino. Os cenários estão positivamente correlacionados — isso torna os testes *menos* propensos a significância, consistente com o nulo honesto (p=0,468). Apresento os p-values como indicativos.
- **[Ver]** 02 crítica #6; 03 Q&A P8; 10 script slide 18.

**11. Baseline Naive_Median + métrica MAE.**
- **[P]** O MAE é a métrica que a mediana minimiza por construção — cartas marcadas?
- **[R]** A mediana é o preditor constante ótimo em MAE, portanto batê-la é *precisamente* a afirmação "há sinal climático explorável". Não é espantalho; é a fasquia certa. (Ter números de RMSE à mão; a direção do resultado é improvável de inverter.)
- **[Ver]** 02 crítica #8; 03 Q&A P3.

---

## Capítulo 4 — Resultados: Descoberta (Objetivo 1)

**12. 48 regras (RDD), 20 (RVV); água no solo na vindima em 54% das RDD.**
- **[P]** São causais? São robustas ou sobreajuste?
- **[R]** Associações estatisticamente validadas com coerência agronómica, não causalidade. Robustez: revalidação LOESS + convergência de 3 algoritmos.
- **[Ver]** 03 Q&A P10; 10 script slide 13.

**13. Inversão direcional (mesma variável, sinal oposto entre regiões).**
- **[P]** É estrutura real ou artefacto?
- **[R]** O CarenR deteta o sinal oposto sem nunca ver as regiões juntas — resolve corretamente uma inversão de sinal, o que é evidência de estrutura de domínio específica da região, não correlação espúria global.
- **[Ver]** 10 script slide 14; 03 Q&A P10.

**14. Convergência entre algoritmos (RIPPER, M5Rules, CarenR).**
- **[P]** Convergência = verdade, ou confundimento partilhado (3 algoritmos sobreajustam às mesmas características confundidas)?
- **[R]** Se fosse confundimento, convergiriam em características *temporais* de era (índice do ano); em vez disso convergem em características fisiológicas (água no solo, stress térmico) que sobrevivem ao LOESS no RDD. A corroboração é mais forte onde o LOESS confirma sinal genuíno.
- **[Ver]** 02 crítica #11; 03 Q&A P10.

**15. Validação LOESS (retenção 90–125% no RDD; colapso 15–16% no RVV).**
- **[P]** Se as características do RVV colapsam, as 20 regras do RVV não são inúteis? [Cuidado: o resumo dizia "90–92%"; ver errata/contradição interna]
- **[R]** Apresentar as regras *com* a evidência que as colapsa é a contribuição — mostra o poder de diagnóstico do LOESS. Um investigador ingénuo reportaria 20 regras e concluiria sucesso; a tese mostra porque estaria errado. **Corrigir a afirmação de retenção para não generalizar o RDD ao RVV.**
- **[Ver]** 02 crítica #4; 03 Q&A P6/P16.

---

## Capítulo 5 — Resultados: Previsão (Objetivo 2)

**16. Nenhum modelo bate a mediana: RDD 189 vs 184 (p=0,468); RVV 172 vs 140 (p=0,004).**
- **[P]** Isto não prova que o clima é irrelevante?
- **[R]** Não. O sinal é real (LOESS: 90–125% de retenção no RDD) mas insuficiente em magnitude para superar o ruído (CV≈47%) num teste retido. "O clima afeta" e "o sinal bate a baseline" são afirmações diferentes; ambas são cientificamente significativas.
- **[Ver]** 03 Q&A P1; 10 script slide 19/20.

**17. Achado equilibrado por era: 5/5 vitórias, binomial p=0,031 (~29% era moderna).**
- **[P]** Isto é post-hoc com n=5 — data dredging / HARKing?
- **[R]** Sim, é post-hoc e exploratório, e rotulo-o como tal em todas as tabelas. Gera uma hipótese testável (pré-registar e validar em dados 2023+), não um resultado confirmatório. Não o defender como evidência.
- **[Ver]** 02 crítica #3; 03 Q&A P2/P11.

**18. Seleção da melhor de 11 variantes.**
- **[P]** Escolheste a melhor variante depois de ver os resultados — enviesamento de seleção.
- **[R]** Sim — e isso *reforça* a conclusão principal, porque mesmo a melhor variante post-hoc não bate a Naive_Median. O enviesamento só importaria se eu reivindicasse vitória. Reporto as 11.
- **[Ver]** 03 Q&A P11/P14; 02 crítica.

**19. MAE ponderado pelo tamanho do conjunto de teste.**
- **[P]** A ponderação dá mais peso aos cenários iniciais (mais confundidos por era) — não mascara o desempenho?
- **[R]** Ponderar pelo tamanho de teste é a escolha estatisticamente correta (mais obs, mais influência); reporto MAE por cenário e médio em paralelo para o leitor ver a estrutura de era. A sensibilidade é uma propriedade dos dados, não algo que escondi.
- **[Ver]** 03 Q&A P17; 02 crítica.

---

## Capítulo 6 — Discussão

**20. Arcabouço das duas decisões (contribuição central).**
- **[P]** É genuinamente novo ou uma reformulação da decomposição viés-variância / erro de previsão?
- **[R]** É uma contribuição de *enquadramento/diagnóstico*: nomeia a tensão, operacionaliza-a para localizar o estrangulamento na Decisão 1, e demonstra-a empiricamente via LOESS (RVV: 140→90 mas 0 regras). Não é um teorema novo; é uma ferramenta de diagnóstico transferível.
- **[Ver]** 02 crítica #13; 03 Q&A P9; 10 script slide 21.

**21. Decisão 1 é o estrangulamento (não a Decisão 2).**
- **[P]** Prova que o gargalo é a destendência e não regras fracas.
- **[R]** O passo das regras já se aproxima do teto do sinal (|r|≈0,47 ⇒ ~22% variância); e o LOESS mostra que melhorar a Decisão 1 elimina as regras. Se covariáveis estruturais melhorarem a Decisão 1 e as regras se tornarem competitivas, o arcabouço é confirmado — previsão testável.
- **[Ver]** 03 Q&A P12; 10 script slide 20/21.

**22. Correção proposta: covariáveis estruturais — deixada em trabalho futuro.**
- **[P1]** Nomeias a correção como *A* solução mas não a fizeste — porquê? **[P2]** As covariáveis (área, quotas) estão correlacionadas com o tempo — não recriam o confundimento?
- **[R1]** Decisão de âmbito: reunir covariáveis para 1934–2022 exige investigação de arquivo além do orçamento da tese; concedo que é o passo de maior valor. **R2** A chave é incluí-las no *modelo de destendência* (Decisão 1) e testar a estacionaridade do resíduo antes da indução — como em cointegração. O risco é real mas gerível.
- **[Ver]** 02 crítica #2; 03 Q&A P20.

---

## Os 17 modelos da validação walk-forward — o que cada um significa

A comparação primária é sempre **CarenR vs. Naive_Median** (não CarenR vs. os outros aprendizes). Os 17 modelos = 3 baselines ingénuas + 3 comparadores + 11 variantes CarenR.

**Baselines ingénuas (referências de previsão)**
- **Naive_Median** — prevê o resíduo mediano de treino (≈0). É o preditor constante ótimo sob MAE, logo é *a fasquia a bater*. Vence no agregado nas duas regiões.
- **Naive** — persistência: prevê o valor (resíduo) do ano anterior. Falha porque o lag-1 não é significativo (ACF).
- **Naive_MA** — média móvel a 3 anos do resíduo. Suaviza, mas não capta sinal climático.

**Comparadores baseados em regras (não-CarenR)**
- **M5Rules** — regras de regressão linear por partes (Weka). Com n<89 colapsa num modelo global (CV R² ≈ 0); serve de referência quantitativa e para *cross-check* das famílias de características.
- **RIPPER (JRip)** — regras de classificação; exige alvo discretizado em tercis (Baixo/Médio/Alto), cresce regras por ganho FOIL e poda por MDL. Fraco em amostras pequenas (exatidão ~28–42%); usado para confirmar que as mesmas características aparecem fora do CarenR.

**Comparador de ML sem regras**
- **DT (Árvore de Decisão)** — árvore sobre todas as características; comparador de previsão *não* baseado em regras. Entre os piores — divisões duras inadequadas a n pequeno e ruído alto.

**As 11 variantes CarenR** (o algoritmo KS é o mesmo; diferem sobretudo na *seleção de características* e na *agregação*)
- **CarenR_Dist** — variante base: todas as 59 características; suporte mínimo por omissão.
- **CarenR_Dist_FS** — seleciona as top-N características por |correlação de Pearson| com o alvo.
- **CarenR_Dist_Thresh** — mantém apenas características com |correlação| ≥ 0,30 (limiar duro).
- **CarenR_Dist_Sup** — todas as 59 características, mas suporte mínimo mais alto → regras mais largas/gerais.
- **CarenR_Dist_Sup_Thresh** — limiar de correlação + suporte mínimo mais alto (combina as duas anteriores).
- **CarenR_Dist_Eta2** — top-N características por η² (ANOVA sobre os bins discretizados); a seleção mais rigorosa. **Melhor variante no RDD.**
- **CarenR_Dist_Lag** — configuração Sup_Thresh + resíduos de produção desfasados (lags escolhidos pelo ACF).
- **CarenR_Dist_Conf** — configuração Sup_Thresh; na agregação as regras são ponderadas por confiança, −log(p-value), em vez de por suporte.
- **CarenR_Dist_Stack** — regras Sup_Thresh combinadas por *stacking* com um segundo passo de regressão sobre lags.
- **CarenR_Dist_Sup_FS** — filtro de suporte + seleção top-N Pearson. **Melhor variante no RVV** (o recurso conservador à mediana evita disparar regras espúrias de era).
- **CarenR_Supervised** — discretização supervisionada por fold (os cortes maximizam a separação do resíduo). **Pior variante nas duas regiões — fuga do alvo** (resultado negativo replicável).

**[P] provável:** "Porquê 11 variantes CarenR? Não é garimpagem?" **[R]** Análise de sensibilidade estruturada, não pesquisa pelo melhor resultado: variam em 3 dimensões (critério de seleção, filtro de suporte, agregação); a conclusão — a melhor configuração é específica da região (η² no RDD, Sup_FS no RVV) — é ela própria uma contribuição. Reporto as 11, e mesmo a melhor não bate a Naive_Median. **[Ver]** 03 Q&A P5/P11.

---

## Como usar este guia

1. Lê a tese com este guia ao lado; em cada ponto marcado, pausa e tenta responder à **[P]** em voz alta antes de ver a **[R]**.
2. Os pontos com **[ERRATA]** são os que já corrigimos — sabe a versão certa e que está na errata.
3. Para treino cego, usa o `06_100_perguntas_treino` (perguntas sem resposta).
4. Prioridade máxima (onde uma defesa boa vacila): **#5 rendimento/ha, #7 pesquisa, #8 multiplicidade, #10 independência, #17 post-hoc, #22 correção por fazer.**
