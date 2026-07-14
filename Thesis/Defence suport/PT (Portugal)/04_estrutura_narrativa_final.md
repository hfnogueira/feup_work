# Deck da Defesa — Final (A tua narrativa de 14 slides, espinha das cinco ideias)

**Estrutura:** a história de 14 slides escolhida pelo candidato.
**Disciplina:** cada slide está aparafusado a exatamente uma das cinco ideias, para que a narrativa coerente sobreviva.

## As cinco ideias (a espinha — o Slide 1 promete-as, o Slide 14 recorda-as)
1. **DIFÍCIL** — sinal climático real, enterrado em enorme ruído (r≈0,47, ~22% da variância, CV≈47%).
2. **AUDITÁVEL** — regras que podes questionar, não um modelo em que tens de confiar.
3. **REAL** — estrutura genuína: 68 regras, três algoritmos concordam, sobrevivem ao teste de esforço.
4. **FALHA COM LEI** — não preveem, e uma lei diz porquê: o compromisso das duas decisões.
5. **DIAGNÓSTICO** — a contribuição é saber onde corrigir, e transfere-se para além do vinho.

## Uma frase narrativa
> Problema difícil (1) → regras auditáveis, não uma caixa preta (2) → as regras são reais (3) → mas não preveem, e consigo provar porquê (4) → esse "porquê" é um diagnóstico transferível (5).

## Mapa de cobertura (prova de que a narrativa se mantém coerente)
| Slide | Ideia | Papel |
|-------|-------|-------|
| 1 Título | (todas) | prometer as cinco |
| 2 O problema | **1 DIFÍCIL** | sinal enterrado em ruído |
| 3 Questões de investigação | **1 DIFÍCIL** → ponte | enquadrar os dois objetivos |
| 4 Porquê estas duas regiões | **1 DIFÍCIL** | experiência natural |
| 5 Metodologia (pipeline) | **2 AUDITÁVEL** | um pipeline honesto + antevisão das duas decisões |
| 6 Porquê CarenR | **2 AUDITÁVEL** | método KS = a postura auditável |
| 7 Objetivo 1 – Descoberta de regras | **3 REAL** | 68 regras + convergência de 3 algoritmos + **teste de esforço LOESS** |
| 8 O que aprendemos (agronomia) | **3 REAL** | insight de domínio, inversão |
| 9 Limiares & biologia | **3 REAL / 2 AUDITÁVEL** | a interpretabilidade compensa |
| 10 Objetivo 2 – Validação preditiva | **4 FALHA COM LEI** | a mediana vence |
| 11 Descoberta ≠ Previsão | **4 FALHA COM LEI** | achado **+ mecanismo das duas decisões** |
| 12 Contribuições | **5 DIAGNÓSTICO** | o que se transfere |
| 13 Limitações & Trabalho Futuro | **5 DIAGNÓSTICO** | a correção por destendência estrutural |
| 14 Mensagem final | (todas) | recordar as cinco |

Cobertura: DIFÍCIL ×3, AUDITÁVEL ×2 (+1 partilhado), REAL ×3, FALHA COM LEI ×2, DIAGNÓSTICO ×3. A ideia 4 é fina em *número* de slides mas carrega os dois slides mais pesados (10–11). Nada fica órfão.

---

# Slide a slide

### Slide 1 — Título / Promessa
- **Ideia:** as cinco.
- **Objetivo:** abrir com as cinco afirmações, não um índice.
- **Mensagem:** aqui está toda a apresentação em cinco coisas a recordar.
- **Visual:** foto de vinha, título da tese, e as cinco etiquetas de uma palavra (DIFÍCIL · AUDITÁVEL · REAL · FALHA COM LEI · DIAGNÓSTICO).
- **Figura:** nenhuma.
- **Texto máx.:** título + cinco etiquetas.
- **Notas:** "Vocês leram a tese, portanto não vou recontá-la. Vou defender cinco afirmações — lembrem-se destas cinco e têm a minha tese."
- **~40s** → "A primeira afirmação: este problema é mais difícil do que parece."

### Slide 2 — O Problema
- **Ideia:** **1 DIFÍCIL.**
- **Objetivo:** estabelecer importância e dificuldade em conjunto.
- **Mensagem:** sinal climático real (r≈0,47) mas apenas ~22% da variância, numa série que oscila ~47% de ano para ano.
- **Visual:** séries de produção RDD/RVV emparelhadas com tendências (+80% / −65%).
- **Figura:** `fig_context_ts_rdd.png` + `fig_context_ts_rvv.png`.
- **Texto máx.:** "Sinal r≈0,47 · ~22% da variância · ruído CV≈47%".
- **Notas:** "O vinho é a cultura mais sensível ao clima na Europa, e prever importa. Mas a produção regional são milhares de produtores e meteorologia local que nenhum índice vê. O sinal é real mas explica mal um quinto da variância. Vitórias fáceis não existem aqui."
- **~80s** → "Por isso coloquei duas perguntas precisas."

### Slide 3 — Questões de Investigação
- **Ideia:** **1 DIFÍCIL** (ponte para o método).
- **Objetivo:** enquadrar os dois objetivos de forma limpa — não uma parede de quatro pontos.
- **Mensagem:** Objetivo 1 — conseguimos *descobrir* regras climáticas interpretáveis? Objetivo 2 — essas regras *preveem*? (RQ1–4 encaixadas nestes dois.)
- **Visual:** duas caixas grandes — DESCOBRIR / PREVER — com as quatro RQ como pequenas legendas.
- **Figura:** nenhuma.
- **Texto máx.:** "Objetivo 1: DESCOBRIR regras interpretáveis · Objetivo 2: elas PREVEEM?"
- **Notas:** "Dois objetivos. Descobrir regras que explicam a produção; depois testar — brutalmente — se essas regras preveem. Toda a apresentação é a distância entre estes dois verbos."
- **~55s** → "Ambas as perguntas coloquei-as em duas regiões escolhidas de propósito."

### Slide 4 — Porquê Estas Duas Regiões
- **Ideia:** **1 DIFÍCIL** (contraste controlado).
- **Objetivo:** enquadrar RDD vs RVV como uma experiência natural que explicará os resultados mais à frente.
- **Mensagem:** Douro (89 anos, +80% gradual, amortecido por quota) vs Vinho Verde (80 anos, −65% estrutural após UE-1986) — o mesmo método, não-estacionaridade oposta.
- **Visual:** mapa de Portugal, ambas as regiões realçadas + faixa de 4 factos.
- **Figura:** nenhuma (faixa personalizada).
- **Texto máx.:** "RDD +80% gradual · RVV −65% quebra estrutural — o mesmo método, duas histórias".
- **Notas:** "Escolhi duas regiões com histórias opostas. O Douro subiu gradualmente, em parte amortecido pela quota do Porto; o Vinho Verde colapsou após a adesão à UE. O mesmo algoritmo em ambas — esse contraste é o que me deixa separar clima de estrutura mais à frente."
- **~70s** → "Aqui está o pipeline que trata ambas de forma justa."

### Slide 5 — Metodologia (Um Slide de Pipeline)
- **Ideia:** **2 AUDITÁVEL.**
- **Objetivo:** mostrar um pipeline rigoroso e sem fuga — e plantar a ideia das duas decisões.
- **Mensagem:** destendência → descobrir regras dentro do fold → teste walk-forward; o erro divide-se em Decisão 1 (tendência) + Decisão 2 (sinal climático). *(Só antevisão — a recompensa está no Slide 11.)*
- **Visual:** o diagrama do pipeline; pequena balança de fundo rotulada "D1 ↔ D2 (voltar a isto)".
- **Figura:** `fig_ch3_pipeline.png` + `fig_ch3_walkforward.png`.
- **Texto máx.:** "Janela expansível · todo o pré-processamento dentro do fold · Erro = Tendência (D1) + Sinal (D2)".
- **Notas:** "Um pipeline. Walk-forward de janela expansível, cada passo de pré-processamento dentro do fold — sem fuga. E lembrem-se de uma ideia: o erro é uma parte de tendência e uma parte de sinal. Voltarei a porque estas duas lutam entre si."
- **~85s** → "O motor dentro deste pipeline é o CarenR — e escolhi-o deliberadamente."

### Slide 6 — Porquê CarenR
- **Ideia:** **2 AUDITÁVEL.**
- **Objetivo:** justificar a interpretabilidade-primeiro + a escolha distribucional (KS), antecipando o "porque não uma caixa preta?"
- **Mensagem:** o produto é uma regra falsificável que um agrónomo pode atacar; o CarenR encontra onde a distribuição inteira se desloca (KS), cada regra trazendo o seu próprio p-value e suporte.
- **Visual:** uma regra literal + gráfico de desvio de cauda KS com duas densidades; painel esbatido "caixa-preta/SHAP≈aprox".
- **Figura:** regra da Tabela 4.1; novo gráfico KS.
- **Texto máx.:** "SE seco pós-floração E água no solo moderada na vindima → +121 mhl (suporte 28%, p=,004)".
- **Notas:** "Um modelo profundo prevê e não sabe explicar. Onde a pergunta é *que condições impulsionam a produção*, isso é desqualificante. O CarenR faz uma pergunta distribucional — um conjunto de condições desloca a distribuição inteira, caudas incluídas — e cada regra reporta a sua própria evidência. Essa auditabilidade é o ponto, primeira aplicação a longos registos regionais de vinho."
- **~85s** → "Então o que descobriu esse motor?"

### Slide 7 — Objetivo 1: Descoberta de Regras
- **Ideia:** **3 REAL.**
- **Objetivo:** entregar o resultado positivo com as suas duas provas mais fortes — convergência e o teste de esforço.
- **Mensagem:** 48 + 20 regras validadas; RIPPER e M5Rules convergem nas mesmas variáveis por critérios diferentes; e sob remoção agressiva de era (LOESS) as características genuínas mantêm 94–125% do seu sinal enquanto os artefactos colapsam para ~15%.
- **Visual:** matriz de convergência (esquerda) + barras de retenção LOESS (direita, verde sobrevive / vermelho colapsa).
- **Figura:** Tabela 4.9 (reduzida) + Tabela 4.4 como barras.
- **Texto máx.:** "68 regras · 3 algoritmos concordam · genuínas 94–125% retido, artefacto de era 15%".
- **Notas:** "O Objetivo 1 tem sucesso. 68 regras. Três otimizadores não relacionados chegam às mesmas características. Depois tentei quebrá-las: o LOESS remove a estrutura de era — as características genuínas sobrevivem intactas, as contaminadas por era colapsam para 15%, o que reporto honestamente para o Vinho Verde. Estrutura real, não artefacto." *(É aqui que vive o argumento LOESS.)*
- **~95s** → "E estas regras não são só estatísticas — são agronomicamente legíveis."

### Slide 8 — O Que Aprendemos de Facto (Insights Agronómicos)
- **Ideia:** **3 REAL** (validade de domínio).
- **Objetivo:** mostrar que as regras significam algo para um viticultor — crítico para o arguente da FCUP.
- **Mensagem:** o impulsionador dominante é a água no solo no período da vindima; a *mesma variável* inverte o sinal entre regiões (+192 mhl Douro, −338 mhl Vinho Verde) — um défice moderado é um ponto ótimo no Douro seco, uma penalização fresco-húmida no Vinho Verde atlântico.
- **Visual:** o destaque da inversão direcional + temas das características de topo (água no solo, humidade pós-floração, moderação de calor).
- **Figura:** Tabela 4.3 (inversão) + Tabela 4.11/4.12 frequência de características.
- **Texto máx.:** "Mesma variável de água no solo: +192 mhl no Douro, −338 mhl no Vinho Verde".
- **Notas:** "As regras leem-se como agronomia. A água no solo na vindima domina ambas as regiões — mas em direções opostas, e o CarenR deteta isso sem nunca ver as regiões em conjunto. Um défice suave concentra os bagos no Douro seco; a mesma gama sinaliza anos frescos-húmidos, de baixa qualidade, no Vinho Verde. O método resolve corretamente uma inversão de sinal — isso é estrutura de domínio real."
- **~85s** → "Mas 'uma variável importa' não chega — onde exatamente estão os limiares biológicos?"

### Slide 9 — Limiares & Interpretação Biológica
- **Ideia:** **3 REAL / 2 AUDITÁVEL** (a recompensa da interpretabilidade).
- **Objetivo:** mostrar a saída concreta e acionável que só um método baseado em regras dá — a recompensa por escolher a auditabilidade.
- **Mensagem:** as regras dão limiares explícitos e biologicamente coerentes (ex. água no solo 74–101 mm → acima da tendência; dias de calor >35 °C limitados), cada um com suporte — é isto que uma caixa preta não consegue entregar a um produtor.
- **Visual:** faixas de limiar anotadas num eixo água-no-solo / calor, mapeadas aos estados fenológicos da vinha.
- **Figura:** bins de regras das Tabelas 4.1–4.2; esquema do ciclo da vinha se disponível.
- **Texto máx.:** "Défice moderado 74–101 mm → +121–192 mhl · calor >35 °C tem de ficar limitado".
- **Notas:** "A interpretabilidade compensa aqui. As regras não só nomeiam variáveis — dão limiares ligados à biologia da vinha: uma faixa específica de água no solo, janelas limitadas de stress térmico, cada uma com uma estimativa de suporte. Isso é uma afirmação auditável e acionável que um produtor ou regulador pode usar e contestar. Nenhum gráfico SHAP entrega isso."
- **~80s** → "Regras reais, legíveis, com limiares. A pergunta decisiva: elas preveem?"

### Slide 10 — Objetivo 2: Validação Preditiva
- **Ideia:** **4 FALHA COM LEI.**
- **Objetivo:** entregar o resultado negativo de forma limpa, sem defensividade.
- **Mensagem:** em walk-forward rigoroso, nenhum modelo baseado em regras bate a Naive_Median — RDD +3% (p=,468), RVV +22% onde as regras prejudicam ativamente (p=,004).
- **Visual:** gráfico de barras de ranking, Naive_Median no lugar 1; seta "…mas olhem lá dentro".
- **Figura:** `fig51_weighted_mae_rdd.png`.
- **Texto máx.:** "Naive_Median vence · RDD 189 vs 184 (p=,468) · RVV 172 vs 140, prejudicial (p=,004)".
- **Notas:** "Não vou disfarçar isto. Prevejam a mediana histórica e batem todos os modelos de regras — 3% no Douro, 22% no Vinho Verde, onde disparar regras piora. Regras reais, sem vantagem preditiva. É esse o enigma. Recusei-me a parar em 'não funcionou'."
- **~85s** → "Porque a falha não é aleatória — obedece a uma lei."

### Slide 11 — Descoberta ≠ Previsão (Achado Central + Mecanismo)
- **Ideia:** **4 FALHA COM LEI** (o coração da tese).
- **Objetivo:** enunciar o achado científico central E o mecanismo que prova que não é um beco sem saída.
- **Mensagem:** a falha tem lei — o *compromisso das duas decisões*: melhorar a tendência (D1) apaga o sinal residual de que as regras precisam (D2). O LOESS prova-o — a baseline do RVV melhora 36%, e as 20 regras desaparecem. O equilíbrio de era inverte o Douro (5/5 vitórias) porque relaxa brevemente a tensão.
- **Visual:** balança de recompensa (D1 ↑ → D2 ↓) com os números do LOESS + o espelho janela-de-vitória RDD / degradação RVV.
- **Figura:** `fig_detrend_rvv.png` + `fig53/fig54` por cenário.
- **Texto máx.:** "Melhor tendência apaga o sinal. LOESS: baseline RVV −36%, regras 20 → 0."
- **Notas:** "Aqui está o achado e porque não é um beco sem saída. Tornem a tendência mais flexível e a baseline do Vinho Verde melhora 36% — mas todas as regras desaparecem, porque a tendência flexível comeu a variação com que as regras foram construídas. A Decisão 1 e a Decisão 2 disputam a mesma variância porque ambas são parametrizadas só pelo tempo. A descoberta e a previsão pedem coisas diferentes ao mesmo sinal. É esse o compromisso das duas decisões — o nome que gostaria que recordassem."
- **~100s** → "Nomear esse compromisso é em si a contribuição."

### Slide 12 — Contribuições
- **Ideia:** **5 DIAGNÓSTICO.**
- **Objetivo:** tornar a contribuição explícita e pontuável — não a deixar subentendida.
- **Mensagem:** (1) o arcabouço de diagnóstico das duas decisões; (2) uma análise de sensibilidade estruturada de 11 variantes (a configuração ótima é específica do problema); (3) o achado da composição por era; (4) validação entre algoritmos; (5) validação do método num banco de ensaio não-estacionário exigente de 90 anos.
- **Visual:** cinco cartões de contribuição compactos.
- **Figura:** nenhuma.
- **Texto máx.:** cinco etiquetas de contribuição de 3–4 palavras.
- **Notas:** "Cinco contribuições. O destaque é o arcabouço das duas decisões — um diagnóstico que explica *porque* a previsão baseada em regras falha mesmo quando existe sinal real. Depois: uma análise sistemática de variantes que mostra que a melhor configuração é específica da região; o regime de composição por era; validação cruzada entre três algoritmos; e um banco de ensaio real exigente."
- **~80s** → "Cada contribuição vem com uma fronteira que enunciarei claramente."

### Slide 13 — Limitações & Trabalho Futuro
- **Ideia:** **5 DIAGNÓSTICO** (a correção decorre do diagnóstico).
- **Objetivo:** assumir os limites, depois mostrar que o arcabouço gera o próximo passo.
- **Mensagem:** a limitação raiz é a destendência só-com-tempo (janela de era n=5 sem potência; apenas agregados anuais) → a correção que o diagnóstico aponta é a destendência estrutural: acrescentar área plantada, quota, contagem de produtores para que o clima fique no resíduo.
- **Visual:** equação de destendência estrutural → resíduo antes/depois (contaminado → limpo).
- **Figura:** Eq. 6.1.
- **Texto máx.:** "y = α + β₁t + β₂·área + β₃·quota + ε → o clima vive em ε".
- **Notas:** "Três limites: destendência só com o tempo — a raiz estrutural de todo o compromisso; a vitória equilibrada por era são cinco cenários, post-hoc, sem potência — uma hipótese, e rotulo-a como tal; e agregados anuais de região única. Mas o diagnóstico é gerativo: coloquem os impulsionadores estruturais na tendência, e as Decisões 1 e 2 deixam de competir. O meu próprio arcabouço entrega ao próximo investigador a correção."
- **~85s** → "Então, cinco coisas para levar da sala."

### Slide 14 — Mensagem Final
- **Ideia:** as cinco (fecho de recompensa).
- **Objetivo:** recordar as cinco ideias como respostas; deixar um princípio transferível.
- **Mensagem:** problema difícil; regras auditáveis; as regras são reais; não preveem e sei porquê; o diagnóstico transfere-se para qualquer série estruturalmente não-estacionária.
- **Visual:** as cinco etiquetas do Slide 1, cada uma assinalada com uma resolução de uma linha.
- **Figura:** referência à imagem de vinha da abertura.
- **Texto máx.:** cinco etiquetas + respostas de 4–5 palavras cada.
- **Notas:** "Cinco coisas. É um problema difícil. Escolhi regras auditáveis. As regras são reais, de três maneiras. Não preveem — e o compromisso das duas decisões diz exatamente porquê. E esse diagnóstico é a contribuição: transfere-se para qualquer série onde a estrutura e o sinal estão entrelaçados. Obrigado — as vossas perguntas."
- **~70s** → *(Fim → Perguntas.)*

---

## Veredito sobre a tua narrativa
**Funciona, e é melhor do que a keynote para um júri que pontua** — acrescenta questões de investigação, recompensa agronómica e contribuições explícitas. Adotada tal como está com dois inegociáveis:
- **O Slide 7 tem de carregar o teste de esforço LOESS** — é a tua prova mais forte de que as regras são reais. Sem ele, "REAL" assenta só na convergência.
- **O Slide 11 tem de ser achado + mecanismo** — "Descoberta ≠ Previsão" é o destaque; o compromisso das duas decisões é o que o transforma de admissão de fracasso numa contribuição. Nunca mostres o 11 sem a balança.

## Tempo
14 slides ≈ 19,5 min com as durações acima, deixando margem para perguntas. Se te alongares, comprime os Slides 3 (RQ, 55→40s) e 9 (limiares, 80→60s) — são os mais cortáveis sem quebrar a espinha.

## Dois travões de ensaio
1. Diz as cinco etiquetas de uma palavra em voz alta no Slide 1 e no Slide 14 — o fecho verbal é o que cimenta a recordação.
2. Diz **"o compromisso das duas decisões"** literalmente nos Slides 11 e 12 para que o júri adote a tua expressão na deliberação.
