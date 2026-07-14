# Como Funciona o CarenR — Passo a Passo

Um percurso em linguagem simples pelo pipeline de regras distribucionais do CAREN, construído um passo de cada vez. Usa isto para conseguires narrar o algoritmo de ponta a ponta (e responder ao Azevedo no quadro). Ver `01_preparacao_arguente_azevedo.md` Parte 4 para o diagrama do reticulado.

---

## Passo 1 — Discretizar as características (transformar números em itens-intervalo)

O CAREN trabalha com **itens**, e um item é uma condição da forma `característica ∈ intervalo`. As características climáticas chegam como números contínuos (água no solo 88 mm, 143 mm, 210 mm…), e o CAREN não consegue construir regras diretamente sobre valores contínuos em bruto — portanto a primeira coisa que faz é **cortar cada característica em bins**.

A tese usa **quartis de frequência igual**: cada característica é dividida em 4 bins, com pontos de corte escolhidos para que cada bin contenha aproximadamente o mesmo *número de anos*, não a mesma *largura*. Em 89 anos do Douro, cada um dos 4 bins contém ≈22 anos.

Exemplo — `swa_hv_y1_b20` (água no solo 20 dias antes da vindima) torna-se quatro itens-intervalo:

```
swa ∈ [20, 74] mm     ← quartil inferior (~22 anos)
swa ∈ [74, 101] mm    ← 2.º quartil
swa ∈ [101, 152] mm   ← 3.º quartil
swa ∈ [152, 350] mm   ← quartil superior
```

Dois pontos que o Azevedo pode sondar:

- **Porquê frequência igual, e não largura igual?** A frequência igual garante que cada bin está bem povoado (~22 obs). O teste KS depois compara a distribuição de um subgrupo com a global; se um bin tivesse apenas 2–3 anos, esse teste seria pouco fiável. A ocupação equilibrada dos bins mantém a estatística de confiança com n pequeno.
- **Onde são calculados os pontos de corte?** Para o Objetivo 1 (descoberta), no conjunto de dados completo. Para o Objetivo 2 (previsão), *apenas dentro de cada fold de treino* — para que nenhuma informação do ano de teste vaze para as fronteiras dos bins. Esta é a salvaguarda contra fuga.

**Saída do Passo 1:** as 59 características contínuas tornam-se um conjunto de **59 × 4 = 236 itens-intervalo**, e cada um dos 89 anos é descrito por que bin ocupa em cada característica. O Passo 1 apenas prepara o *vocabulário de condições* que a pesquisa vai depois combinar.

---

## Passo 2 — Pesquisa: construir subgrupos candidatos nível a nível

O CAREN combina os itens-intervalo em subgrupos, um **nível** de cada vez (um nível = quantas condições estão unidas com E). O motor é o **Apriori**.

- **Nível 1 — condições isoladas.** Cada item por si. Para cada um, conta o **suporte** (fração de anos que o satisfazem). Mantém apenas os ≥ suporte mínimo (limiar da tese: **20%**, ≈18 dos 89 anos); descarta os restantes.
- **Nível 2 — pares.** Forma subgrupos de duas condições *apenas a partir dos sobreviventes do nível 1*, entre características **diferentes**. Conta o suporte de novo; mantém os ≥ 20%.
- **Nível 3–4 — triplos/quádruplos**, construídos apenas a partir de combinações menores sobreviventes, até ao comprimento máximo de regra (3–4).

### A direção da monotonia (acerta nisto)
À medida que **acrescentas** uma condição (isolada → par), o suporte só pode **diminuir ou manter-se igual — nunca aumentar** (uma regra mais estreita cobre menos ou igual número de anos). Equivalentemente: uma regra mais curta tem sempre suporte **≥** qualquer regra mais longa que a estenda. Isto é a **antimonotonia**, e é o que impulsiona a poda: se um subgrupo já está abaixo de 20%, *qualquer* subgrupo maior que o contenha está garantidamente também abaixo de 20% — o CAREN nunca o gera. É por isso que a pesquisa é **exaustiva sobre a região frequente em suporte mas tratável** (ao contrário de uma pesquisa beam/gananciosa, que mantém apenas os *k* melhores ramos e pode perder boas combinações; o CAREN mantém **todos** os candidatos frequentes → garantia de completude).

Nota: dois bins da *mesma* variável nunca são emparelhados — um ano não pode estar em `swa ∈ [20,74]` E `swa ∈ [74,101]` ao mesmo tempo (suporte 0). Os pares são sempre entre características diferentes.

### Exemplo concreto — pares
Sobreviventes do nível 1 (suporte ≥ 20% = ≥18 dos 89 anos):

| Item | Condição | Anos | Suporte |
|------|----------|------|---------|
| A | `swa ∈ [20,74]` mm | 22 | 25% |
| B | `rain_fl ∈ [8,15]` dias | 27 | 30% |
| C | `heat_sm ∈ [3,9]` dias | 24 | 27% |

*(Um quarto item, `tmax_hv ∈ [28,31]`, tinha apenas 12% de suporte → eliminado no Nível 1, nunca aparece num par.)*

O Nível 2 forma todos os pares entre características e conta os anos que satisfazem **ambos**:

| Par | Anos com AMBOS | Suporte | Decisão |
|-----|----------------|---------|---------|
| A∧B | 20 | 22% | ✅ mantém |
| A∧C | 8 | 9% | ❌ elimina |
| B∧C | 19 | 21% | ✅ mantém |

O suporte de cada par (22%, 9%, 21%) é **≤** o de cada progenitor (A 25%, B 30%, C 27%) — acrescentar uma condição só o encolheu. E A∧B∧C **nunca é gerado**, porque A∧C já morreu a 9% (o seu suporte não pode exceder 9%).

**Saída do Passo 2:** os subgrupos candidatos sobreviventes — aqui `{A, B, C, A∧B, B∧C}`. Avaliados **apenas por suporte (cobertura)** até agora. A produção ainda não entrou — **não há p-value nesta fase**. O p-value chega no Passo 3.

---

## Passo 3 — O teste KS: a produção desloca-se de facto neste subgrupo?

Para cada subgrupo candidato sobrevivente, o CAREN traz agora o **alvo** — o resíduo de produção (desvio da tendência, mhl). É a primeira vez que a produção é usada.

Constrói duas distribuições:
- os resíduos dos **anos dentro do subgrupo** (ex. os 20 anos que satisfazem A∧B), e
- os resíduos de **todos os anos** — a distribuição global/de referência.

Depois corre um **teste de Kolmogorov–Smirnov (KS) de duas amostras**:
1. Transforma cada conjunto de resíduos numa **ECDF** (distribuição cumulativa empírica — "fração de anos com resíduo ≤ x").
2. A **estatística KS D** = o *máximo desnível vertical* entre as duas curvas ECDF. D grande = a distribuição inteira do subgrupo está deslocada/remodelada face à global (o "KS gap" no slide 9).
3. A partir de D e dos dois tamanhos de amostra, o CAREN calcula o **p-value** = probabilidade de um desnível tão grande *por acaso* se os anos do subgrupo fossem de facto retirados da mesma distribuição de todos os anos.

**Suporte vs p-value — duas perguntas diferentes:**
- **Suporte** = "este padrão é comum o suficiente para importar?" (cobertura — Passo 2)
- **p-value** = "o desvio de produção é real, ou pode ser ruído?" (sinal — Passo 3)

É por isto que cada regra reporta ambos: `+121 mhl acima da tendência (suporte 28%, p = 0,004)`. Suporte: 28% dos anos correspondem. p = 0,004: um desvio tão forte aconteceria por sorte < 0,5% das vezes.

**Dois pontos para o Azevedo:**
- **Porquê KS, e não um teste t?** O KS é não-paramétrico — compara a distribuição *inteira* (caudas incluídas), não assume normalidade, é válido com n pequeno (80–89). Os resíduos de produção não são gaussianos, portanto um teste só de média perderia desvios de forma.
- **O conjunto de referência inclui o subgrupo.** Essa sobreposição torna o teste *conservador* (as duas ECDF são aproximadas, portanto o D é subestimado, não inflacionado) — di-lo antes de ele levantar.

**Saída do Passo 3:** cada subgrupo candidato carrega agora um **(D, p-value)**. Nada mantido ou descartado ainda — isso é o Passo 4.

---

## Passo 4 — Reter: manter as regras que passam ambos os filtros

Cada candidato tem agora dois números: **suporte** (Passo 2) e **p-value** (Passo 3). O Passo 4 aplica **dois limiares em simultâneo** e mantém apenas os subgrupos que passam ambos:

- **Filtro estatístico:** p ≤ **0,10** — o desvio é improvável de ser ruído.
- **Filtro de suporte:** suporte ≥ **20%** (≈18 dos 89 anos) — comum o suficiente para importar.

Passa ambos → torna-se uma **regra de distribuição**. Falha qualquer um → eliminado. (Um candidato pode morrer de três formas: demasiado raro → falha o suporte, podado no Passo 2; comum mas não distinto → falha o p aqui; ou ambos.)

Forma final legível:

```
SE  rain_fl ∈ [0, 3.3]  E  rain_mat ∈ [0, 3.5]
ENTÃO produção +121 mhl acima da tendência     (suporte 28%, p = 0,004)
```

Os `+121 mhl` são a **mediana dos resíduos dos anos do subgrupo** — o tamanho típico do desvio, e o valor que a regra "prevê" se usada para previsão. Este passo produz as contagens de destaque: **48 regras (Douro), 20 (Vinho Verde)**.

**Dois pontos para o Azevedo:**
- **Os limiares são escolhas de desenho da *tese*, não do algoritmo CAREN.** O CAREN dá o par (suporte, p); "≥20% e ≤0,10" é a tua decisão. A Figura 3.x separa o azul (algoritmo) do verde (as tuas escolhas) exatamente aqui — assume essa fronteira.
- **p ≤ 0,10, não 0,05, é deliberado.** Com n=80–89 e 20% de suporte, os subgrupos têm ~18–25 obs onde o KS tem potência limitada; 0,05 esvazia o conjunto de regras em vários folds. As regras enfrentam um segundo filtro mais duro a jusante (MAE fora da amostra no Objetivo 2), portanto o limiar de descoberta permissivo é compensado mais tarde.

**Pipeline central completo:** discretizar → pesquisar (suporte) → teste KS (p-value) → reter (ambos os filtros) = um conjunto de regras de distribuição interpretáveis.

---

## Passo 5 — Previsão (apenas Objetivo 2): transformar regras numa previsão

Os Passos 1–4 são *descoberta* de regras (Objetivo 1). O Objetivo 2 pergunta se essas regras *preveem*. Quando chega um **ano de teste**:

1. **Que regras disparam?** Verifica as condições climáticas do ano de teste contra cada regra retida; uma regra "dispara" se o ano satisfizer todas as suas condições.
2. **Combina as regras disparadas.** Cada regra disparada carrega um resíduo mediano (o seu desvio `+/− mhl`). O CAREN combina-os por **média ponderada por suporte** — o desvio de cada regra ponderado pela sua fração de suporte, pesos normalizados para somar 1. (A maioria das variantes; algumas usam ponderação por confiança a partir do p-value KS, ou stacking.)
3. **Recurso se nenhuma disparar.** Se nenhuma regra corresponder ao ano de teste, prevê o **resíduo mediano de treino** (≈0) — idêntico à baseline Naive_Median.
4. **Re-tendência.** Soma o desvio de volta à tendência linear extrapolada para obter um valor de produção em mhl, comparável ao valor real para o MAE.

**Porque o recurso importa (di-lo):** porque o CAREN se abstém para a mediana quando incerto, só pode bater ou perder para a baseline *quando de facto dispara uma regra*. Isso torna o teste walk-forward uma pergunta limpa — "as regras disparadas acrescentam valor para além da mediana?" — e é por isso que o resultado do RVV (as regras disparadas são significativamente *piores*, p=0,004) é tão contundente: ali as regras não são apenas inúteis, são ativamente prejudiciais quando agem.

**Todo o Passo 5 corre dentro de cada fold de treino** — a discretização, a filtragem por suporte/KS e os desvios medianos são todos calculados apenas nos anos de treino, depois aplicados ao ano de teste retido. Essa é a garantia de não-fuga.

**Saída do Passo 5:** uma previsão de produção por ano de teste → o MAE walk-forward que o Objetivo 2 avalia contra a Naive_Median.

---

## Implicações & vantagens em conjuntos de dados muito pequenos (n = 80–89)

**Versão de uma linha para a defesa:** *O CAREN é adequado a dados pequenos porque é não-paramétrico, ao nível da distribuição, exaustivo e interpretável — exige pouco dos dados e devolve algo que um humano pode auditar. Mas os dados pequenos também limitam o que pode entregar: pouca potência, multiplicidade inevitável, e o fosso descoberta-vs-previsão que é o próprio achado principal da tese.*

### Porque o CAREN é uma escolha sensata para dados pequenos — vantagens
- **Não-paramétrico por natureza.** O teste KS não estima nada — nem média, variância ou forma. Os modelos paramétricos gastam graus de liberdade que não têm com n<90; o CAREN não gasta nenhum. A maior razão para se adequar aqui.
- **Sensibilidade distribucional bate testes de média com n pequeno.** Comparar ECDFs inteiras (caudas incluídas) deteta um desvio mesmo quando as médias dos grupos se sobrepõem — sinal que um teste t ou um classificador de tercis (RIPPER) perderia.
- **O piso de suporte é uma salvaguarda para dados pequenos.** Exigir ≥20% de cobertura (≈18 anos) impede "regras" que se ajustam a 3 anos de sorte; cada regra é sustentada por uma fatia significativa de um registo curto.
- **A pesquisa exaustiva é comportável — e completa.** Poucas observações → espaço de candidatos pequeno → pesquisá-lo por inteiro. Garantia de completude (não pode perder um padrão frequente) sem necessidade de heurísticas.
- **A interpretabilidade é a moeda certa quando a potência é baixa.** Quando não consegues provar muito estatisticamente, regras auditáveis que um agrónomo pode verificar dão **validade aparente** — uma segunda forma de evidência, humana, que uma caixa preta não dá.
- **Usa cada observação para descoberta + abstém-se quando incerto.** O Objetivo 1 minera todos os dados (sem desperdício num hold-out); o recurso à mediana faz com que a previsão só arrisque errar *quando dispara* → degradação graciosa.

### O que os dados pequenos implicam — cautelas (di-las antes dele)
- **Pouca potência — daí p ≤ 0,10.** ~18–25 obs por subgrupo → potência KS limitada, p-values limítrofes. O limiar mais permissivo é uma resposta fundamentada ao n pequeno, não um truque.
- **Multiplicidade → falsas descobertas esperadas.** Grande espaço de candidatos vs poucas observações → algumas das 48 regras são acaso. Sem nulo por permutação/FDR; nomeia o teste de permutação de rótulos como a correção.
- **A assimetria descoberta ≠ previsão é um fenómeno de n pequeno.** Um padrão real o suficiente para descobrir dentro da amostra (|r|≈0,47, ~22% da variância) não é forte o suficiente para bater a mediana fora da amostra com CV≈47% em ~17 anos de teste. O achado central — em larga medida consequência de dados pequenos e ruidosos.
- **Instabilidade do pré-processamento.** Os pontos de corte dos quartis e a seleção de características em 60–89 pontos internos ao fold podem mudar entre folds; a estabilidade de seleção não foi reportada (pedido justo).
- **Sobreajuste estrutural/de era.** Uma série curta e não-estacionária permite que as regras codifiquem pertença de era em vez de clima (RVV: as regras tornam-se prejudiciais). Difícil separar o clima de uma quebra estrutural com apenas o *tempo* como covariável de destendência.
- **ECDFs grosseiras & p-values exatos-vs-assintóticos.** Cada ECDF move-se em passos de 1/n → D granular; a distinção exato/assintótico desloca os p-values — mais uma razão para tratar as regras como exploratórias.
