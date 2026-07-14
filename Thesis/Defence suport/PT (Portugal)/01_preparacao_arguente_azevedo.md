# Dossiê de Preparação para a Defesa — Arguente: Prof. Paulo Jorge Azevedo

**Quem é:** Professor auxiliar, Dep. Informática, U. Minho; investigador sénior no INESC TEC (HASLab). Doutoramento pelo Imperial College (programação em lógica). **É o autor do motor CAREN e coautor do artigo das Distribution Rules em que assenta toda a tua tese.** Investigação: regras de associação, regras de distribuição, descoberta de subgrupos, medidas de interesse de regras, motifs em séries temporais, análise de desempenho de modelos através de regras.

As publicações dele mais relevantes para ti:

| Artigo | Porque importa para a tua defesa |
|---|---|
| Jorge, Azevedo & Pereira (2006), *Distribution Rules with Numeric Attributes of Interest*, PKDD 2006, LNAI 4213, pp. 247–258 | O formalismo das DR + teste KS que usas. Ele coinventou-o. Domina-o na perfeição. |
| Azevedo (2003), *CAREN — a Java based Apriori implementation for classification purposes*, Relatório Técnico, U. Minho | O CAREN é o motor **dele** — baseado em Apriori, pesquisa exaustiva por níveis. |
| Azevedo & Jorge (2007), *Comparing Rule Measures for Predictive Association Rules*, ECML | Trabalho dele sobre medidas de interesse (a conviction vence). Pode perguntar porque ponderas as regras por suporte em vez de por uma medida de interesse. |
| Pimentel, Azevedo & Torgo (2023), *Subgroup mining for performance analysis of regression models* (Error Distribution Rules), Expert Systems | Ele usa regras para analisar **onde os modelos de regressão falham** — exatamente o que a tua análise de composição por era faz informalmente. Espera uma pergunta; cita-o. |
| Soares, Azevedo, Cerqueira & Torgo (2025), *Meta Subspace Analysis*, Discovery Science 2025 | Extensão recente da mesma ideia ao espaço de metacaracterísticas. |

**Enquadramento estratégico:** ele será *simpático* ao método (é dele) e *exigente* quanto à fidelidade com que o descreves e ao rigor com que o avalias. O teu risco não é "porquê CarenR?" — é descrever mal o CAREN e ter estatística descuidada.

---

## PARTE 0 — CRÍTICO: ELE ESTÁ A LER A VERSÃO SUBMETIDA (NÃO CORRIGIDA)

O júri lê a versão que depositaste — **os erros ainda estão na cópia dele.** Não podes contar que as correções estejam visíveis para ele; tens de conseguir responder *em voz alta* quando ele apontar para a página impressa. A jogada universal:

> **Reconhecer de imediato → afirmar o facto correto → indicar que consta da tua errata.** Sem pedir desculpa em excesso, sem defensividade. Ele é meticuloso com o próprio trabalho; assumir um erro com clareza *aumenta* a tua credibilidade, atrapalhar-te baixa-a.

Leva a **errata impressa** e oferece-a no início ("Identifiquei algumas correções editoriais após a submissão e preparei uma errata; posso entregá-la ao júri?"). Pergunta ao teu orientador se entregar logo à partida é o costume local — se sim, antecipa metade destas.

### Se ele apontar para a página submetida, diz isto:

| Ele aponta (na cópia dele) | O que ele vê | A tua resposta falada |
|---|---|---|
| **p.36** — "CAREN … originally developed by Jorge et al. (2006)" | CAREN atribuído ao artigo de 2006 | "É uma atribuição errada que já corrigi — o motor CAREN é o **seu** trabalho de 2003; Jorge, Azevedo e Pereira 2006 acrescentaram por cima a camada das regras de distribuição. É o item 1 da minha errata." *(Depois enuncia a origem com confiança — lisonjeia-o e prova que conheces a linhagem.)* |
| **p.36 / p.28** — "beam search" / "breadth-first, most promising conditions" | Duas descrições da pesquisa, ambas erradas | "O CAREN faz uma **pesquisa por níveis ao estilo Apriori, podada por suporte mínimo** — exaustiva sobre os itemsets frequentes em suporte, não uma *beam* heurística. As duas passagens estavam inconsistentes; corrigi para isto. Posso esboçar o reticulado se ajudar." *(Prepara-te para o desenhar — ver Parte 1, item 4.)* |
| **p.36–38 / Apênd. A, B** — "support ≥ 15%" | Limiar de suporte de 15% | "O valor efetivamente usado no código é **20%** — `carenr_min_sup = 0.2`. Os capítulos de resultados já estão a 20%; os capítulos iniciais é que estavam atrasados. O meu Apêndice E registou a correção; a errata propaga-a. Portanto: uma regra tem de cobrir **≥18 dos 89 anos** no RDD." |
| **p.99** — DOI `…11871637_25` | DOI errado no artigo de 2006 | "Gralha — o DOI correto termina em `_26`, LNCS 4213, pp. 247–258. Está na errata." |
| **p.100** — "Contrast Association Rules for Extraordinary Nodes" + "TODO: confirm…" | Título fabricado + um TODO literal na bibliografia | "Essa expansão do título estava errada e o TODO não devia ter sido impresso — removi ambos. Estou a confirmar a citação exata do pacote com o Prof. Moreira." *(Não inventes um título de substituição na hora.)* |
| **p.99–100** — "TODO: confirm…" em Fraga/Lopes | Notas provisórias visíveis nas referências | "Marcadores editoriais que passaram; removidos, metadados a confirmar com os meus orientadores." |

**Regra de tom:** cada resposta com uma ou duas frases. Reconhecer, corrigir, seguir em frente — **não** entres numa espiral de desculpas, e **não** argumentes que o erro não importa. "Bem visto, aqui está a versão correta, está na minha errata" é toda a jogada. Nenhum destes toca num resultado, e podes dizê-lo claramente se ele perguntar a relevância: *"nenhuma das correções afeta qualquer tabela, teste ou conclusão — o limiar de 20% foi usado em todos os cálculos; só o texto é que estava desatualizado."*

---

## PARTE 1 — CORREÇÕES (já aplicadas no teu LaTeX de trabalho; a cópia dele ainda as tem)

Estes eram erros factuais sobre *o trabalho dele próprio*. **Estão todos agora corrigidos no LaTeX e o PDF recompila sem problemas** (ver `ERRATA.md`). Listados aqui para saberes o que mudou e nunca dizeres as versões antigas em voz alta. Se algum surgir na defesa, a resposta honesta é "sim — está na minha errata, já corrigido."

1. **Atribuição errada do CAREN [CORRIGIDO].** O Cap.3 dizia que o CAREN foi "originally developed by Jorge et al. (2006)". Errado: o motor CAREN é de **Azevedo** (relatório técnico de 2003, U. Minho); Jorge, Azevedo & Pereira (2006) acrescentaram por cima o enquadramento das **Distribution Rules**. Agora lê-se "…developed by Azevedo (2003) and extended to distribution rules by Jorge et al. (2006)". Nova entrada bibliográfica `Azevedo2003` adicionada. **Di-lo desta forma em voz alta quando introduzires o método.**

2. **DOI errado [CORRIGIDO].** Era `10.1007/11871637_25`; corrigido para `10.1007/11871637_26` (PKDD 2006, LNCS 4213, pp. 247–258, Springer), com volume/páginas adicionados.

3. **Acrónimo inventado [CORRIGIDO].** "CarenR: Contrast Association Rules for Extraordinary Nodes" era uma expansão fabricada que trazia um TODO impresso. Removido; a entrada descreve agora o pacote R de forma neutra. **Confirma ainda a citação exata de Moreira (2022) com o Prof. Moreira** — a versão definitiva não deve adivinhar. (O próprio CAREN remonta ao "class project association rule engine" de Azevedo.)

4. **Inconsistência na estratégia de pesquisa [CORRIGIDO].** O Cap.2 dizia "beam search", o Cap.3 dizia "breadth-first, most promising conditions" — contraditório, e nenhum correspondia ao CAREN, que é uma **pesquisa por níveis ao estilo Apriori podada por suporte mínimo**. Ambos dizem agora isso (verificado contra a chamada do pacote `carenR` em `6_scenario_validation.R`). Esta foi a pergunta de quadro mais provável — **prepara-te para desenhar o reticulado por níveis**: nível 1 = todas as condições de característica-bin isoladas que passam o suporte mínimo; nível *k* construído a partir dos itemsets frequentes do nível-(*k*−1); teste KS aplicado a cada subgrupo sobrevivente; comprimento máximo 3–4.

5. **Limiar de suporte [CORRIGIDO — valor é 20%].** O texto dizia 15% no Cap.2/Cap.3/Apênd. A/B mas 20% no Cap.4–6. O **código usa 0.20** (`carenr_min_sup <- 0.2`), portanto 20% está correto; todo o texto diz agora 20% (e as contagens derivadas: ≈18 anos em 89). Se perguntarem "qual foi o teu limiar de suporte?", a resposta é **20%, ou seja, uma regra tem de cobrir ≥18 dos 89 anos (RDD)**. Nota que esta correção já estava registada no teu Apêndice E — apenas não tinha sido propagada a todos os capítulos.

6. **Notas TODO impressas [CORRIGIDO].** A bibliografia submetida imprimia notas literais "TODO: confirm…" nas entradas Moreira, Fraga e Lopes — visíveis a qualquer leitor. Removidas. **Confirma os metadados reais das três com os teus orientadores** antes da submissão definitiva.

7. **Terminologia.** No vocabulário dele: *distribution rules (DR)*, *antecedente/consequente*, *suporte*, *interesse KS*. Não digas "beam", "gain" ou "heurística de cobertura" para o CAREN.

**Transforma o Apêndice E num trunfo.** O teu apêndice de transparência sobre IA já documenta uma passagem de autoverificação numérica que *apanhou* o erro 15→20% (entre outros). Se o limiar de suporte ou qualquer ponto numérico surgir, refere que a tese inclui um apêndice de verificação explícito — demonstra exatamente o rigor de envolvimento crítico que um arguente quer ver, e reformula "houve erros" como "construí um processo que os encontra e corrige."

---

## PARTE 2 — PERGUNTAS PROVÁVEIS E RESPOSTAS PREPARADAS

### A. Interior do CAREN / Distribution Rules (o terreno dele)

**P1. "Explica exatamente o que é uma regra de distribuição e como o teu uso difere do artigo de 2006."**
R: Uma DR é SE antecedente ENTÃO distribuição empírica da propriedade numérica de interesse, considerada interessante quando o teste KS rejeita a igualdade com a distribuição por omissão (do conjunto de dados inteiro). O meu uso segue o formalismo de 2006; as adições ao nível da tese (explicitamente separadas na Fig. 3.x) são: a propriedade de interesse é um *resíduo sem tendência* calculado dentro de cada fold; um filtro de suporte de 20%; p ≤ 0,10; agregação ponderada por suporte das medianas de subgrupo para previsão pontual; e um recurso à mediana de treino quando nenhuma regra dispara. O algoritmo é o mesmo — a contribuição é o arcabouço de validação temporal em seu redor.

**P2. "Testas cada subgrupo contra o conjunto de dados completo, que contém o próprio subgrupo. As duas amostras não são independentes."**
R: Correto — isto segue o desenho original das DR (subgrupo vs. distribuição por omissão), o que torna o teste conservador na direção que importa: a sobreposição aproxima as duas ECDF, portanto o D é subestimado, não sobrestimado. Comparar subgrupo vs. complemento é uma alternativa conhecida; com n=80–89 e 20% de suporte a diferença é modesta, e mantive a convenção da distribuição por omissão por consistência com a literatura das DR.

**P3. "Milhares de subgrupos candidatos, p ≤ 0,10, sem correção para testes múltiplos. Quantas das tuas 48 regras do RDD são falsas descobertas?"**
Esta é a pergunta difícil mais provável. R: Não me apoio nos p-values em bruto para a validade. Quatro salvaguardas independentes: (1) o suporte mínimo de 20% (≥18 dos 89 anos) remove os candidatos mais frágeis; (2) a revalidação por LOESS mostra que as características de topo mantêm 90–92% da sua correlação depois de removida a cotendência de era — uma regra artefacto de era não sobreviveria a isso; (3) o RIPPER e o M5Rules convergem nas mesmas famílias de variáveis sob critérios completamente diferentes; (4) a validação walk-forward é ela própria um teste fora da amostra das regras. Apresento o conjunto de regras como achados *exploratórios* validados, não como hipóteses confirmatórias — e o enquadramento estatístico honesto no Cap.5 (testes confirmatórios vs. exploratórios, p-values não corrigidos divulgados) aplica o mesmo padrão. Se pressionado: reconhece que uma análise de permutação/FDR (ex., aleatorização por troca do alvo) é uma extensão natural.

**P4. "Porquê p ≤ 0,10 e não 0,05?"**
R: Um compromisso deliberado sensibilidade/potência com n=80–89: com 20% de suporte um subgrupo tem ~18–25 observações, onde o teste KS ainda tem potência limitada; 0,05 esvazia o conjunto de regras em vários folds. Como as regras enfrentam um segundo filtro, mais duro — o MAE fora da amostra — o limiar de descoberta permissivo é compensado a jusante. (Sabe o número: quantas regras sobrevivem a 0,05. Se não sabes, corre isso antes da defesa.)

**P5. "O teste KS é assintótico; com um subgrupo de 12 anos, os p-values são sequer válidos? E os empates?"**
R: Os subgrupos são pequenos mas o conjunto de referência é 60–89; a distinção exato/assintótico nestes tamanhos desloca ligeiramente os p-values, o que é mais uma razão para as regras serem tratadas como exploratórias. Os resíduos são contínuos, portanto os empates são raros. (Se usaste o `ks.test` do R, ele calcula p-values exatos para amostras pequenas sem empates — verifica qual o pacote caren usa e diz isso.)

**P6. "O artigo das DR filtra regras redundantes contra os seus ancestrais (improvement). Fazes isso?"**
R: Sim — a Secção 4.x analisa a sobreposição de regras e regras únicas-vs-redundantes; os conjuntos de regras organizam-se em torno de um pequeno número de características de espinha dorsal com refinamentos contextuais. (Relê a §4 "Rule Set Overlap and Internal Coherence" na semana anterior — sê capaz de dar as contagens.)

**P7. "Porquê discretização em quartis de frequência igual dos antecedentes? O CAREN suporta discretização supervisionada (Fayyad–Irani)."**
R: Testei exatamente isso — o CarenR_Supervised deriva pontos de corte que maximizam a separação dos resíduos dentro de cada fold de treino. Foi a **pior** variante nas duas regiões (240/347 mhl): as fronteiras dos bins são otimizadas nos mesmos dados que avaliam a qualidade da regra, um efeito de fuga do alvo que inflaciona a contagem de regras (46–146 por cenário) e colapsa fora da amostra. Os quartis de frequência igual garantem ocupação uniforme dos bins, de que o teste KS precisa com este n. Este resultado negativo é um dos achados replicáveis da tese.

**P8. "Agregas as regras disparadas por medianas ponderadas por suporte. Porquê suporte e não uma medida de interesse — estatística KS, conviction?"**
R: Avaliei ponderação ao estilo confiança (CarenR_Dist_Conf, pesos a partir dos p-values KS) e stacking; o mecanismo de agregação revelou-se de segunda ordem — o ranking das variantes é determinado pela seleção de características, não pela ponderação (§3.x). A ponderação por suporte favorece estimativas cujas medianas de subgrupo são calculadas sobre mais observações, ou seja, ponderação por fiabilidade. Se ele insistir na conviction: a conviction é definida para consequentes categóricos; para as DR os análogos naturais são derivados do KS, que é o que o Dist_Conf testou.

**P9. "As DR dão-te uma distribuição inteira. Porquê deitá-la fora e prever só a mediana? Porque não avaliar previsões distribucionais (CRPS, quantile loss)?"**
R: Crítica justa e uma extensão muito natural. A previsão pontual foi escolhida por comparabilidade com as baselines e pelo caso de uso agronómico (um único valor de produção esperado). Mas o desenho de recurso já expõe a visão distribucional — quando nenhuma regra dispara, a previsão *é* a mediana da distribuição de treino. Avaliar a ECDF preditiva completa com CRPS está listado como trabalho futuro; com 17 anos de teste por região as estimativas de CRPS seriam elas próprias ruidosas. (Se isto ainda não está no trabalho futuro — acrescenta uma frase ao Cap.7.)

### B. Metodologia de avaliação (o ângulo EDR / desempenho de modelos dele)

**P10. "O teu teste de Wilcoxon trata os 18 cenários de janela expansível como amostras independentes. Partilham a maioria dos dados de treino e os conjuntos de teste sobrepõem-se."**
R: Correto, os cenários estão positivamente correlacionados, o que inflaciona o tamanho efetivo de amostra de qualquer teste. Isto afeta ambas as direções por igual e sobretudo torna os testes *menos* propensos a atingir significância — consistente com o resultado nulo honesto (p=0,468). O resultado negativo do RVV (W=0, p=0,004) é um padrão de sinal unânime nos 9 cenários com disparo de regras, robusto à ressalva de dependência. Sinalizo os p-values como indicativos, não exatos.

**P11. "Selecionaste a melhor de 11 variantes CarenR depois de ver os resultados e depois comparaste-a com a baseline."**
R: Sim — e esse enviesamento de seleção *reforça* a conclusão principal, porque mesmo a melhor variante escolhida a posteriori não bate a Naive_Median (3% atrás no RDD, 22% no RVV). O enviesamento só importaria se eu reivindicasse vitória. A única afirmação positiva (cenários equilibrados por era 10–14, p=0,031) está explicitamente rotulada como post-hoc/exploratória ao longo de todo o texto, exigindo validação prospetiva.

**P12. "A tua análise de composição por era pergunta 'onde é que o modelo falha'. Tens noção de que é exatamente o que fazem as Error Distribution Rules?"**
R: Prepara-te para dizer SIM. "Pimentel, Azevedo & Torgo (2023) formalizam isto como mineração de subgrupos sobre os erros do modelo — EDRs. A minha análise de composição por era faz isto manualmente com uma covariável (proporção de treino pós-1990). Aplicar EDRs à série de erros walk-forward, com a proporção de era e as características climáticas como descritores, seria a generalização com princípios e é uma excelente direção para a secção de trabalho futuro." Considera mesmo acrescentar uma citação a este artigo no Cap.6/Cap.7 antes da defesa — é respeitoso, correto e desarma a pergunta.

**P13. "Porquê MAE ponderado? Ponderar pelo tamanho do conjunto de teste faz os cenários iniciais, confundidos por era, dominarem."**
R: Essa interação é analisada explicitamente na §6.4: ponderar pelo tamanho de teste é a escolha estatisticamente correta (mais observações, mais influência), e reporto o MAE por cenário e o MAE médio em paralelo para que o leitor veja a estrutura de era. A sensibilidade é uma propriedade da composição por era dos dados, não um artefacto que escondi.

**P14. "A Naive_Median é um espantalho / porque é tão difícil de bater?"**
R: É o oposto de um espantalho — é o preditor constante ótimo em MAE na escala dos resíduos, portanto batê-la é precisamente a afirmação "as características climáticas carregam sinal explorável". A decomposição em duas decisões separa a precisão da tendência (Decisão 1) do sinal climático (Decisão 2), portanto a baseline está a fazer trabalho de diagnóstico, não apenas de referência.

### C. Resultados e interpretação

**P15. "O Objetivo 2 falhou. O que aprendemos?"**
R: Três achados transferíveis: (1) a assimetria descoberta–previsão é uma propriedade do sistema — |r|≈0,47 ⇒ ~22% da variância explicada chega para descoberta de padrões estatisticamente válida, não para bater uma mediana com CV≈47% de ruído; (2) a decomposição em duas decisões localiza o estrangulamento na remoção estrutural de tendência, não na aprendizagem de regras — o tempo como única covariável de destendência confunde a reestruturação da indústria com o clima; (3) resultados negativos confirmados com valor prático: a discretização supervisionada tem fuga, o LOESS consome o sinal que devia expor, e as regras do RVV prejudicam ativamente (p=0,004). O caminho em frente são covariáveis estruturais (área, quota de benefício, proporção do sistema de condução) no modelo de destendência — os dados existem no IVV/IVDP.

**P16. "No RVV as tuas regras pioram as previsões. O método não devia detetar que as suas próprias regras são artefactos de era?"**
R: Essa é a leitura correta — e é por isso que o resultado do RVV importa. O teste KS valida as regras contra a distribuição de *treino*; quando treino e teste atravessam uma quebra estrutural (adesão à UE em 1986, conversão latada→cordão, −65% de produção), regras da era de treino estatisticamente válidas codificam pertença de era. O CarenR resiste-lhe parcialmente (a reformulação da destendência, §6.6), mas nenhuma estatística dentro da amostra pode certificar generalização entre eras. Isso é um argumento a favor do próprio protocolo walk-forward — apanhou o que uma avaliação só de descoberta teria deixado passar.

**P17. "Apenas 17 anos de teste por região. Alguma destas conclusões é estável?"**
R: Reconhecido explicitamente no Cap.2 (§ limitações): 17 anos abrangem ~3 ciclos da oscilação dominante de 5,7 anos. É por isso que a tese reporta resultados por cenário, distingue testes confirmatórios de exploratórios e evita afirmações positivas fortes. A alternativa — validação cruzada aleatória — seria temporalmente desonesta e inflacionaria o desempenho.

**P18. "O que é exatamente novo aqui face ao trabalho CarenR anterior do grupo (Moreira et al.)?"**
R: O trabalho anterior aplicou o CarenR para descoberta em dados agrícolas. Esta tese acrescenta: a primeira validação preditiva walk-forward de DR (discretização, seleção de características e indução de regras internas ao fold — fuga zero); a decomposição em duas decisões como diagnóstico; a identificação do confundimento por era e a metodologia de revalidação por LOESS; a análise de sensibilidade estruturada das 11 variantes; e a comparação estrutural entre regiões. Em suma: a tese converte o CarenR de ferramenta de descoberta num pipeline de previsão totalmente avaliado, e mostra precisamente onde e porque a metade preditiva falha.

**P19. "Porque não Exceptional Model Mining / pysubgroup / Cortana? Porquê o CAREN especificamente?"**
R: As DR são a primitiva certa: o alvo é uma única variável numérica e a pergunta é o desvio distribucional — exatamente o critério de interesse das DR, com o teste KS a dar validade não-paramétrica com n<90. O EMM ajustaria um modelo por subgrupo — com ~12–25 observações por subgrupo não há capacidade para isso. E a pesquisa exaustiva podada por suporte do CAREN dá garantias de completude que um pesquisador heurístico não dá. (Bónus: esta resposta elogia as decisões de desenho dele — com rigor.)

**P20. Factos de resposta rápida para ter decorados.**
- RDD: n=89 (1934–2022), 18 cenários; RVV: n=80 (1942–2021), 16 cenários; 17 anos de teste cada.
- RDD: Naive_Median 184,0 vs CarenR_Dist_Eta2 189,2 (diferença de 3%); RVV: 140,4 vs Sup_FS 171,6 (22%).
- Testes: RDD Wilcoxon W=68 p=0,468 (confirmatório, n.s.); teste de sinais RDD equilibrado por era p=0,031 (post-hoc); Wilcoxon RVV com disparo de regras W=0 p=0,004 (negativo confirmatório).
- Regras: 48 RDD, 20 RVV; suporte ≥20% (≈18/89 anos); p ≤ 0,10; bins de quartis de frequência igual; 59 características; comprimento máximo de regra 3–4.
- LOESS span 0,40: baseline RVV 140→~90 mas 0 regras; baseline RDD 184→290.
- Retenção LOESS das características de topo: 90–92% (temperatura na vindima só 56% — inflacionada por era, corretamente não selecionada pelo CarenR).

---

## PARTE 3 — ESTRATÉGIA DE DEFESA

1. **Os itens da Parte 1 já estão corrigidos no LaTeX** (o PDF recompila sem problemas). Leva a errata (`ERRATA.md`) à defesa; se algum item surgir, "já está na minha errata" transforma-o num ponto a teu favor. Ainda por fazer do teu lado: confirmar os metadados de citação de Moreira/Fraga/Lopes com os teus orientadores para o depósito definitivo.
2. **Cita-o de volta.** Trabalha um slide/frase a ligar a tua análise de falha por era ao artigo EDR dele (2023). Os arguentes suavizam quando a sua linha de trabalho é levada a sério, e aqui a ligação é real, não lisonja.
3. **Assume o resultado negativo.** O teu maior trunfo é o enquadramento estatístico honesto (confirmatório vs. exploratório, p-values não corrigidos divulgados, folds sem fuga). Lidera com isso; ele valoriza-o — os próprios artigos dele são meticulosos na avaliação.
4. **Estratégia de pesquisa — confirmada e corrigida.** A chamada do pacote `carenR` faz a geração de candidatos por níveis podada por suporte (estilo Apriori) do CAREN; o teto ao nível da tese é comprimento de regra 3–4. Resposta falada segura: "o pacote R herda a geração de candidatos por níveis podada por suporte do CAREN; limito o comprimento da regra a 3–4." Prepara-te para esboçar o reticulado (ver Parte 1, item 4).
5. **Prepara o único número que não tens:** quantas regras sobrevivem a p ≤ 0,05, e as contagens de regras na análise de redundância. Ele pede números.

---

## PARTE 4 — QUADRO: "Como é que o CAREN pesquisa?" (a pergunta ao vivo mais provável)

Se ele perguntar como o CAREN gera regras, **desenha um reticulado de três linhas e explica-o.** É o algoritmo dele — acertar aqui vale mais do que qualquer slide.

### O que é um "item"
Um *item* é uma única condição característica-bin, em que o bin é um **intervalo numérico explícito** produzido pela tua discretização do Passo 1 em quartis de frequência igual — **não** um rótulo categórico como "baixo". Por exemplo `swa_hv_y1_b20 ∈ [20–74 mm]` (o quartil inferior da água no solo antes da vindima), ou `∈ [74–101 mm]`, `∈ [101–152 mm]`, `∈ [152–350 mm]`. Cada uma das tuas 59 características contribui com 4 itens-intervalo, portanto o conjunto é 59 × 4 itens. A pesquisa explora *conjunções* destas condições de intervalo, um nível de cada vez.

*(Sê preciso nisto se ele insistir: os bins são intervalos de quartis com fronteiras reais calculadas no fold de treino — frequência igual, portanto ~22 das 89 obs por bin no RDD — e um antecedente de regra é uma conjunção como `swa ∈ [20–74] ∧ rain_fl ∈ […]`. Diz "intervalo", nunca "baixo/alto", ao descrever um item.)*

### Desenha três linhas

Rotula os nós com **itens-intervalo** (escreve o intervalo, não "baixo/alto"). Usa nomes curtos — `swa` = água no solo antes da vindima, `rain_fl` = dias húmidos pós-floração, `heat_sm` = dias quentes antes da maturação, `tmax_hv` = temp. máxima na vindima:

- **Nível 1 — condições de intervalo isoladas.** Calcula o **suporte** de cada condição (fração de anos que cobre). Mantém as ≥ 20%, risca as restantes. *(No esboço: `tmax_hv ∈ [28,31]` cai a 12%.)*
- **Nível 2 — pares, formados apenas a partir dos sobreviventes do nível 1.** Calcula o suporte de novo; elimina os abaixo de 20%. *(`swa∈[20,74] ∧ heat_sm∈[3,9]` morre a 9%.)*
- **Nível 3 — triplos, formados apenas a partir dos pares sobreviventes.** O triplo **nunca é gerado**, porque um dos seus pares já falhou. Desenha-o como uma caixa fantasma. Limita o comprimento a 3–4 condições.

```
              A = swa∈[20,74]   B = rain_fl∈[8,15]   C = heat_sm∈[3,9]   D = tmax_hv∈[28,31]

Nível 1:   [A ✓ 34%]   [B ✓ 44%]   [C ✓ 27%]   [D ✗ 12%]        ← filtro de suporte
                \        /   \        /
Nível 2:       [A∧B ✓ 22%]   [A∧C ✗ 9%]   [B∧C ✓ 21%]           ← filtro de suporte
                       \          |          /
Nível 3:                ( A∧B∧C  nunca gerado )                  ← podado via A∧C
```

Cada nó é uma **conjunção de intervalos**; um nó sobrevivente que também passa o teste KS torna-se uma regra como *"SE swa∈[20,74] mm E rain_fl∈[8,15] dias ENTÃO a distribuição da produção desloca-se +… mhl."*

### A ideia que torna o desenho útil — antimonotonia
O suporte só pode **manter-se igual ou diminuir** à medida que acrescentas condições de intervalo (uma conjunção mais estreita cobre menos ou igual número de anos). Portanto, se um itemset pequeno já está abaixo do limiar, *qualquer* itemset maior que o contenha está garantidamente também abaixo — saltas-o sem testar. É essa a cascata: eliminar `tmax_hv∈[28,31]` remove todos os seus pares; eliminar `A∧C` remove o triplo `A∧B∧C`. É isto que mantém tratável uma pesquisa exaustiva.

### Dois filtros, não um (di-lo explicitamente)
O suporte é apenas o **primeiro** filtro — *constrói o reticulado*. Depois o **teste KS** corre em cada subgrupo sobrevivente, comparando a sua distribuição de resíduos com a global; os subgrupos com p ≤ 0,10 tornam-se regras de distribuição (as caixas verdes). Portanto há dois critérios distintos: **suporte = estrutural/cobertura; KS = estatístico.** Os estudantes confundem-nos com frequência — mantê-los separados sinaliza que percebes o método dele.

### Porque "exaustivo, não ganancioso" importa (transforma o erro antigo numa força)
Contrasta-o em voz alta com a redação descartada "beam search". Um aprendiz beam/ganancioso mantém apenas os *k* melhores ramos por nível e pode perder definitivamente uma boa combinação. O CAREN mantém **todos** os candidatos frequentes, portanto tem uma **garantia de completude** sobre a região frequente — uma verdadeira força do desenho dele.

### A frase que responde à pergunta inteira
> *"Geração de candidatos por níveis com poda de suporte à Apriori por antimonotonia — exaustiva sobre os itemsets frequentes, não uma beam heurística — e depois o teste KS como segundo filtro, estatístico, que seleciona as regras. Limito o comprimento da regra a 3–4 condições."*

**Ressalva — mantém-te honesto:** os suportes no esboço (34%, 12%, 9%…) são ilustrativos, não dos teus dados. Desenha a *estrutura*; não cites esses números específicos como resultados reais.
