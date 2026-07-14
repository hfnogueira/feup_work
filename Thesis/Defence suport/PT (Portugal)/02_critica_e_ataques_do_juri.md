# Crítica do Orientador & Mapa de Ataques do Júri

**Tese:** Previsão da Produção Vinícola com Aprendizagem Automática Baseada em Regras — Uma Abordagem de Descoberta de Subgrupos Distribucional
**Autor:** Hugo Filipe Queiróz Nogueira · MECD, FEUP
**Objetivo:** Crítica exigente pré-defesa. Fraquezas ordenadas pela probabilidade de serem levantadas pelo júri. Cada item: categoria · o ataque · a tua exposição · como responder.

---

## A lista de alvos ordenada

### 1. "Porquê modelar a *produção total* e não o rendimento por hectare?"
- **Categoria:** Domínio / metodológica
- **Probabilidade:** Quase certa
- **Ataque:** Produção = área × rendimento/ha. O rendimento/ha é a quantidade sensível ao clima; a produção total incorpora a área plantada, que é *estrutural* — exatamente o confundimento que toda a tese combate. Citas "produção normalizada por hectare" como análoga à tua correção proposta (§6.5) e depois não a usas. O júri dirá que construíste um aparato de destendência elaborado para remover um efeito de área/estrutural que podias ter removido na origem mudando o alvo.
- **Resposta:** Tem uma razão nítida para a produção total ter sido o objeto de estudo (necessidade de previsão/planeamento, ou indisponibilidade da série de área para todo o horizonte). Reconhece que é a extensão óbvia.

### 2. "Nomeias a destendência com covariáveis estruturais como A correção — porque está inteiramente em 'trabalho futuro'?"
- **Categoria:** Experiência em falta
- **Probabilidade:** Quase certa
- **Ataque:** A Eq. 6.1 é apresentada como a resolução da tensão das duas decisões; dizes que os dados estão "disponíveis em princípio no IVV/IVDP". Lê-se como: diagnosticou a doença, prescreveu a cura, não administrou nenhuma dose. Mesmo uma destendência estrutural parcial nos dados pós-1986 poderia ter transformado uma tese nula numa positiva.
- **Resposta:** Defende a decisão de âmbito (esforço de arquivo para reunir covariáveis para 1934–2022 vs. orçamento de tempo da tese). Reconhece que é o próximo passo de maior valor.

### 3. O "sucesso condicional" post-hoc de n=5, p=0,031
- **Categoria:** Estatística
- **Probabilidade:** Muito provável
- **Ataque:** Cinco cenários encaixados, janela escolhida *depois* de ver qual venceu (HARKing), sem controlo de multiplicidade, e os cenários imediatamente seguintes (15–18) falham. O tamanho de amostra não sustenta uma afirmação confirmatória.
- **Resposta:** "Gerador de hipóteses, ponto final; pré-registaria e testaria prospetivamente." Não o defendas como evidência.

### 4. O resumo diz retenção de "90–92%" sob LOESS; a Tabela 4.4 mostra características do RVV a colapsar para 15–16%
- **Categoria:** Credibilidade / contradição interna
- **Probabilidade:** Provável se lido com atenção
- **Ataque:** O teu próprio documento contradiz-se. O valor de 90–92% só é verdadeiro para o Douro (e uma característica do RVV); o destaque generaliza-o a todas as características/ambas as regiões, o que a tua própria Tabela 4.4 desmente. Se apanhado sem antecipação, envenena a confiança em todos os outros números.
- **Resposta:** **CORRIGE O RESUMO E O CAP.7 ANTES DA SUBMISSÃO.** Levanta-o tu próprio no palco: as correlações preservam-se no Douro mas colapsam no Vinho Verde — e esse colapso é um *resultado*, não uma ressalva.

### 5. "A Naive-Median é uma fasquia fraca. Onde está uma verdadeira baseline de previsão?"
- **Categoria:** Baseline em falta / algoritmos alternativos
- **Probabilidade:** Provável
- **Ataque:** Para uma tese de previsão de séries temporais, sem ARIMA/ARIMAX, sem suavização exponencial, sem modelo de espaço de estados. Naive e Naive_MA são brinquedos. Do lado do ML, sem regressão penalizada (LASSO/elastic net) e sem GAM — os concorrentes interpretáveis naturais do CarenR que também lidam com as 59 características colineares. Nunca estabeleceste um teto preditivo (sem RF/gradient boosting nos teus próprios dados — apenas citado de Lopes 2024), portanto não podes dizer quanto custa a interpretabilidade.
- **Resposta:** Reconhece as baselines de séries temporais clássicas e de regressão penalizada como uma lacuna. Se houver tempo antes da submissão, corre uma ARIMAX e uma baseline elastic-net — reforça materialmente a defesa.

### 6. Os "18 cenários" não são independentes → teste de Wilcoxon inválido
- **Categoria:** Estatística (a falha técnica mais séria)
- **Probabilidade:** Um arguente perspicaz vai perguntar
- **Ataque:** O cenário 1 testa 2005–2022, o cenário 2 testa 2006–2022, etc. O ano 2022 está no conjunto de teste de *todos os 18 cenários*. Os MAE por cenário sobrepõem-se/autocorrelacionam-se fortemente, mas o teste de Wilcoxon de postos com sinais trata as 18 diferenças emparelhadas como independentes. Além disso, o §3 descreve prever "o ano seguinte", mas as tabelas preveem *todos os anos restantes* a partir de cada origem — misturando previsões a 1 passo e a 18 passos e recontando os mesmos anos. Agravado pelo MAE ponderado a contar duas vezes os anos iniciais.
- **Resposta:** Reconhece a dependência; apresenta os testes como indicativos, não confirmatórios. Reconcilia o texto do §3 com as tabelas. Idealmente, recalcula uma variante walk-forward limpa a um passo.

### 7. Comparações múltiplas: 17 modelos, p-values não corrigidos
- **Categoria:** Estatística
- **Probabilidade:** Provável de um estatístico
- **Ataque:** Afirmas explicitamente que não foi aplicada correção. Com 17 modelos e múltiplos testes a taxa de erro familiar é grande; combinado com o #3 e o #6 este é um ponto fraco.
- **Resposta:** O resultado de destaque é um *nulo*, portanto a multiplicidade não fabrica aí um falso positivo. A exposição é o p=0,031 — trata-o como exploratório (ver #3).

### 8. "O MAE é a métrica que a tua baseline está definida para minimizar — cartas marcadas?"
- **Categoria:** Estatística / metodológica
- **Probabilidade:** Média
- **Ataque:** A mediana minimiza comprovadamente o MAE entre preditores constantes, e avalias em MAE. "Nada bate a mediana em MAE" está quase garantido à partida. Sob RMSE ou uma perda quantílica/pinball a história poderia diferir.
- **Resposta:** Prepara números de RMSE se possível. Justifica o MAE (robusto aos outliers de produção), mas concede o ponto e nota que a direção do resultado é improvável de inverter.

### 9. Multiplicidade *dentro* da mineração de regras
- **Categoria:** Estatística
- **Probabilidade:** Média
- **Ataque:** A pesquisa por níveis sobre 59 características × 4 bins até comprimento 4 testa um espaço de candidatos enorme com KS p ≤ 0,10 (permissivo). Algumas das "48 regras validadas" são falsos positivos esperados; nenhum nulo de permutação/bootstrap tem em conta a pesquisa.
- **Resposta:** Um teste de permutação de rótulos responderia "quantas regras por acaso?" Reconhece-o como uma salvaguarda em falta; o suporte ≥20% mitiga parcialmente mas não resolve.

### 10. "Elogias o Exceptional Model Mining — porque não o usaste?"
- **Categoria:** Algoritmo alternativo
- **Probabilidade:** Média (armadilha que armaste no trabalho futuro)
- **Ataque:** O EMM captura a estrutura de interação/desvio de regressão que as regras marginais KS não apanham e é possivelmente mais adequado ao teu problema. O mesmo para o RuleFit e o CORELS / Bayesian Rule Lists como aprendizes de regras modernos e mais fundamentados do que o RIPPER de 1995.
- **Resposta:** Enquadra o CarenR como o foco deliberado e o EMM/RuleFit como os próximos comparadores fundamentados; concorda que os comparadores tendem para o antigo.

### 11. Convergência entre algoritmos: robustez ou confundimento partilhado?
- **Categoria:** Argumento fraco
- **Probabilidade:** Média
- **Ataque:** Três algoritmos alimentados com as mesmas características colineares e confundidas por era podem convergir nas mesmas características *confundidas*. Convergência é consistência, não correção. O Objetivo 1 também não tem validação em conjunto retido — regras mineradas na série completa, "validadas" apenas por KS dentro da amostra e LOESS dentro da amostra.
- **Resposta:** Posiciona o Objetivo 1 como descoberta *descritiva* com validade aparente de domínio, não uma afirmação preditiva. Suaviza "validado".

### 12. O recurso-como-virtude sobrevaloriza o não-desempenho
- **Categoria:** Argumento fraco
- **Probabilidade:** Média-baixa
- **Ataque:** O RVV abstém-se 44% das vezes e é prejudicial nos outros 56%, enquadrado como o modelo a "dizer com princípios não sei". Arguente direto: um previsor que ou se abstém ou prejudica não é útil.
- **Resposta:** Versão honesta: o recurso *limita os danos* de regras globalmente prejudiciais. Não sobrevalorizes.

### 13. A decomposição em duas decisões é genuinamente nova?
- **Categoria:** Contribuição / argumento fraco
- **Probabilidade:** Média-baixa
- **Ataque:** Um arguente com pendor teórico pode dizer que reformula ideias conhecidas — a decomposição do erro de previsão, e o resultado econométrico clássico de que a destendência agressiva remove sinal.
- **Resposta:** Reivindica-a precisamente como uma contribuição de *enquadramento/diagnóstico*: nomeá-la, operacionalizá-la para localizar o estrangulamento na Decisão 1, e demonstrar o compromisso empiricamente via LOESS. Não um novo teorema.

### 14. Explosão de características e colinearidade
- **Categoria:** Metodológica
- **Probabilidade:** Menor
- **Ataque:** 59 características de uma grelha combinatória, muitas quase duplicadas (admites que as características de topo do RVV são correlacionadas). Sem tratamento de colinearidade, sem redução de dimensionalidade; inflaciona a redundância das regras (a tua sobreposição de 64,7%). Sem estabilidade de seleção de características reportada entre folds.
- **Resposta:** Reconhece; nota que a filtragem por suporte e a análise de sensibilidade abordam parcialmente a estabilidade. Reportar a estabilidade de seleção é um pedido justo.

### 15. "CV accuracy" do RIPPER/M5Rules — que CV, e viola a tua própria regra?
- **Categoria:** Inconsistência estatística (armadilha limpa)
- **Probabilidade:** Menor
- **Ataque:** O Cap.2 argumenta que a CV aleatória k-fold é inválida em séries não-estacionárias; o Cap.4 reporta depois "CV accuracy 42%" e "CV R² = −0,07". Se forem k-fold aleatório, fizeste aquilo que condenaste.
- **Resposta:** Clarifica o esquema de CV logo à partida; se for k-fold, enquadra esses valores como apenas descritivos dentro da amostra.

---

## Corrigir antes da submissão (não apenas preparar)
- **#4** — a contradição 90–92% vs 15–16%. Está factualmente errado como está escrito e é uma granada de credibilidade.
- **#6** — reconcilia o texto do protocolo do §3 ("ano seguinte") com as tabelas ("todos os anos restantes"), e suaviza o peso causal colocado nos p-values de Wilcoxon.

## Respostas de maior alavancagem para ensaiar até ficarem reflexivas
**#1 (rendimento/ha), #2 (correção por fazer), #5 (baseline verdadeira), #6 (independência).** Estas quatro são onde uma defesa apenas-boa se torna vacilante.

## Conclusão
Intelectualmente madura e invulgarmente honesta — o *diagnóstico* é o valor real e o enquadramento é defensável. Mas três pontos frágeis estruturais — modelar a produção total em vez do rendimento/ha, deixar por fazer a tua própria correção proposta, e um desenho walk-forward cujos folds de teste não são independentes — são o que um júri exigente vai cercar.
