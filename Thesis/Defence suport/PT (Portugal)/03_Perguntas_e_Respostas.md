# Perguntas & Respostas da Defesa — Hugo Nogueira, Tese MECD
**Previsão da Produção Vinícola com Aprendizagem Automática Baseada em Regras**
*Preparado para a defesa de Mestrado na FEUP, junho de 2026*

> **Nota:** A resposta Q5 foi corrigida para "pesquisa por níveis ao estilo Apriori" e "p ≤ 0,10", em linha com a errata (o CAREN não faz *beam search*). Ver `07_ERRATA_imprimir_e_levar.md`.

---

## PARTE A — 20 Perguntas Difíceis do Júri

1. O CarenR encontra regras distribucionais estatisticamente significativas, mas nenhum modelo baseado em regras bate consistentemente a Naive_Median na previsão walk-forward. Isto não significa que as regras não têm valor preditivo?
2. O achado da composição por era (o CarenR vence 5/5 cenários equilibrados por era, p=0,031) foi feito post-hoc, depois de examinar os resultados. Como podes afirmar que é um achado científico e não garimpagem de dados?
3. A tua baseline Naive_Median prevê o resíduo mediano de treino ≈ 0. Não é essencialmente apenas "prever a tendência"? Qualquer previsão competente já faz isto — porque é que batê-la é uma fasquia relevante?
4. É aplicada destendência linear, mas os resíduos do RVV continuam a falhar o teste de estacionaridade ADF após a destendência. Isto não invalida todo o arcabouço de modelação para o RVV?
5. Usas a significância do teste KS (p < 0,05) como critério de descoberta das regras CarenR. Com 59 características e múltiplas variantes, como controlaste as comparações múltiplas?
6. O teste de esforço LOESS mostra que as características dominantes do RVV colapsam para 15–16% de correlação com a remoção de era — mas ainda apresentas 20 regras CarenR para o RVV como um resultado legítimo. Essas regras não são inúteis?
7. O M5Rules colapsa num modelo global em conjuntos de dados com n<89. Dado isto, porque incluí-lo sequer como baseline? Que propósito científico serve um modelo degenerado?
8. Tens apenas 18 cenários walk-forward para o RDD e 16 para o RVV. São amostras estatísticas minúsculas. Como pode qualquer p-value de um teste de Wilcoxon sobre 18 observações ser interpretado de forma significativa?
9. O teu "Arcabouço de Decomposição em Duas Decisões" é essencialmente apenas a decomposição viés-variância ou sinal-ruído reescrita em termos de domínio. O que é de facto novo aqui?
10. A convergência de características entre algoritmos (RIPPER, CarenR, M5Rules a selecionar as mesmas características) podia simplesmente significar que os três sobreajustam ao mesmo sinal confundido por era. Porque é a convergência evidência de estrutura genuína dos dados e não de sobreajuste partilhado?
11. O achado do teste binomial (5/5 vitórias em cenários equilibrados por era, p=0,031) usa n=5. Um teste binomial com n=5 tem quase nenhuma potência e o seu erro de Tipo I a p=0,031 só é significativo se o teste foi pré-especificado. Foi?
12. Afirmas que "diagnosticar modos de falha é em si uma contribuição científica". Isto é uma reformulação post-hoc de resultados negativos. Como distingues diagnóstico com princípios de racionalizar o fracasso?
13. A engenharia de características cria 59 características a partir de dados meteorológicos diários com âncoras fenológicas. Validaste que as datas dos estados fenológicos (Abrolhamento, Floração, Pintor, Vindima) são consistentes ao longo da janela de observação de 89 anos? O calendário dos estados desloca-se ele próprio com as alterações climáticas.
14. Todo o pré-processamento (destendência, discretização, seleção de características) é feito dentro de cada fold. Mas a seleção da variante CarenR (qual das 11 usar) — foi também dentro do fold, ou foi escolhida uma única melhor variante retrospetivamente?
15. O coeficiente de variação dos resíduos de produção é 47%. Isto é um ruído extremamente elevado. Podes justificar que qualquer abordagem de aprendizagem automática, por mais sofisticada, conseguisse aprender de forma fiável com esta relação sinal-ruído?
16. As características do RVV mantêm apenas 15–16% de correlação após destendência LOESS. Mas usaste estas características contaminadas por era para descobrir 20 regras CarenR. Isto não significa que o Objetivo 1 (descoberta de regras) também falhou para o RVV?
17. A tua métrica MAE ponderado atribui maior peso a cenários com conjuntos de treino maiores. É uma escolha razoável, mas significa que os cenários iniciais — quando o conjunto de treino é mais homogéneo — têm menos influência. Poderá esta ponderação estar a mascarar que o CarenR tem bom desempenho cedo e degrada-se ao longo do tempo?
18. Comparas 17 modelos em duas regiões. Nenhuma correção para comparações múltiplas (ex. Bonferroni, FDR) é aplicada aos testes de Wilcoxon. Como justificas os p-values reportados?
19. O RIPPER exige discretização em tercis do alvo contínuo (resíduo de produção). Essa discretização é feita dentro do fold — mas as fronteiras dos tercis mudam entre folds à medida que o conjunto de treino cresce. Isto não significa que as regras do RIPPER não são comparáveis entre folds?
20. A tese recomenda acrescentar covariáveis estruturais (área plantada, quotas da UE, produtores certificados) como direção futura principal. Mas essas covariáveis estão elas próprias fortemente correlacionadas com o tempo — acrescentá-las não vai recriar o mesmo problema de confundimento por era sob outra forma?

---

## PARTE B — Respostas Técnicas Fortes

**Q1: As regras são significativas mas a previsão falha — sem valor preditivo?**

As regras têm valor descritivo e explicativo genuíno, mas isso não é o mesmo que valor preditivo. O teste KS exige apenas que *exista* um desvio distribucional sob o conjunto de condicionamento — não exige que o desvio domine a variância fora da amostra. Com CV=47%, mesmo uma associação real (|r|≈0,47) explica apenas ~22% da variância dos resíduos; 78% é ruído inexplicado. A descoberta distribucional precisa que o sinal *exista*; a previsão precisa que ele *domine*. São requisitos fundamentalmente diferentes. Esta distinção é precisamente o que a Decomposição em Duas Decisões formaliza. As regras não são inúteis — dizem aos produtores que condições climáticas produzem historicamente colheitas excecionais ou catastróficas. Apenas não conseguem prever de forma fiável o valor do ano seguinte só a partir do clima do ano corrente.

**Q2: Achado da composição por era post-hoc — garimpagem de dados?**

É uma preocupação legítima e abordo-a diretamente na tese. A hipótese da composição por era não foi pré-especificada antes de correr a experiência walk-forward — foi formada ao examinar o padrão de vitórias e derrotas. Por essa razão rotulo explicitamente o achado como "POST-HOC EXPLORATÓRIO" e afirmo que exige validação prospetiva. O teste binomial (p=0,031) estabelece que o padrão é improvável sob o nulo de vitórias aleatórias, mas não o estabelece como uma lei científica confirmada. A contribuição é *identificar e nomear* o regime condicional — não reivindicar evidência confirmatória. O trabalho futuro deve pré-registar a hipótese e testá-la em dados de 2023+ para o RDD à medida que forem disponibilizados.

**Q3: A Naive_Median é só "prever a tendência" — batê-la é significativo?**

A Naive_Median é mais específica e mais exigente do que simplesmente "prever a tendência". Prevê o *resíduo* mediano de treino, que é tipicamente próximo de zero mas não exatamente zero, e é o preditor constante ótimo sob perda MAE. Batê-la significa extrair sinal climático genuíno acima da baseline, para além do que a tendência linear já capta. Qualquer modelo que não bata a Naive_Median está a acrescentar ruído, não sinal. A questão de saber se algum modelo bate esta fasquia é precisamente a pergunta científica certa: dado um sinal climático fraco e ruído elevado, existe alguma estrutura aprendível acima da tendência? A resposta para o RDD (limítrofe, condicionalmente sim) e o RVV (não, as regras prejudicam) é em si um resultado informativo.

**Q4: Resíduos do RVV continuam a falhar o ADF após destendência — arcabouço invalidado?**

Falhar o ADF após destendência linear significa que a série tem não-estacionaridade residual — provavelmente um desvio estrutural da média por volta de 1986 (quotas vitivinícolas da UE) que a OLS linear não consegue remover. Isto não invalida o arcabouço; *explica* os resultados do RVV. A não-estacionaridade é a razão mecanística de o confundimento por era ser mais severo no RVV do que no RDD. O arcabouço foi concebido para operar sob estas condições e para diagnosticar precisamente quando é sobrepujado. O teste de esforço LOESS quantifica o grau de contaminação. Um arcabouço que identifica quando e porque falha é cientificamente mais valioso do que um que ignora o problema.

**Q5: Comparações múltiplas com 59 características?**

O CarenR aplica o teste KS regra a regra, mas a pesquisa não é um problema de comparação múltipla de 59 testes independentes. O CarenR constrói regras conjuntivas usando uma pesquisa por níveis ao estilo Apriori podada por suporte mínimo; não testa as 59 características independentemente. O limiar de significância (p ≤ 0,10) é aplicado à *regra induzida final* como filtro pós-indução, não a cada característica candidata independentemente. Além disso, o limiar de suporte (cobertura mínima de 20%) reduz substancialmente o espaço de pesquisa efetivo, já que a pesquisa por níveis só expande itemsets frequentes em suporte. Ainda assim, reconheço que múltiplas regras do mesmo conjunto de dados não são independentes, e reporto todas as regras com os seus p-values em bruto e não ajustados. Uma correção ao estilo Bonferroni seria conservadora dada a estrutura de pesquisa por níveis podada por suporte, mas aplicar correção FDR (ou um nulo por permutação de rótulos) ao conjunto de regras é uma extensão metodológica futura válida.

**Q6: As regras do RVV estão contaminadas por era — porque apresentá-las?**

Apresentar as 20 regras do RVV com a validação LOESS que expõe a sua contaminação é em si a contribuição científica. As regras não são apresentadas como sinal climático fiável; são apresentadas ao lado da evidência que as colapsa — o que demonstra o poder de diagnóstico do protocolo LOESS. Um investigador que aplicasse o CarenR ingenuamente aos dados do RVV reportaria 20 regras e concluiria sucesso. Esta tese mostra porque essa conclusão estaria errada. As regras servem de objeto da experiência de diagnóstico, não de achados validados isoladamente.

**Q7: Porque incluir o M5Rules se degenera?**

Incluir o M5Rules e documentar o seu colapso com n<89 fornece uma referência prática importante. Mostra que a indução de regras por árvore de regressão padrão degenera em pequenos conjuntos de dados agrícolas e colapsa num modelo global — um achado de relevância direta para os profissionais. Demonstrar que o M5Rules tem CV R²=0,004 a −0,07 confirma que o problema não é resolúvel por aprendizes de regras de regressão arbitrários e justifica a escolha de uma abordagem distribucional. Um resultado negativo com um algoritmo bem compreendido é mais informativo do que a sua omissão.

**Q8: Apenas 18 cenários walk-forward — interpretação de Wilcoxon?**

Testes de Wilcoxon de postos com sinais sobre 18 pares têm de facto baixa potência. O teste consegue detetar diferenças direcionais consistentes mas tem capacidade limitada para distinguir efeitos de dimensão modesta. Reporto p-values exatos e reconheço esta limitação explicitamente. O resultado global do RDD (p=0,468, não significativo) reflete corretamente que a evidência é insuficiente para confirmar a vantagem do CarenR globalmente. O resultado negativo confirmatório para o RVV (p=0,004) tem boa potência precisamente porque o efeito é grande e consistente. A limitação corta nos dois sentidos: significa que não posso reivindicar que o CarenR vence (para o RDD no geral), mas também que quando não encontro vantagem significativa não estou a concluir incorretamente o fracasso com dados insuficientes.

**Q9: O Arcabouço das Duas Decisões é só viés-variância reescrito?**

A decomposição viés-variância aplica-se ao erro quadrático esperado e decompõe o erro do modelo em viés, variância e ruído irredutível — uma propriedade do estimador. O Arcabouço das Duas Decisões é diferente: aplica-se ao MAE num contexto walk-forward e decompõe o *erro total de previsão* numa decisão de tendência interpretável em termos de domínio (afetada por desvios de regime estruturais, não pela variância do modelo) e numa decisão de sinal residual (afetada pela força do sinal climático e pela contaminação de era). O arcabouço atribui a falha a *mecanismos causais* — não-estacionaridade estrutural vs. sinal fraco — em vez de a propriedades do estimador. Este enquadramento gera diretamente direções de investigação acionáveis (covariáveis estruturais corrigem a Decisão 1; séries mais longas dentro da era corrigem a Decisão 2) de um modo que a viés-variância não faz.

**Q10: A convergência entre algoritmos podia ser sobreajuste partilhado?**

Se os três algoritmos estivessem a sobreajustar ao mesmo sinal de era, deveriam convergir em *características temporais específicas da era* (ex. índice do ano, características que tendem monotonicamente) e não em características fisiologicamente significativas (água no solo na vindima, dias húmidos pós-floração). As características em que convergem — disponibilidade de água no solo na vindima, dias de stress térmico à volta da maturação — são as que têm base agronómica mais forte para explicar a variação de produção dentro da era. O teste de esforço LOESS confirma que estas características no RDD mantêm 90–125% da sua correlação após remoção de era. No RVV, não — e a convergência no RVV é mais fraca. Portanto a evidência de convergência é mais forte precisamente onde o teste LOESS confirma independentemente sinal genuíno, e mais fraca onde não confirma. Essa corroboração entre testes independentes é o argumento-chave.

**Q11: Teste binomial n=5, foi pré-especificado?**

Não, não foi pré-especificado — a hipótese da composição por era emergiu do exame dos resultados. Sou explícito quanto a isto. O achado p=0,031 é descrito como exploratório, não confirmatório. Com n=5, o teste binomial só consegue detetar efeitos com probabilidade ≥ ~0,97 a α=0,05 (todas as 5 vitórias). O padrão observado (5/5) é o resultado mais extremo possível — significa que o CarenR venceu *todos* os cenários equilibrados por era. O teste estatístico é quase secundário face à observação categórica. Dito isto, aceito plenamente que é necessário um teste prospetivo em novos dados antes de isto poder ser reportado como achado confirmado.

**Q12: Diagnosticar a falha como contribuição — racionalização post-hoc?**

O diagnóstico é pré-hoc na estrutura, mesmo que post-hoc na descoberta. A Decomposição em Duas Decisões foi concebida como um arcabouço geral aplicável a qualquer problema de previsão com decomposição tendência + resíduo, e aplico-o para explicar tanto o sucesso (RDD equilibrado por era) como a falha (RVV no geral) usando a mesma maquinaria analítica. Racionalização seria inventar uma explicação *específica do resultado negativo* que não pudesse ser falsificada. O Arcabouço das Duas Decisões é falsificável: se as covariáveis estruturais melhorarem a Decisão 1 no RVV e as regras se tornarem então competitivas, o arcabouço é confirmado. Se a Decisão 1 melhorar mas as regras continuarem a falhar, o arcabouço prevê que a Decisão 2 tem de ser o estrangulamento — uma previsão testável. Isto é diagnóstico, não racionalização.

**Q13: Consistência dos estados fenológicos ao longo de 89 anos?**

É uma limitação genuína. A tese usa âncoras fenológicas fixas (dia-do-ano para Abrolhamento, Floração, Pintor, Vindima) estimadas a partir de dados regionais agregados. As alterações climáticas deslocaram a fenologia da vinha — a vindima no Douro ocorre agora ~2–3 semanas mais cedo do que em 1934. Isto significa que "água no solo 20 dias antes da vindima" em 1934 e em 2020 não se refere ao mesmo período meteorológico. As características são calculadas relativamente a janelas de calendário fixas em vez de datas fenológicas reais. Idealmente, usar-se-iam datas fenológicas observadas por ano e região. Esses dados não estavam disponíveis para toda a janela de 89 anos. Reconheço isto como uma limitação de qualidade das características que pode explicar algum ruído no sinal.

**Q14: A seleção de variantes foi dentro do fold ou retrospetiva?**

A seleção da variante CarenR foi feita comparando o desempenho walk-forward global das 11 variantes após a experiência completa — foi retrospetiva, não dentro do fold. É uma limitação genuína. Tratar a seleção de variantes como dentro do fold seria a abordagem plenamente rigorosa mas exigiria uma estrutura de validação cruzada aninhada, computacionalmente intensiva e estatisticamente difícil com apenas 18/16 cenários. A abordagem atual arrisca selecionar uma variante que por acaso se ajusta à janela walk-forward específica disponível. Apresento resultados para as 11 variantes (não só a melhor), o que mitiga parcialmente isto — o júri pode inspecionar se a melhor variante reportada é um outlier ou parte de um padrão consistente.

**Q15: CV=47%, ruidoso demais para qualquer ML funcionar?**

Teoricamente, com CV=47% e |r|≈0,47 (características climáticas a explicar ~22% da variância dos resíduos), o R² máximo alcançável fora da amostra só a partir do clima é aproximadamente 0,22 em condições ideais — e a validação walk-forward com conjuntos de treino pequenos degrada-o ainda mais. A questão não é se 22% é grande, mas se é aprendível e consistente. O achado do RDD equilibrado por era sugere que, com dados de treino suficientes dentro da era, uma pequena vantagem consistente é aprendível. A tese não afirma resolver o problema de previsão — estabelece *quando e porque* é resolúvel, o que exige caracterizar os limites. A resposta honesta é: um CV de 47% está perto ou no limiar do que os métodos distribucionais conseguem extrair de forma fiável. Isso é um achado, não um fracasso.

**Q16: O Objetivo 1 (descoberta de regras) também falhou para o RVV?**

Depende do que "falha" significa. Se o Objetivo 1 é "encontrar regras estatisticamente significativas", o CarenR tem sucesso — 20 regras com p<0,05. Se o Objetivo 1 é "encontrar regras que descrevam sinal climático genuíno dentro da era", então para o RVV o teste LOESS mostra que as características dominantes são artefactos de era, não sinal genuíno. Apresento ambas as leituras honestamente. A tese não esconde isto — a tabela LOESS marca explicitamente duas características do RVV como "Impulsionadas por era; colapsam sob LOESS". O que defendo é que o diagnóstico LOESS *é* uma contribuição: diz aos futuros investigadores que os dados pré-1986 do RVV devem ser excluídos ou modelados com covariáveis estruturais antes de se tentar a indução de regras. Descobrir o que *não* funciona sob que condições tem valor científico.

**Q17: O MAE ponderado mascara a degradação inicial vs. tardia do desempenho?**

É uma pergunta metodológica perspicaz. Ponderar pelo tamanho do conjunto de treino de facto reduz o peso dos cenários iniciais — que têm conjuntos de treino menores e mais homogéneos. Um modelo com bom desempenho cedo e que se degrada mais tarde (à medida que entra mais contaminação entre eras no treino) seria desfavorecido por este esquema de ponderação face ao MAE não ponderado. Não analisei explicitamente as tendências de desempenho ao longo do tempo, o que é uma limitação. Um gráfico de desempenho móvel (MAE por cenário ao longo do tempo) revelaria se há degradação sistemática. Esta é uma extensão válida que o júri poderia propor.

**Q18: 17 modelos, sem correção para comparações múltiplas?**

A comparação primária é a melhor variante do CarenR vs. Naive_Median — um único teste pré-especificado por região. A tabela de 17 modelos é apresentada como um ranking descritivo, não como 17 testes de hipótese independentes. Aplicar a correção de Bonferroni a 17 comparações exigiria p<0,003 para significância, o que tornaria o resultado limítrofe do RDD (p=0,468) ainda menos notável — mas a conclusão já é "não significativo". Para o RVV (p=0,004), mesmo sob correção de Bonferroni (0,004 × 17 = 0,068), o resultado é limítrofe significativo. Reporto p-values não corrigidos e reconheço a limitação, consistente com o enquadramento exploratório da comparação multi-modelo.

**Q19: As fronteiras dos tercis do RIPPER mudam entre folds — comparabilidade?**

Sim, as fronteiras dos tercis são específicas do fold. No fold k, as fronteiras são calculadas a partir do conjunto de treino até ao ano k; no fold k+1, acrescenta-se mais um ano e as fronteiras deslocam-se. Isto significa que a pertença às classes "Baixo/Médio/Alto" não é idêntica entre folds. No entanto, esta é a abordagem *correta* — usar dados retidos para definir as fronteiras seria fuga. A consequência é que as regras do RIPPER são treinadas e avaliadas sobre discretizações específicas do fold. A comparabilidade entre folds das regras do RIPPER é de facto limitada, o que é em parte a razão de eu me focar no MAE (na escala contínua original após reverter as previsões) e não na exatidão de classificação. As saídas discretas do RIPPER são reconvertidas em previsões contínuas via centroides de classe do fold de treino — isto restaura a comparabilidade ao nível da métrica mesmo que não ao nível da regra.

**Q20: As covariáveis estruturais recriariam o confundimento por era?**

Este é o desafio de trabalho futuro mais sofisticado. Tens razão que as séries de área plantada e de quota da UE estão elas próprias correlacionadas com o tempo — acrescentá-las ingenuamente poderia substituir proxies temporais pelo sinal climático em vez de remover o confundimento por era. A chave é *como* as covariáveis estruturais são incorporadas. A proposta é incluí-las no *modelo de destendência* (Decisão 1) para que o resíduo após remover a tendência estrutural seja um sinal climático mais limpo para a indução de regras (Decisão 2). Se a área plantada explicar a quebra de regime por volta de 1986 no RVV, então os resíduos de "produção ~ área plantada + regime de quota" seriam estacionários, ou pelo menos mais do que os resíduos de "produção ~ tempo". Esta é a abordagem de destendência estrutural, análoga aos modelos de cointegração em econometria. O risco que identificas é real, mas é gerido garantindo que o resíduo-covariável é testado quanto à estacionaridade antes de avançar para a indução de regras.

---

## PARTE C — Potenciais Fraquezas

1. **O confundimento por era é severo e reconhecido mas não resolvido.** A principal ameaça metodológica não é controlada no sistema final — apenas diagnosticada. Para o RVV, as regras dominantes são artefactos de era. A tese deteta isto mas não o corrige.

2. **Hipótese post-hoc.** O achado da composição por era (5/5 vitórias, p=0,031) é o resultado preditivo mais positivo da tese e é inteiramente post-hoc. Sem validação prospetiva, não pode ser tratado como confirmado.

3. **A seleção de variantes não é dentro do fold.** A escolha de qual das 11 variantes CarenR reportar como "melhor" foi feita após examinar os resultados, introduzindo enviesamento de seleção na comparação preditiva.

4. **Amostras muito pequenas.** 18 e 16 cenários walk-forward fornecem potência estatística limitada. Testes de Wilcoxon sobre 18 pares não conseguem detetar efeitos de dimensão moderada de forma fiável.

5. **As âncoras fenológicas são estáticas.** As janelas de calendário fixas para os estados de crescimento da vinha são uma aproximação que se degrada ao longo da janela de observação de 89 anos à medida que o clima desloca a fenologia.

6. **CarenR_Assoc não implementado em walk-forward.** A variante associativa foi excluída da validação preditiva por problemas de alinhamento de fatores, tornando a comparação incompleta.

7. **Sem análise espacial ou sub-regional.** Os agregados anuais ao nível da DO mascaram heterogeneidade dentro da região que poderia carregar sinal preditivo adicional.

8. **O M5Rules colapsa sempre.** Incluir uma baseline que degenera num modelo global limita a informatividade da comparação no extremo superior da complexidade.

9. **Destendência apenas linear.** Usar a tendência linear OLS como único método de destendência não consegue lidar com a quebra estrutural não-linear claramente visível nos dados do RVV.

10. **Sem quantificação de incerteza.** Previsões pontuais sem intervalos de previsão limitam a utilidade operacional de qualquer previsão baseada em regras.

---

## PARTE D — Como Defender Cada Fraqueza

**W1 — Confundimento por era não resolvido:**
"Tem razão que isto é diagnosticado mas não resolvido — e essa distinção é deliberada. Resolvê-lo exige covariáveis estruturais (área plantada, quotas) que estão fora do âmbito só-clima desta tese. A contribuição é estabelecer *exatamente o que* é necessário para o resolver e *porquê* — o que fornece uma agenda de investigação clara e acionável. Uma tese que afirmasse resolver este problema sem os dados necessários seria cientificamente mais fraca, não mais forte."

**W2 — Hipótese post-hoc:**
"Rotulo-a explicitamente como exploratória e apelo à validação prospetiva. Chamar exploratório a um achado não é o mesmo que descartá-lo. O pré-registo e a descoberta de hipóteses são ambos fases legítimas do processo científico. Descobrir um regime condicional e nomeá-lo para que possa ser testado prospetivamente é uma contribuição científica na fase de geração de hipóteses."

**W3 — Seleção de variantes não dentro do fold:**
"É uma limitação genuína que reconheço. A mitigação é a transparência: reporto as 11 variantes, não só a melhor, para o júri poder ver se a variante selecionada é um outlier ou parte de um padrão robusto. Uma abordagem plenamente rigorosa aninharia a seleção de variantes dentro do fold, mas com apenas 18 cenários isso deixaria potência insuficiente para a avaliação exterior. A melhor variante reportada (CarenR_Eta2 para o RDD) não é um outlier dramático — várias variantes ficam próximas."

**W4 — Amostras pequenas:**
"O problema da amostra pequena é inerente aos dados agrícolas anuais e não pode ser resolvido sem mudar de domínio. A tese contorna-o no desenho: o teste de Wilcoxon é não-paramétrico e não assume normalidade; o MAE ponderado reduz a variância ao enfatizar os folds de treino maiores; os achados estatísticos são reportados com p-values exatos e claramente sinalizados quando têm baixa potência. A alternativa — não correr estatística inferencial de todo — seria menos rigorosa, não mais."

**W5 — Âncoras fenológicas estáticas:**
"É uma aproximação real. A solução ideal exige registos de observação fenológica de longo prazo emparelhados com os dados meteorológicos, que não existem para toda a janela de 1934–2022. O recurso prático — janelas de calendário fixas — é a mesma abordagem usada na maioria da literatura de ML agrícola a esta escala temporal. Trabalho futuro com datas fenológicas observadas melhoraria a qualidade das características. Nomeio isto explicitamente como limitação em vez de a obscurecer."

**W6 — CarenR_Assoc não em walk-forward:**
"O problema de alinhamento de fatores surge porque os níveis de fator do CarenR_Assoc são calculados no conjunto de treino e podem não cobrir todos os níveis do ano de teste — um problema fora da amostra padrão da codificação categórica. Resolvê-lo exige uma estratégia de recodificação dentro do fold que não implementei. A omissão está documentada e nomeada como extensão natural; não afeta a validade dos resultados do CarenR_Dist."

**W7 — Sem análise sub-regional:**
"Os dados agregados anuais são a única série consistentemente disponível com 89 anos de comprimento para estas DO. A desagregação sub-regional exigiria séries mais curtas e incompletas para cada sub-zona e reduziria ainda mais a amostra. A tese é explícita quanto a operar ao nível agregado da DO. A análise sub-regional é uma direção futura assim que registos sub-regionais mais longos estiverem disponíveis."

**W8 — O M5Rules degenera:**
"Uma baseline degenerada é informativa precisamente porque degenera. Estabelece que a indução de regras por árvore de regressão padrão falha nesta dimensão de problema, o que motiva o uso de métodos distribucionais. Se o M5Rules tivesse bom desempenho, a complexidade do CarenR seria desnecessária. A degeneração não é uma falha no desenho experimental — é um resultado."

**W9 — Apenas destendência linear:**
"A destendência linear é a abordagem padrão na modelação de séries temporais agrícolas e a mais defensável para aplicação dentro do fold, porque a OLS é simples, transparente e não requer afinação. A destendência LOESS é usada como *teste de esforço* (não como a destendência de produção) precisamente para caracterizar o que a destendência linear deixa escapar — que é todo o objetivo da análise de confundimento por era. Se tivesse usado o LOESS como destendência de produção, o teste de esforço LOESS seria circular."

**W10 — Sem quantificação de incerteza:**
"As previsões pontuais são a saída natural do formato de regra do CarenR — as regras preveem um desvio distribucional, não um intervalo. Acrescentar intervalos de previsão exigiria ou bootstrapping da indução de regras ou invólucros de previsão conforme, ambos extensões válidas. Isto está anotado como trabalho futuro. A tese avalia o MAE de previsão pontual, que é padrão na literatura de previsão agrícola para este tipo de modelo."

---

*Fim do documento de Perguntas & Respostas da Defesa*
