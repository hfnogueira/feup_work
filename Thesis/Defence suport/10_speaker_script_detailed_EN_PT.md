# Detailed Speaker Script — Defense (EN + PT)



**Wine Production Forecasting Using Rule-Based Machine Learning** · Hugo Nogueira · MECD, FEUP
A fuller, closer-to-spoken script for each of the 25 slides — for study and rehearsal. Deliver naturally; don't read verbatim. Facts corrected throughout (support ≥ 20%, level-wise/Apriori search, LAI family = 4/20 RVV rules, heat-stress days and soil water dominant in RVV).

---

# Guião detalhado para a defesa — versão portuguesa revista

## Slide 1 — Título (~35 s)

Bom dia e obrigado pela vossa presença.

A minha dissertação parte de uma pergunta aparentemente simples sobre a produção de vinho em Portugal: o que consegue o clima explicar sobre a produção anual e, sobretudo, o que consegue prever de forma fiável?

Estas duas questões não são equivalentes, e a diferença entre explicar e prever constitui o fio condutor desta apresentação.

Este trabalho é relevante porque a produção de vinho é uma das atividades agrícolas mais sensíveis ao clima na Europa. Além disso, quem toma decisões com base numa previsão — cooperativas, entidades reguladoras ou responsáveis pelo planeamento — não pode depender apenas de uma *black box*. Precisa de compreender o resultado, questioná-lo e perceber as condições que estão por detrás da previsão.

Nos próximos vinte minutos vou abordar três pontos: primeiro, porque este problema de previsão é particularmente difícil; segundo, como construí uma avaliação baseada em regras em torno do algoritmo CarenR; e, por fim, o que os resultados nos dizem sobre previsão, interpretabilidade e sobre a evidência que ainda está em falta.

---

## Slide 2 — Contexto: uma questão de decisão (~45 s)

Começo por explicar porque é que este problema é relevante.

A produção regional de vinho é uma variável importante para a tomada de decisão. As cooperativas planeiam a logística da vindima com base nela, os reguladores definem quotas, as cadeias de abastecimento dimensionam a capacidade de armazenamento e os mercados formam expectativas relativamente aos preços.

No entanto, este único valor anual resulta da combinação de muitos fatores: o clima, a gestão das vinhas, a área plantada, a regulação e as alterações estruturais que ocorrem gradualmente no setor.

E aqui está o ponto essencial: para apoiar estas decisões, não basta que um modelo produza apenas um número.

Quando uma previsão falha, ou quando a região está a atravessar uma transformação estrutural, é importante perceber se o modelo identificou efetivamente um sinal climático ou se se limitou a acompanhar a tendência histórica da produção.

Esta distinção é muito difícil de fazer com uma *black box*. É precisamente neste espaço que a minha tese se posiciona.

---

## Slide 3 — Problema de investigação: previsão auditável (~1 min)

O problema não é apenas fazer previsão. É fazer previsão de uma forma auditável.

Por um lado, os modelos modernos de Machine Learning conseguem prever a produtividade vitivinícola com resultados razoáveis, mas nem sempre produzem informação que um agrónomo consiga interpretar diretamente. Um valor SHAP, por exemplo, não é uma regra explícita que possa ser facilmente discutida ou contestada.

Por outro lado, os métodos estatísticos clássicos são mais interpretáveis, mas podem não captar as condições climáticas locais e não lineares que realmente influenciam a produção.

O CarenR procura ocupar deliberadamente um espaço intermédio.

O algoritmo identifica subgrupos, definidos através de regras explícitas, nos quais a distribuição da produção difere de forma significativa da distribuição global. Cada regra inclui as respetivas condições, o seu *support* e um *p-value* estatístico.

Observem o exemplo apresentado no slide: quando a água no solo durante a vindima é moderada e existem poucos dias húmidos depois da maturação, a produção fica aproximadamente 192 mil hectolitros acima da tendência. Esta regra cobre 21% dos anos e apresenta um *p-value* inferior a 0,01.

É este o tipo de resultado que a tese procura produzir. Não apenas uma conclusão genérica como “a humidade é importante”, mas uma combinação concreta e testável de janelas fenológicas e respetivos limiares.

Tanto quanto foi possível identificar na literatura, nenhum trabalho anterior reúne simultaneamente estas cinco características: interpretabilidade, análise de longo prazo, capacidade preditiva, uma variável-alvo contínua e aplicação ao contexto de uma região vitivinícola.

---

## Slide 4 — Porque é difícil: um sinal real, mas diluído (~45 s)

Porque é que este problema é tão difícil?

Porque a variável-alvo agrega quatro dimensões muito diferentes num único valor regional anual.

Primeiro, a biologia da videira estende-se por vários anos. O potencial produtivo do próximo ano é parcialmente determinado no ano anterior, através da formação dos gomos e da acumulação de reservas.

Segundo, a agregação regional esconde uma enorme variabilidade. Milhares de vinhas, com solos, altitudes, sistemas de condução e microclimas distintos, são reduzidas a um único valor.

Terceiro, a estrutura do setor altera-se ao longo do tempo. A regulação, a área plantada, as políticas agrícolas e as condições de mercado podem deslocar a *baseline* da produção independentemente das condições meteorológicas.

Finalmente, a amostra é pequena. Temos apenas entre 80 e 89 observações anuais para trabalhar com 59 *features* construídas.

Em conjunto, estes fatores criam um problema muito exigente em termos de relação sinal-ruído.

Daí a cautela central desta tese, à qual voltarei mais à frente: um resultado preditivo fraco não significa necessariamente que não exista um sinal climático. Pode significar que esse sinal existe, mas está diluído ou confundido com a própria evolução estrutural da série de produção.

---

## Slide 5 — Experiência natural: duas histórias diferentes (~40 s)

Esta é a ideia de desenho experimental que sustenta toda a tese.

Em vez de estudar apenas uma região, analiso duas regiões às quais aplico exatamente o mesmo *pipeline*, mas que apresentam histórias estruturais muito diferentes.

No Douro, observamos um crescimento gradual de aproximadamente 80% ao longo de 89 anos, acompanhado por uma variabilidade interanual relativamente baixa, com um coeficiente de variação próximo dos 27%.

Na Região dos Vinhos Verdes, observamos um declínio estrutural acentuado ao longo de 80 anos e uma variabilidade muito superior, com um coeficiente de variação próximo dos 47%.

Portanto, temos o mesmo algoritmo, as mesmas *features* e a mesma estratégia de validação, mas duas formas opostas de não-estacionaridade.

É isto que transforma os dois estudos de caso numa experiência natural comparativa. Quando os resultados das duas regiões divergem, essa diferença pode ser interpretada à luz das suas trajetórias estruturais, e não como consequência de escolhas arbitrárias de modelação.

---

## Slide 6 — Ciclo da videira: memória de dois anos (~50 s)

Antes de apresentar o método, é importante compreender um aspeto biológico que influencia todo o desenho do estudo.

A resposta da videira é sazonal, mas também desfasada no tempo.

A colheita atual concretiza-se no ano um, através de fases como a floração, o vingamento, o pintor e a vindima. No entanto, o seu potencial máximo foi parcialmente determinado no ano anterior, o ano zero.

Foi nesse ano anterior que fatores como a temperatura e a radiação influenciaram o número potencial de cachos, enquanto o stress hídrico e térmico condicionou a acumulação de reservas de hidratos de carbono.

Este efeito de *carry-over* explica porque não utilizei simplesmente médias meteorológicas anuais.

Cada *feature* foi associada a uma fase fenológica específica, tanto no ano corrente como no ano anterior.

Esta opção tem duas vantagens. Em primeiro lugar, respeita a biologia da videira. Em segundo, aumenta a interpretabilidade das regras. Uma condição como “dias húmidos após a floração” corresponde a uma janela agronomicamente relevante, e não a um mês de calendário escolhido arbitrariamente.

---

## Slide 7 — Feature engineering: a biologia, não o calendário (~40 s)

Este slide mostra o trabalho de *feature engineering* que materializa essa ideia.

Parti de aproximadamente trinta mil registos meteorológicos diários por região e transformei-os em 59 preditores anuais.

Essa transformação foi estruturada em torno de quatro fases fenológicas: abrolhamento, floração, pintor e vindima. As variáveis foram calculadas tanto para o ano corrente como para o ano anterior.

O número exato de registos não é o aspeto mais importante.

O essencial é a transformação de dados meteorológicos diários e de variáveis relacionadas com o estado da vinha em *features* anuais que incorporam memória biológica e estão alinhadas com o calendário da videira.

É esta transformação que permite que as condições apresentadas numa regra tenham um significado que um especialista consegue reconhecer e interpretar.

---

## Slide 8 — Posicionamento: entre explicação e previsão (~30 s)

É importante clarificar o âmbito deste trabalho.

Esta tese não pretende comparar o CarenR com todos os modelos de *black box*, nem afirma superar modelos de Deep Learning em termos de precisão.

O objetivo é estudar uma combinação específica que continua pouco explorada na literatura: uma variável-alvo contínua, séries longas e não estacionárias, regras interpretáveis e uma validação preditiva rigorosa — tudo no mesmo estudo.

Os modelos modernos de Machine Learning podem oferecer precisão sem transparência. Os métodos de descoberta de regras oferecem transparência, mas, neste contexto, o seu valor preditivo ainda não estava demonstrado.

A questão que coloco é se podemos obter simultaneamente interpretabilidade e capacidade preditiva.

E, igualmente importante, o que podemos aprender quando isso não acontece.

---

## Slide 9 — Desenho da investigação: dois objetivos (~40 s)

A tese está organizada em torno de dois objetivos, que exigem níveis de evidência diferentes.

O primeiro objetivo é a descoberta de padrões.

A questão é saber se os algoritmos conseguem identificar estrutura histórica relevante: que condições agroclimáticas distinguem anos acima ou abaixo da tendência e se esses sinais representam relações climáticas genuínas dentro de cada período histórico ou apenas artefactos associados a diferentes eras.

O segundo objetivo é a validação preditiva.

Aqui, a questão é mais exigente: saber se essa estrutura reduz efetivamente o erro em períodos futuros e compreender em que condições consegue, ou não, produzir previsões úteis.

Os mesmos dados e a mesma família de regras são utilizados nos dois objetivos, mas o nível de exigência é diferente.

Para a descoberta, basta demonstrar que existe uma associação. Para a previsão, essa associação tem de superar uma *baseline* em dados *out of sample*.

O principal resultado, que irei desenvolver ao longo da apresentação, é o seguinte: o Objetivo 1 é alcançado; o Objetivo 2 não é alcançado quando consideramos os resultados agregados.

E compreender a razão dessa diferença constitui uma das principais contribuições da tese.

---

## Slide 10 — Desenho do estudo: comparação controlada (~30 s)

Este slide torna explícita a lógica da comparação.

Foram mantidos constantes nas duas regiões o processo de *feature engineering*, a estratégia de *detrending*, os algoritmos baseados em regras e a métrica de avaliação.

O que varia são o regime climático, a trajetória estrutural da produção, o nível de ruído e a composição histórica dos diferentes períodos.

No total, são analisadas duas regiões, com 89 e 80 observações anuais, respetivamente.

Como o método é mantido constante, qualquer divergência nos resultados passa a constituir evidência interpretável sobre a estrutura dos dados.

É precisamente isso que permite tratar este estudo como uma experiência natural comparativa, e não apenas como dois estudos de caso independentes.

---

## Slide 11 — Algoritmos: CarenR e modelos de comparação (~1 min)

Foram avaliadas cinco famílias de modelos, mas existe um método central: o CarenR.

O CarenR estabelece a ligação entre as duas partes da tese. Por um lado, descobre regras distribucionais interpretáveis. Por outro, as suas variantes são também avaliadas como modelos preditivos.

De forma simplificada, o processo inclui quatro etapas.

Primeiro, as *features* climáticas são discretizadas em *bins* de igual frequência.

Segundo, é realizada uma pesquisa *level-wise*, inspirada no algoritmo Apriori, através da combinação progressiva de condições entre *features* e *bins*.

Terceiro, os resíduos do subgrupo identificado por cada regra são comparados com os resíduos globais através de um teste de Kolmogorov–Smirnov.

Finalmente, são retidas as regras que apresentam significância estatística e *support* suficiente.

Os restantes algoritmos têm funções específicas.

O RIPPER e o M5Rules são também algoritmos baseados em regras. Permitem verificar se as mesmas famílias de *features* surgem quando são utilizados critérios de otimização diferentes e funcionam, simultaneamente, como modelos de comparação na componente preditiva.

A Decision Tree é utilizada apenas como *benchmark* preditivo.

Por fim, a mediana, o último valor observado e a média móvel funcionam como *baselines* simples de previsão.

O ponto fundamental é que estes algoritmos não desempenham todos a mesma função. Alguns contribuem sobretudo para a interpretabilidade, outros para a comparação preditiva, e o CarenR funciona como ponte entre estas duas dimensões.

---

## Slide 12 — Objetivo 1: a lógica do teste KS (~1 min)

Este slide apresenta o funcionamento conceptual do CarenR.

Para cada regra candidata, o algoritmo coloca a seguinte questão: os anos que cumprem estas condições climáticas apresentam uma distribuição dos resíduos de produção diferente da distribuição observada no conjunto de todos os anos?

O CarenR discretiza as *features* em intervalos, combina esses intervalos numa condição legível e compara depois as duas distribuições.

No gráfico, a curva preta representa todos os anos. A curva bordô representa apenas os anos que cumprem a regra.

É importante destacar que o CarenR não verifica apenas se as médias são diferentes.

A estatística de Kolmogorov–Smirnov mede a distância máxima entre as duas distribuições cumulativas. Desta forma, consegue detetar alterações na forma da distribuição e nas respetivas caudas, e não apenas diferenças na média.

O cartão da regra, apresentado no lado direito, é o que torna o resultado auditável.

Um agrónomo pode questionar diretamente as condições da regra, verificar que o subgrupo cobre 28% dos anos e analisar o respetivo *p-value*, neste caso 0,004.

Esta é a principal vantagem de interpretabilidade relativamente a uma explicação produzida por uma *black box*.

No slide seguinte, veremos como estas regras se traduzem em resultados concretos.

---

## Slide 13 — Resultados da descoberta de padrões (~40 s)

O Objetivo 1 foi alcançado e os conjuntos de regras obtidos são suficientemente compactos para serem analisados diretamente.

No Douro foram identificadas 48 regras.

A variável mais consistente é a água no solo durante a vindima, que aparece em 54% das regras. Surge frequentemente combinada com condições secas após a floração e com níveis de calor moderados, estando associada a anos de produção acima da tendência.

Na Região dos Vinhos Verdes foram identificadas 20 regras.

Neste caso, as *features* dominantes são os dias de stress térmico, que aparecem em até metade das regras, e a água no solo na vindima, presente em 25% das regras. A família de variáveis relacionadas com o LAI, ou com a estrutura do *canopy*, aparece em 4 das 20 regras.

É importante formular cuidadosamente esta conclusão.

Estamos perante associações estatisticamente validadas e agronomicamente coerentes, mas não perante experiências controladas. Como tal, não podemos afirmar causalidade.

Outro resultado relevante é que a mesma variável pode estar associada a efeitos diferentes, ou mesmo opostos, nas duas regiões.

Por isso, a interpretação das regras tem necessariamente de ser específica ao contexto regional.

---

## Slide 14 — Regras principais: padrões identificados (~1 min 20 s)

Estas são as regras com maior significância estatística.

Começando pelo Douro, no lado esquerdo, quatro das cinco principais regras estão associadas a produção acima da tendência.

Em conjunto, apresentam uma narrativa agronómica coerente: condições relativamente secas durante a floração e a maturação, combinadas com níveis de calor controlados, caracterizam os anos de maior produção.

A regra mais forte, a regra quatro, combina níveis moderados de água no solo antes da vindima com condições secas provenientes do ano anterior. Este subgrupo apresenta uma produção aproximadamente 192 mil hectolitros acima da tendência.

A única regra associada a produção abaixo da tendência combina um LAI elevado com condições húmidas durante a floração.

Esta associação é agronomicamente plausível: um *canopy* excessivamente vigoroso num ano húmido pode favorecer o crescimento vegetativo, direcionando os recursos para as folhas em detrimento do desenvolvimento do fruto.

Na Região dos Vinhos Verdes, apresentada do lado direito, os desvios têm uma magnitude absoluta superior e quatro das cinco regras estão associadas a produção abaixo da tendência.

Este padrão é consistente com o contexto atlântico da região, no qual os anos menos produtivos são frequentemente caracterizados por condições mais frescas e húmidas. As regras são dominadas sobretudo por variáveis relacionadas com os dias de stress térmico e com a água no solo.

A principal conclusão não é que uma destas regras esteja pronta para ser operacionalizada.

O mais relevante é que as variáveis relacionadas com disponibilidade de água e stress térmico surgem repetidamente como famílias explicativas importantes e são também selecionadas pelos três algoritmos considerados.

---

## Slide 15 — Limiares exatos: perspetiva de auditoria (~1 min)

Esta é a versão de auditoria das regras apresentadas no slide anterior e, do ponto de vista da aplicação ao negócio, é uma das perspetivas mais relevantes.

Os rótulos simplificados foram substituídos pelos intervalos exatos reportados pelo CarenR.

É precisamente este o valor da interpretabilidade.

Uma regra não corresponde apenas a um valor abstrato de importância. Identifica o subgrupo, indica quantos anos cumprem as condições, apresenta o *p-value* do teste KS e quantifica a magnitude do desvio dos resíduos.

No Douro, as regras mais fortes mostram que janelas secas após a floração e após a maturação — por exemplo, entre zero e aproximadamente três dias húmidos —, por vezes combinadas com níveis moderados de água no solo antes da vindima, entre 74 e 101 milímetros, estão associadas a produção acima da tendência.

Na Região dos Vinhos Verdes, quatro das cinco regras representam um padrão fresco, húmido e com poucos dias de calor intenso.

A única regra associada a produção acima da tendência combina temperaturas mais elevadas durante o abrolhamento do ano anterior com condições moderadas durante a floração.

A mensagem que gostaria que o júri retivesse é esta: estes intervalos explícitos permitem que um agrónomo ou uma entidade reguladora questione diretamente o resultado.

É possível discutir a plausibilidade de um intervalo entre 74 e 101 milímetros. É muito mais difícil ter o mesmo tipo de discussão com uma simples barra de *feature importance*.

---

## Slide 16 — Validação entre algoritmos (~1 min)

Uma questão natural é saber se estas regras são apenas artefactos do processo de pesquisa específico do CarenR.

Este slide procura responder a essa preocupação.

O CarenR, o RIPPER e o M5Rules otimizam objetivos fundamentalmente diferentes.

O CarenR utiliza um teste distribucional baseado na estatística KS. O RIPPER utiliza critérios relacionados com ganho de informação para classificação. O M5Rules procura reduzir o erro quadrático num problema de regressão.

Por isso, quando os três algoritmos selecionam a mesma família de *features*, essa convergência constitui evidência relevante.

Duas famílias são confirmadas pelos três métodos: a água no solo durante a vindima e as variáveis relacionadas com o LAI, ou com a estrutura do *canopy*.

É importante ser rigoroso relativamente ao LAI.

Esta família aparece em 4 das 20 regras da Região dos Vinhos Verdes. A conclusão da análise entre algoritmos é que os três métodos a selecionam, e não que seja uma variável particularmente frequente no conjunto de regras.

Também não devemos interpretar esta convergência como prova causal.

O que podemos afirmar é que existe evidência convergente de que estas famílias de *features* captam estrutura real nos dados, em vez de refletirem apenas ruído específico de um determinado algoritmo.

---

## Slide 17 — Validação da descoberta: duas verificações (~45 s)

Este slide responde, com alguma nuance, à questão do possível confundimento entre as *features* e as diferentes eras históricas.

A preocupação é a seguinte: uma variável que evolua ao longo das décadas de forma semelhante à produção pode parecer informativa após um *detrending* linear, mesmo que não exista uma relação climática real dentro de cada era.

Para testar esta possibilidade, apliquei LOESS como uma estratégia de *detrending* mais agressiva, capaz de remover uma parte substancial da estrutura associada às diferentes eras.

Os resultados são distintos nas duas regiões.

No Douro, os sinais mantêm-se. Isto sugere que representam relações climáticas genuínas dentro das próprias eras.

Na Região dos Vinhos Verdes, o sinal associado aos dias húmidos permanece, mas as regras relacionadas com o LAI e com a água no solo durante a vindima desaparecem.

Isto indica que estas duas famílias estavam parcialmente associadas à mudança estrutural da região.

O desaparecimento destas regras não é uma limitação que deva ser escondida. É, por si só, um resultado importante e antecipa algumas das razões pelas quais a previsão falha nesta região.

A segunda verificação é a convergência entre algoritmos apresentada no slide anterior.

Em conjunto, estas duas análises sustentam a credibilidade dos resultados da descoberta.

No entanto — e este é o ponto de transição da apresentação — credibilidade explicativa não significa necessariamente utilidade preditiva.

---

## Slide 18 — Método de previsão: walk-forward validation (~40 s)

Para o Objetivo 2, procurei reproduzir condições realistas de previsão.

Foi utilizada uma estratégia de *walk-forward validation* com uma janela de treino expansiva: 18 cenários no Douro e 16 cenários na Região dos Vinhos Verdes.

Em cada cenário, o modelo é treinado exclusivamente com dados do passado e avaliado nos períodos que surgem posteriormente.

Um aspeto fundamental é que todo o pré-processamento é realizado dentro de cada *training fold*.

Isto inclui o *detrending*, a discretização, a seleção de *features* e a indução de regras.

Desta forma, não existe qualquer *data leakage* proveniente dos anos de teste.

Foram comparados 17 modelos com base no erro absoluto médio ponderado. Valores mais baixos representam melhores resultados.

A principal *baseline* é a mediana calculada no conjunto de treino.

Esta escolha é deliberada, porque a mediana é a previsão constante ótima quando a função de perda utilizada é o MAE.

Assim, superar esta *baseline* significa demonstrar que as *features* climáticas contêm sinal adicional que pode ser explorado para melhorar a previsão.

---

## Slide 19 — Resultados da previsão (~45 s)

Este é o resultado preditivo, apresentado de forma direta.

Nenhum dos modelos baseados em regras consegue superar globalmente a mediana em qualquer uma das regiões.

No Douro, a melhor variante do CarenR apresenta um erro de 189 mil hectolitros, comparativamente a 184 mil hectolitros para a mediana.

Isto corresponde a um resultado aproximadamente 3% pior. O teste de Wilcoxon apresenta um *p-value* de 0,468, pelo que a diferença não é estatisticamente significativa.

Na Região dos Vinhos Verdes, a diferença é consideravelmente maior.

O melhor modelo baseado em regras apresenta um erro de 172 mil hectolitros, comparativamente a 140 mil hectolitros para a mediana, o que corresponde a um resultado 22% pior.

Além disso, nos nove cenários em que as regras foram efetivamente ativadas, a sua utilização prejudicou significativamente a previsão, com um *p-value* de 0,004.

Portanto, o resultado negativo não significa apenas que não houve melhoria.

Na região mais difícil, quando as regras foram ativadas, produziram previsões significativamente piores.

Este resultado sustenta a interpretação central da tese: a descoberta de regras pode revelar estrutura relevante nos dados históricos, mas, perante um declínio estrutural forte, essa estrutura não se traduz necessariamente em capacidade de previsão futura.

---

## Slide 20 — Interpretação: um sinal real, mas demasiado fraco (~50 s)

É importante separar duas ideias que são frequentemente confundidas, porque esta distinção está no centro da tese.

A descoberta pergunta se a distribuição de um determinado subgrupo é diferente da distribuição global.

Com uma correlação absoluta próxima de 0,47, essa associação é claramente detetável ao longo de 89 anos de dados.

A previsão coloca uma questão mais exigente: a utilização dessa regra reduz o erro em períodos futuros?

Nesse caso, os resultados são menos favoráveis.

O clima explica aproximadamente 22% da variância dos resíduos. Isto significa que cerca de 78% da variabilidade permanece por explicar, seja por fatores não observados, seja por ruído.

Para a descoberta, basta que o sinal exista e seja estatisticamente detetável.

Para melhorar a previsão, esse sinal tem de ser suficientemente forte e estável para dominar as restantes fontes de variabilidade.

Por isso, o principal resultado da tese — a descoberta funciona, mas a previsão não funciona de forma consistente — não deve ser interpretado como um fracasso.

É, na realidade, o diagnóstico central.

O Machine Learning baseado em regras consegue explicar uma parte genuína da relação entre clima e produção, mas os dados disponíveis não são suficientes para produzir previsões regionais fiáveis.

O slide seguinte transforma este diagnóstico num framework metodológico.

---

## Slide 21 — Framework das duas decisões (~1 min)

Esta é provavelmente a contribuição metodológica que mais gostaria que retivessem.

Qualquer previsão aplicada a uma série com tendência envolve duas decisões distintas.

A primeira decisão consiste em modelar a tendência estrutural.

Quanto melhor for o modelo da tendência, menores serão os resíduos e mais baixo será o nível de erro que uma *baseline* simples consegue atingir.

A segunda decisão consiste em extrair o sinal climático que permanece nesses resíduos.

Para que as regras sejam estáveis e detetáveis, é necessário que exista estrutura residual suficientemente forte.

O problema é que estas duas decisões podem entrar em tensão.

Na Região dos Vinhos Verdes, a utilização de um *detrending* LOESS mais flexível melhora a *baseline*, reduzindo o erro de aproximadamente 140 para 90 mil hectolitros.

Portanto, do ponto de vista da primeira decisão, o resultado melhora significativamente.

No entanto, ao mesmo tempo, a indução de regras cai para zero. Todas as regras desaparecem.

Ou seja, ao melhorar a modelação da tendência, removemos também a estrutura residual de que o algoritmo necessitava para identificar o sinal climático.

A razão é estrutural: o tempo é a única covariável utilizada no *detrending*. Por isso, o modelo não consegue separar adequadamente as transformações do setor dos efeitos climáticos.

Este framework é aplicável a outros problemas de previsão agroclimática que utilizem séries longas e estruturalmente não estacionárias.

A tensão entre modelar a tendência e preservar o sinal não é exclusiva deste caso.

---

## Slide 22 — Contribuições (~1 min)

O que permanece como contribuição científica após um resultado preditivo negativo?

Destaco cinco pontos.

O primeiro, e mais fundamental, é a decomposição do problema em duas decisões.

Este framework separa a modelação da tendência da extração do sinal climático e permite explicar porque um modelo baseado em regras pode falhar na previsão, mesmo quando existe um sinal real nos dados.

O segundo contributo é uma avaliação *walk-forward* rigorosa.

Foram avaliados 17 modelos ao longo de 18 cenários no Douro e 16 cenários na Região dos Vinhos Verdes, com todo o pré-processamento realizado dentro de cada *fold*.

O terceiro contributo é uma análise sistemática de sensibilidade envolvendo 11 variantes do CarenR.

Os resultados mostram que a melhor estratégia de seleção de *features* depende do problema e não pode ser considerada universal.

O quarto contributo é a análise da composição por eras.

Esta análise permite diagnosticar situações em que a capacidade preditiva aparente depende da composição histórica dos períodos de treino e de teste.

O quinto contributo é a validação entre algoritmos.

Esta comparação mostra que famílias de variáveis importantes, como a água no solo durante a vindima e os dias de stress térmico, não são apenas artefactos de um único método.

O resultado preditivo é negativo. As contribuições metodológicas não o são.

---

## Slide 23 — Limitações e trabalho futuro (~1 min)

É importante apresentar de forma transparente as limitações do trabalho, porque essa transparência faz também parte da contribuição científica.

Destaco três limitações principais.

A primeira é que o tempo é a única covariável utilizada no processo de *detrending*.

Como consequência, a mudança estrutural da produção e os efeitos climáticos permanecem parcialmente misturados. Esta é a origem da tensão identificada ao longo da tese.

A segunda limitação é a dimensão reduzida dos conjuntos de teste.

Foram considerados 18 cenários no Douro e 16 na Região dos Vinhos Verdes.

O único resultado preditivo encorajador — uma janela equilibrada entre eras com cinco resultados positivos em cinco cenários no Douro — é exploratório, uma vez que apresenta apenas cinco observações. Esta limitação é explicitamente reconhecida ao longo do trabalho.

A terceira limitação é a utilização de agregados anuais ao nível de uma única região.

Desta forma, não existe resolução espacial dentro da região nem resolução temporal ao longo da campanha vitícola.

O trabalho futuro decorre diretamente deste diagnóstico.

Será necessário incluir covariáveis estruturais, como a área plantada, as quotas de produção ou o número de produtores certificados, para separar melhor a evolução do setor dos efeitos climáticos.

Será também importante pré-registar e validar prospetivamente o critério de equilíbrio entre eras.

Finalmente, será relevante aplicar esta metodologia a outras séries regionais, especialmente séries com tendências estruturais menos acentuadas.

A conclusão honesta é que esta tese identifica concretamente o que precisa de ser resolvido antes de estas regras poderem ser utilizadas em previsões operacionais fiáveis.

---

## Slide 24 — Próximos passos (~45 s)

Retomo agora a pergunta apresentada no início e procuro respondê-la com a nuance necessária.

Consegue o Machine Learning baseado em regras prever a produção de vinho em Portugal?

Ainda não de forma fiável, pelo menos quando trabalhamos apenas com estas séries regionais anuais.

Mas consegue explicar parte da relação entre clima e produção de uma forma auditável e agronomicamente relevante?

Sim. E essa capacidade tem valor científico e potencial valor prático.

O próximo passo concreto consiste em modelar explicitamente a componente estrutural.

Isso implica incorporar covariáveis estruturais, aumentar a resolução espacial para captar a heterogeneidade dentro de cada região, testar regiões com tendências menos pronunciadas e realizar validação prospetiva.

Uma extensão metodológica natural seria também explorar *Exceptional Model Mining*.

Em vez de identificar apenas os subgrupos onde a distribuição marginal da produção é diferente, esta abordagem permitiria identificar os contextos em que a própria relação entre clima e produção apresenta um comportamento excecional.

---

## Slide 25 — Obrigado / Questões (~30 s)

Termino com a principal mensagem que gostaria de deixar.

Compreender porque uma previsão falha é, em si mesmo, uma contribuição científica, porque nos permite identificar com precisão que evidência está ainda em falta antes de estes métodos poderem ser utilizados de forma fiável.

Muito obrigado pela vossa atenção.

Terei todo o gosto em responder às vossas questões.

























---
## Slide 1 — Title (~35 s)

**EN:** "Good morning, and thank you. My dissertation asks a deceptively simple question about Portuguese wine: *what can climate explain about annual production — and what can it reliably predict?* Those are not the same question, and the gap between them is the story of this talk. The work matters because wine production is one of Europe's most climate-sensitive activities, and the people who act on a forecast — cooperatives, planners, regulators — cannot act on a black box; they need something they can read and challenge. So over the next twenty minutes I'll do three things: first, show why this forecasting task is genuinely hard; second, how I built a rule-based evaluation around an algorithm called CarenR; and third, what the results tell us about prediction, interpretation, and what evidence is still missing."

**PT:** 

Bom dia e obrigado pela vossa presença.

A minha dissertação parte de uma pergunta aparentemente simples sobre a produção de vinho em Portugal: o que consegue o clima explicar sobre a produção anual e, sobretudo, o que consegue prever de forma fiável?

Estas duas questões não são equivalentes, e a diferença entre explicar e prever constitui o fio condutor desta apresentação.

Este trabalho é relevante porque a produção de vinho é uma das atividades agrícolas mais sensíveis ao clima na Europa. Além disso, quem toma decisões com base numa previsão — cooperativas, entidades reguladoras ou responsáveis pelo planeamento — não pode depender apenas de uma black box. Precisa de compreender o resultado, questioná-lo e perceber as condições que estão por detrás da previsão.

Nos próximos vinte minutos vou abordar três pontos: primeiro, porque este problema de previsão é particularmente difícil; segundo, como construí uma avaliação baseada em regras em torno do algoritmo CarenR; e, por fim, o que os resultados nos dizem sobre previsão, interpretabilidade e sobre a evidência que ainda está em falta.---

## Slide 2 — Context: a decision question (~45 s)

**EN:** "Let me start with why anyone should care. Regional wine production is a decision variable: cooperatives plan harvest logistics around it, regulators set quotas, supply chains size their storage, and markets form price expectations. But that single annual number is driven by many things at once — climate, vineyard management, planted area, regulation, and slow structural change in the industry. And here is the crucial point: a model that outputs only a number is not enough for these decisions. If the forecast is wrong, or the region is structurally changing, the stakeholder needs to know *whether the model actually found a climate signal, or whether it just followed historical drift.* That distinction is impossible with a black box — and it's exactly where this thesis positions itself."

**PT:** "Deixem-me começar por porque isto importa. A produção vinícola regional é uma variável de decisão: as cooperativas planeiam a logística da vindima com base nela, os reguladores definem quotas, as cadeias de abastecimento dimensionam o armazenamento, e os mercados formam expectativas de preço. Mas esse único número anual é impulsionado por vários fatores em simultâneo — clima, gestão do vinhedo, área plantada, regulação, e mudança estrutural lenta na indústria. E eis o ponto crucial: um modelo que devolve apenas um número não chega para estas decisões. Se a previsão estiver errada, ou a região estiver a mudar estruturalmente, a parte interessada precisa de saber *se o modelo encontrou de facto um sinal climático, ou se apenas seguiu a deriva histórica.* Essa distinção é impossível com uma caixa preta — e é exatamente aí que esta tese se posiciona."

---

## Slide 3 — Research problem: auditable forecasting (~1 min)

**EN:** "So the problem is not only forecasting — it's *auditable* forecasting. On one side, modern machine learning predicts wine yield reasonably well, but it doesn't produce anything an agronomist can read; a SHAP value is not a rule you can argue with. On the other side, classical statistics is interpretable but can miss the local, non-linear climate conditions that actually matter. CarenR sits deliberately in the middle: it searches for subgroups — defined by explicit rules — where the production distribution differs meaningfully from the overall baseline, and every rule it returns carries its conditions, its support, and a statistical p-value. Look at the example on the slide: *if harvest soil water is moderate and post-maturity wet days are low, then production runs about 192 thousand hectolitres above trend, with 21% support and p below 0.01.* That is the kind of output the thesis is built to produce — not 'wetness matters,' but a concrete, testable combination of phenological windows and thresholds. And no prior work combines all five properties: interpretable, long-term, predictive, continuous target, applied to a wine region."

**PT:** "Portanto o problema não é só previsão — é previsão *auditável*. De um lado, o machine learning moderno prevê o rendimento vinícola razoavelmente bem, mas não produz nada que um agrónomo consiga ler; um valor SHAP não é uma regra que se possa contestar. Do outro, a estatística clássica é interpretável mas pode falhar as condições climáticas locais e não-lineares que realmente importam. O CarenR fica deliberadamente no meio: procura subgrupos — definidos por regras explícitas — onde a distribuição da produção difere significativamente da baseline global, e cada regra que devolve traz as suas condições, o seu suporte e um p-value estatístico. Vejam o exemplo no slide: *se a água no solo na vindima for moderada e os dias húmidos pós-maturação forem poucos, então a produção fica cerca de 192 mil hectolitros acima da tendência, com 21% de suporte e p abaixo de 0,01.* É este o tipo de resultado que a tese foi construída para produzir — não 'a humidade importa', mas uma combinação concreta e testável de janelas fenológicas e limiares. E nenhum trabalho anterior combina as cinco propriedades: interpretável, de longo prazo, preditivo, alvo contínuo, aplicado a uma região vinícola."

---

## Slide 4 — Why it's hard: real but diluted signal (~45 s)

**EN:** "Why is this hard? Because the target folds four very different things into one annual regional number. The biology spans years — next year's potential is partly set this year, in the buds and the reserves. The aggregation hides variation — thousands of vineyards with different soils, altitudes, and microclimates collapse into one figure. The structure changes over time — policy, planted area, and market conditions move the production baseline independently of weather. And the sample is small — only 80 to 89 annual observations to support 59 engineered features. Put together, this makes the signal-to-noise problem severe. So my central caution, which I'll return to: a weak forecasting result does *not* automatically mean there's no climate signal. It may mean the signal is real but diluted — or confounded by the production series itself."

**PT:** "Porque é difícil? Porque o alvo dobra quatro coisas muito diferentes num único número regional anual. A biologia estende-se por anos — o potencial do próximo ano é em parte definido este ano, nos gomos e nas reservas. A agregação esconde variação — milhares de vinhedos com solos, altitudes e microclimas diferentes colapsam num só valor. A estrutura muda ao longo do tempo — política, área plantada e condições de mercado deslocam a baseline de produção independentemente do tempo meteorológico. E a amostra é pequena — apenas 80 a 89 observações anuais para sustentar 59 características construídas. Juntando tudo, o problema de relação sinal-ruído torna-se severo. Por isso a minha cautela central, à qual voltarei: um resultado de previsão fraco *não* significa automaticamente que não há sinal climático. Pode significar que o sinal é real mas diluído — ou confundido pela própria série de produção."

---

## Slide 5 — Natural experiment: two histories (~40 s)

**EN:** "Now the design idea that holds the whole thesis together. I don't study one region — I study two, chosen because they run the same pipeline into two opposite histories. The Douro has a long, gradual rise of about +80% over 89 years, with relatively low year-to-year variability, a coefficient of variation around 27%. Vinho Verde has a sharp structural decline over 80 years and much higher variability, a CV near 47%. Same algorithm, same features, same validation — opposite non-stationarity. This is what turns two case studies into a controlled natural experiment: when the outcomes diverge later, I can attribute the difference to structural history rather than to arbitrary modelling choices."

**PT:** "Agora a ideia de desenho que sustenta toda a tese. Não estudo uma região — estudo duas, escolhidas porque correm o mesmo pipeline em duas histórias opostas. O Douro tem uma subida longa e gradual de cerca de +80% ao longo de 89 anos, com variabilidade interanual relativamente baixa, um coeficiente de variação à volta de 27%. O Vinho Verde tem um declínio estrutural acentuado ao longo de 80 anos e variabilidade muito maior, um CV perto de 47%. O mesmo algoritmo, as mesmas características, a mesma validação — não-estacionaridade oposta. É isto que transforma dois estudos de caso numa experiência natural controlada: quando os resultados divergirem mais à frente, posso atribuir a diferença à história estrutural e não a escolhas de modelação arbitrárias."

---

## Slide 6 — Grapevine cycle: two-year memory (~50 s)

**EN:** "Before the method, one piece of biology that shapes everything. The vine's response is seasonal *and* delayed. The current harvest is realised in year one — flowering, fruit set, veraison, harvest — but its ceiling was partly set in the previous year, year zero, when temperature and light fixed the number of bunches and prior water and thermal stress built the carbohydrate reserves. That carry-over is why I don't use generic annual averages. Every feature is anchored to a phenological stage, in both the current and the previous year. And that anchoring pays off twice: it respects the biology, and it makes the rules interpretable, because a condition like 'wet days after flowering' points to an agronomically meaningful window, not an arbitrary calendar month."

**PT:** "Antes do método, uma peça de biologia que molda tudo. A resposta da vinha é sazonal *e* desfasada. A colheita corrente realiza-se no ano um — floração, vingamento, pintor, vindima — mas o seu limite foi em parte definido no ano anterior, o ano zero, quando a temperatura e a luz fixaram o número de cachos e o stress hídrico e térmico anterior construiu as reservas de hidratos de carbono. Esse carry-over é a razão por que não uso médias anuais genéricas. Cada característica está ancorada a um estado fenológico, no ano corrente e no anterior. E essa ancoragem compensa duplamente: respeita a biologia, e torna as regras interpretáveis, porque uma condição como 'dias húmidos após a floração' aponta para uma janela agronomicamente significativa, não um mês de calendário arbitrário."

---

## Slide 7 — Feature engineering: biology, not calendar (~40 s)

**EN:** "This slide is the engineering behind that idea. I start from roughly thirty thousand daily meteorological records per region and transform them into 59 annual predictors. The transformation is anchored on four phenological stages — budburst, flowering, veraison, harvest — and computed for both the current and previous year. The exact record count isn't the point; the point is the *move* from raw daily weather and vineyard-state data into annual features that carry biological memory and align with the vine's calendar. That is what lets a rule condition mean something an expert recognises."

**PT:** "Este slide é a engenharia por detrás dessa ideia. Parto de cerca de trinta mil registos meteorológicos diários por região e transformo-os em 59 preditores anuais. A transformação está ancorada em quatro estados fenológicos — abrolhamento, floração, pintor, vindima — e calculada para o ano corrente e o anterior. A contagem exata de registos não é o essencial; o essencial é a *passagem* de dados diários brutos de meteorologia e estado do vinhedo para características anuais que carregam memória biológica e se alinham com o calendário da vinha. É isso que faz com que uma condição de regra signifique algo que um perito reconhece."

---

## Slide 8 — Positioning: between explanation and prediction (~30 s)

**EN:** "Let me be honest about scope. This thesis is not a contest against every black-box model, and it doesn't claim to beat deep learning on accuracy. It targets a specific missing combination in the literature: a continuous target, long non-stationary data, interpretable rule output, *and* rigorous predictive validation — all four together. Modern ML gives accuracy without transparency; rule discovery gives transparency without proven predictive value here. The question I ask is whether we can have both — and, just as importantly, what happens when we can't."

**PT:** "Deixem-me ser honesto quanto ao âmbito. Esta tese não é uma disputa contra todos os modelos caixa-preta, e não afirma bater o deep learning na exatidão. Visa uma combinação específica em falta na literatura: alvo contínuo, dados longos não-estacionários, saída de regras interpretável, *e* validação preditiva rigorosa — as quatro em conjunto. O ML moderno dá exatidão sem transparência; a descoberta de regras dá transparência sem valor preditivo comprovado aqui. A pergunta que faço é se podemos ter ambos — e, tão importante quanto, o que acontece quando não podemos."

---

## Slide 9 — Research design: two goals (~40 s)

**EN:** "The thesis is built on two goals with two different standards of evidence. Goal 1, Discovery: can the algorithms find meaningful historical structure — which agroclimatic conditions distinguish above- and below-trend years, and are those genuine within-era signals or era artefacts? Goal 2, Validation: does that structure actually reduce error in genuine future periods, and under what conditions does it succeed or fail? The same data and the same rule family serve both — but the bar is different. Discovery only needs an association to exist; prediction needs it to beat a baseline out of sample. My headline result, which I'll build to: Goal 1 is achieved, Goal 2 is not in aggregate — and the *why* is the contribution."

**PT:** "A tese assenta em dois objetivos com dois padrões de evidência diferentes. Objetivo 1, Descoberta: conseguem os algoritmos encontrar estrutura histórica significativa — que condições agroclimáticas distinguem anos acima e abaixo da tendência, e são esses sinais genuínos dentro da era ou artefactos de era? Objetivo 2, Validação: essa estrutura reduz de facto o erro em períodos futuros genuínos, e sob que condições tem sucesso ou falha? Os mesmos dados e a mesma família de regras servem ambos — mas o nível de exigência é diferente. A descoberta só precisa que uma associação exista; a previsão precisa que ela bata uma baseline fora da amostra. O meu resultado de destaque, para o qual vou construindo: o Objetivo 1 é alcançado, o Objetivo 2 não o é no agregado — e o *porquê* é a contribuição."

---

## Slide 10 — Study design: controlled comparison (~30 s)

**EN:** "This slide makes the experiment explicit. Held constant across both regions: the feature engineering, the detrending logic, the rule learners, and the evaluation metric. Allowed to differ: the climate regime, the structural trajectory, the noise level, and the era composition. Two regions, 89 plus 80 annual observations. Because the method is fixed, any divergence in results becomes interpretable evidence about data structure — which is precisely what elevates this from two case studies to a natural experiment."

**PT:** "Este slide torna a experiência explícita. Mantido constante nas duas regiões: a engenharia de características, a lógica de destendência, os aprendizes de regras e a métrica de avaliação. Deixado variar: o regime climático, a trajetória estrutural, o nível de ruído e a composição por era. Duas regiões, 89 mais 80 observações anuais. Como o método é fixo, qualquer divergência nos resultados torna-se evidência interpretável sobre a estrutura dos dados — que é precisamente o que eleva isto de dois estudos de caso a uma experiência natural."

---

## Slide 11 — Algorithms: CarenR + comparators (~1 min)

**EN:** "Five model families, but one primary lens. CarenR is the method that links the two halves of the thesis: it discovers interpretable distributional rules, and its variants are also tested as predictors. Mechanically it does four steps — discretise the climate features into equal-frequency bins; search level-wise, in the Apriori style, combining feature-bin conditions; compare each subgroup's residuals against the global residuals with a KS test; and retain the rules that are significant and have enough support. The others play defined roles: RIPPER and M5Rules are rule learners, so they let me cross-check whether the same feature families show up under completely different criteria, and they double as prediction comparators; the Decision Tree is a prediction-only, non-rule benchmark; and the naive median, last-value, and moving-average are prediction-only references. The key point for the defence: these algorithms are not all doing the same job — some serve interpretability, some serve forecasting comparison, and CarenR is the bridge."

**PT:** "Cinco famílias de modelos, mas uma lente principal. O CarenR é o método que liga as duas metades da tese: descobre regras distribucionais interpretáveis, e as suas variantes são também testadas como preditores. Mecanicamente faz quatro passos — discretizar as características climáticas em bins de frequência igual; pesquisar por níveis, ao estilo Apriori, combinando condições de característica-bin; comparar os resíduos de cada subgrupo com os resíduos globais através de um teste KS; e reter as regras que são significativas e têm suporte suficiente. Os outros têm papéis definidos: o RIPPER e o M5Rules são aprendizes de regras, portanto deixam-me verificar se as mesmas famílias de características aparecem sob critérios completamente diferentes, e servem também de comparadores de previsão; a Árvore de Decisão é uma referência só de previsão, não baseada em regras; e a mediana ingénua, o último valor e a média móvel são referências só de previsão. O ponto-chave para a defesa: estes algoritmos não fazem todos o mesmo trabalho — uns servem a interpretabilidade, outros a comparação de previsão, e o CarenR é a ponte."

---

## Slide 12 — Goal 1 question: the KS idea (~1 min)

**EN:** "Here's CarenR conceptually. The question it asks for every candidate rule is on the slide: *do the years matching these climate conditions have a production-residual distribution that differs from all years?* It discretises the features into intervals, combines them into a readable condition, and then compares two distributions. Look at the plot: the black curve is all years, the burgundy curve is only the years that match the rule. Crucially, CarenR is not just checking whether the means differ — the Kolmogorov–Smirnov statistic measures the maximum gap between the *whole* cumulative distributions, so it detects shifts in shape and in the tails, not only the average. And the little rule card on the right is what makes this auditable: an agronomist can challenge the conditions, check that the subgroup covers 28% of years, and inspect the p-value of 0.004. That is the interpretability advantage over any black-box explanation — and it sets up the next slide, where these rules become concrete findings."

**PT:** "Aqui está o CarenR conceptualmente. A pergunta que faz para cada regra candidata está no slide: *os anos que correspondem a estas condições climáticas têm uma distribuição de resíduos de produção diferente da de todos os anos?* Discretiza as características em intervalos, combina-as numa condição legível, e depois compara duas distribuições. Vejam o gráfico: a curva preta são todos os anos, a curva bordô são apenas os anos que correspondem à regra. Crucialmente, o CarenR não verifica só se as médias diferem — a estatística de Kolmogorov–Smirnov mede o desnível máximo entre as distribuições cumulativas *inteiras*, portanto deteta desvios na forma e nas caudas, não apenas na média. E o cartãozinho da regra à direita é o que torna isto auditável: um agrónomo pode contestar as condições, verificar que o subgrupo cobre 28% dos anos, e inspecionar o p-value de 0,004. É essa a vantagem de interpretabilidade sobre qualquer explicação de caixa preta — e prepara o próximo slide, onde estas regras se tornam achados concretos."

---

## Slide 13 — Discovery results (~40 s)

**EN:** "Goal 1 succeeds, and the rule sets are compact enough to read. For the Douro, 48 rules, and the single most consistent driver is harvest soil water — it appears in 54% of all rules, usually with dry post-flowering conditions and bounded heat associating with above-trend production. For Vinho Verde, 20 rules, and here the dominant features are heat-stress days — in up to half the rules — and harvest soil water at 25%; the leaf-area, or canopy, family appears in 4 of the 20. I'll flag one thing carefully: these are statistically validated associations with agronomic coherence, not controlled experiments, so I won't claim causality. And notice the headline — the *same* variable can point in opposite directions across regions, which is why interpretation has to stay context-specific."

**PT:** "O Objetivo 1 tem sucesso, e os conjuntos de regras são compactos o suficiente para ler. Para o Douro, 48 regras, e o impulsionador mais consistente é a água no solo na vindima — aparece em 54% de todas as regras, geralmente com condições secas pós-floração e calor limitado a associarem-se a produção acima da tendência. Para o Vinho Verde, 20 regras, e aqui as características dominantes são os dias de stress térmico — em até metade das regras — e a água no solo na vindima a 25%; a família da área foliar, ou copado, aparece em 4 das 20. Vou sublinhar uma coisa com cuidado: são associações estatisticamente validadas com coerência agronómica, não experiências controladas, portanto não afirmarei causalidade. E reparem no destaque — a *mesma* variável pode apontar em direções opostas entre regiões, e é por isso que a interpretação tem de permanecer específica do contexto."

---

## Slide 14 — Top rules: patterns (~1 min 20 s)

**EN:** "These are the strongest rules by significance. Start with the Douro on the left — four of the top five are above-trend, and they tell one coherent agronomic story: dry conditions around flowering and maturity, with bounded heat, define the good years. The strongest, rule four, combines moderate pre-harvest soil water with dry carry-over from the previous year and lands at +192 thousand hectolitres above trend. The one below-trend rule, high LAI plus wet flowering, is agronomically sensible: an over-vigorous canopy in a wet year pushes growth toward leaves rather than fruit. Now Vinho Verde on the right — the deviations are larger in absolute terms and four of five are below-trend, reflecting the Atlantic climate, where cool and moist conditions dominate the poor years; the rules are built mainly on heat-stress days and soil water. The takeaway is not that one listed rule should be deployed tomorrow — it's that water-related and heat-stress variables keep recurring as the meaningful explanatory families, and, as the note at the bottom says, they survive across all three algorithms."

**PT:** "Estas são as regras mais fortes por significância. Começar pelo Douro à esquerda — quatro das cinco de topo são acima da tendência, e contam uma história agronómica coerente: condições secas à volta da floração e da maturação, com calor limitado, definem os bons anos. A mais forte, a regra quatro, combina água no solo pré-vindima moderada com carry-over seco do ano anterior e fica em +192 mil hectolitros acima da tendência. A única regra abaixo da tendência, LAI alto mais floração húmida, faz sentido agronomicamente: um copado demasiado vigoroso num ano húmido empurra o crescimento para as folhas em vez do fruto. Agora o Vinho Verde à direita — os desvios são maiores em termos absolutos e quatro de cinco são abaixo da tendência, refletindo o clima atlântico, onde condições frescas e húmidas dominam os anos fracos; as regras assentam sobretudo em dias de stress térmico e água no solo. A conclusão não é que uma regra listada deva ser implementada amanhã — é que variáveis de água e de stress térmico se repetem como as famílias explicativas relevantes, e, como diz a nota em baixo, sobrevivem nos três algoritmos."

---

## Slide 15 — Exact thresholds: audit view (~1 min)

**EN:** "This is the audit version of the same rules, and it's the view that matters most to the business. The friendly labels from the previous slide are now the actual intervals CarenR reports. This is the whole point of interpretability: a rule is not a black-box importance score — it names the subgroup, tells you how many years match it, gives the KS p-value, and states the size of the residual shift. For the Douro, the strongest rules say that dry post-flowering and post-maturity windows — for example wet days between zero and about three — sometimes with moderate pre-harvest soil water between 74 and 101 millimetres, correspond to above-trend production. For Vinho Verde, four of five are the cool, moist, low-heat pattern, and the one above-trend rule is warm previous-year budburst with mild flowering. I want the committee to leave with this: these explicit intervals are exactly what let an agronomist or a regulator challenge the finding directly — you can argue with 74 to 101 millimetres in a way you can never argue with a feature-importance bar."

**PT:** "Esta é a versão de auditoria das mesmas regras, e é a visão que mais importa para o negócio. Os rótulos amigáveis do slide anterior são agora os intervalos reais que o CarenR reporta. É este o objetivo de toda a interpretabilidade: uma regra não é uma pontuação de importância de caixa preta — nomeia o subgrupo, diz quantos anos lhe correspondem, dá o p-value KS, e indica a dimensão do desvio de resíduo. Para o Douro, as regras mais fortes dizem que janelas secas pós-floração e pós-maturação — por exemplo dias húmidos entre zero e cerca de três — por vezes com água no solo pré-vindima moderada entre 74 e 101 milímetros, correspondem a produção acima da tendência. Para o Vinho Verde, quatro de cinco são o padrão fresco, húmido, de baixo calor, e a única acima da tendência é abrolhamento quente do ano anterior com floração amena. Quero que o júri saia com isto: estes intervalos explícitos são exatamente o que permite a um agrónomo ou regulador contestar o achado diretamente — pode-se discutir 74 a 101 milímetros de uma forma que nunca se poderia discutir com uma barra de importância de característica."

---

## Slide 16 — Cross-algorithm validation (~1 min)

**EN:** "A sceptic will ask: are these rules just artefacts of CarenR's particular search? This slide is my answer. CarenR, RIPPER and M5Rules optimise fundamentally different objectives — a distributional KS test, information gain for classification, and squared-error reduction for regression — so when they agree on a feature, that agreement is strong evidence. Two features are confirmed by all three: harvest soil water and the LAI, or canopy, family. I want to be precise about the LAI number, because it's easy to overstate: the LAI family appears in 4 of the 20 Vinho Verde rules — the cross-algorithm claim is that all three *methods select it*, not that it's frequent. And I'll be careful with the wording: this is not causal proof. It is convergent evidence that these feature families capture genuine structure in the data, rather than noise specific to one algorithm."

**PT:** "Um cético vai perguntar: serão estas regras apenas artefactos da pesquisa particular do CarenR? Este slide é a minha resposta. O CarenR, o RIPPER e o M5Rules otimizam objetivos fundamentalmente diferentes — um teste distribucional KS, ganho de informação para classificação, e redução do erro quadrático para regressão — portanto quando concordam numa característica, essa concordância é evidência forte. Duas características são confirmadas pelos três: a água no solo na vindima e a família LAI, ou copado. Quero ser preciso quanto ao número do LAI, porque é fácil exagerar: a família LAI aparece em 4 das 20 regras do Vinho Verde — a afirmação entre algoritmos é que os três *métodos a selecionam*, não que seja frequente. E terei cuidado com a formulação: isto não é prova causal. É evidência convergente de que estas famílias de características captam estrutura genuína nos dados, e não ruído específico de um algoritmo."

---

## Slide 17 — Validation: two checks (~45 s)

**EN:** "This slide answers the era-confound question — Research Question 2 — with nuance. The worry is that a feature which co-trends with production over decades looks informative under linear detrending even if there's no real within-era relationship. So I stress-test with LOESS, an aggressive detrend that strips out era-level structure. Two things happen. In the Douro, the signals persist — they're genuine within-era climate signal. In Vinho Verde, the wet-days signal persists, but LAI and harvest soil water collapse, which tells me they were partly entangled with structural change. That collapse is not a caveat to hide; it's a result, and it foreshadows why prediction fails in Vinho Verde. The second check is the cross-algorithm convergence from the previous slide. Together they say the discovery is credible — but, and this is the pivot of the talk, credibility is not yet predictive utility."

**PT:** "Este slide responde à questão do confundimento por era — a Questão de Investigação 2 — com nuance. A preocupação é que uma característica que co-tende com a produção ao longo de décadas pareça informativa sob destendência linear mesmo sem uma relação real dentro da era. Por isso faço um teste de esforço com LOESS, uma destendência agressiva que remove a estrutura ao nível da era. Acontecem duas coisas. No Douro, os sinais persistem — são sinal climático genuíno dentro da era. No Vinho Verde, o sinal de dias húmidos persiste, mas o LAI e a água no solo na vindima colapsam, o que me diz que estavam em parte enredados com a mudança estrutural. Esse colapso não é uma ressalva para esconder; é um resultado, e antecipa porque a previsão falha no Vinho Verde. A segunda verificação é a convergência entre algoritmos do slide anterior. Juntas, dizem que a descoberta é credível — mas, e este é o ponto de viragem da apresentação, a credibilidade ainda não é utilidade preditiva."

---

## Slide 18 — Prediction method: walk-forward (~40 s)

**EN:** "For Goal 2 I recreate genuine prediction-time conditions. This is an expanding-window walk-forward: 18 scenarios for the Douro, 16 for Vinho Verde. In every scenario the model is trained only on the past and evaluated on what comes next, and — this is the part that protects every number I'm about to show — all preprocessing happens inside each training fold: detrending, discretisation, feature selection, and rule induction, with no leakage from the test years. I compare 17 models on weighted mean absolute error, lower is better. And the primary comparator is deliberate: the training median, because the median is the optimal constant forecast under MAE. So beating it is exactly the claim that climate features carry signal you can exploit."

**PT:** "Para o Objetivo 2 recrio condições genuínas de tempo-de-previsão. É um walk-forward de janela expansível: 18 cenários para o Douro, 16 para o Vinho Verde. Em cada cenário o modelo é treinado apenas no passado e avaliado no que vem a seguir, e — esta é a parte que protege todos os números que vou mostrar — todo o pré-processamento acontece dentro de cada fold de treino: destendência, discretização, seleção de características e indução de regras, sem fuga dos anos de teste. Comparo 17 modelos por erro absoluto médio ponderado, menor é melhor. E o comparador primário é deliberado: a mediana de treino, porque a mediana é a previsão constante ótima sob MAE. Portanto batê-la é exatamente a afirmação de que as características climáticas carregam sinal que se pode explorar."

---

## Slide 19 — Prediction results (~45 s)

**EN:** "And here is the honest result — I won't dress it up. No rule-based model beats the median overall, in either region. In the Douro the best CarenR variant is 189 versus 184 thousand hectolitres, three percent worse, and a Wilcoxon test gives p equal to 0.468 — not significant. In Vinho Verde the gap is 22 percent, 172 versus 140, and worse still: among the nine scenarios where the RVV rules actually fired, they *significantly harmed* prediction, p equal to 0.004. So the negative result isn't just 'no improvement' — in the harder region, firing rules made things worse. This supports the central interpretation: rule discovery can reveal structure, but under strong structural decline that structure does not translate into reliable future forecasting."

**PT:** "E aqui está o resultado honesto — não o vou disfarçar. Nenhum modelo baseado em regras bate a mediana no total, em nenhuma das regiões. No Douro a melhor variante CarenR fica em 189 contra 184 mil hectolitros, três por cento pior, e um teste de Wilcoxon dá p igual a 0,468 — não significativo. No Vinho Verde a diferença é de 22 por cento, 172 contra 140, e pior ainda: nos nove cenários em que as regras do RVV de facto dispararam, *prejudicaram significativamente* a previsão, p igual a 0,004. Portanto o resultado negativo não é só 'sem melhoria' — na região mais difícil, disparar regras piorou as coisas. Isto sustenta a interpretação central: a descoberta de regras pode revelar estrutura, mas sob declínio estrutural forte essa estrutura não se traduz em previsão futura fiável."

---

## Slide 20 — Interpretation: real but too weak (~50 s)

**EN:** "Let me separate two ideas that are very easy to conflate, because this distinction is the heart of the thesis. Discovery asks: is the subgroup's distribution different? With an absolute correlation around 0.47, that association is clearly detectable across 89 years of data. Prediction asks a harder question: does the rule reduce *future* error? And there the arithmetic is unforgiving — climate explains about 22 percent of the residual variance, which leaves roughly 78 percent unobserved or noisy. Discovery only needs the signal to *exist*; prediction needs it to *dominate*. So the thesis result — discovery works, prediction largely doesn't — is not a failure. It's the main diagnosis: rule-based machine learning can genuinely explain part of the climate–production relationship, but these data are not sufficient for reliable regional forecasting. The next slide turns that diagnosis into a formal framework."

**PT:** "Deixem-me separar duas ideias muito fáceis de confundir, porque esta distinção é o coração da tese. A descoberta pergunta: a distribuição do subgrupo é diferente? Com uma correlação absoluta à volta de 0,47, essa associação é claramente detetável em 89 anos de dados. A previsão faz uma pergunta mais difícil: a regra reduz o erro *futuro*? E aí a aritmética é implacável — o clima explica cerca de 22 por cento da variância dos resíduos, o que deixa cerca de 78 por cento não-observado ou ruído. A descoberta só precisa que o sinal *exista*; a previsão precisa que ele *domine*. Portanto o resultado da tese — a descoberta funciona, a previsão em larga medida não — não é um fracasso. É o diagnóstico principal: o machine learning baseado em regras consegue genuinamente explicar parte da relação clima–produção, mas estes dados não chegam para previsão regional fiável. O próximo slide transforma esse diagnóstico num arcabouço formal."

---

## Slide 21 — Two-decision framework (~1 min)

**EN:** "This is the methodological contribution I'd most like you to remember. Any forecast on a trending series hides two decisions. Decision 1 is how you model the structural trend — a better trend leaves smaller residuals and lowers the naive error floor. Decision 2 is how you extract the climate signal from what's left — and stable rules need residual structure that is large enough to detect. The problem is that these two decisions are in tension, and I can prove it. In Vinho Verde, switching to a flexible LOESS detrend improves the baseline from 140 down to about 90 — a much better Decision 1 — but it simultaneously drives rule induction to zero; every rule disappears. Sharpening the trend consumed the very signal the rules needed. And the reason is structural: time is the only detrending covariate, so it cannot separate industry structure from climate. This framework transfers to any agri-climatic forecasting problem with long histories and structural non-stationarity — the same tension between trend and signal."

**PT:** "Esta é a contribuição metodológica que mais gostaria que recordassem. Qualquer previsão numa série com tendência esconde duas decisões. A Decisão 1 é como modelas a tendência estrutural — uma tendência melhor deixa resíduos menores e baixa o piso de erro ingénuo. A Decisão 2 é como extrais o sinal climático do que resta — e regras estáveis precisam de estrutura residual grande o suficiente para detetar. O problema é que estas duas decisões estão em tensão, e consigo prová-lo. No Vinho Verde, mudar para uma destendência LOESS flexível melhora a baseline de 140 para cerca de 90 — uma Decisão 1 muito melhor — mas ao mesmo tempo leva a indução de regras a zero; todas as regras desaparecem. Afiar a tendência consumiu o próprio sinal de que as regras precisavam. E a razão é estrutural: o tempo é a única covariável de destendência, portanto não consegue separar a estrutura da indústria do clima. Este arcabouço transfere-se para qualquer problema de previsão agroclimática com histórias longas e não-estacionaridade estrutural — a mesma tensão entre tendência e sinal."

---

## Slide 22 — Contributions (~1 min)

**EN:** "So what remains, scientifically, after a negative forecasting result? Five things. First and most fundamental, the two-decision decomposition — a diagnostic that separates trend modelling from climate-signal extraction and explains *why* rule-based prediction can fail even when real signal exists. Second, a rigorous walk-forward evaluation, 17 models across 18 and 16 scenarios, with all preprocessing inside each fold. Third, a systematic sensitivity analysis across 11 CarenR variants, which shows the best feature-selection strategy is problem-specific rather than universal. Fourth, era-composition analysis, which diagnoses when apparent skill depends on the historical composition of the training and test periods. And fifth, cross-algorithm validation, showing that the key feature families — harvest soil water and heat-stress days — are not artefacts of one algorithm. The forecasting number is negative; the methodological contributions are not."

**PT:** "Então o que permanece, cientificamente, após um resultado negativo de previsão? Cinco coisas. Primeira e mais fundamental, a decomposição em duas decisões — um diagnóstico que separa a modelação da tendência da extração do sinal climático e explica *porque* a previsão baseada em regras pode falhar mesmo quando existe sinal real. Segunda, uma avaliação walk-forward rigorosa, 17 modelos em 18 e 16 cenários, com todo o pré-processamento dentro de cada fold. Terceira, uma análise de sensibilidade sistemática de 11 variantes CarenR, que mostra que a melhor estratégia de seleção de características é específica do problema e não universal. Quarta, a análise de composição por era, que diagnostica quando a capacidade aparente depende da composição histórica dos períodos de treino e teste. E quinta, a validação entre algoritmos, que mostra que as famílias-chave — água no solo na vindima e dias de stress térmico — não são artefactos de um algoritmo. O número de previsão é negativo; as contribuições metodológicas não são."

---

## Slide 23 — Limitations & future work (~1 min)

**EN:** "I want to be transparent about the limits, because being honest about them is part of the contribution. Three main ones. First, time is the only detrending covariate, so structural change and climate remain partly mixed — that is the root cause of the whole tension. Second, the test sets are small: 18 and 16 scenarios, and the one encouraging predictive result — the five-out-of-five era-balanced window in the Douro — is exploratory, n equals five, and I label it as such throughout. Third, I work with single-region annual aggregates, so there's no spatial or within-season resolution. The future-work path follows directly from the diagnosis: add structural covariates — planted area, quotas, certified producers — to separate structure from climate; pre-register and prospectively validate that era-balanced threshold; and test other regional series, especially ones with weaker trends. The honest closing point is that the thesis identifies exactly what must be fixed before these rules could become reliable operational forecasts."

**PT:** "Quero ser transparente quanto aos limites, porque ser honesto sobre eles faz parte da contribuição. Três principais. Primeiro, o tempo é a única covariável de destendência, portanto a mudança estrutural e o clima permanecem em parte misturados — essa é a causa raiz de toda a tensão. Segundo, os conjuntos de teste são pequenos: 18 e 16 cenários, e o único resultado preditivo encorajador — a janela equilibrada por era de cinco em cinco no Douro — é exploratório, n igual a cinco, e rotulo-o como tal ao longo de todo o texto. Terceiro, trabalho com agregados regionais anuais, portanto não há resolução espacial nem intra-estação. O caminho de trabalho futuro decorre diretamente do diagnóstico: acrescentar covariáveis estruturais — área plantada, quotas, produtores certificados — para separar a estrutura do clima; pré-registar e validar prospetivamente esse limiar equilibrado por era; e testar outras séries regionais, especialmente com tendências mais fracas. O ponto de fecho honesto é que a tese identifica exatamente o que tem de ser corrigido antes de estas regras poderem tornar-se previsões operacionais fiáveis."

---

## Slide 24 — Next steps (~45 s)

**EN:** "Let me bring it back to the opening question, and I'll answer it deliberately, with nuance. Can rule-based machine learning forecast Portuguese wine production? Not yet, not reliably, from these annual regional series. But can it *explain* the climate–production relationship in a way that is auditable and agronomically meaningful? Yes — and that is worth having. The concrete next step is to model structure explicitly: bring in structural covariates, raise the spatial resolution to capture within-region heterogeneity, test regions with weaker trends, and validate prospectively. And a natural methodological extension is Exceptional Model Mining, to find where the climate–production *relationship* itself is exceptional, not just where the marginal distribution shifts."

**PT:** "Deixem-me trazer de volta a pergunta inicial, e vou respondê-la deliberadamente, com nuance. Consegue o machine learning baseado em regras prever a produção vinícola portuguesa? Ainda não, não de forma fiável, a partir destas séries regionais anuais. Mas consegue *explicar* a relação clima–produção de forma auditável e agronomicamente significativa? Sim — e isso vale a pena ter. O próximo passo concreto é modelar a estrutura explicitamente: trazer covariáveis estruturais, aumentar a resolução espacial para captar a heterogeneidade dentro da região, testar regiões com tendências mais fracas, e validar prospetivamente. E uma extensão metodológica natural é o Exceptional Model Mining, para encontrar onde a própria *relação* clima–produção é excecional, não apenas onde a distribuição marginal se desloca."

---

## Slide 25 — Thank you / Questions (~30 s)

**EN:** "I'll close on the one sentence I most want to leave with you: understanding *why* a forecast fails is itself a scientific contribution, because it tells us precisely what evidence is missing before these methods can be deployed reliably. Thank you for your attention — I'd be glad to take your questions."

**PT:** "Fecho com a única frase que mais quero deixar-vos: compreender *porque* uma previsão falha é em si uma contribuição científica, porque nos diz precisamente que evidência falta antes de estes métodos poderem ser implementados de forma fiável. Obrigado pela vossa atenção — terei todo o gosto em responder às vossas perguntas."
