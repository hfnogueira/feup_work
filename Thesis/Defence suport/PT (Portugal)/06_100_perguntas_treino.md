# 100 Perguntas Mais Prováveis da Defesa

**Tese:** Previsão da Produção Vinícola com Aprendizagem Automática Baseada em Regras — Uma Abordagem de Descoberta de Subgrupos Distribucional
**Autor:** Hugo Filipe Queiróz Nogueira · MECD, FEUP
**Formato:** Perguntas antecipadas do júri, por tema. Cada entrada: *porque o júri a faz* · *que conhecimento testa*. **Sem resposta por opção** — usa como folha de treino.

> **Nota:** Algumas perguntas (ex. Q50, Q58) referem "beam search"; face à errata, o CAREN faz pesquisa por níveis ao estilo Apriori. Mantidas tal como estão porque o júri lê a versão submetida e pode perguntar exatamente assim — mas responde com a descrição correta (ver `01_preparacao_arguente_azevedo.md`, Parte 4).

---

## Introdução

1. **Numa frase, qual é a contribuição científica desta tese?**
   - *Porquê:* Testa se consegues separar contribuição de atividade.
   - *Testa:* Capacidade de articular uma afirmação ao nível de tese, não um resumo.

2. **Esta tese é sobretudo uma contribuição de método ou de aplicação?**
   - *Porquê:* Posiciona toda a defesa; obriga-te a comprometer.
   - *Testa:* Autoconsciência de onde vive de facto a novidade.

3. **O título diz "previsão", mas a previsão falha. O título é honesto?**
   - *Porquê:* Sonda excesso de reivindicação no enquadramento.
   - *Testa:* Honestidade intelectual; capacidade de reformular um resultado nulo.

4. **Quem é o beneficiário pretendido — cientistas de dados ou viticultores?**
   - *Porquê:* A interpretabilidade justifica-se pela audiência; verificam se se sustenta.
   - *Testa:* Clareza de propósito e raciocínio sobre as partes interessadas.

5. **Porquê duas regiões em vez de uma região estudada em profundidade?**
   - *Porquê:* O desenho de duas regiões é a tua escolha mais forte; verificam se sabes porquê.
   - *Testa:* Compreensão da comparação controlada vs. estudo de caso.

---

## Motivação

6. **Porque precisamos de regras interpretáveis se os modelos caixa-preta preveem melhor?**
   - *Porquê:* A justificação central de toda a abordagem.
   - *Testa:* Raciocínio sobre o compromisso exatidão–interpretabilidade.

7. **Nunca quantificas o custo em exatidão da interpretabilidade. Como sabes que vale a pena pagá-lo?**
   - *Porquê:* Afirmas que a interpretabilidade importa mas nunca mediste o teto.
   - *Testa:* Consciência da referência caixa-preta em falta.

8. **A produção regional anual é de facto uma quantidade relevante para decisões dos produtores?**
   - *Porquê:* Desafia a motivação prática.
   - *Testa:* Fundamentação de domínio da formulação do problema.

9. **O SHAP dá explicações post-hoc. Porque é insuficiente para o teu caso de uso?**
   - *Porquê:* Testa a distinção que traças entre explicações auditáveis e aproximadas.
   - *Testa:* Compreensão dos métodos de explicabilidade.

10. **Que decisão real mudaria se um produtor tivesse as tuas regras?**
    - *Porquê:* Força uma afirmação concreta de impacto.
    - *Testa:* Tradução da saída estatística em ação.

11. **Porque é este um problema digno de Mestrado e não um exercício de engenharia?**
    - *Porquê:* Estabelece o peso científico.
    - *Testa:* Capacidade de defender a profundidade da questão de investigação.

---

## Produção vinícola

12. **Porquê modelar a produção total em vez do rendimento por hectare?**
    - *Porquê:* A pergunta de domínio mais perigosa; a área é um confundimento estrutural.
    - *Testa:* Se percebes que produção = área × rendimento/ha.

13. **A quota de benefício limita administrativamente a produção de Porto. Como afeta isso o teu alvo?**
    - *Porquê:* Uma covariável não observada desliga a produção do clima.
    - *Testa:* Consciência de impulsionadores não climáticos que não consegues ver.

14. **Explica o ciclo bianual da vinha e porque justifica as tuas características y0.**
    - *Porquê:* A agronomia central por detrás da tua engenharia de características.
    - *Testa:* Fisiologia da videira (primórdios, reservas).

15. **Que mecanismo agronómico torna o défice moderado de água no solo *benéfico* no Douro?**
    - *Porquê:* A tua regra de topo do RDD depende disso.
    - *Testa:* Fisiologia da concentração vs. diluição do bago.

16. **Porque é que a mesma característica de água no solo inverte de direção entre regiões?**
    - *Porquê:* O teu ponto de destaque de validação do método.
    - *Testa:* Compreensão da dependência do contexto climático.

17. **Quanto do declínio do Vinho Verde é clima vs. política da UE?**
    - *Porquê:* O confundimento do RVV é central para a tua análise de falha.
    - *Testa:* Domínio dos impulsionadores estruturais (arranque, latada→cordão).

18. **As datas fenológicas STICS e a água no solo VSIM são medições ou saídas de modelo?**
    - *Porquê:* Duas das tuas variáveis-chave são modeladas, não observadas.
    - *Testa:* Honestidade quanto à proveniência dos dados e propagação de erro.

---

## Clima

19. **Que variáveis climáticas dominam a produção do Douro, e segundo o trabalho de quem?**
    - *Porquê:* Fundamenta as tuas características na literatura (Cunha).
    - *Testa:* Conhecimento dos impulsionadores temperatura de primavera / água no solo.

20. **Como separas uma tendência de aquecimento do sinal climático dentro da era?**
    - *Porquê:* É o problema do confundimento por era em miniatura.
    - *Testa:* Compreensão do diagnóstico de retenção LOESS.

21. **A temperatura na vindima mostra inflação de era. Porque é isso um alerta?**
    - *Porquê:* Apenas 56% de retenção LOESS — um artefacto de cotendência.
    - *Testa:* Capacidade de distinguir sinal real de cotendência suave.

22. **Porquê janelas ancoradas na fenologia em vez de meses de calendário fixos?**
    - *Porquê:* Justifica a escolha de construção das características.
    - *Testa:* Compreensão de que a fenologia se desloca 2–4 semanas por ano.

23. **As alterações climáticas ameaçam a estacionaridade que as tuas regras assumem?**
    - *Porquê:* As regras olham para trás; o clima futuro afasta-se da história.
    - *Testa:* Consciência da não-estacionaridade sob aquecimento.

24. **Poderia um único evento meteorológico extremo dominar o resíduo de um ano e quebrar uma regra?**
    - *Porquê:* Os agregados regionais escondem choques dentro da estação.
    - *Testa:* Compreensão das fontes de variância inexplicada.

---

## Estatística

25. **O resultado p = 0,031 é post-hoc com n = 5. Defende-o ou retira-o.**
    - *Porquê:* HARKing mais amostra minúscula.
    - *Testa:* Disciplina confirmatório vs. exploratório.

26. **Não aplicaste correção para comparações múltiplas nos 17 modelos. Justifica.**
    - *Porquê:* Inflação do erro familiar.
    - *Testa:* Consciência de multiplicidade.

27. **Os teus 18 cenários walk-forward partilham a maioria dos anos de teste. O teste de Wilcoxon é válido?**
    - *Porquê:* Conjuntos de teste sobrepostos violam a independência.
    - *Testa:* Compreensão dos pressupostos do teste e da autocorrelação.

28. **Mineras milhares de subgrupos a KS p ≤ 0,10. Quantas das tuas 48 regras são falsos positivos?**
    - *Porquê:* Multiplicidade do espaço de pesquisa dentro da mineração de regras.
    - *Testa:* Se percebes o problema da distribuição nula da pesquisa de subgrupos.

29. **Porquê KS p ≤ 0,10 em vez de 0,05?**
    - *Porquê:* Um limiar permissivo sob multiplicidade pesada.
    - *Testa:* Justificação do nível de significância.

30. **Explica a estatística KS e porque se adequa a n = 80.**
    - *Porquê:* Fundamental para o CarenR.
    - *Testa:* Conhecimento da comparação não-paramétrica de ECDF.

31. **Porquê MAE, quando a baseline mediana é por construção o minimizador do MAE?**
    - *Porquê:* Escolheste a perda sob a qual a tua baseline é ótima.
    - *Testa:* Raciocínio de seleção de métrica; o RMSE mudaria a história?

32. **Qual é a potência estatística de um teste sobre 5–18 cenários?**
    - *Porquê:* Desenho sem potência.
    - *Testa:* Literacia em análise de potência.

33. **O ADF e o KPSS discordam para os resíduos do RVV. O que te diz isso?**
    - *Porquê:* O veredito misto de estacionaridade é diagnóstico.
    - *Testa:* Compreensão de raiz unitária vs. estacionaridade de tendência.

34. **Porquê o Wilcoxon de postos com sinais e não um teste t emparelhado?**
    - *Porquê:* Pressupostos distribucionais em amostras pequenas.
    - *Testa:* Fundamentação da seleção do teste.

---

## Aprendizagem Automática

35. **Porque não incluíste uma baseline de regressão penalizada (LASSO/elastic net)?**
    - *Porquê:* Falta o concorrente interpretável óbvio.
    - *Testa:* Amplitude de conhecimento de baselines de ML.

36. **Onde está um modelo clássico de séries temporais (ARIMA/ARIMAX)?**
    - *Porquê:* Uma tese de previsão sem ARIMA é conspícua.
    - *Testa:* Fundamentos de previsão de séries temporais.

37. **Qual é o teto preditivo? O que alcançariam Random Forest ou gradient boosting nos *teus* dados?**
    - *Porquê:* Citas RF de outros mas nunca o correste.
    - *Testa:* Disposição para estabelecer um limite superior.

38. **Como evitas a fuga de dados no pipeline walk-forward?**
    - *Porquê:* A credibilidade de cada número preditivo depende disso.
    - *Testa:* Compreensão do pré-processamento dentro do fold.

39. **A tua razão características/observações é 59:80. Não é um problema de maldição da dimensionalidade?**
    - *Porquê:* O risco de sobreajuste é severo.
    - *Testa:* Raciocínio sobre dimensionalidade e tamanho de amostra.

40. **Como lidas com a colinearidade entre características quase duplicadas?**
    - *Porquê:* As características combinatórias são altamente correlacionadas.
    - *Testa:* Higiene do espaço de características.

41. **A seleção de características é estável entre folds? São escolhidas as mesmas características de cada vez?**
    - *Porquê:* Seleção instável mina a interpretabilidade.
    - *Testa:* Se mediste a estabilidade de seleção (não a reportaste).

42. **A Árvore de Decisão tem o pior desempenho. Isso significa que a estrutura em árvore é errada para estes dados, ou que a árvore foi mal afinada?**
    - *Porquê:* Distingue falha do modelo de falha de configuração.
    - *Testa:* Compreensão dos efeitos dos hiperparâmetros.

---

## Aprendizagem de Regras

43. **Define descoberta de subgrupos e como difere da classificação.**
    - *Porquê:* Vocabulário fundamental.
    - *Testa:* Indução descritiva vs. preditiva.

44. **Porque são os métodos baseados em regras "interpretáveis" de um modo que o SHAP não é?**
    - *Porquê:* A justificação central da família.
    - *Testa:* Raciocínio regra-explícita vs. atribuição.

45. **Qual é a diferença entre regras de associação e regras de distribuição?**
    - *Porquê:* O CarenR assenta em regras de distribuição.
    - *Testa:* Compreensão da preservação da distribuição-alvo completa.

46. **As regras sobrepõem-se 65% par a par. O teu conjunto de regras não é redundante?**
    - *Porquê:* Desafia a afirmação de coerência.
    - *Testa:* Compreensão do desenho de subgrupos sobrepostos.

47. **Como defines o limiar de suporte mínimo, e porquê 20%?**
    - *Porquê:* Uma escolha de desenho experimental, não algorítmica.
    - *Testa:* Raciocínio do compromisso suporte/robustez.

48. **Porque são aprendizes mais antigos (RIPPER 1995) os teus comparadores e não modernos (RuleFit, CORELS, Bayesian Rule Lists)?**
    - *Porquê:* O teu conjunto de comparadores tende para o antigo.
    - *Testa:* Consciência do panorama atual de ML interpretável.

49. **Como convertes regras descritivas numa previsão pontual?**
    - *Porquê:* A ponte descoberta→previsão.
    - *Testa:* Compreensão do mecanismo de agregação.

---

## CarenR

50. **Guia-me pelo pipeline CarenR de ponta a ponta.**
    - *Porquê:* Estabelece o domínio do teu método principal.
    - *Testa:* Discretização → pesquisa (por níveis) → KS → suporte → agregação.

51. **Que partes da Figura 3.3 são o algoritmo CarenR e quais são as tuas escolhas de desenho da tese?**
    - *Porquê:* Tens de assumir o que acrescentaste vs. herdaste.
    - *Testa:* A fronteira entre arcabouço e contribuição.

52. **Porquê discretização em quartis de frequência igual em vez de largura igual ou bins supervisionados?**
    - *Porquê:* A discretização determina as condições das regras.
    - *Testa:* Fundamentação da fiabilidade do KS para bins equilibrados.

53. **O CarenR_Supervised foi a tua pior variante. Porque falha a discretização supervisionada?**
    - *Porquê:* Um resultado negativo limpo sobre fuga do alvo.
    - *Testa:* Compreensão da fuga na engenharia de características.

54. **A agregação por mediana ponderada por suporte é ad hoc. Porque não um combinador aprendido?**
    - *Porquê:* A regra de previsão é heurística.
    - *Testa:* Justificação da fórmula de agregação.

55. **Porque vence o η² no RDD mas o Sup_FS no RVV?**
    - *Porquê:* A inversão de ranking é um resultado-chave.
    - *Testa:* Compreensão de força-do-sinal vs. conservadorismo-da-seleção.

56. **O recurso faz o CarenR cair na mediana. Isso não garante que não pode bater a baseline?**
    - *Porquê:* Sonda o teto imposto pelo teu próprio desenho.
    - *Testa:* Compreensão de que o valor só pode vir de regras disparadas.

57. **Um modelo que se abstém 44% das vezes e prejudica 56% das vezes no RVV é de facto útil?**
    - *Porquê:* Desafia o enquadramento recurso-como-virtude.
    - *Testa:* Avaliação honesta da utilidade líquida.

58. **A beam search é gananciosa. Poderia perder os melhores subgrupos?**
    - *Porquê:* Completude da pesquisa. *(Nota: o CAREN é por níveis/exaustivo — corrige a premissa.)*
    - *Testa:* Compreensão das limitações da pesquisa heurística.

59. **O trabalho CarenR anterior foi apenas dentro da amostra. O que é exatamente novo no teu uso dele?**
    - *Porquê:* Fixa a tua contribuição incremental.
    - *Testa:* Posicionamento face a Moreira 2022 / Jorge 2006.

---

## RIPPER

60. **Porque é o RIPPER um comparador justo quando precisa de um alvo discretizado?**
    - *Porquê:* A discretização em tercis perde informação.
    - *Testa:* Justiça da comparação entre métodos.

61. **Explica o ganho FOIL e a poda MDL.**
    - *Porquê:* Mecânica central do RIPPER.
    - *Testa:* Crescimento de regras e controlo de sobreajuste.

62. **A regra por omissão do RIPPER cobre a maioria dos anos. O que te diz esse modo de falha?**
    - *Porquê:* A regra por omissão domina em dados pequenos.
    - *Testa:* Compreensão do desequilíbrio de classes e cobertura.

63. **A exatidão validada por CV é ~28–42%. O RIPPER contribui com algo?**
    - *Porquê:* Mal acima da baseline aleatória de 33%.
    - *Testa:* Quando um comparador é informativo apesar de fraca exatidão.

64. **A tua "CV accuracy" — é k-fold aleatório? Isso não viola a tua própria posição de não-CV-aleatória?**
    - *Porquê:* Potencial inconsistência interna com o Cap.2.
    - *Testa:* Consistência da metodologia.

---

## M5Rules

65. **O M5Rules colapsou num modelo linear global. Porque incluí-lo de todo?**
    - *Porquê:* Nunca dividiu — é um comparador real?
    - *Testa:* Compreensão de porque as divisões falham a n≈80, mínimo-folha 4.

66. **Explica a divisão por redução de desvio-padrão e o alisamento das folhas.**
    - *Porquê:* Mecânica central do M5.
    - *Testa:* Conhecimento da construção de árvores de modelo.

67. **O M5Rules dá um declive negativo de água no solo; o CarenR dá uma faixa positiva de défice moderado. Qual está certa?**
    - *Porquê:* Uma contradição aparente entre métodos.
    - *Testa:* Estrutura linear-marginal vs. não-linear específica-do-bin.

68. **O CV R² é negativo para o RVV. Porque reportar sequer coeficientes de um modelo sobreajustado?**
    - *Porquê:* Interpretar coeficientes de um modelo pior do que a média.
    - *Testa:* Disciplina sobre o que é interpretável.

69. **O colapso do M5Rules significa que a relação é verdadeiramente linear, ou que o método está esfomeado de dados?**
    - *Porquê:* Duas conclusões muito diferentes.
    - *Testa:* Distinguir limitação do modelo de estrutura dos dados.

---

## Avaliação

70. **Justifica a métrica MAE ponderado. Porquê ponderar pelo tamanho do conjunto de teste?**
    - *Porquê:* A ponderação interage com a composição por era.
    - *Testa:* Compreensão de como a métrica é construída.

71. **A ponderação dá aos cenários iniciais, os mais confundidos, a maior influência. Não é ao contrário?**
    - *Porquê:* A métrica penaliza o CarenR precisamente onde ele é mais fraco.
    - *Testa:* Consciência da interação métrica–desenho.

72. **O texto do teu protocolo diz "prever o ano seguinte", mas as tabelas preveem todos os anos restantes. O que fizeste?**
    - *Porquê:* Uma inconsistência interna na descrição do desenho.
    - *Testa:* Precisão sobre a tua própria experiência.

73. **A extrapolação de tendência a vários passos domina os cenários iniciais. O teu MAE mede clima ou tendência?**
    - *Porquê:* O passo de re-tendência do resíduo carrega a maior parte do erro.
    - *Testa:* Decomposição das fontes de erro.

74. **Apenas 17 anos de teste por região em ~3 ciclos de produção. Alguma conclusão pode ser robusta?**
    - *Porquê:* A janela de avaliação é curta.
    - *Testa:* Honestidade quanto ao alcance estatístico.

75. **Porquê walk-forward em vez de desenhos de CV em blocos/aninhados?**
    - *Porquê:* Existem esquemas alternativos temporalmente válidos.
    - *Testa:* Conhecimento das opções de validação de séries temporais.

76. **Como mudariam as tuas afirmações de significância um teste de permutação ou block-bootstrap?**
    - *Porquê:* Um nulo mais honesto para dados dependentes.
    - *Testa:* Literacia em reamostragem.

77. **Que baseline seria verdadeiramente difícil de bater — e usaste-a?**
    - *Porquê:* A mediana é fácil sob MAE; ARIMAX ou persistência-com-deriva é mais difícil.
    - *Testa:* Rigor da baseline.

---

## Previsão

78. **Di-lo claramente: algum modelo bateu a baseline ingénua? Sim ou não.**
    - *Porquê:* Força o resultado nulo a descoberto.
    - *Testa:* Franqueza sob pressão.

79. **Porque é que as mesmas regras que descrevem o passado falham em prever o futuro?**
    - *Porquê:* A assimetria central.
    - *Testa:* Raciocínio descoberta vs. previsão.

80. **No RVV, acrescentar dados modernos piora o CarenR. Explica o mecanismo.**
    - *Porquê:* Afinação de era contra-intuitiva.
    - *Testa:* Compreensão do sobreajuste específico de era.

81. **Se |r| ≈ 0,47 dentro da amostra, porque não se traduz em capacidade fora da amostra?**
    - *Porquê:* Correlação dentro da amostra ≠ valor preditivo.
    - *Testa:* Raciocínio sobre o fosso de generalização.

82. **A janela equilibrada por era vence 5/5, depois perde 2019–2022. Isso não destrói a "promessa"?**
    - *Porquê:* A janela promissora colapsa logo a seguir.
    - *Testa:* Robustez da afirmação condicional.

83. **Uma previsão direcional (acima/abaixo da tendência) é mais alcançável do que uma pontual? Testaste-a?**
    - *Porquê:* Um alvo mais fraco mas útil que não avaliaste.
    - *Testa:* Reformulação da tarefa de previsão.

84. **Como usaria um produtor um modelo que só bate a mediana sob condições de era específicas?**
    - *Porquê:* Aplicabilidade prática de um resultado condicional.
    - *Testa:* Ponte da estatística para o uso.

---

## Discussão

85. **A decomposição em duas decisões é genuinamente nova, ou uma reformulação da decomposição do erro?**
    - *Porquê:* A tua melhor contribuição desafiada quanto à originalidade.
    - *Testa:* Articulação precisa do que é novo.

86. **Dizes que a Decisão 1 é o estrangulamento. Prova que não é a Decisão 2 (regras fracas).**
    - *Porquê:* O diagnóstico tem de ser defendido.
    - *Testa:* Evidência de que o passo das regras está perto do teto do sinal.

87. **Porque divergem as duas regiões tão acentuadamente — de grau ou de tipo?**
    - *Porquê:* Diferença estrutural vs. gradual.
    - *Testa:* Compreensão da magnitude do confundimento.

88. **Convergência entre algoritmos — poderia refletir confundimento partilhado e não verdade?**
    - *Porquê:* Três métodos sobre as mesmas características confundidas podem concordar de forma errada.
    - *Testa:* Ceticismo quanto ao teu próprio argumento de robustez.

89. **O teu |r| ≈ 0,47 coincide com Fraga et al. Isso significa que o teto do sinal é intrínseco?**
    - *Porquê:* Posiciona o teu resultado face a trabalho anterior.
    - *Testa:* Interpretação fundamentada na literatura.

90. **Se corresses tudo de novo com rendimento/ha, o que preverias que muda?**
    - *Porquê:* Força uma previsão falsificável sobre a tua própria correção.
    - *Testa:* Raciocínio científico sobre o confundimento.

---

## Limitações

91. **Qual é a limitação mais séria, nas tuas próprias palavras?**
    - *Porquê:* Maturidade de autocrítica.
    - *Testa:* Priorização das fraquezas.

92. **Duas das tuas variáveis-chave (água no solo, LAI) são saídas de modelo. Como se propaga esse erro?**
    - *Porquê:* A incerteza VSIM/STICS não está quantificada.
    - *Testa:* Consciência da propagação de erro.

93. **Nunca validas as regras em dados verdadeiramente retidos para o Objetivo 1. "Validado" é a palavra certa?**
    - *Porquê:* A validação do Objetivo 1 é toda dentro da amostra.
    - *Testa:* Precisão da afirmação "validado".

94. **O resumo diz 90–92% de retenção LOESS; a Tabela 4.4 mostra 15–16% para o RVV. Qual está correta?**
    - *Porquê:* Uma contradição interna direta.
    - *Testa:* Domínio dos teus próprios números.

95. **Agregados anuais de região única impedem a análise espacial. Isso limita toda a conclusão?**
    - *Porquê:* A agregação esconde heterogeneidade.
    - *Testa:* Compreensão dos limites de resolução.

---

## Trabalho Futuro

96. **Propões a destendência estrutural como correção. Porque não a fizeste dentro da tese?**
    - *Porquê:* A prescrição central fica por fazer.
    - *Testa:* Justificação de âmbito e consciência de viabilidade.

97. **O que entraria exatamente no vetor de covariáveis estruturais, e os dados pré-1986 estão disponíveis?**
    - *Porquê:* Viabilidade da tua própria proposta.
    - *Testa:* Domínio concreto das fontes de dados (IVV/IVDP).

98. **Porque detetaria o EMM interações que as regras marginais KS do CarenR não apanham?**
    - *Porquê:* Citas o EMM como a generalização natural.
    - *Testa:* Compreensão do interesse baseado em modelo.

99. **Como pré-registarias a hipótese equilibrada por era para um teste confirmatório?**
    - *Porquê:* Transforma o achado post-hoc em ciência.
    - *Testa:* Conhecimento de pré-registo e desenho prospetivo.

100. **Se um novo estudante continuasse isto, qual é a primeira experiência que deveria correr?**
    - *Porquê:* Testa se sabes o próximo passo de maior alavancagem.
    - *Testa:* Priorização de investigação.
