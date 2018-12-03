# Jogo da Velha em Cobol.

Projeto desenvolvido em Cobol. 
Estudo e prática de ler e gravar arquivo sequencial. 
 
# O Jogo : 
  - Faz análise do Jogo e tenta impedir usuário de fechar linha de jogo
  - Faz análise do Jogo e tenta fechar uma linha de jogo
  - Lê arquivo de bloqueio (onde são gravados os dados da jogada do usuário e seleciona a coordenada mais jogada disponível)
  - O jogo contém placar (quantas vezes o usuário e o cobol ganharam)
  - A aplicação reinicia um novo jogo ou então encerra a aplicação caso o usuário desejar
  
# Leitura dos arquivos:

- Formato em que o jogo é gravado:<br />
                      00001<br />
                      10001<br />
                      20001<br />
- Primeiro número de cada linha 0, 1 e 2 se referem a linha do tabuleiro(Jogo) em questão
- Os três seguintes números são as coordenadas do tabuleiro(Jogo). No caso 0 em coordenada 00, 0 em coordenada 01 e assim por diante.
- O ultimo número se refere a iteração de jogo, sendo após a jogada do usuário a aplicação grava o próximo status de 
Jogo com a iteração 2 no arquivo bloqueio.txt
e após a jogada do cobol a aplicação grava o status do jogo no arquivo ataque.txt seguindo a mesma regra.

  
  
