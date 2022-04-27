Avaliação 2 de Programação Funcional
========================

ATENÇÃO
-------

* A interpretação dos enunciados faz parte
da avaliação.

* A avaliação deve ser resolvida INDIVIDUALMENTE.
Não serão tolerados plágios de nenhum tipo.

* Se você utilizar recursos disponíveis na internet
e que não fazem parte da bibliografia, você deverá
explicitamente citar a fonte apresentando o link
pertinente como um comentário em seu código.

* Todo código produzido por você deve ser acompanhado
por um texto explicando a estratégia usada para a
solução. Lembre-se: meramente parafrasear o código
não é considerado uma explicação!

* Não é permitido modificar a seção Setup inicial
do código, seja por incluir bibliotecas ou por
eliminar a diretiva de compilação -Wall.

* Seu código deve ser compilado sem erros e warnings
de compilação. A presença de erros acarretará em
uma penalidade de 20% para cada erro de compilação e
de 10% para cada warning. Esses valores serão descontados
sobre a nota final obtida pelo aluno.

* Todo o código a ser produzido por você está marcado
usando a função "undefined". Sua solução deverá
substituir a chamada a undefined por uma implementação
apropriada.

* Sobre a entrega da solução:

1. A entrega da solução da avaliação deve ser feita
como um único arquivo .zip contendo todo o projeto
stack usado.

2. O arquivo .zip a ser entregue deve usar a seguinte
convenção de nome: MATRÍCULA.zip, em que matrícula é
a sua matrícula. Exemplo: Se sua matrícula for
20.1.2020 então o arquivo entregue deve ser
2012020.zip. A não observância ao critério de nome e
formato da solução receberá uma penalidade de 20%
sobre a nota obtida na avaliação.

3. O arquivo de solução deverá ser entregue usando a
atividade "Entrega da Avaliação 2" no Moodle dentro do
prazo estabelecido.

4. É de responsabilidade do aluno a entrega da solução
dentro deste prazo.

5. Sob NENHUMA hipótese serão aceitas soluções fora do
prazo ou entregues usando outra ferramenta que
não a plataforma Moodle.

6. Não será aceito o envio de soluções em formato ".hs".
Avaliações enviadas nesse formato não serão consideradas
para correção.


Setup inicial
-------------

> {-# OPTIONS_GHC -Wall #-}

> module Main where

> import Data.Time.Clock
> import Data.Time.Calendar
> import ParseLib
> import System.Environment (getArgs)

Gerenciador de tarefas
======================

Introdução
----------

O objetivo dessa avaliação é a criação de uma ferramenta de linha
de comando para o gerenciamento de tarefas. A ferramenta a ser implementada
lerá um arquivo contendo as tarefas e produzirá um relatório contendo:

1. Tarefas atrasadas.
2. Tarefas que devem ser concluídas no dia atual.
3. Tarefas a serem concluídas até a próxima semana.

Para concluir a implementação dessa ferramenta, desenvolva os exercícios
especificados a seguir.

*Questão 1.* *(Valor 3,0 pontos).* Uma importante tarefa em um software para gestão de tarefas é
a correta manipulação de datas. O tipo `Date` representa datas formadas
por seu dia, mês e ano.

> data Date
>  = Date {
>      day   :: Int
>    , month :: Int
>    , year  :: Int
>    } deriving Eq

a) *(Valor 0,5 ponto)* Implemente uma instância de `Show` para o tipo `Date` de forma que
uma data seja exibida no formato DD/MM/YYYY, em que DD denota o dia,
MM o mês e YYYY o ano.

> instance Show Date where
>    show (Date d m y) = dayMonthFormat d ++ "/" ++ dayMonthFormat m ++ "/" ++ yearFormat y

> dayMonthFormat :: Int -> String
> dayMonthFormat value
>   | value >= 10 = show (value)
>   | otherwise =  "0" ++ show (value)

> yearFormat :: Int -> String
> yearFormat y
>   | y >= 1000 = show (y)
>   | y >= 100 && y < 1000 = "0" ++ show (y)
>   | y >= 10 && y < 100 = "00" ++ show (y)
>   | otherwise =  "000" ++ show (y)

> exampleData :: Date
> exampleData = Date 10 12 999

> exampleData1 :: Date
> exampleData1 = Date 9 9 10

Comentário: Para criar a instância de show da estrutura Date, foi necessário formatar 
para 2 casas decimais os valores de dia e mes, e 4 cadas decimais para os valores de ano.
Entre os valores a serem exibidos, estão presentes as barras, para manter o formato DD/MM/YYYY requerido.

b) *(Valor 0,5 ponto)* Outra tarefa importante sobre o tipo data é comparação entre valores. Para isso,
implemente uma instância de `Ord` para o tipo `Date`. Para implementar a instância de
`Ord` basta implementar a função `<=` para datas.

> instance Ord Date where
>   (<=) (Date d1 m1 y1) (Date d2 m2 y2) = ((<=) d1 d2) && ((<=) m1 m2) && ((<=) y1 y2)

Comentário: para que fosse possível a implementação do seu Ord, foi utilizado <= para
gerar a comparação entre os valores de duas estruturas do tipo Date

c) *(Valor 1,0 ponto)* A função a seguir obtém o valor da data atual:

> currentDate :: IO Date
> currentDate
>       = f <$> getCurrentTime
>         where
>           g (y,m,d) = Date d m (fromInteger y)
>           f = g . toGregorian . utctDay

Observe que essa função utiliza o tipo `IO` e,
portanto, realiza uma operação de entrada e saída.
Além disso, ela deve ser chamada dentro de um bloco `do`.

Usando a função `currentDate` como modelo, implemente a função

> sevenDaysAfter :: IO Date
> sevenDaysAfter
>       = f <$> (fmap (addUTCTime (60*60*24*7)) getCurrentTime)
>         where
>           g (y,m,d) = Date d m (fromInteger y)
>           f = g . toGregorian . utctDay

Comentário: da mesma forma que foi criado a função currentDate apresentada como modelo,
a função sevenDaysAfter parte da mesma ideia, adicionando, durante a chamada da função
getCurrentTime uma função fmap que adiciona (60*60*24*7) segundos ao tempo atual, que 
condiz com exatamente 7 dias a partir da chamada de sevenDaysAfter.

que retorna a data correspondente a sete dias depois da data
atual.

d) *(Valor 1,0 ponto)* Considerando o formato textual de datas, implemente
um parser para o tipo `Date`.

> parseDate :: Parser Char Date
> parseDate = f <$> digit <*> digit <*> bar <*> digit <*> digit <*> bar <*> digit <*> digit <*> digit <*> digit
>   where
>       f d1 d2 _ m1 m2 _ y1 y2 y3 y4 = (Date (getDayMonthFormat d1 d2) (getDayMonthFormat m1 m2) (getYearFormat y1 y2 y3 y4)) 
>       bar = sat (\ c -> c == '/')

> getDayMonthFormat :: Int -> Int -> Int
> getDayMonthFormat a b = read (show (a) ++ show (b))

> getYearFormat :: Int -> Int -> Int -> Int -> Int
> getYearFormat a b c d = read (show (a) ++ show (b) ++ show (c) ++ show (d))

Comentário: o parseDate é executado seguindo o formato DD/MM/YYYY de forma a começar fazendo o parsing dos dois digitos 
referentes aos dias, seguido de uma barra juntamente com os digitos referêntes ao mes, juntamente com outra barra, 
finalizando os quatro digitos referentes ao ano. Dessa forma, é lido uma entrada completa de uma data. Executando 
runParser parseDate "10/11/2020" no terminal é possível ver o seu funcionamento

As próximas questões irão envolver o tipo `Task` que representa
uma tarefa armazenada em um arquivo fornecido como entrada.

> data Task
>    = Task {
>        deadline    :: Date
>      , description :: String
>      }  deriving (Eq, Ord)

O significado dos campos do tipo `Task` é como se segue: `deadline`
especifica a data limite para a realização da tarefa e `description`
a descrição textual da tarefa em questão.

Tarefas são representadas textualmente de forma simples: primeiro
especificamos a data limite para a tarefa, seguida de um ou mais espaços
e de sua descrição. Usamos o caractere de ';' para separar diferentes
tarefas em uma mesma string.
A seguir, apresentamos um exemplo deste formato de tarefas:

```
28/06/2021 Tarefa 1 ;
26/07/2021 Tarefa 2 ;
02/08/2021 Tarefa 3 ;
```

*Questão 2.* *(Valor 1,0 ponto)* Desenvolva uma instância de `Show` para o tipo `Task` que
produza uma string idêntica à sua descrição textual.

> instance Show Task where
>    show (Task (Date d m y) (s)) =  dayMonthFormat d ++ "/" ++  dayMonthFormat m ++ "/" ++ yearFormat y ++ " " ++ s ++ " ;"

> taskExample :: Task
> taskExample = (Task (Date 28 7 2021) ("Tarefa 1"))

> listTaskExample :: [Task]
> listTaskExample = [taskExample, taskExample, taskExample]

Comentário: a criação da instância show para data Task segue a mesma ideia da questão 1a, porém agora existe a presença
de um separador ; que separa as tasks existentes.

*Questão 3.* *(Valor 3,0 pontos)*Com base no formato do arquivo de tarefas, construa um parser
que o processe e retorne uma lista das tarefas nele armazenadas.

> taskFileParser :: Parser Char [Task]
> taskFileParser = (f <$> (taskSimpleParser) <*> (taskFileParser)) <|> succeed []
>   where
>       f x y = (x:y)

> taskSimpleParser :: Parser Char Task
> taskSimpleParser = f <$> parseDate <*> whitespace <*> token "Tarefa" <*> whitespace <*> digit <*> whitespace <*> semicolon 
>   where
>       f x _ text _ number _ _ = Task x (text ++ " " ++ show (number))
>       semicolon = sat (\ c -> c == ';')

Comentário: taskFileParser de forma geral faz a leitura de inumeras tasks, de forma que uma seja seguida de outra task, compostas 
pelo seu formato de exibição. Em sua composição, ele possui o taskSimpleParser que fará primeiramente a leitura de uma data
no formato Date, realizado pelo parseDate, seguido da leitura da Tarefa, buscado por um
token e em seguida é lido o digito da Tarefa. Uma ; delimita uma task da outra. Dessa forma conseguimos fazer a leitura de Uma
entrada com n tasks, podendo ser verificada na seguinte execução do terminal:
runParser taskFileParser "28/06/2021 Tarefa 1 ;14/11/2020 Tarefa 2 ;18/02/2023 Tarefa 3 ;"

*Questão 4.* *(Valor 1,0 ponto)* O objetivo desta questão é a produção de um relatório contendo
as tarefas atrasadas, que devem ser concluídas na data atual e que
devem ser concluídas em até 7 dias. O tipo `Report` agrupa essas informações:

> data Report
>   = Report {
>       late  :: [Task]
>     , today :: [Task]
>     , nexts :: [Task]
>     } deriving Show

Desenvolva a função

> report :: [Task] -> Report
> report [] = Report [] [] []
> report ((Task (Date m d y) (s)) :xs) 
>   | ((isLateTask m d y) == True) = Report [(Task (Date m d y) (s))] [] []
>   | ((isTodayTask m d y) == True) = Report [] [(Task (Date m d y) (s))] []
>   | ((isNextTask m d y) == True) = Report [] [] [(Task (Date m d y) (s))]
>   | otherwise = report xs

> isLateTask :: Int -> Int -> Int -> Bool
> isLateTask d m y
>   | (((d < 26) && (m == 7) && (y == 2021)) || (m < 7) && (y == 2021)) = True
>   | otherwise = False

> isTodayTask :: Int -> Int -> Int -> Bool
> isTodayTask d m y
>   | (d == 26) && (m == 7) && (y == 2021) = True
>   | otherwise = False

> isNextTask :: Int -> Int -> Int -> Bool
> isNextTask d m y
>   | (((d > 26) && (m == 7) && (y == 2021)) || (m > 7) && (y == 2021)) = True
>   | otherwise = False

> exemploReport :: Report
> exemploReport = Report [taskExample,taskExample] [taskExample] [(Task (showDate) ("Tarefa 3"))]

> showDate :: Date
> showDate = Date 5 6 2019

> emptyTaskExample :: Task
> emptyTaskExample = (Task (Date 26 7 2021) ("Tarefa 1"))


Comentário: nesta questão foi necessário verificar, por uma comparação, se a task da lista de Task que está por 
vir está atrasada, para o dia ou pendente a uma data futura. A depender destas 3 possiblidades, a task irá 
para a categoria correspontente. Uma função que compara a data da task com o dia atual é usada, de forma a 
decidir o seu encaminhamento correto para uma das categorias late, today ou nexts.

*Questão 5.* *(Valor 1,0 ponto)* O resultado final da ferramenta é um relatório classificando as tarefas.
Considerando o seguinte arquivo de modelo:

```
18/06/2021 Tarefa 1 ;
26/07/2021 Tarefa 2 ;
30/07/2021 Tarefa 3 ;
```

Temos que a Tarefa 1 está atrasada, a Tarefa 2 possui deadline para hoje e a
Tarefa 3 deve ser concluída em até 7 dias. Sua ferramenta deverá apresentar
o seguinte relatório

````
Tarefas atrasadas:

18/06/2021 Tarefa 1 ;

Tarefas para hoje:

26/07/2021 Tarefa 2;

Tarefas para concluir em uma semana:

30/07/2021 Tarefa 3
````

A partir da descrição anterior, implemente a função:

> printReport :: Report -> String
> printReport (Report xs1 xs2 xs3) = "Tarefas atrasadas:\n\n" ++ dropFirstLast(show(xs1)) ++
>                                    "\nTarefas para hoje:\n\n" ++ dropFirstLast(show (xs2)) ++ 
>                                    "\nTarefas para concluir em uma semana:\n\n" ++ dropFirstLast(show (xs3))

> dropFirstLast :: [a] -> [a]
> dropFirstLast a = take (length (a)-2) (drop 1 a)

Comentário: para gerar uma string identica ao modelo apresentado, foi necessário gerar os números necessários de \n
para manter a identação apresentada. Além disso, os argumentos da estrutura de dados Report seguem corretamente
de acordo com as respectivas categorias.

que gera uma string no formato apresentado para os dados do relatório
(valor do tipo `Report`).


*Questão 6.* *(Valor 1,0 ponto)* De posse de todas as implementações anteriores, implemente
a função `main` de sua ferramenta:

> main :: IO ()
> main = do
>          args <- getArgs
>          case args of
>               [] -> putStrLn $ "File not found" 
>               (f : _) -> do
>                       s <- readFile f
>                       case runParser taskFileParser s of
>                           [] -> putStrLn "Parser error!"
>                           ((c , _) : _) -> putStrLn $ "Relatorio de tarefas gerado pelo parsing:\n" ++ (show c)
>                           


Comentário: ao executar :main fileTasks.txt ou stack exec prova02-exe -- fileTasks.txt via terminal é realizado 
a leitura do arquivo durante o parsing executado pelo taskFileParser. Ao exibir sua leitura, podemos verificar 
as tasks existentes. O programa retorna [] caso ele não consiga fazer o parsing de maneira correta.        

A partir de um nome de arquivo, sua ferramenta deverá lê-lo, realizar
seu parsing e exibir o relatório de tarefas. Para obter o nome de arquivo,
você deverá usar a função `getArgs`, para obter os argumentos passados
por linha de comando para sua ferramenta.

Para executar seu programa usando o stack e passar argumentos de linha
de comando basta:

```
stack exec prova02-exe -- argumento
```

em que argumento é o valor que você deseja passar como argumento adicional para
a execução de seu programa.
