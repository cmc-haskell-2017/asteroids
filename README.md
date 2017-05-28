# Asteroids

Игра "Астероиды"

Вам предложено сыграть за корабль с именем "Player". Против Вас играют агрессивные боты "Bot". 
Ваша задача : убивать астероиды, убивать ботов, собирать бонусы для того, чтобы выжить.

Как играть : 

"Up", "Down", "Left", "Right" - управление кораблем

"Space" - при нажатии корабль стреляет, при отпускании стрельба прекращается

"Tab" - показать статистику игры"

![Астероиды.](images/sd.gif)


[![Build Status](https://travis-ci.org/cmc-haskell-2017/project-template.svg?branch=master)](https://travis-ci.org/cmc-haskell-2017/project-template)


## Сборка и запуск

Соберите проект при помощи [утилиты Stack](https://www.haskellstack.org):

```
stack setup
stack build
```

Собрать и запустить одиночную игру можно при помощи команды

```
stack build && stack exec asteroids
```

Собрать и запустить сервер можно при помощи команды

```
stack build && stack exec asteroids-server <номер порта>
```

Собрать и запустить клиент можно при помощи команды

```
stack build && stack exec asteroids-client <IP-адрес сервера> <номер порта сервера>
```

Запустить тесты можно при помощи команды

```
stack test
```

Чтобы запустить интепретатор GHCi и автоматически подгрузить все модули проекта, используйте команду

```
stack ghci
```

