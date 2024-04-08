# uni_plot
Простой плоттер на freepascal

##Зависимости
- Synapse
- CairoCanvas (Lazarus компонент)
- ICUtils 
- Uni_Graphics


## Установка компонент (uni_graphics)

Для инсталляции необходимо установить следующие пакеты:

1. Synapse

1.1 Забрать из репозитария

``` sh
git clone https://github.com/geby/synapse
```

1.2 Установить ./synapse/laz_synapse.lpk штатными средствами lazarus

2. CairoCanvas

1.1 Находиться в самом lazarus. Установить штатными средствами 

``` sh
cd ~/fpcupdeluxe/lazarus/components/cairocanvas/cairocanvas_pkg.lpk
```

3. ICUtils

``` sh
git clone https://github.com/XHermitOne/icutils_pack
cd ./icutils_pack
```

1.1 icutils.lpk установить штатными средствами 

4. uni_graphics

``` sh
git clone https://github.com/XHermitOne/uni_plot
cd ./uni_plot
```

1.1 uni_graphics.lpk установить штатными средствами 


## Компактный вариант пакета (uni_graphics_compact)

Для того чтобы не устанавливать все зависимости сделан компактный вариант пакета.


## Примеры

1. ./examples/graphic. Пример заполнения графика по данным из файла.
2. ./examples/trend. Пример заполнения временного графика/тренда. Данные заполняются по событию таймера последовательно по точкам.
3. ./examples/trend_control. Пример, как можно управлять прокруткой и масштабированием компонента.


