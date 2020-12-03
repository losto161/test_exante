metric
=====

An OTP application

Build
-----

    $ rebar3 compile

Test
----
    $ rebar3 ct
    
Description
-----
Так как обработка ошибок не предусмотрена спецификацией, то валидация параметров
не реализовывалась. По этой же причине принят компромисс: если метрика имеет непериодическую
природу или период обновления больше измерительного окна (то есть, когда в пределах измерительного
окна нет ни одного значения метрики), то среднее значение считается равным нулю (что в общем случае 
некорректно, правильнее говорить о недетерменированности значения на заданном интервале).

Скользящее среднее рассчитывается по формуле простого арифметического (не взвешенного). 
Для уменьшения вычислительной погрешности при суммировании чисел с плавающей точкой
применяется компенсационное суммирование (Кэхэн). Реализация "наивная", так как погрешность
в любом случае накапливается, в том числе и при суммировании самих погрешностей.

Для нагрузочного тестирования добавлен модуль metric_benchmark. Предполагается, что тест запускается
на ноде, отличной от ноды с metric_app. Функция start_test на вход принимает имя ноды с metric_app, 
максимальное количество метрик и период обновления в секундах или атом random (в этом случае для
каждой метрики период обновления будет выбран случайным образом из диапазона 1-60).
        