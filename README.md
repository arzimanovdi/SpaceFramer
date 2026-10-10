# SpaceFramer
## Описание программы
- Программа создает текстовый документ с исходными данными для ПК ЛИРА-САПР, по которым можно сгенерировать КЭ сетку плоских и цилиндрических ферм типа "МАРХИ".
- Можно задавать длину элементов, кол-во шагов по оси X, Y для плоской системы, длину пролета, высоту стрелы и кол-во звеньев нижней дуги для цилиндрической
## Примеры:
<img width="1107" height="639" alt="Ферма мархи" src="https://github.com/user-attachments/assets/d65cb26a-f978-48d9-8724-f2c99af349bc" />
<img width="1279" height="609" alt="Ферма плоская" src="https://github.com/user-attachments/assets/e0cd1676-fad4-4990-96ed-45f52e4227cf" />

## Технические подробности
- Код написан на Fortran 90.
- Скомпилирован gFortran под Windows x64
## P.S.
- Хочу выразить благодарность д.т.н. проф. Макееву С.А. а также Онлайн Университету https://sopromex.ru/

## For English users
This program creates a txt file with LIRA-SAPR importable parameters, wich can be used to generate a FEM grid of MARCHI truss.
The input parameters are: beam length, step by x, step y

I used gfortran compiler to make a binary.

Special Thanks to S.A.Makeev and https://sopromex.ru/
