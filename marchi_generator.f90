PROGRAM marchi_generator
!
!	 Дата			Программист
!	27/08/2026		Арзиманов Д.И.
!
!	Описание программы:
!
!	Программа предназначена для генерации фермы типа МАРХИ в формате текстового файла ЛИРА-САПР
!	
!	y   -	n   -   xy
!	  \   /   \   /
!	|   n - | - n   |
! 	  / | \   / | \
!	2   -   n   -   n 
!     \ | /   \ | /
!	|   n - | - n   |
!     /   \   /   \
!	1   -   2   -   x
!
USE iso_fortran_env
IMPLICIT NONE
!Ввод переменных
INTEGER :: ios                                  !Статус файла
INTEGER :: node_number_1, node_number_2         !Номера элементов
INTEGER :: n                                    !Счетчик цикла для суммирования
REAL :: z                                       !Координата нижней сетки
REAL :: beam_length                             !Длина стержня
REAL :: node_x                                  !Координаты узлов x
REAL :: node_y                                  !Координаты узлов y
INTEGER :: x,y                                  !Кол-во шагов по x и y
INTEGER :: i,j,k                                !Счетчики циклов
CHARACTER(len=32):: filename                    !Имя файла
LOGICAL :: x_check, y_check
CHARACTER(len=*), PARAMETER :: RED=achar(27)//"[31m"
CHARACTER(len=*), PARAMETER :: GREEN=achar(27)//"[32m"
CHARACTER(len=*), PARAMETER :: ESC=achar(27)//"[0m"
! для смены кодировки windows
call system('chcp 65001 > nul')
!Ввод данных
filename = 'marchi'//'.txt'              
WRITE(*,*) 'Введите длину стержня в метрах в  формате (0.000):'
READ(*,*) beam_length
!Ввод и Проверка x
x_check = .FALSE.
DO WHILE (x_check .EQV. .FALSE.)
        WRITE(*,*) 'Введите кол-во шагов по x, (x>=2)'
        READ(*,*) x
        IF (x<2) THEN
        WRITE(*,*)'Кол-во шагов не может быть меньше 2-х!'
        x_check = .FALSE.
        ELSE
        x_check = .TRUE.
        END IF
END DO
!Ввод и Проверка y
y_check = .FALSE.
DO WHILE (y_check .EQV. .FALSE.)
        WRITE(*,*) 'Введите кол-во шагов по y, (x>=2)'
        READ(*,*) y
        IF (y<2) THEN
        WRITE(*,*)'Кол-во шагов не может быть меньше 2-х!'
        y_check = .FALSE.
        ELSE
        y_check = .TRUE.
        END IF
END DO
!Открываем файл для записи
OPEN(unit=10, file=filename, status='replace', action='write', iostat=ios)
!Проверяем, успешно ли открылся
IF (ios /=0) THEN
        PRINT *, 'Ошибка при открытии файла!'
        STOP
END IF
!Запись имени задачи и неизменные параметры: признак схемы, размерность, единицы измерения, имя загружения...
WRITE(10,100)
100 FORMAT ('( 0/1 ; Marchi/ 2; 5/ 28; 0 0 1  0 1 0  1 0 0; / 33;М 1 СМ 100 Т 1 С 1 / 39; 1: LOAD 1 ; / 52; 0.001 /)')
!
!========БЛОК ГЕНЕРАЦИИ ВЕРХНЕЙ ПРЯМОУГОЛЬНОЙ СЕТКИ КЭ=======
!
!Вертикальная  разлиновка элементами
!Начальные параметры
WRITE(10,*)'( 1/'
node_number_1 = -1
node_number_2 = 0
DO i=1,x
        node_number_1 = node_number_1 + 1
        node_number_2 = node_number_2 + 1
        DO j=1, y-1
        node_number_1 = node_number_1 + 1
        node_number_2 = node_number_2 + 1
        WRITE(10,110)node_number_1, node_number_2
        110 FORMAT ('10 2 ' ,I6 ,I6, ' /')
        END DO
END DO
!Горизонтальная разлиновка
!Обнуляем номера элементов
n = 0
DO j=1,y
        node_number_1 = 0
        node_number_2 = 0 + y
        n = n + 1
        node_number_1 = node_number_1 + n
        node_number_2 = node_number_2 + n
        WRITE(10,120) node_number_1, node_number_2
        DO i=1,x-2
        node_number_1 = node_number_1 + y
        node_number_2 = node_number_2 + y
        WRITE(10,120) node_number_1, node_number_2
        120 FORMAT ('10 2 ' ,I6 ,I6, ' /')
        END DO
END DO
!======КОНЕЦ БЛОКА ГЕНЕРАЦИИ ВЕРХНЕЙ СЕТКИ======
!
!======НАЧАЛО БЛОКА ГЕНЕРАЦИИ НИЖНЕЙ ПРЯМОУГОЛЬНОЙ СЕТКИ КЭ======
!Вертикальная  разлиновка элементами
IF(y>2) THEN
!Начальные параметры
node_number_1 = x * y - 1
node_number_2 = x * y
DO i=1,x-1
        node_number_1 = node_number_1 + 1
        node_number_2 = node_number_2 + 1
        DO j=1, y-2
        node_number_1 = node_number_1 + 1
        node_number_2 = node_number_2 + 1
        WRITE(10,150)node_number_1, node_number_2
        150 FORMAT ('10 2 ' ,I6 ,I6, ' /')
        END DO
END DO
END IF
!Горизонтальная разлиновка
IF(x>2) THEN
!Обнуляем счетчик элементов
n = 0
DO j=1,y-1
        node_number_1 = x * y
        node_number_2 = x * y + y - 1
        n = n + 1
        node_number_1 = node_number_1 + n
        node_number_2 = node_number_2 + n
        WRITE(10,160) node_number_1, node_number_2
        DO i=1,x-3
        node_number_1 = node_number_1 + y - 1
        node_number_2 = node_number_2 + y - 1
        WRITE(10,160) node_number_1, node_number_2
        160 FORMAT ('10 2 ' ,I6 ,I6, ' /')
        END DO
END DO
END IF
!======КОНЕЦ БЛОКА ГЕНЕРАЦИИ НИЖНЕЙ СЕТКИ======
!======БЛОК ГЕНЕРАЦИИ СВЯЗЕЙ======
!
!======ПРАВЫЕ СВЯЗИ======
node_number_1 = 0
node_number_2 = x * y
DO i=1,x-1
        node_number_1 = node_number_1 + 1                  ! 1    4
        node_number_2 = node_number_2 + 1                  ! 13   15 
        WRITE(10,170)node_number_1, node_number_2
              DO j=1, y-2 
                node_number_1 = node_number_1 + 1          ! 2         
                node_number_2 = node_number_2              ! 13
                WRITE(10,170)node_number_1, node_number_2
                node_number_2 = node_number_2 + 1          ! 2
                WRITE(10,170)node_number_1, node_number_2  ! 14       
              END DO          
        node_number_1 = node_number_1 + 1                  ! 3
        node_number_2 = node_number_2                      ! 14
        WRITE(10,170)node_number_1, node_number_2
        170 FORMAT ('10 2 ' ,I6 ,I6, ' /')
END DO
!======КОНЕЦ БЛОКА ПРАВЫХ СВЯЗЕЙ======
!
!======БЛОК ЛЕВЫХ СВЯЗЕЙ======
!
node_number_1 = y
node_number_2 = x * y
DO i = 1, x-1
        node_number_1 = node_number_1 + 1                  ! 6   
        node_number_2 = node_number_2 + 1                  ! 16    
        WRITE(10,170)node_number_1, node_number_2
              DO j=1, y-2 
                node_number_1 = node_number_1 + 1          ! 7         
                node_number_2 = node_number_2              ! 16
                WRITE(10,170)node_number_1, node_number_2
                node_number_2 = node_number_2 + 1          ! 7
                WRITE(10,170)node_number_1, node_number_2  ! 17       
              END DO          
        node_number_1 = node_number_1 + 1                  ! 3
        node_number_2 = node_number_2                      ! 14
        WRITE(10,170)node_number_1, node_number_2
END DO
WRITE(10,*)')'
!
!======КОНЕЦ БЛОКА СВЯЗЕЙ=====
!
!Ввод жесткостей
WRITE(10,180)
180 FORMAT ('( 3/2 37879.8 32.07 32.07 24.281 0 0 /0 Y 0.0190253 0.0190253 Z 0.0190253 0.0190253 /0 RO 0.0141499/0 S8 13 8.9 8.9 0 0/)')
WRITE(10,190)
190 FORMAT ('{13/ [2] Section = Pipe File  = |TRUBA2.profiles.srt| Shape = |89 x 7| }')
!
!======БЛОК ВВОДА КООРДИНАТ======
!
WRITE(10,*)'( 4/'
!Инициализируем начальные координаты верхней сетки кэ
node_x=0.0-beam_length
!Ввод координат
DO i=1, x
        node_y = 0.0-beam_length
        node_x = node_x + beam_length
        DO j=1, y
        node_y = node_y + beam_length
        WRITE(10,130) node_x, node_y
        130 FORMAT (' ',F9.5,' ',F9.5,' 0.00000',' /')
        END DO
END DO
!Инициализируем начальные координаты нижней сетки кэ
node_x = 0.0 - beam_length / 2
!Вычисляем первую координату нижней сетки
z = (-beam_length / 2) * 2**0.5
!Вводим координаты 
DO i=1, x-1
        node_y = 0.0 - beam_length / 2
        node_x = node_x + beam_length
        DO j=1, y-1
        node_y = node_y + beam_length
        WRITE(10,140) node_x, node_y, z
        140 FORMAT (' ',F9.5,' ',F9.5,' ',F9.5,' /')
        END DO
END DO
WRITE(10,*)')'
!======КОНЕЦ БЛОКА ВВОДА КООРДИНАТ =====
CLOSE(unit=10)  !Закрываем файл
WRITE(*,'(A, A, /, A, A)')GREEN,'Текстовый файл успешно создан!', &
&'Нажмите любую клавишу для продолжения...',ESC 
READ(*,*)
END PROGRAM marchi_generator
