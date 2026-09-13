PROGRAM marchi_generator
!
!	 Дата			Программист			Версия
!	09/09/2026		Арзиманов Д.И.		1.1
!
!	Описание программы:
!
!	Программа предназначена для генерации плоских и циллиндрических ферм типа МАРХИ в формате текстового файла ЛИРА-САПР
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
REAL :: node_x, node_y, node_z                  !Координаты узлов x
REAL :: node_x1, node_y1, node_z1				!Координаты точек внутреннего радиуса циллиндрической оболочки
REAL :: node_x2, node_y2, node_z2				!Координаты точек внешнего радиуса циллиндрической оболочки
REAL :: span, hight, arch_hight                 !Пролет, высота
REAL :: inner_radius, outer_radius              !Внутренний и наружний радиусы
REAL :: inner_angle, outer_angle, rotation_angle!Внутренний и наружний углы, угол поворота ячейки
REAL :: alpha_inner, alpha_outer				!Внутренний и наружный углы оси абсцисс
REAL :: inner_element, outer_element			!Длина звена нижнего и верхнего пояса
REAL, PARAMETER :: PI = 4.0 * ATAN(1.0)         !число Пи
INTEGER :: x,y                                  !Кол-во шагов по x и y
INTEGER :: i,j,k                                !Счетчики циклов
INTEGER :: cell
CHARACTER(len=32):: filename                    !Имя файла
CHARACTER(len=1):: cyl                          !Проверка типа системы
LOGICAL :: x_check, y_check
CHARACTER(len=*), PARAMETER :: RED=achar(27)//"[31m"
CHARACTER(len=*), PARAMETER :: GREEN=achar(27)//"[32m"
CHARACTER(len=*), PARAMETER :: ESC=achar(27)//"[0m"
! для смены кодировки windows
call system('chcp 65001 > nul')
!Присвоение имени файлу и расширения
filename = 'marchi'//'.txt'
!Ввод шагов по x и y
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
!=======ВЫБОР ТИПА СИСТЕМЫ========
!
DO
WRITE(*,*) 'Структура цилиндрическая (y/n) ?'
READ(*,*) cyl
IF (cyl == 'y' .OR. cyl == 'n') THEN
        EXIT !Выход из цикла при корректном ответе
ELSE
        WRITE(*,'(A,A,A)')RED,'Ошибка! Введите y или n.',ESC
END IF
END DO
cylinder: IF (cyl == 'n') THEN
! 
!========СИСТЕМА ПЛОСКАЯ=======
!
!Ввод длины стержня
WRITE(*,*) 'Введите длину стержня в метрах в  формате (0.000):'
READ(*,*) beam_length
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
!
!========СИСТЕМА ЦИЛЛИНДРИЧЕСКАЯ========
!
ELSE cylinder
		!Ввод параметров циллиндрической системы
        WRITE(*,*) 'Введите пролет в метрах в формате 0.000'
        READ(*,*) span
        WRITE(*,*) 'Введите стрелу (высоту) арки в метрах в формате 0.000'  
        READ (*,*) hight
		!внутренний радиус
        inner_radius = hight / 2 + span ** 2 / (8 * hight)
        WRITE(*,'(A,F10.5,A)') 'Внутренний радиус =', inner_radius, 'м'
		!внутренний угол
        inner_angle = 2 * ASIN(span / (2 * inner_radius )) * 180 / PI
        WRITE(*, '(A,F10.5,A)') 'Угол развала по опорам = ', inner_angle, 'градусов'
		!Вычисление угла для одной ячейки
		rotation_angle = inner_angle / REAL(x - 2.0)
		WRITE(*,'(A,F10.5,A)') 'Шаг угла поворота = ', rotation_angle, 'градусов'
		!Вычисление угла от оси абсцисс до нулевой координаты
		alpha_inner = (90 - inner_angle / 2) + inner_angle
		WRITE(*,'(A,F10.5,A)') 'Начальный угол = ', alpha_inner, 'градусов'
		!Вычисление длины звена нижнего пояса
		node_z1 = inner_radius * SIND(alpha_inner - rotation_angle) - inner_radius + hight
		node_x1 = inner_radius * COSD(alpha_inner - rotation_angle) + span / 2
		inner_element = SQRT(node_x1**2+node_z1**2)
		WRITE(*,'(A,5X,F8.5,A)') 'Длина нижнего звена = ', inner_element, ' м'
		!Вычисление высоты треугольника
		arch_hight = ( inner_angle * PI / 180 * inner_radius ) / ((x - 2) * 2**0.5 - inner_angle * PI / 180) 
		WRITE(*,'(A,5X,F8.5,A)') 'Высота призмы = ', arch_hight, ' м' 
		!Вычисления для внешнего радиуса
		outer_element = arch_hight * 2 ** 0.5
		outer_radius = inner_radius + arch_hight
		outer_angle = inner_angle + rotation_angle
		alpha_outer = (90 - outer_angle / 2) + outer_angle
		WRITE(*,'(A,5X,F8.5,A)') 'Длина верхнего звена = ', outer_element, ' м'
		!
		!=========БЛОК ВВОДА КООРДИНАТ========= 
		!
		WRITE(10,*)'( 4/'
		!Вычисление координат по внешнему радиусу
		DO i = 0, x - 1
			node_x2 = outer_radius * COSD(alpha_outer - rotation_angle * i) + span / 2
			node_y2 = 0.0 - outer_element
			node_z2 = outer_radius * SIND(alpha_outer - rotation_angle * i) - inner_radius + hight
			DO j = 1, y
				node_y2 = node_y2 + outer_element
				WRITE(10,200) node_x2, node_y2, node_z2
				200 FORMAT (' ',F9.5,' ',F9.5,' ',F9.5,' /')
			END DO
		END DO
		!Вычисление координат по внутреннему радиусу
		DO i = 0, x - 2
			node_x1 = inner_radius * COSD(alpha_inner - rotation_angle * i) + span / 2
			node_y1 = 0.0 - outer_element / 2
			node_z1 = inner_radius * SIND(alpha_inner - rotation_angle * i) - inner_radius + hight
			DO j = 1, y - 1
				node_y1 = node_y1 + outer_element
				WRITE(10,210) node_x1, node_y1, node_z1
				210 FORMAT (' ',F9.5,' ',F9.5,' ',F9.5,' /')
			END DO
		END DO
		WRITE(10,*)')'
		!
		!========КОНЕЦ БЛОКА ВВОДА КООРДИНАТ=========
		!
END IF cylinder
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
CLOSE(unit=10)  !Закрываем файл
WRITE(*,'(A, A, /, A, A)')GREEN,'Текстовый файл успешно создан!', &
&'Нажмите любую клавишу для продолжения...',ESC 
READ(*,*)
END PROGRAM marchi_generator
