#lang racket

;; ===============================
;; Taller 2 – Programación Declarativa. Victor Andres Marquez #00046921
;; ===============================

;; Ejercicio 1 – Contar elementos positivos en una lista
(define (contar-positivos lst)
  (length (filter (lambda (x) (> x 0)) lst)))

(display "Ejercicio 1 - Positivos: ")
(display (contar-positivos '(3 -2 7 0 -5 9)))
(newline)

;; Ejercicio 2 – Generar lista de cuadrados pares
(define (cuadrados-pares lst)
  (map (lambda (x) (* x x))
       (filter even? lst)))

(display "Ejercicio 2 - Cuadrados pares: ")
(display (cuadrados-pares '(1 2 3 4 5 6 7 8)))
(newline)

;; Ejercicio 3 – Calcular el factorial de un número
(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

(display "Ejercicio 3 - Factorial de 5: ")
(display (factorial 5))
(newline)

;; Ejercicio 4 – Elevar cada número al cubo
(define (cubos lst)
  (map (lambda (x) (* x x x)) lst))

(display "Ejercicio 4 - Cubos: ")
(display (cubos '(2 3 4)))
(newline)

;; Ejercicio 5 – Sumar todos los elementos impares
(define (suma-impares lst)
  (foldl + 0 (filter odd? lst)))

(display "Ejercicio 5 - Suma de impares: ")
(display (suma-impares '(1 2 3 4 5 6 7)))
(newline)

;; Ejercicio 6 – Determinar si una lista contiene números negativos
(define (contiene-negativos? lst)
  (ormap (lambda (x) (< x 0)) lst))

(display "Ejercicio 6 - Contiene negativos?: ")
(display (contiene-negativos? '(5 9 -3 2)))
(newline)

;; Ejercicio 7 – Calcular la suma acumulada de una lista
(define (suma-acumulada lst)
  (reverse
   (foldl (lambda (x acc)
            (cons (+ x (if (null? acc) 0 (car acc))) acc))
          '()
          lst)))

(display "Ejercicio 7 - Suma acumulada: ")
(display (suma-acumulada '(1 2 3 4)))
(newline)

;; Ejercicio 8 – Concatenar cadenas de texto en una lista
(define (concatenar-cadenas lst)
  (foldr string-append "" lst))


(display "Ejercicio 8 - Concatenar cadenas: ")
(display (concatenar-cadenas '("Hola" " " "Mundo")))
(newline)

;; Ejercicio 9 – Generar lista con el doble de los números mayores que 5
(define (doble-mayores-5 lst)
  (map (lambda (x) (* 2 x))
       (filter (lambda (x) (> x 5)) lst)))

(display "Ejercicio 9 - Dobles mayores a 5: ")
(display (doble-mayores-5 '(3 6 8 2 10)))
(newline)

;; Ejercicio 10 – Invertir el orden de una lista
(define (invertir lst)
  (foldl (lambda (x acc) (cons x acc)) '() lst))

(display "Ejercicio 10 - Lista invertida: ")
(display (invertir '(1 2 3 4)))
(newline)

;; Ejercicio 11 – Crear una función que reciba una función como parámetro
(define (aplicar-funcion f lst)
  (map f lst))

(display "Ejercicio 11 - Aplicar función cuadrado: ")
(display (aplicar-funcion (lambda (x) (* x x)) '(1 2 3 4)))
(newline)

;; Ejercicio 12 – Reto integrador: promedio de números mayores a 5
(define (promedio-mayores-5 lst)
  (let* ((mayores (filter (lambda (x) (> x 5)) lst))
         (suma (foldl + 0 mayores))
         (cantidad (length mayores)))
    (/ suma cantidad)))

(display "Ejercicio 12 - Promedio mayores a 5: ")
(display (promedio-mayores-5 '(3 8 10 4 9 2 7)))
(newline)
