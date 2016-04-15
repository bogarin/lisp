(defpackage :testUnit
  (:use :common-lisp :ltk)
  (:export #:main))

(in-package :testUnit)
;;;;------------------------------------------------------------------------------------------------------------

(defun main()
    (defvar *tablero*)
    (defvar *arbol*)
    (defvar *hay-ganadores*)
    (defvar *niveles*)
    (defvar *ficha-humano*)
    (defvar *ficha-ordenador*)
    (defvar *fin-del-juego*)
    (defvar *comienza-humano*) ;variable que indica si el humano comienza el juego
    (defvar *tabla*)
    (defvar *sub-tabla*)
    (defvar *jugada-numero*) ; numero de jugada del ordenador

    (setf *debug-tk* nil)
    (setf *tablero* '(n n n n n n n n n))
    (setf *hay-ganadores* nil)
    (setf *niveles* 8)
    (setf *ficha-humano* 'o)
    (setf *ficha-ordenador* 'x)
    (setf *fin-del-juego* nil)
    (setf *comienza-humano* t)
    (setf *tabla* nil)
    (setf *sub-tabla* nil)
    (setf *jugada-numero* 1)

    ;;;;--------------------------------------------------------------------------------
    ;;;; Funciones para cargar y guardar tabla de jugadas para remover de *arbol* ------
    ;;;;---------------------------------------------------------------------------------

      (defun guardar-tabla (archivo) ;Funcion para guardar *tabla* en un archivo de salida
        (with-open-file (salida archivo
    			    :direction :output
    			    :if-exists :supersede)
          (with-standard-io-syntax
    	(print *tabla* salida))))

    ;;;;------------------------------------------------------------------------

      (defun cargar-tabla (archivo) ;Funcion para recuperar *tabla* desde un archivo de entrada
        (with-open-file (entrada archivo)
          (with-standard-io-syntax
    	(setf *tabla* (read entrada)))))

    ;;;;------------------------------------------------------------------------------------------------------
    ;;;; Funcion para generar el arbol de jugadas----------------------------------------------------------
    ;;;;-----------------------------------------------------------------------------------------------------

    (defun genera-arbol (tablero ficha nivel) ;;;;funcion que genera una lista de posibilidades de jugadas: coloca la ficha en el tablero
      (let
        ((lista nil)
           (pos 0)
           (copia nil)
           (sublista nil)
           (inicio (get-internal-real-time)))

        (when (> nivel 0)
            (progn
                (decf nivel) ;decremento en 1 nivel
                (loop
                     (when (= pos 9) (return))
                     (setq m (nth pos tablero))
                     (when (equal m 'n)
                         (progn
                           (setf copia (subseq tablero 0 9));hago una copia del tablero jugado
                           (setf (nth pos copia) ficha) ;coloca la ficha en su lugar ficha es 'x o 'o (en copia)
                            (setf jugada-siguiente (buscar-jugada-anterior tablero));////
                            (when (not (equal jugada-siguiente copia));//// ;la jugada siguiente NO DEBE SER IGUAL A COPIA. para evitar perder
                                 (when (not (gana tablero ficha)) ;si no gana, tengo que agregar las sublistas generadas
                                   (progn
                                     (if (equal ficha 'x)
                                        (setf sublista (genera-arbol copia 'o nivel))
                                        (setf sublista (genera-arbol copia 'x nivel)))
                                     (when (not (null sublista)) (push sublista lista)) ;copia el sub-arbol generado por colocar la otra ficha...
                                       (push copia lista))))))  ;agrega a la lista la jugada
                     (incf pos)
                     (setf tiempo-real (- (get-internal-real-time) inicio))
                     (when (< 2000000 tiempo-real) (return))) ;Temporizacion de 2 segundos
                 lista))))

    ;;;;----------------------------------------------------------------------------

)
