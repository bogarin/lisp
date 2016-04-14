;1.- La app en ocasiones indica que el jugador gano sin haber siquiera iniciado una partida.
;2.- La app en ocasiones falla en iniciar un nuevo juego.
;3.- Verificar correctamente todos los posibles casos en los que un jugador puede ganar.
;4.- La app falla en ocasiones permitiendo unicamente a la maquina hacer marcas en el tablero.
;5.- La app falla al poner mas de dos "X" sin haber dado turno al jugador.
;6.- La app falla al indicar que el jugador perdio, sin haber agotado antes todas las posibilidades.

;(load "c:/Descargas/ltk.lisp")
;(load "c:/Descargas/tic-tac-toe-3.1.lisp")
;(tic-tac-toe:main)



;;;; El siguiente programa es el juego tic-tac-toe que contiene respuestas basadas en arboles de posibilidades de jugadas

;;;; Este programa debe cumplir los siguientes requisitos:

;;;; Ser simple y elegante.
;;;; Ocupar pocas lineas de codigo.
;;;; Ser minimalista en la interfaz grafica.
;;;; Utilizar recursividad

(defpackage :tic-tac-toe
  (:use :common-lisp :ltk)
  (:export #:main))

(in-package :tic-tac-toe)

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

;;;;-------------------------------------------------------------------------------
;;;; Función Acerca de... ---------------------------------------------------------
;;;;-------------------------------------------------------------------------------

(defun acerca-de ()
  (with-ltk()
    (let*
  ((copyleft (make-instance 'label))
   (boton-ok (make-instance 'button :text "Aceptar" :command (lambda() (setf *exit-mainloop* t)))))

(setf (text copyleft) "tic-tac-toe 3.0 - Copyleft René Alejandro Coronel 2014")

(minsize *tk* 400 100)
(maxsize *tk* 400 100)
(wm-title *tk* "Acerca de...")

(place copyleft 15 15)
(place boton-ok 160 50))))





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
;;;; Funcion para generar el arbol de jugadas ----------------------------------------------------------
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

  (defun buscar-jugada-anterior (tablero) ;funcion auxiliar que busca tablero anterior "first" en *tabla* y devuelve la
    (let ;jugada siguiente que debera ser eliminada
	((jugada-siguiente nil))

      (when *tabla*
	(loop for registro in *tabla* do
	     (when (equal (first registro) tablero) ;si el primero de registro es igual al tablero anterior
	       (progn
		 (setq jugada-siguiente (second registro))
		 (return))))) ;devuelve la jugada siguiente a ser eliminada
      jugada-siguiente))

;;;;-------------------------------------------------------------------------------------------------
;;;; Función para mostrar arbol: quedo de versiones anteriores. -------------------------------------
;;;;--------------------------------------------------------------------------------------------------
  (defun mostrar (arbol) ;;;;simple y elegante .-|
    (when (not (null arbol))
      (when (listp (first arbol)) ;cuando el primero de la lista "es" una lista
	(progn
	  (princ (first arbol))
	  (mostrar (car (rest arbol)))
	  (mostrar (cdr (rest arbol)))))))

;;;;---------------------------------------------------------------------------------------------------
;;;; Funciones para buscar ganadores -----------------------------------------------------------------
;;;;---------------------------------------------------------------------------------------------------

  (defun ganadores (arbol ficha) ;genera una lista que contiene los tableros ganadores de una rama segun ficha
    (let
	((tablero nil)
	 (lista nil))
      (dolist (tablero arbol)
	(when (not (listp (first tablero))) ;tableros de un nivel determinado
	  (when (gana tablero ficha)
	    (push tablero lista))))
      lista))


;;;;-------------------------------------------------------------------------------------------------------------------
;;;;
  (defun raiz-ganador-nivel-x (ficha nivel);Devuelve los nodos raiz de las jugadas ganadoras segun nivel y ficha
    (let
	((ramas nil)
	 (ramas-x nil)
	 (arbol *arbol*)
	 (nodo nil)
	 (nodo-x nil)
	 (ganadores-x nil)
	 (ganadores-humano nil));////


      (setq *hay-ganadores* nil)
      (loop
	 (setq nodo (first arbol));el nodo raiz

	 (when (null nodo) (return))
	 (setq arbol (remove nodo arbol))

	 (setq ramas (first arbol))
	 (setq arbol (remove ramas arbol))

	 (if (= nivel 1)
	     (setq ganadores-x (ganadores ramas ficha))
	     (setq ganadores-x (ganadores-nivel-x ramas ficha nivel)))

	 (when (not (null ganadores-x)) (return)))

      nodo))

;;;;----------------------------------------------------------------------------------------------
  (defun ganadores-nivel-x (ramas ficha nivel) ;funcion que explora un  nivel y devuelve los ganadores del mismo
    (let
	((nodo-x nil)
	 (ramas-y nil) ;ramas de un nivel subsiguiente en profundidad
	 (ramas-x ramas)
	 (ganadores-humano nil));////
      (loop
	 (setq nodo-x (first ramas-x))
	 (when (null nodo-x) (return))
	 (setq ramas-x (remove nodo-x ramas-x))
	 (setq ramas-y (first ramas-x))
	 (setq ramas-x (remove ramas-y ramas-x))

	 (if (= nivel 2)
	     (progn
	       (setq lista-ganadores (ganadores ramas-y ficha))
	       (when (not (null lista-ganadores))
		 (progn (setq *hay-ganadores* T) (return))))

	     (progn
	       (decf nivel)
	       (when (not (null ramas-y))
		 (when (listp (first ramas-y))
		   (ganadores-nivel-x ramas-y ficha nivel)))))

	 (when *hay-ganadores* (return)))

      lista-ganadores))
;;;;---------------------------------------------------------------------------------

  (defun elimina-raiz-ganador-nivel-x (ficha nivel);Elimina los nodos raiz de las jugadas ganadoras segun nivel
    (let
	((ramas nil)
	 (ramas-x nil)
	 (arbol *arbol*)
	 (nodo nil)
	 (nodo-x nil)
	 (ganadores-x nil)
	 (ganadores-humano nil));////


      (setq *hay-ganadores* nil)
      (loop
	 (setq nodo (first arbol));el nodo raiz

	 (when (null nodo) (return))
	 (setq arbol (remove nodo arbol))

	 (setq ramas (first arbol))
	 (setq arbol (remove ramas arbol))

	 (if (= nivel 1)
	     (setq ganadores-x (ganadores ramas ficha))
	     (setq ganadores-x (ganadores-nivel-x ramas ficha nivel)))

	 (when (not (null ganadores-x)) ; Cuando hay ganadores...
	   (progn
	     (setq *arbol* (remove nodo *arbol*))
	     (setq *arbol* (remove ramas *arbol*))))))) ;elimina el nodo raiz y sus ramas

;;;;---------------------------------------------------------------------------------------------------
;;;;------- Reglas -------------------------------------------------------------------------------------
;;;;-------------------------------------------------------------------------------------------------
  (defun en-raya (tablero ficha pos-1 pos-2 pos-3);función que devuelve verdadero si las posiciones coinciden con ficha

    (and (equal ficha (nth pos-1 tablero))
	 (equal ficha (nth pos-2 tablero))
	 (equal ficha (nth pos-3 tablero))))


  (defun gana (tablero ficha) ;funcion que verifica si los jugadores ganan segun ficha

    (or (en-raya tablero ficha 0 1 2);horizontales
	(en-raya tablero ficha 3 4 5)
	(en-raya tablero ficha 6 7 8)
	(en-raya tablero ficha 0 3 6);verticales
	(en-raya tablero ficha 1 4 7)
	(en-raya tablero ficha 2 5 8)
	(en-raya tablero ficha 0 4 8);diagonales
	(en-raya tablero ficha 2 4 6)))

;;;;--------------------------------------------------------------------------------------------------------------
;;;; Una jugada del ordenador -----------------------------------------------------------------------------------
;;;;--------------------------------------------------------------------------------------------------------------

  (defun una-jugada-ordenador () ;una jugada del ordenador
    (let
	((nivel 1)
	 (jugada nil)
	 (raices-ganadores nil))

      (setf *arbol* (genera-arbol *tablero* *ficha-ordenador* *niveles*)) ;genera las jugadas posibles

      (setf raices-ganadores (ganadores *arbol* *ficha-ordenador*))
      (when (not (null raices-ganadores)) ;Puedo ganar en una sola jugada?
	(setf jugada (first raices-ganadores))) ;Hay que controlar que las raices sean ganadoras

;      (when (null jugada)
;	(elimina-raiz-ganador-nivel-x *ficha-humano* 1))

      (when (null jugada) ;Si es nil jugada, buscara un camino para ganar
	(loop
	   (when (= nivel 9) (return)) ;busca jugadas hasta ocho niveles

	   (if *arbol* ;si arbol es distinto de nil
	       (progn
		 (if (evenp nivel);* si nivel es par
		     (setf jugada (raiz-ganador-nivel-x *ficha-ordenador* nivel)) ;cuando es par busca si hay ganadores en el nivel y devuelve el nodo raiz
		     (elimina-raiz-ganador-nivel-x *ficha-humano* nivel))) ;elimino los nodos raices q tienen las jugadas ganadoras humano del nivel
	       (return)) ;si arbol es nil sale del bucle

	   (when (not (null jugada)) (return)) ;si hay una jugada sale del bucle
	   (incf nivel)))

      (if (null jugada)  ;si todavia es nil la jugada, hago una jugada al azar
	  (setf (nth (nth (random (length (blancos *tablero*))) (blancos *tablero*)) *tablero*) *ficha-ordenador*) ;jugada al azar
	  (setq *tablero* jugada)) ;si la jugada no era nil la asigno a tablero

      (incf *jugada-numero*)))


;* Los niveles pares (incluido el cero: raices) corresponden a jugadas de la maquina en el *arbol*, y los niveles impares al humano

  (defun blancos (tablero) ;f auxiliar, devuelve las posiciones de los espacios en blanco
    (let
	((lista nil))

      (loop for pos from 0 to 8 do
	   (when (equal (nth pos tablero) 'n) (push pos lista)))

      lista))

;;;;-----------------------------------------------------------------------------
;;;; Temporizacion (ejemplo) ----------------------------------------------------
;;;;-----------------------------------------------------------------------------

  (defun temporizacion (diferencia) ;Por ejemplo (temporizacion 2000000) es un tiempo de 2 segundos
    (let
	((inicio (get-internal-real-time)))

      (loop
	 (setf tiempo-real (- (get-internal-real-time) inicio))
	 (when (< diferencia tiempo-real) (return)))))

;;;;------------------------------------------------------------------------------------------------------------
;;;; Interfaz gráfica
;;;;-------------------------------------------------------------------------------------------------------------

  (with-ltk()
    (let* ((canvas (make-instance 'canvas))
	   (estado-juego (make-instance 'label))
	   (nuevo-menu (make-instance 'menu))
	   (menu-juego (make-instance 'menu :text "Juego" :master nuevo-menu))
	   (menu-ayuda (make-instance 'menu :text "Ayuda" :master nuevo-menu))
	   (boton-juego-nuevo (make-instance 'menubutton
					     :text "Juego nuevo"
					     :master menu-juego
					     :command (lambda ()
							(progn
							  (juego-nuevo)
							  (asigna-turnos)))))
	   (boton-salir (make-instance 'menubutton
				       :text "Salir"
				       :master menu-juego
				       :command (lambda ()
						  (progn
						    (setf *exit-mainloop* t)))))

	   (boton-about (make-instance 'menubutton
				       :text "Acerca de..."
				       :master menu-ayuda
				       :command (lambda () (acerca-de))))



	   (pos-x nil)
	   (pos-y nil)
	   (linea-1 (create-line canvas '(60 0 60 180)))
	   (linea-2 (create-line canvas '(120 0 120 180)))
	   (linea-3 (create-line canvas '(0 60 180 60)))
	   (linea-4 (create-line canvas '(0 120 180 120))))


      (when (probe-file "tabla.dat")
	(cargar-tabla "tabla.dat"));si existe el archivo de tabla lo abre

      (bind canvas "<ButtonPress-1>" (lambda(evento)
				       (when (not *fin-del-juego*)
					 (setq columna (multiple-value-bind (entero frac) (floor (/ (event-x evento) 60)) entero))
					 (setq fila (multiple-value-bind (entero frac) (floor (/ (event-y evento) 60)) entero))

					 (setq pos (+ (* 3 fila) columna)) ;me da la posicion entre 0 y 8;
					 (setf m (nth pos *tablero*));consulto el estado de tablero en la posicion pos
					 (when (equal m 'n)
					   (progn
					     (setf (nth pos *tablero*) *ficha-humano*) ;coloca la ficha en tablero
					     (circulo fila columna)))))) ;dibuja el circulo


      (bind canvas "<ButtonRelease-1>" (lambda(evento)
					 (if (gana *tablero* *ficha-humano*)
					     (progn
					       (setf (text estado-juego) "GANASTE");Si gana se termina el juego
					       (configure estado-juego :foreground "blue")
					       (setq *fin-del-juego* t)
					       (loop for registro in *sub-tabla* do ;lo que hay en sub-tabla, lo guardo en tabla
						    (push registro *tabla*));solamente cuando gana el humano
					       (guardar-tabla "tabla.dat")) ;y guardo en el archivo

					     (jugada-ordenador)))) ;Si no gana, juega el ordenador


      (bind *tk* "<KeyPress-F2>" (lambda(evento) (juego-nuevo)));Al presionar F2 inicia un nuevo juego
      (bind *tk* "<KeyRelease-F2>" (lambda(evento) (asigna-turnos)))

 ;;;;-------------------------------------------------------------------------------
;;;;-------------------------------------------------------------------------------
;;;;-------------------------------------------------------------------------------

      (defun jugada-ordenador()
	(let
	    ((i 0)
	     (j 0)
	     (fila nil)
	     (columna nil)
	     (hay-espacio nil))

	  (when (not *fin-del-juego*)
	    (loop ;hay que controlar que haya espacios en blanco para poder jugar
	       (when (= j 9) (return))
	       (setf cuadro (nth j *tablero*))
	       (when (equal cuadro 'n) (progn (setq hay-espacio t) (return))) ;sale si encuentra un blanco 'n
	       (incf j))

	    (if hay-espacio
		(progn
		  (setf tablero-anterior (subseq *tablero* 0 9));guardo el tablero anterior
		  (setf tablero-siguiente (subseq *tablero* 0 9)) ;debo usar este simbolo pues *tablero* cambia con ficha 'o
		  (una-jugada-ordenador); juega el ordenador

		  (loop ;debo buscar diferencias entre el tablero nuevo y el anterior para determinar la posicion de la ficha
		     (when (= i 9) (return))
		     (setf tab (nth i *tablero*))
		     (setf ant (nth i tablero-anterior))
		     (when (not (equal tab ant))
		       (progn
			 (setf (nth i tablero-siguiente) 'x)
			 (setf columna (mod i 3)) ;la columna es el resto
			 (setf fila (multiple-value-bind (entero frac) (floor (/ i 3)) entero)) ;division entera, me da la fila
			 (return)))
		     (incf i))

		  (crux fila columna) ;coloco la ficha de la maquina en el tablero visual

		  (when (> *jugada-numero* 2) ;cuando el numero de jugada es mayor que 2...
		    (progn
		      (setq registro (list tablero-anterior tablero-siguiente)) ;genera el registro: tablero anterior / siguiente
		      (push registro *sub-tabla*))) ;coloca el registro en *sub-tabla*

		  (when (gana *tablero* *ficha-ordenador*)
		    (progn
		      (setf (text estado-juego) "PERDISTE, JE JE JE...")
		      (configure estado-juego :foreground "red")
		      (setq *fin-del-juego* t))))

		(setq *fin-del-juego* t))))) ;si no hay espacio en blanco, termina el juego

      (defun juego-nuevo ()
	(let
	    ()
	  (setf (text estado-juego) " ")
	  (setf *tablero* '(n n n n n n n n n))
	  (setf *fin-del-juego* nil)
	  (setq *sub-tabla* nil); cuando empieza un juego nuevo, *sub-tabla* se pone a nil. Pues guardara un solo juego
	  (setq *jugada-numero* 1)
	  (setf marco (create-rectangle canvas 0 0 180 180))
	  (itemconfigure canvas marco :fill "white")
	  (itemconfigure canvas marco :outline "white")	;Ahora hay que poner en blanco la parte grafica, el tablero grafico
	  (setf linea-1 (create-line canvas '(60 0 60 180)))
	  (setf linea-2 (create-line canvas '(120 0 120 180)))
	  (setf linea-3 (create-line canvas '(0 60 180 60)))
	  (setf linea-4 (create-line canvas '(0 120 180 120)))

	  (itemconfigure canvas linea-1 :width 5)
	  (itemconfigure canvas linea-2 :width 5)
	  (itemconfigure canvas linea-3 :width 5)
	  (itemconfigure canvas linea-4 :width 5)))


      (defun asigna-turnos()
	(let
	    ()

	  (if *comienza-humano* ;se organizan los turnos humano/ordenador
	      (progn
		(setq *comienza-humano* nil)
		(jugada-ordenador))
	      (setq *comienza-humano* t))))


      (defun circulo (fila columna)
	(let
	    ()
	  (setf x-1 (+ (* columna 60) 10))
	  (setf x-2 (+ x-1 40))
	  (setf y-1 (+ (* fila 60) 10))
	  (setf y-2 (+ y-1 40))
	  (setf circulo (create-oval canvas x-1 y-1 x-2 y-2))
	  (itemconfigure canvas circulo :width 5)
	  (itemconfigure canvas circulo :outline "blue")))

      (defun crux (fila columna)
	(let
	    ()
	  (setf x-1 (+ (* columna 60) 12))
	  (setf x-2 (+ x-1 35))
	  (setf y-1 (+ (* fila 60) 12))
	  (setf y-2 (+ y-1 35))
	  (setf linea-1 (create-line canvas (list x-1 y-1 x-2 y-2)))
	  (setf linea-2 (create-line canvas (list x-1 y-2 x-2 y-1)))
	  (itemconfigure canvas linea-1 :width 5)
	  (itemconfigure canvas linea-1 :fill "red")
	  (itemconfigure canvas linea-2 :width 5)
	  (itemconfigure canvas linea-2 :fill "red")))


      (configure canvas :width 180)
      (configure canvas :height 180)
      (configure canvas :background "white")

      (itemconfigure canvas linea-1 :width 5)
      (itemconfigure canvas linea-2 :width 5)
      (itemconfigure canvas linea-3 :width 5)
      (itemconfigure canvas linea-4 :width 5)

      (minsize *tk* 280 280)
      (maxsize *tk* 280 280)
      (wm-title *tk* "Tic Tac Toe")

      (configure *tk* :menu nuevo-menu)


      (place estado-juego 110 10)
      (place canvas 50 50))))



;;;; Notas de la version 3.0

;;;; Dudas:
;;;;
;;;;
;;;;
;;;; Se agrega:
;;;;
;;;;    * Funciones para eliminar los nodos raices de jugadas ganadoras para humano segun nivel, para los niveles impares
;;;;      (niveles pares: jugadas ordenador - niveles impares: jugadas humano)
;;;;
;;;; Falta:
;;;;
;;;;    * hay que comentar las lineas
;;;;
;;;;
;;;;
;;;;

;;;;; valores del evento: x = #S(EVENT :X 94 :Y 39
;;;;      :KEYCODE
;;;;      :CHAR
;;;;      :WIDTH
;;;;      :HEIGHT
;;;;      :ROOT-X 441
;;;;      :ROOT-Y 313
;;;;      :MOUSE-BUTTON 1
