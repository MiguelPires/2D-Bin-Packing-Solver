;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Grupo 15
;;;
;;; 	68593 - André Pires
;;; 	76433 - Miguel Pires
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :user)
(eval-when (:compile-toplevel :load-toplevel :execute)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;		Auxiliary Data
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Board variables
(defparameter *width* nil)
(defparameter *height* nil)
(defparameter *profundidade-max* nil)

;; Time keeping variables
(defparameter *max-time* 300)						; maximum time (in seconds)
(defparameter *time-percentage* 0.9)				; percentage of time we want don't want to exceed
(defparameter *initial-time* 0)						; initial time (in seconds)

;; Values for statistics and comparison
(defparameter *nos-expandidos* 0)
(defparameter *nos-gerados* 0)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 		Piece
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	piece : representacao das pecas a colocar no tabuleiro
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct piece
    width
    height
    position
    orientation))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	copia-peca: peca -> copia da peca original
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copia-peca (peca)
	(make-piece
		:width (piece-width peca)
		:height (piece-height peca)
		:position (piece-position peca)
		:orientation (piece-orientation peca)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	Position (of a piece)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;		make-pos: h v -> posicao com duas coordenadas
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-pos (h v) ;; h-horizontal v-vertical
 	(list h v))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	pos-h: pos -> coordenada horizontal da posicao fornecida
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pos-h (pos)
 	(first pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	pos-v: pos -> coordenada vertical da posicao fornecida
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pos-v (pos)
 	(second pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	pos=?: posicao1 posicao2 -> T caso as posicoes fornecidas sejam iguais;
;;;                             nil c.c.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pos=? (p1 p2)
 	(equal p1 p2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 		Estado
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	estado : representacao do estado utilizado pelas procuras
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct estado
	pecas-livres
	pecas-postas
	espacos-livres
	heuristica)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	copia-estado: estado -> copia do estado original
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copia-estado (estado)
	(make-estado
		:pecas-livres (copia-lista-pecas (estado-pecas-livres estado))
		:pecas-postas (copia-lista-pecas (estado-pecas-postas estado))
		:espacos-livres (copy-array (estado-espacos-livres estado))
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	copia-estado: estado -> copia do estado original
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copia-lista-pecas (lista)
	(let ((lista-copia nil))
		(dolist (peca lista)
			(push (copia-peca peca) lista-copia)
		)
		lista-copia
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;			Layout Problem
;;;
;;;  A layout problem consists of a list of pieces
;;; and a rectangle in which the pieces must be fitted
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	make-layout-problem: pieces rectangle -> layout do problema (rectangulo e pecas)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-layout-problem (pieces rectangle)
	(list pieces rectangle))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;			Rectangle
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	make-rectangle: width height -> rectangulo (lista com dimensoes do rectangulo)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rectangle (width height)
	(list width height))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	Search strategies
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; place-pieces: layout search -> solucao do problema (lista de pecas)
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun place-pieces (layout search)
	(let ((estado-resultado nil)
		 (problema (inicializa-programa layout)))

	(cond
		((or
			(equal search "profundidade")
			(equal search "profundidade-iterativa")
          	(equal search "largura"))
				; ordena as pecas da maior para a mais pequena
    			(sort (estado-pecas-livres (problema-estado-inicial problema)) #'comp-piece)
				(setf estado-resultado (basic-search problema search t)))
		((or
			(equal search "ida*")
          	(equal search "a*"))
				(setf estado-resultado (basic-search problema search t)))
        ((or
        	(equal search "a*.best.heuristic")
        	(equal search "best.approach.satisfaction"))
            	(setf estado-resultado (basic-search problema "a*" t)))
        ((equal search "a*.best.alternative.heuristic")
        	(setf (problema-heuristica problema) #'heuristica-fill)
        	(setf estado-resultado (basic-search problema "a*" t)))
        ((equal search "iterative.sampling.satisfaction")
            (setf estado-resultado (iss-satisfaction problema)))
        ((equal search "ILDS")
            (setf estado-resultado (ilds problema)))
        ((or
        	(equal search "best.approach.optimization")
        	(equal search "alternative.approach.optimization"))
        		(setf *height* (* 2 (calcula-altura-minima-possivel problema)))
            	(setf estado-resultado (a*-optimization problema)))
        ((equal search "iterative.sampling.optimization")
        	(setf *height* (* 2 (calcula-altura-minima-possivel problema)))
            (setf estado-resultado (iss-optimization problema)))
        (t (format t "The search strategy '~A' doesn't exist~%" search)
        	(return-from place-pieces)))

	;(desenha-tabuleiro (estado-pecas-postas estado-resultado))		; NOTA: desenha as pecas para debug visual
	;(format t "Nos expandidos: ~A~%" *nos-expandidos*)				; NOTA: estes valores permitem comparar a
	;(format t "Nos gerados: ~A~%" *nos-gerados*)					; performance dos algoritmos/heuristicas

	(cond ((null estado-resultado) nil)
		  (t (estado-pecas-postas estado-resultado)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	basic-search: problema search-strategy &optional arvore -> estado objectivo
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun basic-search (problema search-strategy &optional arvore)
	(let ((solucao  (procura problema search-strategy :espaco-em-arvore? arvore)))

	(cond ((or (null solucao) (null (first solucao))) nil)
		(t
			(incf *nos-expandidos* (third solucao))
			(incf *nos-gerados* (fourth solucao))
			(first (last (first solucao)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;		Auxiliary Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	inicializa-programa: layout -> problema
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inicializa-programa (layout)
	(let  ((estado-inicial))

		; inicializa parametros globais
		(setf *width* (first (second layout)))
		(setf *height* (second (second layout)))
		(setf *nos-gerados* 0)
		(setf *nos-expandidos* 0)
		(setf *initial-time* (get-internal-run-time))

		(setf estado-inicial (make-estado
	                            :pecas-livres (first layout)
								:pecas-postas '()
								:espacos-livres (make-array *width* :initial-element 0)
								:heuristica *height*))
		(cria-problema estado-inicial
	                    (list #'operador)
	                    :heuristica #'heuristica-media-espaco-livre
	                    :custo #'custo
	                    :objectivo? #'estado-objectivo-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	impossivel-resolver-p: lista-pecas -> T caso nao exista solucao para o problema
;;;                                       nil, c.c
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun impossivel-resolver-p (lista-pecas)
	(let ((area-total 0))

		(dolist (peca lista-pecas)
	  		(incf area-total (* (piece-width peca) (piece-height peca))))

		(if (> area-total (* *width* *height*))
			T
			nil)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	comp-piece: peca1 peca2 -> T caso a peca1 seja maior ou igual que a peca2
;;;                            nil, c.c
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun comp-piece (peca1 peca2)
	(>= (* (piece-height peca1) (piece-width peca1)) (* (piece-height peca2) (piece-width peca2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; exceeded-time-p: () -> T caso tenho sido ultrapassado o tempo limite
;;;                        nil, c.c
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun exceeded-time-p ()
  (let ((elapsed-time  (/ (- (get-internal-run-time)
                             *initial-time*)
                          internal-time-units-per-second)))
    (cond ((< elapsed-time (* *max-time* *time-percentage*)) nil)
          (t t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;		Support Functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	custo: estado -> custo de transitar de estado
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun custo (estado)
	(declare (ignore estado))
	(/ 1.0 (1+ *width*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;		Heuristics
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	heuristica-media-espaco-livre: estado -> valor heuristico do estado
;;;
;;; Descricao: Retorna a media do espaco que falta preencher para cada coluna
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heuristica-media-espaco-livre (estado)
	(let ((soma 0.0))

		(dotimes (index *width*)
			(incf soma (aref (estado-espacos-livres estado) index)))

		(setf (estado-heuristica estado) (- *height* (/ soma *width*)))

		(estado-heuristica estado)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	heuristica-fill: estado -> valor heuristico do estado
;;;
;;; Descricao: Retorna a diferenca entre a altura do tabuleiro com a coluna
;;; mais alta, somada com o numero de espacos vazios a esta altura a dividir
;;; pela largura fornecida.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heuristica-fill (estado)
	(let ((altura-coluna)
          (melhor-altura *height*)
          (espacos-vazios 0.0))

      	(dotimes (coluna *width*)
			(setf altura-coluna (aref (estado-espacos-livres estado) coluna))

            (cond ((< altura-coluna melhor-altura)
                	(setf melhor-altura altura-coluna)
                	(setf espacos-vazios 1.0))
            	  ((eq altura-coluna melhor-altura)
            	  	(incf espacos-vazios))))

		(setf (estado-heuristica estado) (+ (- *height* melhor-altura) (/ espacos-vazios *width*)))
		(estado-heuristica estado)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	heuristica-numero-colunas-altas: estado -> valor heuristico do estado
;;;
;;; Descricao: Retorna a diferenca da altura maxima da area fornecida com a
;;; altura da coluna mais alta, somada com a divisao do numero de colunas 'a
;;; altura da coluna mais alta mais a largura do tabuleiro e a largura do tabuleiro.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heuristica-numero-colunas-altas (estado)
	(let ((mais-alta -1)
			(altura 0)
			(colunas 0))

		(dotimes (index *width*)
			(setf altura (aref (estado-espacos-livres estado) index))

			(when (> altura mais-alta)
				(setf mais-alta altura)
				(setf colunas 1.0))
			(when (eq altura mais-alta)
				(incf colunas)))

	(+ (- *height* mais-alta) (/ (- *width* colunas) *width*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	heuristica-area-colunas-altas: estado -> valor heuristico do estado
;;;
;;; Descricao: Retorna a diferenca da area do tabuleiro fornecido e da area
;;; ocupada pelas colunas 'a altura mais alta.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun heuristica-area-colunas-altas (estado)
	(let ((mais-alta -1)
			(altura 0)
			(colunas 0))

		(dotimes (index *width*)
			(setf altura (aref (estado-espacos-livres estado) index))

			(when (> altura mais-alta)
				(setf mais-alta altura)
				(setf colunas 1.0))
			(when (eq altura mais-alta)
				(incf colunas)))

	(- (* *height* *width*) (* mais-alta colunas))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	operador: estado -> estados sucessores do estado fornecido
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun operador (estado)
	(let ((pecas-livres (estado-pecas-livres estado))
		  (novo-estado)
          (peca-a-testar)
          (width-a-testar)
          (height-a-testar)
		  (sucessores)
		  (sucessores-dimensoes) ;;guarda as dimensoes das pecas ja postas para nao gerar sucessores iguais
          (indice-init 0)
          (indice-fim -1)
          (coluna-adjacente)
          (altura-coluna)
          (ultima-altura *height*)
          (altura-actual *height*))

		;; I think this is probably the only two lines that aren't ANSI compliant
		(define-hash-table-test comparador-dimensoes dimensoes= hash-dimensoes)
		(setf sucessores-dimensoes (make-hash-table :test 'comparador-dimensoes))

		(if (exceeded-time-p)
			(return-from operador nil))

        ;; encontra um espaco vazio e tenta encaixar pecas nesse espaco
        (do () ((not (null sucessores)))
            (dotimes (coluna *width*)

				(setf altura-coluna (aref (estado-espacos-livres estado) coluna))

                (when (< altura-coluna altura-actual)
                    (setf altura-actual altura-coluna)
                    (setf indice-init coluna)
                    (setf indice-fim -1))			; significa que o espaco actual ainda nao foi fechado

                (when (and (> altura-coluna altura-actual) (eq indice-fim -1))
                    (setf indice-fim (1- coluna)))

                (when (and (eq coluna (1- *width*)) (eq indice-fim -1))
                    (setf indice-fim coluna)))

    		(dolist (peca pecas-livres)
                (do ((i 0 (incf i)))
                	((equal i 2))
                    (setf peca-a-testar (copia-peca peca))

                    (cond ((equal i 0)
                            (setf (piece-orientation peca-a-testar) :H)
                            (setf width-a-testar (piece-width peca))
                            (setf height-a-testar (piece-height peca)))

                        ((equal i 1)
                            (setf (piece-orientation peca-a-testar) :V)
                            (setf width-a-testar (piece-height peca))
                            (setf height-a-testar (piece-width peca))))

                    ; nao testa pecas com dimensoes ja testadas
        			(when (and (not (gethash peca-a-testar sucessores-dimensoes nil))
                             (>= (1+ (- indice-fim indice-init)) width-a-testar)
                             (>= (- *height* altura-actual) height-a-testar))

        				; cria novo estado
        			 	(setf novo-estado (copia-estado estado))

						; atribuiu posicao a' peca
        			 	(setf (piece-position peca-a-testar) (make-pos indice-init altura-actual))

        				(push peca-a-testar (estado-pecas-postas novo-estado))

        				(setf (estado-pecas-livres novo-estado)
        					(remove peca (estado-pecas-livres novo-estado) :test #'dimensoes= :count 1))

                        ; actualiza o vector de alturas para conter a nova peca
                        (do ((coluna indice-init (1+ coluna)))
                            ((eq coluna (+ indice-init width-a-testar)))
                            (incf (aref (estado-espacos-livres novo-estado) coluna) height-a-testar))

        				(push novo-estado sucessores)
        				; assinala que foi gerada uma peca com estas dimensoes
        				(setf (gethash peca-a-testar sucessores-dimensoes) t))))

			; se nenhuma peca cabia no espaco, o espaco e ignorado
			(when (null sucessores)
	            (cond ((and (< (1- indice-init) 0) (>= (1+ indice-fim) *width*)) (return))
	            	((< (1- indice-init) 0) (setf coluna-adjacente (1+ indice-fim)))
	            	((>= (1+ indice-fim) *width*) (setf coluna-adjacente (1- indice-init)))
	            	((< (aref (estado-espacos-livres estado) (1- indice-init)) (aref (estado-espacos-livres estado) (1+ indice-fim)))
	            		(setf coluna-adjacente (1- indice-init)))
	            	(t (setf coluna-adjacente (1+ indice-fim))))

                (setf ultima-altura (aref (estado-espacos-livres estado) coluna-adjacente))
				(do ((coluna indice-init (1+ coluna)))
					((> coluna indice-fim))
						(setf (aref (estado-espacos-livres estado) coluna) ultima-altura))

				; limpa o estado
				(setf altura-actual *height*)
				(setf indice-init 0)
				(setf indice-fim -1)))

        sucessores))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	estado-objectivo-p: estado -> T se o estado fornecido 'e o estado objectivo
;;;                               nil c.c.
;;;
;;; Nota: um estado objectivo foi atingido quando todas as pecas foram
;;; posta de forma valida
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun estado-objectivo-p (estado)
	(null (estado-pecas-livres estado)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; 	dimensoes=: peca1 peca2 -> T se as pecas forem semelhantes a nivel de espaco
;;;                             ocupado no tabuleiro
;;;                            nil c.c.
;;;
;;; Nota: compara duas pecas com base na sua orientacao/dimensoes
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dimensoes= (peca1 peca2)
	(or
		; se a orientacao das pecas for igual, testa a width com width e height com height
		(and (eq (piece-orientation peca1) (piece-orientation peca2))
			 (eq (piece-width peca1) (piece-width peca2))
			 (eq (piece-height peca1) (piece-height peca2)))
		; se as orientacoes forem diferentes, testa width com height
		(and (not (eq (piece-orientation peca1) (piece-orientation peca2)))
			 (eq (piece-width peca1) (piece-height peca2))
			 (eq (piece-height peca1) (piece-width peca2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;		hash-dimensoes: peca -> hash que corresponde a uma peca na hash table
;;;
;;; Descricao: esta funcao de hash faz o hashing com base numa lista com
;;; as dimensoes da peca dado que e' isso que queremos comparar
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun hash-dimensoes (peca)
	(cond ((eq (piece-orientation peca) :H)
			(sxhash (list (piece-width peca) (piece-height peca))))
			(t (sxhash (list (piece-height peca) (piece-width peca))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		Satisfaction Strategies
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		ILDS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ilds (problema)
    (let ((discrepancia -1)
          (estado-objectivo nil)
          (estado nil))

          (setf *profundidade-max* (length (estado-pecas-livres (problema-estado-inicial problema))))
          (do () ((or
                 	(< *profundidade-max* discrepancia)    	; se a discrepancia maxima foi atingida
                 	(exceeded-time-p)))                		; se o tempo de procura tiver sido excedido

            (incf discrepancia)
            (setf estado (copia-estado (problema-estado-inicial problema)))
            ; profundidade inicial de 0
            (setf estado-objectivo (ilds-expande-estado problema estado discrepancia discrepancia 0))

            ; se a procura encontrou um caminho retorna o mesmo
            (if estado-objectivo
            	(return-from ilds estado-objectivo)))))

(defun ilds-expande-estado (problema estado discrepancia-actual discrepancia-max profundidade)
    (let ((sucessores nil)
          (estado-objectivo nil)
          (estado-sucessor))

    		(incf *nos-expandidos*)
            (incf profundidade)
            (if (funcall (problema-objectivo? problema) estado)
            	(return-from ilds-expande-estado estado))

            (setf sucessores (ilds-gera-sucessores estado problema))

            (if (null sucessores)
                (return-from ilds-expande-estado nil))

           	(when (or (<= discrepancia-actual (- *profundidade-max* profundidade)) ; a discrepancia e sempre menor que a profundidade restante
                          (> (- *profundidade-max* profundidade) discrepancia-max))

            	(dotimes (indice (1+ discrepancia-actual))
            		(setf estado-sucessor (first (last (nth indice sucessores))))

            		(if (null estado-sucessor)											; nao existem mais sucessores
            			(return))

                    (setf estado-objectivo (ilds-expande-estado problema
                                                                estado-sucessor
                                                                (- discrepancia-actual indice)
                                                                discrepancia-max
                                                                profundidade))
                    (if estado-objectivo
                        (return-from ilds-expande-estado estado-objectivo))))

        (return-from ilds-expande-estado nil)))

(defun ilds-gera-sucessores (estado problema)
    (let* ((heuristica (problema-heuristica problema))
          (sucessores (funcall (first (problema-operadores problema)) estado))
       	  (sucessores-ordenados (mapcar #'(lambda(estado)
        										(list (funcall heuristica estado) estado))
        							sucessores)))			; guarda os sucessores com o seu valor de h

    	(incf *nos-gerados* (list-length sucessores))
    	(sort sucessores-ordenados #'< :key #'first)))		; ordena os sucessores por valor de h

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		Iterative Sampling - Satisfaction
;;
;; Nota: As funcoes de expansao e geracao de estados desta estrategia sao partilhadas
;; com a estrategia ISS mas com objectivo de optimizacao. Deste modo, as duas funcoes
;; estao junto da funcao iss-optimization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iss-satisfaction (problema)
    (let ((estado-objectivo nil)
          (estado nil))

     	(do () ((exceeded-time-p))   ; se o tempo de procura tiver sido excedido
	        (setf estado (copia-estado (problema-estado-inicial problema)))
	        (setf estado-objectivo (iss-expande-estado problema estado))

	        ; se a procura encontrou um caminho retorna o mesmo
	        (if estado-objectivo
	        	(return-from iss-satisfaction estado-objectivo)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		Estrategias de Optimizacao
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		calcula-altura-minima-possivel: problema -> menor altura possivel
;;
;; Descricao: Esta funcao divide a area (somada) das pecas pela largura de forma
;; a obter a altura minima em que estas podem ser colocadas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calcula-altura-minima-possivel (problema)
	(let ((melhor-altura-possivel)
          (area-total 0.0))

		; calcula a area ocupada por todas as pecas
    	(dolist (peca (estado-pecas-livres (problema-estado-inicial problema)))
    		(incf area-total (* (piece-width peca) (piece-height peca))))

    	; calcula a altura minima necessaria para colocar todas as pecas
    	(setf melhor-altura-possivel (multiple-value-bind
			(quotient)
			(ceiling (/ area-total *width*))
			quotient))

    	melhor-altura-possivel))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		A* com objectivo de optimizacao
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun a*-optimization (problema)
	(let ((melhor-altura (1+ *height*))
	      (melhor-solucao nil)
	      (altura-solucao 0)
	      (altura 0)
	      (solucao-actual nil)
	      (array-alturas)
	      (melhor-altura-possivel (calcula-altura-minima-possivel problema)))

	    (do () ((exceeded-time-p))               			; se o tempo de procura tiver sido excedido
    	    (setf solucao-actual (basic-search problema "a*" t))

	        (unless (null solucao-actual)
	        	(setf array-alturas (estado-espacos-livres solucao-actual))

	        	; determina a altura da coluna mais alta da solucao encontrada
	            (dotimes (index *width*)
	            	(setf altura (aref array-alturas index))
	            	(if (> altura altura-solucao)
	            		(setf altura-solucao altura)))

	            ; guarda a solucao se a sua maior altura for menor que a melhor encontrada ate agora
	            (when (< altura-solucao melhor-altura)
	            	(setf melhor-altura altura-solucao)
	            	(setf melhor-solucao solucao-actual)

	            	; se nao e' possivel obter um resultado mais compacto, termina a procura
	            	(if (eq melhor-altura melhor-altura-possivel)
	            		(progn (setf *height* melhor-altura-possivel) (return))
	            		(setf *height* (1- altura-solucao))))

	            (setf altura-solucao 0)))

    	melhor-solucao))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		Iterative Sampling - Optimizacao
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iss-optimization (problema)
    (let ((estado nil)
          (melhor-altura (1+ *height*))
          (melhor-solucao nil)
          (altura-solucao 0)
          (altura 0)
          (solucao-actual nil)
          (array-alturas)
          (melhor-altura-possivel (calcula-altura-minima-possivel problema)))

        (do () ((exceeded-time-p))         ; se o tempo de procura tiver sido excedido

            (setf estado (copia-estado (problema-estado-inicial problema)))
            (setf solucao-actual (iss-expande-estado problema estado))

            (unless (null solucao-actual)
            	(setf array-alturas (estado-espacos-livres solucao-actual))

            	; determina a altura da coluna mais alta da solucao encontrada
	            (dotimes (index *width*)
	            	(setf altura (aref array-alturas index))
	            	(if (> altura altura-solucao)
	            		(setf altura-solucao altura)))

	            ; guarda a solucao se a sua maior altura for menor que a melhor encontrada ate agora
	            (when (< altura-solucao melhor-altura)
	            	(setf melhor-altura altura-solucao)
	            	(setf melhor-solucao solucao-actual))

                (setf altura-solucao 0)
	            ; se nao e' possivel obter um resultado mais compacto, termina a procura
	            (if (eq melhor-altura melhor-altura-possivel)
	            	(return))))

    	melhor-solucao))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		Funcoes de Suporte para as procuras de ISS
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun iss-expande-estado (problema estado)
    (let ((sucessor nil)
          (estado-objectivo nil))

		(incf *nos-expandidos*)
        (if (funcall (problema-objectivo? problema) estado)			; testa se e' objectivo
        	(return-from iss-expande-estado estado))

        (setf sucessor (iss-gera-sucessor estado problema))			; gera sucessor aleatoriamente

        (if (null sucessor)											; se nao existir sucessor, falhou
            (return-from iss-expande-estado nil))

			(setf estado-objectivo (iss-expande-estado problema sucessor))	; continua a amostra

        (if estado-objectivo											; foi encontrada uma solucao
            (return-from iss-expande-estado estado-objectivo))

    	(return-from iss-expande-estado nil)))

(defun iss-gera-sucessor (estado problema)
    (let* ((sucessores (funcall (first (problema-operadores problema)) estado))
    	  (numSucessores (length sucessores)))

    	(incf *nos-gerados* numSucessores)
    	(cond ((> numSucessores 1) (nth (random numSucessores) sucessores))
    		  ((eq numSucessores 1) (first sucessores))
    		  (t nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		Funcoes de validacao do tabuleiro
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun desenha-tabuleiro (lista-pecas)
	(let* ((elemento-default "[]")
		(matrix (make-array (list *width* *height*) :initial-element elemento-default))
		(numPeca 1))

		(dolist (peca lista-pecas)
			(when (eq (piece-orientation peca) :H)
				(do ((coluna (first (piece-position peca)) (1+ coluna)))
					((eq coluna (+ (first (piece-position peca)) (piece-width peca))))
						(do ((linha (second (piece-position peca)) (1+ linha)))
							((eq linha (+ (second (piece-position peca)) (piece-height peca))))
								(setf (aref matrix coluna linha) numPeca))))

			(when (eq (piece-orientation peca) :V)
				(do ((coluna (first (piece-position peca)) (1+ coluna)))
					((eq coluna (+ (first (piece-position peca)) (piece-height peca))))
						(do ((linha (second (piece-position peca)) (1+ linha)))
							((eq linha (+ (second (piece-position peca)) (piece-width peca))))
								(setf (aref matrix coluna linha) numPeca))))

			(incf numPeca))

		(do ((linha (1- *height*) (1- linha)))
			((eq linha -1))
				(do ((coluna 0 (1+ coluna)))
					((eq coluna *width*))
						(cond ((and (not (equal (aref matrix coluna linha) elemento-default))
								(< (aref matrix coluna linha) 10))
								(format t " ~A " (aref matrix coluna linha)))
							(t (format t "~A " (aref matrix coluna linha)))))
			(format t "~%"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;		Test parameters
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;		Have solution
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter p1c '((#S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 13)))

(defparameter p2c '((#S(PIECE :WIDTH 4 :HEIGHT 4 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 13)))
(defparameter  p3c '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 15)))

(defparameter  p4c '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 17)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;		Have tight solution
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter p1b '((#S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 9)))

(defparameter p2b  '((#S(PIECE :WIDTH 4 :HEIGHT 4 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 10)))

(defparameter p3b '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 13)))

(defparameter  p4b '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 14)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;		Don't have a solution
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter p1a '((#S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 8)))

(defparameter  p2a '((#S(PIECE :WIDTH 4 :HEIGHT 4 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 5 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (17 9)))

(defparameter  p3a '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 12)))

(defparameter  p4a '((#S(PIECE :WIDTH 5 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 3 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 2 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 1 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 3 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 4 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL)
	  #S(PIECE :WIDTH 5 :HEIGHT 2 :POSITION NIL :ORIENTATION NIL))
       (15 13)))
