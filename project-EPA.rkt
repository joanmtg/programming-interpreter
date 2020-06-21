#lang eopl

; Integrantes                  Códigos
;
; Jhonier Andrés Calero Rodas  1424599
; Joan Manuel Tovar Guzmán     1423124
; Juan Pablo Moreno Muñoz      1423437
;
; Universidad del Valle
; Fundamentos de Lenguajes de Programación
; 2016
;

; BNF del Lenguaje

; <programa>  ::= <expresion>
; <expresion> ::= <identificador>
;             ::= <numero>
;             ::= <caracter>
;             ::= <cadena>
;             ::= <bool-expresion>
;             ::= ok
;             ::= let {<identificador> = <expresion>}*(,) in <expresion> end
;             ::= var {<identificador> = <expresion>}*(,) in <expresion> end
;             ::= let rec {<identificador> ({<identificador>}*(,)) = <expresion>}* in <expresion> end
;             ::= set <identificador> := <expresion>
;             ::= begin <expresion> ; <expresion> end
;             ::= <primitiva> ({<expresion>}*)
;             ::= if <expresion> then <expresion> {elseif <expresion> then <expresion>}* else <expresion> end
;             ::= proc ({<identificador>}*(,)) <expresion> end
;             ::= apply <identificador> ({<expresion>}*(,))
;             ::= meth ({identificador}, {<identificador>}*(,)) <expresion> end
;             ::= for <identificador> = <expresion> to <expresion> do <expresion> end
;             ::= object {<identificador> => <expresion>}* end
;             ::= get <identificador>.<identificador>
;             ::= send <identificador>.<identificador>({<expresion>}*(,))
;             ::= update <identificador>.<identificador> := <expresion>
;             ::= clone (<identificador>)
; <bool-expresion> ::= <bool-primitiva> ({<expresion>}*)
;                  ::= <bool-oper> ({<expresion>}*)
;                  ::= true
;                  ::= false
; <primitiva>      ::= + | - | * | / | % | &
; <bool-primitiva> ::= < | > | <= | >= | is
; <bool-oper>      ::= not | and | or


; Especificación Léxica

(define OBLIQ-spec-lex
'((white-sp
   (whitespace) skip)
  (comment
   (">>" (arbno (not #\newline))) skip)
  (caracter
   ("'" (or letter digit) "'") symbol)
  (cadena
   ( "\"" (arbno (or letter digit "?" " ")) "\"") string)   ; \" es la forma de meter una comilla dentro de comillas, si se pone """ explota XD
  (identificador                                             
   (letter (arbno (or letter digit "?"))) symbol)
  (numero
   (digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit)) number)
  (ok
     ("ok") symbol)
 ))

; Especificación Sintáctica (Gramática)

(define OBLIQ-grammar
  '((program (expresion) a-program)

    ;Variantes de expresion
    
    (expresion (numero) num-exp)
    (expresion (caracter) carac-exp)
    (expresion (identificador) id-exp)
    (expresion (cadena) cad-exp)
    (expresion (ok) ok-exp)
    (expresion (bool-expresion) bool-exp)

    (expresion (primitiva "(" (arbno expresion) ")") prim-exp)

    ;Gramática de let
    (expresion ("let" (separated-list identificador "=" expresion ",") "in" expresion "end") let-exp)
     
    ;Gramática de var
    (expresion ("var" (separated-list identificador "=" expresion ",") "in" expresion "end") var-exp)

    ;Gramática de let rec
    (expresion ("let rec" (arbno identificador "(" (separated-list identificador ",") ")"  "=" expresion) "in" expresion "end") letrec-exp)
    
    ;Gramática de if
    (expresion ("if" expresion "then" expresion (arbno "elseif" expresion "then" expresion) "else" expresion "end") if-exp)

    ;Gramática de set
    (expresion ("set" identificador ":=" expresion) set-exp)
    
    ;Gramática de begin
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    
    ;Gramática de proc
    (expresion ("proc"  "(" (separated-list identificador ",") ")" expresion "end") proc-exp)
    
    ;Gramática de apply
    (expresion ("apply" identificador "(" (separated-list expresion ",") ")") apply-exp)
    
    ;Gramática de for
    (expresion ("for" identificador "=" expresion "to" expresion "do" expresion "end") for-exp)

    ;Variantes de primitiva
            
    (primitiva ("+") add-prim)
    (primitiva ("-") substract-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") remainder-prim) 
    (primitiva ("&") append-prim)      


    ;Variantes de bool-expresion

    (bool-expresion (bool-primitiva "(" (arbno expresion) ")") bool-exp-prim)
    (bool-expresion (bool-oper "(" (arbno expresion) ")") bool-exp-oper)  
    (bool-expresion ("true") bool-exp-true)
    (bool-expresion ("false") bool-exp-false)


    ;Variantes de bool-primitiva
    
    (bool-primitiva ("<") menor-prim)
    (bool-primitiva (">") mayor-prim)
    (bool-primitiva ("<=") menorIgual-prim)
    (bool-primitiva (">=") mayorIgual-prim)
    (bool-primitiva ("is") is-prim)


    ;Variantes de bool-oper
    
    (bool-oper ("not") not-oper)
    (bool-oper ("and") and-oper)
    (bool-oper ("or") or-oper)
    

    ;Gramática de las expresiones relacionadas con Objetos

    (expresion ("meth" "(" identificador (arbno "," identificador) ")" expresion "end") meth-exp)
    (expresion ("object" "{" (arbno identificador "=>" expresion) "}" "end") obj-exp)
    (expresion ("get" identificador "." identificador) get-exp)
    (expresion ("send" identificador "." identificador "(" (separated-list expresion ",") ")" ) send-exp)
    (expresion ("update" identificador "." identificador ":=" expresion) update-exp)
    (expresion ("clone" "(" identificador ")") clone-exp)
    
    ))


;Construidos automáticamente:

(sllgen:make-define-datatypes OBLIQ-spec-lex OBLIQ-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes OBLIQ-spec-lex OBLIQ-grammar)))

;*******************************************************************************************
;Parser, Scanner, Interfaz

;El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)

(define scan&parse
  (sllgen:make-string-parser OBLIQ-spec-lex OBLIQ-grammar))

;El Analizador Léxico (Scanner)

(define just-scan
  (sllgen:make-string-scanner OBLIQ-spec-lex OBLIQ-grammar))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm))
    (sllgen:make-stream-parser 
      OBLIQ-spec-lex
      OBLIQ-grammar)))

;*******************************************************************************************
;El Intérprete

;eval-program: <programa> -> numero
; función que evalúa un programa teniendo en cuenta un ambiente dado (se inicializa dentro del programa)

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expresion body (amb-inicial)))
      )
    )
  )

; Ambiente inicial
(define amb-inicial
  (lambda ()
    (extend-amb
     '(w x y)
      '(3 4 7)
     'let
     (empty-amb)
     )))

; eval-expresion: <expresion> <ambiente> -> numero | bool | caracter | string | <metodo> | <objeto>
; evalua la expresión en el ambiente de entrada

(define eval-expresion
  (lambda (exp amb)
    (cases expresion exp
      (num-exp (datum) datum)
      (id-exp (id) (apply-amb amb id))
      (carac-exp (caracter) caracter)
      (cad-exp (cadena) (substring cadena 1 (- (string-length cadena) 1)))  ;Ejemplo, entra "\"abc\"" y se conserva lo de la mitad
      (ok-exp (ok) ok)
      (bool-exp (exp)
                (eval-bool-expresion exp amb))

      ;prim-exp
      ;Se recibe una primitiva y los argumentos
      ;Se llama la función auxiliar aplicar-prim pasándole
      ;la primitiva y los argumentos evaluados

      (prim-exp (primitiva args)
                (aplicar-prim primitiva (eval-expresions args amb)))

      ;let-exp
      ;Se evalúa el cuerpo del let en un nuevo ambiente
      ;Dicho ambiente contiene los ids, los valores asociados
      ;a los ids, el tipo del ambiente ('let) y el ambiente viejo.
      
      (let-exp (ids rands expresion)
               (let ((valores (eval-expresions rands amb)))
                 (eval-expresion expresion (extend-amb ids valores 'let amb))))

      ;var-exp
      ;Se evalúa el cuerpo del var en un nuevo ambiente
      ;Dicho ambiente contiene los ids, los valores asociados
      ;a los ids, el tipo del ambiente ('var) y el ambiente viejo.
      
      (var-exp (ids rands expresion)
               (let ((valores (eval-expresions rands amb)))
                 (eval-expresion expresion (extend-amb ids valores 'var amb))))

      ;letrec-exp
      ;Se evalúa el cuerpo del letrec en un ambiente extendido recursivamente
      
      (letrec-exp (proc-names idss bodies exp)
                  (eval-expresion exp (extend-amb-recursively proc-names idss bodies amb)))

      ;if-exp
      ;Revisa si la primera condición es un boolean
      ;Si es un boolean y es #t, evalúa el primer
      ;resultado en el ambiente.

      ;Si la primera condición es #f, llama una
      ;función auxiliar que evalúa los elseif's
      
      (if-exp (condicion resultado lcondiciones lresultados else)
              (let ((first-cond (eval-expresion condicion amb)))
                (if (not (boolean? first-cond)) (eopl:error "La primera expresión no es un boolean")
                    (if first-cond
                        (eval-expresion resultado amb)
                        (evaluar-elseif lcondiciones lresultados else amb)
                        )
                    )
                ))


      ;set-exp

      (set-exp (sym exp)
               (begin
                 (setref!                   ;Aplica setref! a:
                  (apply-amb-ref amb sym)   ;Una referencia con el símbolo ingresado
                  (eval-expresion exp amb)) ;El nuevo valor a tomar
               (eval-expresion (ok-exp 'ok) amb)))

      ;begin-exp
      ;Llama una función auxiliar aux-begin con la primera expresión,
      ;la lista de expresiones y el ambiente.
      (begin-exp (exp exps)
                 (aux-begin exp exps amb))

      ;proc-exp
      ;Crea un proced que contiene los ids ingresados, el cuerpo y el ambiente
      (proc-exp (ids body)
                (closure ids body amb))

      ;apply-exp
      ;Busca el valor asociado al "proc-name" en el ambiente y evalúa los argumentos en el ambiente.
      ;Si lo que se encontró fue un proced, llama una función auxiliar apply-proced con el procedimiento y los argumentos
      ;Si lo que se encontró no es un proced, manda error
      
      (apply-exp (proc-name args)
                 (let ((proc (apply-amb amb proc-name))
                       (rands (eval-expresions args amb)))
                 (if (proced? proc)
                     (apply-proced proc rands)
                     (eopl:error 'eval-expression "~s no es un procedimiento" proc))))
      ;for-exp
      ;Llama una funcción auxiliar aux-for con:
      ; *El id del contador
      ; *La condición final evaluada en el ambiente
      ; *El cuerpo del for
      ; *Un nuevo ambiente que contiene:
      ;     *Una lista de ids que contiene el id del contador
      ;     *Un vector que contiene el valor inicial del contador
      ;     *El tipo del ambiente: 'var
      ;     *El ambiente viejo
      
      (for-exp (id-contador inicial final exp)
               (aux-for id-contador
                        (eval-expresion final amb)
                        exp
                        (extend-amb (list id-contador) (list (eval-expresion inicial amb)) 'var amb)))

      ;meth-exp
      ;Crea un método con los ids y el cuerpo
      
      (meth-exp (self-id ids body)
                (a-method (cons self-id ids) body))

      ;obj-exp
      ;Crea un objeto con los ids y un vector relacionado con los ids.
      
      (obj-exp (ids campos)
               (an-object ids (list->vector (eval-expresions campos amb)))) 

      ;get-exp
      
      (get-exp (obj-name field-name)
               (let ((obj (apply-amb amb obj-name)))  ;Busca el objeto en el ambiente
                 (if (objeto? obj)                    ;Revisa si lo encontrado es un objeto
                     (get-field obj field-name)       ;Llama la función get-field con el objeto y el nombre del campo
                     (eopl:error 'eval-expresion "~s no es un objeto" obj-name)))) ;error en caso de que no sea un objeto

      ;send-exp
      
      (send-exp (obj-name meth-name rands)
                (let ((obj (apply-amb amb obj-name))      ;Busca el objeto en el ambiente
                      (args (eval-expresions rands amb))) ;Evalúa los rands en el ambiente
                  (if (objeto? obj)                       ;Revisa si lo encontrado es un objeto 
                      (let ((method (get-field obj meth-name)))  ;Extrae el valor del campo del objeto
                        (if (metodo? method)                     ;Revisa si lo obtenido es un método
                            (apply-method method obj args amb)   ;Llama apply-method con el método, el objeto, los args y el ambiente
                            (eopl:error 'eval-expresion "~s no es un método" meth-name))) ;error en caso de no ser un método
                      (eopl:error 'eval-expresion "~s no es un objeto" obj-name))))       ;error en caso de no ser un objeto

      ;upadte-exp
      
      (update-exp (obj-name field-name new-value)
                  (let ((obj (apply-amb amb obj-name)))  ;Busca el objeto en el ambiente
                    (if (objeto? obj)                    ;Revisa si lo obtenido es un objeto
                        (begin
                          (setref!                               ;Aplica setref a
                           (get-field-reference obj field-name)  ;Una referencia al campo del objeto
                           (eval-expresion new-value amb))       ;El valor que tomará el campo
                          (eval-expresion (ok-exp 'ok) amb)
                          )
                        (eopl:error 'eval-expresion "~s no es un objeto" obj-name)))) ;error en caso de no ser un objeto

      ;clone-exp
      
      (clone-exp (obj-name)
                 (let ((obj (apply-amb amb obj-name)))  ;Busca el objeto en el ambiente
                   (if (objeto? obj)                    ;Revisa si lo obtenido es un objeto
                       (clone obj)                      ;Aplica la función auxiliar clone al objeto
                        (eopl:error 'eval-expresion "~s no es un objeto" obj-name)))) ;error en caso de no ser un objeto
      )))


; eval-expressions: <list-of expresion> <ambiente> -> <list-of valor>
; Aplica eval-expresion a cada uno de las expresiones de una lista.

(define eval-expresions
  (lambda (listExps amb)
    (map (lambda (exp) (eval-expresion exp amb)) listExps)))

; clone: <objeto> -> <objeto>
; Recibe un objeto y devuelve una copia de éste
; Sus campos son iguales, pero el vector que contiene
; los valores del nuevo objeto es independiente
; del vecotr de valores del objeto original

(define clone
  (lambda (obj)
    (cases objeto obj
      (an-object (campos valores)
                 (let ((lista-valores (vector->list valores)))   ;Convierte el vector a una lista
                   (an-object campos (list->vector lista-valores))))) ;Crea un nuevo objeto con los campos y un nuevo vector
    )
  )


; aux-for: <identificador> <number> <expresion> <ambiente> -> ok
; Función que recibe un identificador, un número, una expresión y un ambiente
; Evalúa las expresiones y si el valor al cual hace referencia el id es igual 
; al valor de final retorna un ok y termina de evaluar las expresiones, en caso  
; contrario sumará 1 al el valor de id y seguirá evaluando las expresiones.

(define aux-for
  (lambda (id final exp amb)
    (cond
      [(> (apply-amb amb id) final) (eval-expresion (ok-exp 'ok) amb)]
      [else (begin (display (eval-expresion exp amb)) (display "\n") (aux-for id final exp (add1 id amb)))]
      )
    )
  )

; add1: <identificador> <ambiente> -> <number>
; Función que recibe un identficador y un ambiente.
; Busca el valor al cual hacer referencia el identificador en el ambiente, le 
; suma uno a dichio valor, y por ende se modifica el valor de la referencia,
; con el nuevo valor y el ambiente.

(define add1
  (lambda (id amb)
    (let ((ref (apply-amb-ref amb id)))
      (let ((new-value  (+ (deref ref) 1) ))
        (begin (setref! ref new-value) amb)))))
    


; aux-begin: <expresion> <list-of expresion> <ambiente> -> <valor>
; Evalúa todas las expresiones recursivamente
; Primero evalúa el primer parámetro.
; Si la lista es vacía, devuelve la primera expresión evaluada en el ambiente.

(define aux-begin
  (lambda (exp exps amb)
    (let ((exp (eval-expresion exp amb)))
      (if (null? exps)
          exp
          (aux-begin (car exps) (cdr exps) amb)       
       )
      )
    )
  ) 

; evaluar-elseif: <list-of expresion> <list-of expresion> <expresion> <ambiente> -> <valor>
; Función que evalúa cada una de las expresiones en una lista de expresiones (condiciones).
; y de la lista de resultados, en caso de que la lista sea vacía, evaluará la expresión else-exp. 

(define evaluar-elseif
  (lambda (lconditions lresults else-exp ambiente)
    (cond
      [(null? lconditions) (eval-expresion else-exp ambiente)]
      [(eval-expresion (car lconditions) ambiente) (eval-expresion (car lresults) ambiente)]
      [else (evaluar-elseif (cdr lconditions) (cdr lresults) else-exp ambiente)]
      )))

; eval-bool-expresion: <bool-expresion> <ambiente> -> <boolean>
; Función que recibe un bool-expresion, un ambiente
; y evalúa evalúa la bool-expresion en el ambiente,
; teniendo en cuenta las variantes de el tipo de dato bool-expresion

(define eval-bool-expresion
  (lambda (exp amb)
    (cases bool-expresion exp

      ;Caso de bool-exp-prim
      ;Llama una función auxiliar para aplicar la primitiva a los argumentos
      (bool-exp-prim (bool-prim args)
                     (aplicar-bool-prim bool-prim (eval-expresions args amb)))

      ;Caso de bool-exp-oper
      ;Llama una función auxiliar para aplicar el oper a los argumentos
      (bool-exp-oper (bool-oper args)
                     (aplicar-bool-oper bool-oper (eval-expresions args amb)))

      (bool-exp-true () #t)  ;Devuelve #t
      (bool-exp-false () #f) ;Devuelve #f
      )))


;eval-bool-expresions : <list-of bool-expresion> <ambiente> -> lista
;Aplica eval-bool-expresion a cada uno de las bool-expresions de una lista

(define eval-bool-expresions
  (lambda (listExps amb)
    (map (lambda (exp) (eval-bool-expresion exp amb)) listExps)))


; aplicar-prim: <primitiva> <list> ->  ( <number> | <cadena> )
; Recibe una primitiva (+, -, *, /, % ó &), una lista de argumentos y aplica
; la primitiva a estos argumentos
;
; Por ejemplo:
; Si recibe (add-prim) y '(3 4), devuelve (+ 3 4) = 7
; Si recibe (append-prim) y '("Hallo " "Welt"), devuelve (append "Hallo " "Welt") = "Hallo Welt"

(define aplicar-prim
  (lambda (prim args)
    (cases primitiva prim
      (add-prim ()(+ (car args) (cadr args)))
      (substract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (div-prim () (/ (car args) (cadr args)))
      (remainder-prim () (remainder (car args) (cadr args)))
      (append-prim() (string-append (car args) (cadr args))))
    )
  )

; aplicar-bool-prim: <bool-primitiva> <list-of number> -> <boolean>
; Aplica un bool-primitiva (>, <, <=, >= ó =) a una lista de argumentos de tipo entero
;
; Por ejemplo:
; Si recibe (menor-prim) y '(3 4) devuelve (< 3 4) = #t
; Si recibe (is-prim) y '(7 6) devuelve  (= 7 6) = #f 

(define aplicar-bool-prim
  (lambda (bool-prim args)
    (cases bool-primitiva bool-prim
      (menor-prim () (< (car args) (cadr args)))
      (mayor-prim () (> (car args) (cadr args)))
      (menorIgual-prim () (<= (car args) (cadr args)))
      (mayorIgual-prim () (>= (car args) (cadr args)))
      (is-prim () (= (car args) (cadr args)))

      )))


; aplicar-bool-oper: <bool-oper> <list-of boolean> -> <boolean>
; Aplica un bool-oper (or, not ó and) a una lista de argumentos
;
; Por ejemplo:
; Si recibe (not-oper) y '(#f) devuelve (not #f) = #t
; Si recibe (and-oper) y '(#t #f) devuelve (and #t #f) = #f
; Si recibe (or-oper) y '(#t #f) devuelve (or #t #f) = #t

(define aplicar-bool-oper
  (lambda (bool-operando args)

    ; Si hay algún dato que no sea boolean, manda error  
    (if (not (bool-list? args)) (eopl:error 'aplicar-bool-oper "Ha ingresado un dato que no es boolean")
    
        (cases bool-oper bool-operando
          (not-oper () (not (car args)))
          (and-oper () (and (car args) (cadr args)))
          (or-oper () (or (car args) (cadr args)))
          )
        )
    )
  )

; bool-list?: <list> -> boolean
; Devuelve true si todos los elementos de la lista son booleans
; De lo contrario, devuelve #f

(define bool-list?
  (lambda (lista)
    (cond [(null? lista) #t]
          [(not (boolean? (car lista))) #f]
          [else (bool-list? (cdr lista))]
          )
    )
  )


;Procedimientos
;<proced> ::= ({<symbol>}* <expresion> <ambiente>)
;             <closure (ids body amb)>

(define-datatype proced proced?
  (closure
   (ids (list-of symbol?)) ; Campo para los identificadores de los argumentos que recibe el procedimiento
   (body expresion?) ; Campo para el cuerpo del procedimiento
   (amb ambiente?)) ; Campo para el ambiente en el cual se va a evaluar el procedimiento
  )

; apply-proced: <proced> <lista> -> <valor>
; Recibe un procedimiento y una lista de argumentos.
; Evalúa el cuerpo del procedimiento en un nuevo ambiente.
; El nuevo ambiente contendrá:
;  *Los ids del método
;  *Los argumentos (los valores relacionados a los ids
;  *El tipo del ambiente: 'var
;  *El ambiente viejo

(define apply-proced
  (lambda (proc rands)
    (cases proced proc
      (closure (ids body amb)
               (eval-expresion body (extend-amb ids rands 'var amb)))
      )
    )
  )


; Ambientes
;
; <ambiente> ::= ()
;                <empty-amb-record()>
;            ::= ({<symbol>}* <vector> <symbol> <ambiente>)
;                <extended-amb-record (syms vec type amb)>
;
; Definición del tipo de dato ambiente

(define-datatype ambiente ambiente?
  (empty-amb-record) ; Instancia para representar el ambiente vacío
  (extended-amb-record (syms (list-of symbol?)) ; Campo para la lista de los ids
                       (vec vector?) ; Campo para el vector de valores correspondientes a los ids
                       (type symbol?) ; Campo para saber si el ambiente es de tipo 'var o 'let
                       (amb ambiente?) ; Campo para saber el ambiente del cual extiende el que se va a crear
                       ))


; empty-amb:      -> <ambiente>
; Función que crea un ambiente vacío

(define empty-amb  
  (lambda ()
    (empty-amb-record)))       ;llamado al constructor de ambiente vacío 


; extend-amb: <list-of identificadores> <list-of valores> <ambiente> -> <ambiente>
; Función que crea un ambiente extendido
; Recibe una lista de identificadores, una lista de valores, un símbolo que representa
; el tipo del ambiente a crearse (let o var) y un ambiente. Lo que hace esta función
; es crear un ambiente de que contiene:

; • Una lista con los identificadores que entran como parámetro
; • Un vector que se crea con la lista de valores
; • El símbolo que representa el tipo de ambiente
; • El ambiente viejo, es decir, del cual extiende este nuevo ambiente


(define extend-amb
  (lambda (syms vals type amb)
    (extended-amb-record syms (list->vector vals) type amb)))


; extend-amb-recursively: <list-of symbols> <list-of <list-of symbols>> <list-of expresion> <ambiente> -> <ambiente>
; Función que crea un ambiente extendido para procedimientos recursivos
;
; Esta función recibe:
; • Una lista de identificadores (los nombres de los procedimientos)
; • Una lista de listas de identificadores (una lista con las listas de argumentos de cada procedimiento
; • Una lista de expresiones (los cuerpos de los procedimientos)
; • Un ambiente

; Lo que hace esta función es crear un nuevo ambiente con:
; • Una lista con los nombres de los procedimientos
; • Una vector de procedimientos, donde cada procedimiento contiene un ambiente para conocerse a sí mismo y a los demás métodos declarados en un let rec
; • El tipo del ambiente (en este caso let)
; • El ambiente viejo (del que extiende el nuevo ambiente


(define extend-amb-recursively
  (lambda (proc-names idss bodies old-amb)
    (let ((len (length proc-names)))        
      (let ((vec (make-vector len)))
        (let ((amb (extended-amb-record proc-names vec 'let old-amb)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (closure ids body amb)))
            (iota len) idss bodies)
          amb)))))

; iota: number -> list
; Función que retorna una lista de los números desde 0 hasta end

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))


; apply-amb: <identificador> <ambiente> -> <valor>
; Función que busca un símbolo en el ambiente y devuelve su valor
; Primero llama apply-amb-ref con el ambiente y el símbolo, y
; a la referencia que sale de ahí, le aplica la función deref,
; lo que extrae el valor de la referencia

(define apply-amb
  (lambda (amb sym)
    (deref (apply-amb-ref amb sym))))


; apply-amb-ref: <ambiente> <identificador> -> <reference>
; 
; Función que busca un símbolo en un ambiente y
; devuelve una referencia que contiene:
;
; *La posición del valor en el vector del ambiente
; *El vector de valores
; *El tipo del ambiente donde se guardó el valor ('let o 'var)

(define apply-amb-ref
  (lambda (amb sym)
    (cases ambiente amb
      (empty-amb-record ()
                        (eopl:error 'apply-amb "No binding for ~s" sym))
      (extended-amb-record (syms vec type amb)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vec type)                                  
                                 (apply-amb-ref amb sym)))))))

; Referencias
;
; <reference> ::= (<numero> <vector> <symbol>)
;                 <a-ref (position vec type)>
;
;Definición del datatype reference

(define-datatype reference reference?
  (a-ref (position integer?) ; Campo para la posición del vector en el que está la referencia
         (vec vector?) ; Campo para los valores
         (type symbol?)  ; Campo para el tipo ('var o 'let)
         ))

; Función deref
; deref: <reference> -> <expresion>
;
; Propósito: Función que recibe una referencia y devuelve
; el valor al que se está referenciando.

(define deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec type)
             (vector-ref vec pos)))))

; Función setref!
; setref!: <reference> <valor> -> <number>
;
; Recibe una referencia y un valor. En caso de que el tipo de la referencia sea de tipo var, 
; cambia el valor de la posición de la referencia en el vector al valor recibido como segundo parámetro.
; De resto, es decir, si es de tipo let, no se cambia el valor.

(define setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec type)
             (if (equal? type 'var)
                 (vector-set! vec pos val)
                 1)))))


;****************************************************************************************
;Funciones Auxiliares

; Funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

; Función list-find-position
; list-find-position: <identificador> <list-of identificadores>  -> (<number>|<bool>)

; Propósito: Función que busca un identificador en una lista y
; devuelve la posición en la cual se encuentra.

(define list-find-position
  (lambda (sym lista)
    (aux-list-find-position sym lista 0)))

; Función aux-list-find-position
; aux-list-find-position: <identificador> <list-of identificadores> <number> -> (<number>|<bool>)

; Propósito: Función que busca un identificador en una lista y
; con la ayuda de un contador devuelve la posición en la que se
; encuentra

(define aux-list-find-position
  (lambda (sym lista contador)
    (cond [(null? lista) #f]
          [(equal? sym (car lista)) contador]
          [else (aux-list-find-position sym (cdr lista) (+ contador 1))]
          )
    )
  )
        

; Objetos
;
; <objeto> ::= ({<symbol>}* <vector>)
;              <an-object (campos valores)>
;

(define-datatype objeto objeto?
  (an-object
   (campos (list-of symbol?))  ;ids de los campos del objeto
   (valores vector?)))         ;valores de dichos campos


; Métodos
;
; <metodo> ::= ({<symbol>}* expresion)
;              <a-method (ids body)>
;

(define-datatype metodo metodo?
  (a-method
   (ids (list-of symbol?))   ;ids de los argumentos del método
   (body expresion?)))       ;cuerpo del método


; Función get-field
; get-field: <objeto> <identificador> -> <>
;
; Propósito: Función que  recibe un objeto, un id y devuelve
; el valor correspondiente del campo en el objeto.

(define get-field
  (lambda (obj id)
    (deref (get-field-reference obj id))))

; Función get-field-reference
; get-field-reference: <objeto> <identificador> -> <reference>
;
; Propósito: Función que recibe un objeto, un id y devuelve una
; referencia que contiene:
; * La posición del campo "id" en el objeto
; * El vector que contiene los valores de los campos del objeto.
; * El tipo de la referencia, que es var

(define get-field-reference
  (lambda (obj id)
    (cases objeto obj
      (an-object (campos valores)
                 (let ((pos (list-find-position id campos)))
                   (if (number? pos)
                       (a-ref pos valores 'var)
                       (eopl:error 'get-field "El campo ~s no existe en el objeto" id))))
      )
    
    )
  )


; Función apply-method
; apply-method: <method> <objeto> <list> <ambiente> -> <valor>
;
; Propósito: Función que recibe:
;  *Un método (en sintaxis abstracta)
;  *Un objeto (el self)
;  *Los rands a los cuales se les va a aplicar el método
;  *El ambiente viejo
;
; Lo que hace es evaluar el cuerpo del método en un nuevo ambiente.
; El nuevo ambiente contiene:
;  *Los ids del método
;  *Un vector donde el primero es el self y el resto los rands
;  *Tipo var 
;  *El ambiente viejo

(define apply-method
  (lambda (method objeto rands amb)
    (cases metodo method
      (a-method (ids body)
                (eval-expresion body (extend-amb ids (cons objeto rands) 'var amb)))
      )
    )
  )
                       

(interpretador)
