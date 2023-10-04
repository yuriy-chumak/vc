#!/usr/bin/env ol
(import (lib c!))

; ====================================================================
(import (lang sexp))
(import (only (lang intern) string->uninterned-symbol))
(define (runes->number runes)
   (list->number runes 10))
(import (olvm syscalls))

(define (write-data data fd)
   (syscall 1 fd data))


(import (owl parse))

(define (api? format line handler)
   (let loop ((l (string->runes line)) (f (string->runes format)) (args #null) (types #null))
      (cond
         ((and (eq? l #null)  ; шаблон совпал
               (eq? f #null))
            (apply handler
               (map (lambda (p t)
                       ((if (eq? t #\?) runes->string runes->number)
                          (reverse
                             (let loop ((p p) (a #null))
                                (if (or (null? p) (eq? (car p) #\/))
                                   a
                                   (loop (cdr p) (cons (car p) a)))))))
                  (reverse args)
                  (reverse types)))
            #true)
         ((or (eq? l #null)  ; шаблон не совпал
              (eq? f #null))
            #false)
         ((eq? (car l) (car f))
            (loop (cdr l) (cdr f) args types))
         ((or (eq? (car f) #\?)   ; строка в шаблоне
              (eq? (car f) #\#))  ; число в шаблоне
            (let ((type (car f)))
               (let subloop ((p l))
                  (cond
                     ((null? p)
                        (loop p (cdr f) (cons l args) (cons type types)))
                     ((eq? (car p) #\/)
                        (loop p (cdr f) (cons l args) (cons type types)))
                     (else
                        (subloop (cdr p)))))))
         (else
            #false))))

(define has-two-dots? (string->regex "m/\\.\\./"))
(define (starts-with string sub)
   (if (> (string-length sub) (string-length string))
      #false
      (string-eq? (substring string 0 (string-length sub)) sub)))

; рабочий вариант, но много копипасты
(define-syntax REST
   (syntax-rules (else path api? args send-401
                  send-unauthorized send-forbidden send-not-implemented
                  session account url request
                  API URL GET PUT POST PATCH DELETE REQUEST)
      ((REST) #false)
      ((REST (else exp . rest))
         (begin exp . rest))        ; (begin ...)

      ((REST (API template (vars...) .body) .rest)
         (unless (api? template path
               (lambda (vars...)
                  (begin .body) ; ok?
                  (send-not-implemented)))
            (REST .rest)))
      ((REST (REQUEST request-type template (vars...) .body) .rest)
         (unless (and
               (string-eq? (ref request 1) request-type)
               (api? template path
                  (lambda (vars...)
                     .body)))
            (REST .rest)))

      ; специализации запросов
      ((REST (GET template (vars...) .body) .rest)
         (REST (REQUEST "GET" template (vars...) .body) .rest))
      ((REST (POST template (vars...) .body) .rest)
         (REST (REQUEST "POST" template (vars...) .body) .rest))
      ((REST (PUT template (vars...) .body) .rest)
         (REST (REQUEST "PUT" template (vars...) .body) .rest))
      ((REST (PATCH template (vars...) .body) .rest)
         (REST (REQUEST "PATCH" template (vars...) .body) .rest))
      ((REST (DELETE template (vars...) .body) .rest)
         (REST (REQUEST "DELETE" template (vars...) .body) .rest))

      ((REST (clause exp . rest-exps) .rest) ; any other clause
         (if clause
            ((lambda () exp . rest-exps)) ; (begin ...)
            (REST . rest)))))

; --------------------------------------------------------------------
; sha1
(import (lib sha1))
(import (otus ffi))

; --------------
; цвета
(define RED "\e[0;31m")
(define GREEN "\e[0;32m")
(define YELLOW "\e[0;33m")
(define BLUE "\e[0;34m")
(define MAGENTA "\e[0;35m")
(define CYAN "\e[0;36m")
(define WHITE "\e[0;37m")
(define END "\e[0;0m")
(define (LOGD . args)
   (apply print-to (cons stderr args)))
; --------------
(define (neq? x q) (not (eq? x q)))

; -=( http run 6002 )=-------------------------------------
(import (lib http)
      (file json))

(for-each (lambda (port) ; временно 6002, 6004, и 6006
(http:run port (lambda (fd request headers stream return)
   (LOGD "\nRequest: " BLUE request END)

   ; аналог функции write для сокета
   (define (send . args)
      (for-each (lambda (arg)
         (display-to fd arg)) args))

   (define (respond color status . args)
      (LOGD color "Sending " status END)
      (send "HTTP/1.0 " status "\r\n")
      (send "Content-Type: text/html"        "\r\n"
            "Access-Control-Allow-Origin: *" "\r\n" ; TEMP, allow any thirdparty clients to play
            "Cache-Control: no-store"        "\r\n"
            "Server: " (car *version*) "/" (cdr *version*) "\r\n"
            "Connection: close" "\r\n"
            "\r\n")
      (apply send args)
      (return #true))

   ; --------------------
   ; http/ response codes
   (define (send-200) (respond GREEN "200 OK" "{}"))              (define send-ok send-200) ; ({} во избежание ошибки парсинга json)
   (define (send-204) (respond GREEN "204 No Content"))           (define send-no-content send-204)
   (define (send-400) (respond RED   "400 Bad Request"))          (define send-bad-request send-400)
   (define (send-401) (respond RED   "401 Unauthorized"))         (define send-unauthorized send-401)
   (define (send-403) (respond RED   "403 Forbidden"))            (define send-forbidden send-403)
   (define (send-404) (respond RED   "404 Not Found"))            (define send-not-found send-404)
   (define (send-405) (respond RED   "405 Method Not Allowed"))   (define send-method-not-allowed send-405)
   (define (send-422) (respond RED   "422 Unprocessable Entity")) (define send-unprocessable-entity send-422)
   (define (send-500) (respond RED   "500 Internal Server Error"))(define send-internal-server-error send-500)
   (define (send-501) (respond RED   "501 Not Implemented"))      (define send-not-implemented send-501)

   ; отправить json в сокет
   (define (send-json json)
      (LOGD "Sending json (with 200 OK): " GREEN json END)
      (send "HTTP/1.0 " "200 OK" "\r\n")
      (send "Content-Type: application/json" "\r\n"
            "Access-Control-Allow-Origin: *" "\r\n" ; TESTING, allow any thirdparty clients to play
            "Cache-Control: no-store"        "\r\n"
            "Server: " (car *version*) "/" (cdr *version*) "\r\n"
            "Connection: close" "\r\n"
            "\r\n")
      (write-json json fd)
      (send "\r\n")
      (return #true))

   (define (send-data content-type data)
      (LOGD "Sending data (with 200 OK):")
      (send "HTTP/1.0 " "200 OK" "\r\n")
      (send "Content-Type: " content-type "\r\n"
            "Content-Length: " (size data) "\r\n"
            "Server: " (car *version*) "/" (cdr *version*) "\r\n"
            "Connection: close" "\r\n"
            "\r\n")
      (write-data data fd)
      (return #true))

   (define (send-exec content-type . args)
      (LOGD "Sending as '" content-type "' result of " GREEN args END)
      (send "HTTP/1.0 " "200 OK" "\r\n"
            "Content-Type: " content-type "\r\n"
            "Access-Control-Allow-Origin: *" "\r\n"
            "Server: " (car *version*) "/" (cdr *version*) "\r\n"
            "Connection: close" "\r\n"
            "\r\n")

      (define In (pipe))
      (define Out (pipe))

      (define pid (system (list "/bin/sh" "-c" (apply string-append args)) In Out))
      (close-pipe In) ; we don't use In rigth now
      (define out (port->string (car Out)))
      (close-pipe Out)

      (send out)
      #true)

   ; ========================================================================
   ; ------------------------------------------------------------------------
   ; основные переменные функции
   (let*((al (ref request 2))   ; al - address line
         (pu (http:parse-url al)) ; pu - parsed url
         (path (ref pu 1))
         (args (ref pu 2))

         (content-type (headers 'content-type #f))
         (content-length (headers 'content-length #f))
         (_ (print "content-type: " content-type))
         (_ (print "content-length: " content-length))

         (body stream (if content-length
            (uncons (try-parse (times (string->number content-length) byte) stream) #false)
            (values #false stream))))

      ; ------------------------------------------------------------
      (REST
         ; -------------------------------------
         ; Cross-Domain Access Processor
         ((string-eq? (ref request 1) "OPTIONS")
            (LOGD "Sending " GREEN "200 OK" END)
            (send "HTTP/1.0 " "200 OK" "\r\n")
            (send "Access-Control-Allow-Origin: *" "\r\n"
                  "Access-Control-Allow-Methods: GET,POST,PUT,PATCH,DELETE" "\r\n"
                  "Access-Control-Allow-Headers: Content-Type,X-Gallery-SID" "\r\n"
                  "Server: " (car *version*) "/" (cdr *version*)   "\r\n"
                  "Connection: close" "\r\n"
                  "\r\n")
            (return #true))

         ; Логин
         ; TODO: add sqlite extension function sha256
         ((and (string-eq? path "/live_webrtc_ingest_handshake")
               (string-eq? (ref request 1) "POST"))
            (let ((port (open-output-file "request.txt")))
               (print-to port (list->string body))
               (close-port port))
            (send-exec "application/json"
               "/usr/bin/curl -X POST 'https://upload.youtube.com/live_webrtc_ingest_handshake' -d @request.txt |tee response.txt"))
         (else
            (send-404)))))))
   ; current ports
   '(7002 7004 7006))
