(defmodule ipxerlay
  ;(behavior 'gen_server)
  
  ;(export
   ;(init 1)
   ;(handle_info 2)
   ;(terminate 2))
  (export
   (start_link 1)
   (test_call 0)

   (process_msg 2)))

(defrecord settings
  (udp-options (list 'binary #(active false) #(recbuf 65536) #(reuseaddr true))))

(defrecord ipx-socket
  (network 0)
  addr
  port)
(defrecord ipx-header
  (checksum #xffff)
  (len 30)
  (routed? 0)
  (type 0)
  dst-addr
  dst-sock
  src-addr
  src-sock)

(defun pack
  ((struct) (when (is-ipx-socket struct))
   (binary
    ((ipx-socket-network struct) (size 4) (unit 8))
    ((ipx-socket-addr struct) (size 4) (unit 8))
    ((ipx-socket-port struct) (size 2) (unit 8))
    ))
  ((struct) (when (is-ipx-header struct))
   (binary
    ((ipx-header-checksum struct) (size 2) (unit 8))
    ((ipx-header-len struct) (size 2) (unit 8))
    ((ipx-header-routed? struct) (size 8))
    ((ipx-header-type struct) (size 8))
    ((pack (ipx-header-dst-addr struct)) binary)
    ((ipx-header-dst-sock struct) (size 2) (unit 8))
    ((pack (ipx-header-src-addr struct)) binary)
    ((ipx-header-src-sock struct) (size 2) (unit 8)))))
(defun unpack
  ((bin) (when (== (byte_size bin) 10)) (let (((binary
	  (n (size 4) (unit 8))
	  (a (size 4) (unit 8))
	  (p (size 2) (unit 8)))
	 bin))
    (make-ipx-socket network n addr a port p)))
  ((bin) (when (== (byte_size bin) 30)) (let (((binary
	  (c (size 2) (unit 8))
	  (l (size 2) (unit 8))
	  (r? (size 8))
	  (t (size 8))
	  (dp binary (size 10))
	  (d (size 2) (unit 8))
	  (sp binary (size 10))
	  (s (size 2) (unit 8)))
	 bin))
    (make-ipx-header checksum c len l routed? r? type t dst-addr
		     (unpack dp) dst-sock d src-addr (unpack sp) src-sock s))))
(defun test_call ()
  (let ((socket (make-ipx-socket
		 addr #xffffffff
		 port #xffff)))
    (: io format '"~p~n" (list (unpack (pack
	     (make-ipx-header
	      dst-addr socket
	      dst-sock 0
	      src-addr socket
	      src-sock 0)))))))
;(defun do-tests ()
;  (andalso (== (byte_size (pack (make-ipx-socket))) 10)
;	   (== (byte_size (pack (make-ipx-header))) 30)))
(defun check-packet (header size)
  (let ((len (ipx-header-len header)))
     (andalso (== (ipx-header-checksum header) #xffff)
	   (>= len 30) (=< len size)
	   (== (bor (ipx-socket-network (ipx-header-src-addr header))
		    (ipx-socket-network (ipx-header-dst-addr header)))
	       (ipx-header-routed? header) 0))))
(defun nil-socket? (socket)
  (== (ipx-socket-addr socket) (ipx-socket-port socket) 0))
(defun broadcast-socket? (socket)
  (andalso (== (ipx-socket-addr socket) #xffffffff)
	   (== (ipx-socket-port socket) #xffff)))

; to-do: ipv6
(defun ip-to-int
  ((ip) (when (== (tuple_size ip) 4))
   (let (((tuple a b c d) ip))
     (+ (* a 16777216) (* b 65536) (* c 256) d))))
(defmacro int-to-ipv4 (ip)
  ;`(tuple (band ,ip 255) (bsr (band ,ip 65280) 8) (bsr (band ,ip 16711680) 16) (bsr ,ip 24))
  `(tuple (bsr ,ip 24) (bsr (band ,ip 16711680) 16) (bsr (band ,ip 65280) 8) (band ,ip 255))
)
(defmacro make-client (ip port arg)
  `(tuple (cons ,ip ,port) ,arg))

(defun process_msg (parent arg)
  (let* (((tuple (= (cons ip p) sin) msg) arg)
	 ; better than using header sock opt
	((binary (header binary (size 30)) (_rest bytes)) msg)
	(struct (unpack header)))
    (if (not (check-packet struct (byte_size msg)))
      (let (('fail 'wrong-packet))))
    (let* ((dst (ipx-header-dst-addr struct))
	   (src (ipx-header-src-addr struct)))
      (if (andalso (nil-socket? dst) (nil-socket? src))

		   (let* ((socket (make-ipx-socket
				  addr (ip-to-int ip)
				  port p))
			  (sock (ipx-header-src-sock struct)))
		     (! parent (tuple 'ack sin (pack (make-ipx-header
							      dst-addr socket
							      dst-sock sock
							      src-addr socket
							      src-sock sock)))))
		   (if (broadcast-socket? dst)
		     ; pass who we are
		     (! parent (tuple 'bcst sin msg))
		     (! parent (tuple 'single (cons (int-to-ipv4 (ipx-socket-addr dst))
						    (ipx-socket-port dst)) msg)))))))
(defun init
  ((args) (when (is_atom (car args)) (is_atom (cadr args)))
   (let ((port (list_to_integer (atom_to_list (car args)))))
     (let ((options (settings-udp-options (make-settings))))
     (case args
       ((list _port-atom type-atom ip-atom) (when (is_atom ip-atom))
	(let* (((tuple 'ok ip) (: inet_parse address (atom_to_list ip-atom)))
	       ((tuple 'ok socket) (: gen_udp open port
				      (++ (list type-atom (tuple 'ip ip))
					  options))))
	  (tuple 'ok (tuple socket ()))))
       ((list _port-atom fd-atom)
	(let (((tuple 'ok socket) (: gen_udp open port
				  (cons (tuple 'fd (list_to_integer (atom_to_list fd-atom)))
					options))))
	  (tuple 'ok (tuple socket ())))))))))
;(defun terminate (_reason state)
;  (let (((tuple fd _calls-list) state))
;    (: gen_udp close fd)))

(defun handle_info (fd lists)
  ;(let (((tuple 'udp fd ip port msg) info))
  (let (((tuple procs ignore clients) lists))
  (: inet setopts fd (list #(active once)))
  (receive ((tuple 'udp fd ip port msg)
	    (let ((sin (cons ip port)))
	      ; don't process bogus clients
	      (case (: lists any (lambda (x) (== x sin)) ignore)
		('false
		 (let ((pid (spawn_link 'ipxerlay 'process_msg (list (self) (make-client ip port msg)))))
		       (handle_info fd (setelement 1 lists (cons (make-client ip port pid) procs)))))
		('true
		 (: io format '"Ignored: ~p, ignore: ~p~n" (list sin ignore))
		 (handle_info fd lists)))))
	   ((tuple _ pid 'normal)
	    (handle_info fd (setelement 1 lists (: lists filter (lambda (x) (/= (element 2 x) pid)) procs))))
	   ;; lot of synchronous pokery
	   ((tuple type (= (cons ip port) sin) msg)
	      (case type
		('single
		 (let ((target (: lists keyfind sin 1 clients)))
		   (: io format '"Trying to send to: ~p, clients: ~p~n" (list sin clients))
		   (if (/= target 'false)
		     (let ((elm (element 1 target)))
		       (: gen_udp send fd (car elm) (cdr elm) msg))
		     ; we'll remove it during cleanup
		     ))
		 (handle_info fd lists))
		('bcst
		 (: lists foreach (lambda (x) (let ((elm (element 1 x)))
						(: io format '"Sending to ~p from broadcast~n" (list elm))
						(: gen_udp send fd (car elm) (cdr elm) msg)))
		    (: lists filter (lambda (x) (/= (element 1 x) sin)) clients))
		 (handle_info fd lists))
		('ack
		 (: io format '"New client: ~p, clients: ~p~n" (list sin clients))
		 (: gen_udp send fd ip port msg)
		 (handle_info fd (setelement 3 lists (cons (make-client ip port ()) clients))))))
	   ;;
	   ((tuple _ pid _)
	    ; it must be in the list
	    (let ((elm (: lists keyfind pid 2 procs)))
		(handle_info fd (tuple (: lists delete elm procs) (cons (element 1 elm) ignore) clients))))))
    ;(tuple 'noreply state))
)
(defun start_link (args)
  ;(if (do-tests)
    ;(: gen_server start_link #(local ipxerlay) 'ipxerlay args ())
    (let (((tuple 'ok (tuple socket ())) (init args)))
      (let (((tuple 'ok port) (: inet port socket)))
	(: io format '"Using port ~B~n" (list port))
	(process_flag 'trap_exit 'true)
	(handle_info socket (tuple () () ()))))
    ;#(error tests-failed))
)
