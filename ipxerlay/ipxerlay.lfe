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

(defmacro make-client (ip port arg)
  `(tuple (cons ,ip ,port) ,arg))

(defun process_msg (parent arg)
  (let* (((tuple (cons ip p) msg) arg)
	((binary (header binary (size 30)) (_rest bytes)) msg)
	(struct (unpack header)))
    (if (check-packet struct (byte_size msg))

      (
      (if (andalso (nil-socket? (ipx-socket-network (ipx-header-dst-addr struct)))
		   (nil-socket? (ipx-socket-network (ipx-header-src-addr struct))))
	 (let ((socket (make-ipx-socket
			addr ip
			port p)))
	   (! parent (tuple 'ack (list ip p) (pack (make-ipx-header
						    dst-addr socket
						    dst-sock (ipx-socket-network (ipx-header-src-addr struct))
						    src-addr socket
						    src-sock (ipx-socket-network (ipx-header-src-addr struct))))))))
       (if (broadcast-socket? (ipx-socket-network (ipx-header-dst-addr struct)))
	 (! parent (tuple 'bcst (list ip p) msg))
	 (! parent (tuple 'single (list ip p) msg)))
      )

      (let (('fail 'wrong-packet))
      ))))
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
	    ;(: io format '"~p~n" (list (list (tuple 'fd fd)(tuple 'ip ip)(tuple 'port port))))
	    ; don't process bogus clients
	    (if (: lists any (lambda (x) (== x (cons ip port))) ignore)
	      ;(: io format '"Ignored: ~p~n" (list ignore))
	      (handle_info fd lists)
	      (let ((pid (spawn_link 'ipxerlay 'process_msg (list (self) (make-client ip port msg)))))
		;(: io format '"Processing message ~p~n" (list msg))
		(handle_info fd (setelement 1 lists (cons (make-client ip port pid) procs))))))
	   ((tuple _ pid 'normal)
	    (handle_info fd (setelement 1 lists (: lists filter (lambda (x) (/= (element 2 x) pid)) procs))))
	   ((tuple 'ack (cons ip port) msg)
	    (: gen_udp send fd ip port msg)
	    (handle_info fd (setelement 3 lists (cons (make-client ip port ()) (element 3 lists)))))
	   ((tuple _ pid _)
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
