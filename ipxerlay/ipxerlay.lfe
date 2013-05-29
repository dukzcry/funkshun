(defmodule ipxerlay
  ;(behavior 'gen_server)
  
  ;(export
   ;(init 1)
   ;(handle_info 2)
   ;(terminate 2))
  (export
   (start_link 1)
   (test_call 0)))

(defrecord settings
  (udp-options (list 'binary #(active false) #(recbuf 65536))))

(defrecord ipx-socket
  (network 0)
  (addr 0)
  (port 0))
(defrecord ipx-header
  (checksum 0)
  (len 0)
  (routed? 0)
  (type 0)
  (dst-addr (make-ipx-socket))
  (dst-sock 0)
  (src-addr (make-ipx-socket))
  (src-sock 0))

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
	      checksum #xffff
	      len 30
	      dst-addr socket
	      src-addr socket)))))))
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

(defun process_msg (msg)
  ;(: io format '"Processing message ~p~n" (list msg))
  (let (((binary (header binary (size 30)) (_rest bytes)) msg))
    (let ((struct (unpack header)))
      (andalso (check-packet struct (byte_size msg))
	       (nil-socket? (ipx-socket-network (ipx-header-dst-addr struct)))
	       (nil-socket? (ipx-socket-network (ipx-header-src-addr struct)))))))

(defun init
  ((args) (when (is_atom (car args)) (is_atom (cadr args)))
   (let ((port (list_to_integer (atom_to_list (car args)))))
     (let ((options (settings-udp-options (make-settings))))
     (case args
       ((list _port-atom type-atom ip-atom) (when (is_atom ip-atom))
	(let* (((tuple 'ok ip) (: inet_parse address (atom_to_list ip-atom)))
	       ((tuple 'ok socket) (: gen_udp open port
				      (++ (list type-atom (tuple 'ip ip)) options))))
	  (tuple 'ok (tuple socket ()))))
       ((list _port-atom fd-atom)
	(let (((tuple 'ok socket) (: gen_udp open port
				  (cons (tuple 'fd (list_to_integer (atom_to_list fd-atom)))
					options))))
	  (tuple 'ok (tuple socket ())))))))))
;(defun terminate (_reason state)
;  (let (((tuple fd _calls-list) state))
;    (: gen_udp close fd)))

(defun handle_info (fd ignore)
  ;(let (((tuple 'udp fd ip port msg) info))
  (: inet setopts fd (list #(active once)))
  (receive ((tuple 'udp fd ip port msg)
	    ;(: io format '"~p~n" (list (list (tuple 'fd fd)(tuple 'ip ip)(tuple 'port port)
					     ;(tuple 'ignore ignore))))
	    (if (: lists any (lambda (x) (== (element 2 x) (cons ip port))) ignore)
	      (handle_info fd ignore)
	    (let ((pid (spawn_link (lambda () (process_msg msg)))))
	      (handle_info fd (cons (tuple pid (cons ip port)) ignore)))))
	   ((tuple _ pid 'normal)
	    (handle_info fd (: lists filter (lambda (x) (/= (element 1 x) pid)) ignore)))
	   ((tuple _ pid _)
	    (handle_info fd ignore)))
    ;(tuple 'noreply state))
)
(defun start_link (args)
  ;(if (do-tests)
    ;(: gen_server start_link #(local ipxerlay) 'ipxerlay args ())
    (let (((tuple 'ok (tuple socket ())) (init args)))
      (let (((tuple 'ok port) (: inet port socket)))
	(: io format '"Using port ~B~n" (list port))
	(process_flag 'trap_exit 'true)
	(handle_info socket ())))
    ;#(error tests-failed))
)
