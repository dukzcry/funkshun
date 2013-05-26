(defmodule ipxerlay
  ;(behavior 'gen_server)
  
  ;(export
   ;(init 1)
   ;(handle_info 2)
   ;(terminate 2))
  (export
   (start_link 1)
   (test_call 0)))

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

(defun handle_msg (msg)
  ;(: io format '"Processing message ~p~n" (list msg))
  (let (((binary (header binary (size 30)) (rest bytes)) msg))
    (check-packet (unpack header) (byte_size msg))))

(defun init
  ((args) (when (is_atom (car args)) (is_atom (cadr args)) (is_atom (car (cddr args))))
    (let* (((list type-atom ip-atom port-atom) args)
	   ((tuple 'ok ip) (: inet_parse address (atom_to_list ip-atom)))
	   ((tuple 'ok socket) (: gen_udp open (list_to_integer (atom_to_list port-atom))
				  (list 'binary type-atom (tuple 'ip ip) #(active false)))))
      (tuple 'ok (tuple socket ()))))
  ((args) (when (is_atom (car args)) (is_atom (cadr args)))
    (let* (((list port-atom fd-atom) args)
	   ((tuple 'ok socket) (: gen_udp open (list_to_integer (atom_to_list port-atom))
				  (list 'binary (tuple 'fd (list_to_integer (atom_to_list fd-atom)))
					#(active false)))))
      (tuple 'ok (tuple socket ())))))
;(defun terminate (_reason state)
;  (let (((tuple fd _calls-list) state))
;    (: gen_udp close fd)))

(defun handle_info (fd state)
  ;(let (((tuple 'udp fd ip port msg) info))
  (: inet setopts fd (list #(active once)))
  (receive ((tuple 'udp fd ip port msg)
	    ;(: io format '"~p~n" (list (list (tuple 'fd fd)(tuple 'ip ip)(tuple 'port port))))
	    (spawn (lambda () (handle_msg msg)))
	    (handle_info fd state)))
    ;(tuple 'noreply state))
)
(defun start_link (args)
  ;(if (do-tests)
    ;(: gen_server start_link #(local ipxerlay) 'ipxerlay args ())
    (let (((tuple 'ok (tuple socket ())) (init args)))
      (let (((tuple 'ok port) (: inet port socket)))
	(: io format '"Using port ~B~n" (list port))
	(handle_info socket ())))
    ;#(error tests-failed))
)
