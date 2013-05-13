(defmodule ipxerlay
  (export (main 0)))

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
  ((bin) (when (=:= (byte_size bin) 10)) (let (((binary
	  (n (size 4) (unit 8))
	  (a (size 4) (unit 8))
	  (p (size 2) (unit 8)))
	 bin))
    (make-ipx-socket network n addr a port p)))
  ((bin) (when (=:= (byte_size bin) 30)) (let (((binary
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
		     (unpack dp) dst-sock d src-addr (unpack sp) src-sock s)))
)

(defun main () (: io format '"~p~n" (list (unpack (pack (make-ipx-header))))))
