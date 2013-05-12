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

(defun encode
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
    ((encode (ipx-header-dst-addr struct)) binary)
    ((ipx-header-dst-sock struct) (size 2) (unit 8))
    ((encode (ipx-header-src-addr struct)) binary)
    ((ipx-header-src-sock struct) (size 2) (unit 8))
)))

(defun main () (: io format '"~p~n" (list (encode (make-ipx-header)))))
