(lang dune 3.6)

(name aslp-rpc)

(generate_opam_files true)


(source (github UQ-PAC/aslp-rpc))

(documentation https://uq-pac.github.io/aslp-rpc/)

(package
  (name aslp-rpc)
  (synopsis "ASLp RPC libraries")
  (version 0.1.0)
  (authors "ailrst")
  (maintainers "ailrst")
  (license Apache-2.0)
  (allow_empty)
 )

(package
 (name aslp_server_http)
 (documentation https://uq-pac.github.io/aslp-rpc/aslp_server_http)
 (synopsis "REST server for ASLp")
 (version 0.1.0)
 (authors "ailrst")
 (maintainers "ailrst")
 (source (github UQ-PAC/aslp-rpc))

 (license BSD-3-Clause)
 (depends
   ("ocaml" (>= "5.0"))
    "eio"
    "cohttp-eio"
    "eio_main"
    "yojson"
    "asli"
 )
)

(package
  (name aslp_client_server_ocaml)
  (synopsis "ASLP connector using marshalling over a Unix socket")
  (depends
    ocaml
    mtime
    aches
    lwt
    asli
    aslp_offline
  )
  (version 0.1.2)
  (source (github UQ-PAC/aslp-rpc))
  (authors "ailrst")
  (maintainers "ailrst")
  (license BSD-3-Clause)
  (documentation https://uq-pac.github.io/aslp-rpc/aslp_client_server_ocaml)
)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
