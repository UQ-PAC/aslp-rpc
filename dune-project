(lang dune 3.6)

(name aslp-rpc)

(generate_opam_files true)


(source
(github UQ-PAC/aslp-rpc))

(package
 (name aslp_server_http)
 (synopsis "REST server for ASLp")
 (description "")
 (version 0.1)
 (authors "ailrst")
 (maintainers "ailrst")

 (license BSD-3-Clause)
 (source
 (github UQ-PAC/aslp-rpc))
 (documentation https://github.com/ailrst/aslp-rpc/aslp-server-http )
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
  (description "ASLP connector using marshalling over a Unix socket")
  (depends
    ocaml
    mtime
  )

  (version 0.1)
  (authors "ailrst")
  (maintainers "ailrst")
  (license BSD-3-Clause)

)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
