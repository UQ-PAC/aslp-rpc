(lang dune 2.8)

(generate_opam_files true)

(package
 (name aslp_server_http)
 (synopsis "REST server for ASLp")
 (description "")

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
    "yojson"
    "asli"
 )
 )

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
