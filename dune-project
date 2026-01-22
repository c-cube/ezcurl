(lang dune 3.0)

(name ezcurl)

(version 0.2.4)

(generate_opam_files true)

(license MIT)

(maintainers "simon.cruanes.2007@m4x.org")

(authors "Simon Cruanes")
(source (github c-cube/ezcurl))
(documentation https://c-cube.github.io/ezcurl/)

(package
 (name ezcurl)
 (synopsis "Friendly wrapper around OCurl")
 (tags
  ("curl" "web" "http" "client"))
 (depends
  (ocurl
   (>= 0.8))
  (odoc :with-doc)
  (tiny_httpd (and (>= 0.19) :with-test))
  (ocaml
   (>= 4.03))))

(package
 (name ezcurl-lwt)
 (synopsis "Friendly wrapper around OCurl, Lwt version")
 (tags
  ("curl" "web" "http" "client" "lwt"))
 (depends
  (ezcurl
   (= :version))
  lwt
  (mdx :with-test)
  (odoc :with-doc)
  (ocaml
   (>= 4.03))))
