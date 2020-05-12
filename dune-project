(lang dune 2.0)
(name tjr_lru_cache)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/tjr_lru_cache))
(homepage             https://github.com/tomjridge/tjr_lru_cache)
(documentation        https://tomjridge.github.io/ocamldocs/)
(generate_opam_files  true)


(package 
 (name tjr_lru_cache)
 (synopsis "An LRU cache, part of ImpFS")
 (depends 
tjr_fs_shared
lru
minicli
containers
))


