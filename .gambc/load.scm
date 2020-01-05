(include "set-compiler.scm")
(include "set-config.scm")

;; (regen-lib-load-form)
(begin
  (c/load "lib/cj-source")
  (c/load "lib/define-macro-star")
  (c/load "lib/fixnum")
  (c/load "lib/cj-phasing")
  (c/load "lib/simple-match-1")
  (c/load "lib/cj-symbol-with")
  (c/load "lib/cj-gambit-sys-0")
  (c/load "lib/optim-values")
  (c/load "lib/srfi-1")
  (c/load "lib/list-util-1")
  (c/load "lib/string-util-4")
  (c/load "lib/test")
  (c/load "lib/cj-inline-1")
  (c/load "lib/on")
  (c/load "lib/cj-env")
  (c/load "lib/cj-symbol")
  (c/load "lib/fixnum-more")
  (c/load "lib/cj-exception")
  (c/load "lib/test-lib-1")
  (c/load "lib/C")
  (c/load "lib/string-util-1")
  (c/load "lib/improper-list")
  (c/load "lib/list-util")
  (c/load "lib/named")
  (c/load "lib/srfi-11")
  (c/load "lib/simple-match")
  (c/load "lib/cj-struct-tag")
  (c/load "lib/lazy-1")
  (c/load "lib/debuggable-promise")
  (c/load "lib/lazy")
  (c/load "lib/scheme-meta")
  (c/load "lib/cj-source-util-2")
  (c/load "lib/char-util")
  (c/load "lib/code-util")
  (c/load "lib/cj-functional-2")
  (c/load "lib/improper-list-1")
  (c/load "lib/predicates-1")
  (c/load "lib/cj-struct")
  (c/load "lib/fallible-1")
  (c/load "lib/cj-typed-1")
  (c/load "lib/cj-typed")
  (c/load "lib/cj-gambit-sys")
  (c/load "lib/cj-functional")
  (c/load "lib/cj-source-2")
  (c/load "lib/define-nested")
  (c/load "lib/slib-sort")
  (c/load "lib/list-util-2")
  (c/load "lib/cj-alist")
  (c/load "lib/string-util")
  (c/load "lib/cj-match")
  (c/load "lib/table-1")
  (c/load "lib/cj-warn")
  (c/load "lib/cj-source-wraps")
  (c/load "lib/cj-expansion")
  (c/load "lib/cut")
  (c/load "lib/srfi-13-kmp")
  (c/load "lib/cj-env-2")
  (c/load "lib/local-test")
  (c/load "lib/string-util-2")
  (c/load "lib/list-util-lazy")
  (c/load "lib/list-util-3")
  (c/load "lib/continuation-carp")
  (c/load "lib/dot-oo")
  (c/load "lib/define-module")
  (c/load "lib/enum")
  (c/load "lib/vector-util")
  (c/load "lib/cj-source-quasiquote")
  (c/load "lib/string-interpolate")
  (c/load "lib/define-strict-and-lazy")
  (c/load "lib/cj-inline")
  (c/load "lib/symboltable-1")
  (c/load "lib/predicates")
  (c/load "lib/cj-cmp")
  (c/load "lib/lazy-debug")
  (c/load "lib/cj-path")
  (c/load "lib/show")
  (c/load "lib/stream")
  (c/load "lib/weak-srfi-1")
  (c/load "lib/test-logic")
  (c/load "lib/cj-math")
  (c/load "lib/test-random")
  (c/load "lib/easy-1")
  (c/load "lib/oo-util")
  (c/load "lib/test-lib")
  (c/load "lib/more-oo")
  (c/load "lib/dsssl")
  (c/load "lib/symboltable")
  (c/load "lib/code-macro-expand")
  (c/load "lib/cj-seen")
  (c/load "lib/tree-util")
  (c/load "lib/joo")
  (c/load "lib/jclass")
  (c/load "lib/class")
  (c/load "lib/easy")
  '(c/load "lib/exceptions")
  '(c/load "lib/2d-shape")
  '(c/load "lib/2d-polar")
  '(c/load "lib/M")
  '(c/load "lib/if-let")
  '(c/load "lib/monad/monad-ops")
  '(c/load "lib/monad/generic")
  '(c/load "lib/monad/syntax")
  '(c/load "lib/Maybe")
  '(c/load "lib/Result")
  '(c/load "lib/U")
  '(c/load "lib/address")
  '(c/load "lib/alist")
  '(c/load "lib/atomic-box")
  '(c/load "lib/auto-gdb")
  '(c/load "lib/cj-port")
  '(c/load "lib/bag")
  '(c/load "lib/unclean")
  '(c/load "lib/string-util-3")
  '(c/load "lib/template")
  '(c/load "lib/hashcollection")
  '(c/load "lib/bitset")
  '(c/load "lib/boolean")
  '(c/load "lib/cached")
  '(c/load "lib/catch-throw")
  '(c/load "lib/cc")
  '(c/load "lib/check-equal")
  '(c/load "lib/cj-c-types")
  '(c/load "lib/cj-string")
  '(c/load "lib/cj-string-flatten")
  '(c/load "lib/cj-c-util")
  '(c/load "lib/cj-cmp-location")
  '(c/load "lib/cj-cmp-test")
  '(c/load "lib/cj-curry")
  '(c/load "lib/cj-env-test")
  '(c/load "lib/oo-vector-lib")
  '(c/load "lib/oo-lib-string")
  '(c/load "lib/cj-exception-handler")
  '(c/load "lib/cj-ffi")
  '(c/load "lib/cj-gambit-sys-0-bench")
  '(c/load "lib/cj-html-util")
  '(c/load "lib/cj-http-status")
  '(c/load "lib/keyword-alist")
  '(c/load "lib/posix/cj-c-errno_Cpart")
  '(c/load "lib/posix/cj-c-errno")
  '(c/load "lib/posix/cj-posix")
  '(c/load "lib/string-bag")
  '(c/load "lib/cj-io-util")
  '(c/load "lib/cj-let-named-star")
  '(c/load "lib/cj-posixpath")
  '(c/load "lib/cj-setf")
  '(c/load "lib/cj-shortcuts")
  '(c/load "lib/cj-source-lambda")
  '(c/load "lib/cj-source-test")
  '(c/load "lib/cj-source-util-test")
  '(c/load "lib/stream-Maybe")
  '(c/load "lib/cj-sxml")
  '(c/load "lib/oo-util-lazy")
  '(c/load "lib/cj-ssxpath")
  '(c/load "lib/cj-standarddeclares")
  '(c/load "lib/cj-sxml-io")
  '(c/load "lib/cj-sxml-keyed")
  '(c/load "lib/cj-sxml-keyed-benchmark")
  '(c/load "lib/srfi-1-macros")
  '(c/load "lib/cj-sxml-serializer")
  '(c/load "lib/cj-sxmltemplates")
  '(c/load "lib/cj-syntax")
  '(c/load "lib/cj-u8vector-util")
  '(c/load "lib/cj-ulist")
  '(c/load "lib/utf8")
  '(c/load "lib/u8vector0")
  '(c/load "lib/hex")
  '(c/load "lib/cj-url-encode")
  '(c/load "lib/cj-url-encode-test")
  '(c/load "lib/english")
  '(c/load "lib/code-comparison-chain")
  '(c/load "lib/unixtime-types")
  '(c/load "lib/unixtime-Cpart-compiletime")
  '(c/load "lib/unixtime-Cpart")
  '(c/load "lib/md5")
  '(c/load "lib/realrandom")
  '(c/load "lib/tempfile")
  '(c/load "lib/oo-lib-vector")
  '(c/load "lib/fstable")
  '(c/load "lib/fscache")
  '(c/load "lib/dateparse")
  '(c/load "lib/unixtime")
  '(c/load "lib/cj-warn-t")
  '(c/load "lib/clojure-base")
  '(c/load "lib/table")
  '(c/load "lib/clojure")
  '(c/load "lib/clojure-examples")
  '(c/load "lib/clojure-test")
  '(c/load "lib/code-cj-functional")
  '(c/load "lib/wbtree")
  '(c/load "lib/wbcollection")
  '(c/load "lib/collection-on")
  '(c/load "lib/warn-plus")
  '(c/load "lib/math-approximate")
  '(c/load "lib/rgb-types")
  '(c/load "lib/colorspaces")
  '(c/load "lib/rgb")
  '(c/load "lib/color")
  '(c/load "lib/compat")
  '(c/load "lib/fluid-let")
  '(c/load "lib/compile")
  '(c/load "lib/constants")
  '(c/load "lib/continuation")
  '(c/load "lib/typed-list")
  '(c/load "lib/typed-alist")
  '(c/load "lib/joo-introspection")
  '(c/load "lib/corescheme")
  '(c/load "lib/failing")
  '(c/load "lib/corescheme-to-scheme")
  '(c/load "lib/corescheme-optimize")
  '(c/load "lib/corescheme-moretest")
  '(c/load "lib/eol")
  '(c/load "lib/csv-defaults")
  '(c/load "lib/posix/interrupts")
  '(c/load "lib/data-compressor")
  '(c/load "lib/debug")
  '(c/load "lib/debuggable-promise-everywhere")
  '(c/load "lib/delimcc-simple")
  '(c/load "lib/delimcc-simple-test")
  '(c/load "lib/domain-name")
  '(c/load "lib/dot-oo-introspection")
  '(c/load "lib/dot-oo-optim")
  '(c/load "lib/dot-oo-test")
  '(c/load "lib/dot")
  '(c/load "lib/easy-table")
  '(c/load "lib/gambit-error")
  '(c/load "lib/error")
  '(c/load "lib/fallible")
  '(c/load "lib/exception-modules")
  '(c/load "lib/failure")
  '(c/load "lib/fast-math")
  '(c/load "lib/fixnum-test")
  '(c/load "lib/format-time")
  '(c/load "lib/gambit-uri")
  '(c/load "lib/oo-lib-u8vector")
  '(c/load "lib/hide")
  '(c/load "lib/improper-length-test")
  '(c/load "lib/improper-list-more")
  '(c/load "lib/joo-test")
  '(c/load "lib/json-write")
  '(c/load "lib/keyword-typed-alist")
  '(c/load "lib/list-range")
  '(c/load "lib/math/vectorlib")
  '(c/load "lib/parallel")
  '(c/load "lib/math/fftw_Cpart-macros")
  '(c/load "lib/math/fftw_Cpart")
  '(c/load "lib/math/fftw")
  '(c/load "lib/throwing")
  '(c/load "lib/math/formula")
  '(c/load "lib/math/image/effects/utils")
  '(c/load "lib/math/image/effects/light")
  '(c/load "lib/math/mathlib")
  '(c/load "lib/math/vectorlib-1")
  '(c/load "lib/math/image/effects/newspaper")
  '(c/load "lib/posix/mmap")
  '(c/load "lib/u8-parse")
  '(c/load "lib/math/image/pnm")
  '(c/load "lib/math/image/pnmatrix-macros")
  '(c/load "lib/math/image/pnmatrix-compiled")
  '(c/load "lib/math/image/pnmatrix-base")
  '(c/load "lib/math/image/pnmatrix")
  '(c/load "lib/math/image/effects/rim")
  '(c/load "lib/math/image/effects/transparency")
  '(c/load "lib/math/image/pnm-utils")
  '(c/load "lib/math/image/pnmatrix-tests")
  '(c/load "lib/math/integral")
  '(c/load "lib/math/interpolate")
  '(c/load "lib/math/predicates")
  '(c/load "lib/maybe")
  '(c/load "lib/try")
  '(c/load "lib/range")
  '(c/load "lib/vector-binsearch")
  '(c/load "lib/math/mapfn")
  '(c/load "lib/math/smoothfn")
  '(c/load "lib/seq")
  '(c/load "lib/math/statistics")
  '(c/load "lib/math/vectorlib-2")
  '(c/load "lib/oo-lib-u32vector")
  '(c/load "lib/math/visualize/plot")
  '(c/load "lib/math/visualize/examples/rwanda")
  '(c/load "lib/maybe-list")
  '(c/load "lib/memcmp")
  '(c/load "lib/memcmp-test")
  '(c/load "lib/memoize")
  '(c/load "lib/string-quote")
  '(c/load "lib/mime-type")
  '(c/load "lib/mod/config-example")
  '(c/load "lib/mod/gambit")
  '(c/load "lib/mod/imperative-load-tree")
  '(c/load "lib/mod/lib")
  '(c/load "lib/mod/mod")
  '(c/load "lib/mod/monad")
  '(c/load "lib/mod/monadic-load-tree")
  '(c/load "lib/mod/remote")
  '(c/load "lib/mod/usersyntax")
  '(c/load "lib/monad/io")
  '(c/load "lib/monad/transaction-cps")
  '(c/load "lib/monad/transaction-multival")
  '(c/load "lib/more-io-util")
  '(c/load "lib/number-util")
  '(c/load "lib/oo-gambit")
  '(c/load "lib/oo-lib-f32vector")
  '(c/load "lib/oo-lib-f64vector")
  '(c/load "lib/oo-lib-s8vector")
  '(c/load "lib/oo-lib-u16vector")
  '(c/load "lib/oo-lib-s16vector")
  '(c/load "lib/oo-lib-s32vector")
  '(c/load "lib/oo-lib-u64vector")
  '(c/load "lib/oo-lib-s64vector")
  '(c/load "lib/oo-lib-all")
  '(c/load "lib/oo-list-vector")
  '(c/load "lib/oo-vector-lib-test")
  '(c/load "lib/oo-vector-list")
  '(c/load "lib/optim-values-test")
  '(c/load "lib/parameter-once")
  '(c/load "lib/parse1")
  '(c/load "lib/partial-apply")
  '(c/load "lib/posix/cj-posix-example")
  '(c/load "lib/pseudorandom")
  '(c/load "lib/queue")
  '(c/load "lib/vectormap")
  '(c/load "lib/trie")
  '(c/load "lib/radixtree")
  '(c/load "lib/read-csv-util")
  '(c/load "lib/spreadsheet-reference")
  '(c/load "lib/read-csv")
  '(c/load "lib/realunixtime")
  '(c/load "lib/tsort")
  '(c/load "lib/require")
  '(c/load "lib/require-util")
  '(c/load "lib/ringbuffer")
  '(c/load "lib/safe-fx")
  '(c/load "lib/segv")
  '(c/load "lib/single-cond")
  '(c/load "lib/skein-code")
  '(c/load "lib/skein")
  '(c/load "lib/skein-test")
  '(c/load "lib/sqlite3-error")
  '(c/load "lib/sqlite3-errors")
  '(c/load "lib/sqlite3")
  '(c/load "lib/sqlite3-test")
  '(c/load "lib/srfi-11-bench")
  '(c/load "lib/statprof")
  '(c/load "lib/statprof/ring")
  '(c/load "lib/statprof/example")
  '(c/load "lib/string-case")
  '(c/load "lib/string-case-bench")
  '(c/load "lib/svg")
  '(c/load "lib/symboltable-bench")
  '(c/load "lib/symboltable-lib")
  '(c/load "lib/symboltable-test")
  '(c/load "lib/ternary")
  '(c/load "lib/time-result")
  '(c/load "lib/timepoint")
  '(c/load "lib/tinymt")
  '(c/load "lib/token-table")
  '(c/load "lib/track")
  '(c/load "lib/u8vector")
  '(c/load "lib/unixtime-Cpart-strptime")
  '(c/load "lib/utf8-test")
  '(c/load "lib/uuid")
  '(c/load "lib/values-util")
  '(c/load "lib/vector-util-1")
  '(c/load "lib/wbmcollection")
  '(c/load "lib/wbtable")
  '(c/load "lib/wbsymboltable")
  '(c/load "lib/wbtable-bench")
  '(c/load "lib/wbtree-benchmarks")
  '(c/load "lib/wbtree-test")
  '(c/load "lib/write-csv")
  '(c/load "lib/xhtml"))

(i/load "copycat")


