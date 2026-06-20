(package
  (name (args))
  (owner "playx")
  (version "0.1.1")
  (license "BSD-3-Clause")
  (description "R7RS-small CLI parser library")
  (authors "Adel Prokurov")
  (readme "README.md")
  (dialects r7rs)
  (source-path "src")
  (libraries
    ((r7rs (args))
     (r7rs (args grammar))
     (r7rs (args help optional))
     (r7rs (args option))
     (r7rs (args parser))
     (r7rs (args results))
     (r7rs (args runner))
     (r7rs (args string)))))

(dependencies)

(dev-dependencies)

(overrides)
