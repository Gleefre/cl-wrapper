(defsystem "wrapper"
  :description "wrap-if is a macro that hides code duplication using macrology."
  :version "0.0.1"
  :author ("tuntap"
           "Grolter <varedif.a.s@gmail.com>")
  :licence "MIT"
  :depends-on ("alexandria" "captures")
  :components ((:file "wrapper")))
