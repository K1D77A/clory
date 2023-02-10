;;;; package.lisp

(defpackage #:clory
  (:use #:cl)
  (:nicknames #:ory)
  (:import-from #:hu.dwim.defclass-star
                #:defclass*)
  (:export #:ory-condition
           #:ory-default-error
           #:ory-generic-error
           #:code
           #:edebug
           #:details
           #:id
           #:message
           #:reason
           #:request
           #:status
           #:status-code
           #:result
           #:headers
           #:uri
           #:dstream
           #:ory
           #:error-debug
           #:error-name
           #:error-description
           #:eerror

           #:call-api

           #:*ory*))
     

