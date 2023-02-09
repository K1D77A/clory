(in-package #:clory)

(pushnew (or T :as-string) hu.dwim.defclass-star:*allowed-slot-definition-properties*)

(defapi oauth2/clients ("/admin/clients" get-request)
        ((page-size          
          :as-string "page_size")
         (page-token
          :as-string "page_token")
         (client-name
          :as-string "client_name")
         (owner
          :as-string "owner")))

(defapi oauth2/clients-create ("/admin/clients" post-request%json)
        ())

(defapi oauth2/clients-delete ("/admin/clients/:id" delete-request)
        ())

(defapi oauth2/clients-get ("/admin/clients/:id" get-request)
        ())

(defapi oauth2/clients-patch ("/admin/clients" patch-request)
        ())


(defapi oauth2/introspect ("/admin/oauth2/introspect" post-request%form)
        ())
