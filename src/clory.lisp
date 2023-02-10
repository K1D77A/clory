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

(defapi oauth2/clients-put/lifespans ("/admin/clients/:id/lifespans" put-request)
        ())

(defapi oauth2/clients-patch ("/admin/clients" patch-request)
        ())

(defapi oauth2/clients-put ("/admin/clients/:id" put-request)
        ())

(defapi oauth2/consent-get ("/admin/oauth2/auth/requests/consent" get-request)
        ((consent-challenge
          :as-string "consent_challenge")))

(defapi oauth2/consent-accept ("/admin/oauth2/auth/requests/consent/accept" put-request)
        ((consent-challenge
          :as-string "consent_challenge")))

(defapi oauth2/consents-reject ("/admin/oauth2/auth/requests/consent/reject" put-request)
        ((consent-challenge 
          :as-string "consent_challenge")))

(defapi oauth2/login-requests-get ("/admin/oauth2/auth/requests/login" get-request)
        ((login-challenge
          :as-string "login_challenge")))

(defapi oauth2/login-requests-accept ("/admin/oauth2/auth/requests/login/accept" put-request)
        ((login-challenge
          :as-string "login_challenge")))


(defapi oauth2/login-requests-reject ("/admin/oauth2/auth/requests/login/reject" put-request)
        ((login-challenge
          :as-string "login_challenge")))

(defapi oauth2/logout-requests-get ("/admin/oauth2/auth/requests/logout" get-request)
        ((login-challenge
          :as-string "logout_challenge")))

(defapi oauth2/logout-requests-accept
    ("/admin/oauth2/auth/requests/logout/accept" put-request)
        ((login-challenge
          :as-string "logout_challenge")))


(defapi oauth2/logout-requests-reject
    ("/admin/oauth2/auth/requests/logout/reject" put-request)
        ((login-challenge
          :as-string "logout_challenge")))

(defapi oauth2/revoke-consent-session ("/admin/oauth2/auth/sessions/consent" delete-request)
        ((subject
          :as-string "subject")
         (client
          :as-string "client")
         (all
          :as-string "all")))

(defapi oauth2/consent-sessions-get ("/admin/oauth2/auth/sessions/consent" get-request)
        ((page-size          
          :as-string "page_size")
         (page-token
          :as-string "page_token")
         (subject
          :as-string "subject")
         (login-session-id
          :as-string "login_session_id")))

(defapi oauth2/revoke-all-login-sessions
    ("/admin/oauth2/auth/sessions/login" delete-request)
        ((subject
          :as-string "subject")))

(defapi oauth2/introspect ("/admin/oauth2/introspect" post-request%form)
        ())

(defapi oauth2/token-delete ("/admin/oauth2/tokens" delete-request)
        ((client-id
          :as-string "client_id")))



