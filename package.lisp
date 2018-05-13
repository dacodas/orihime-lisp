(defpackage :goo-page
  (:use #:cl)
  (:export
   #:page-is-single
   #:page-is-multiple
   #:page-entry
   #:entry-contents
   #:page-select-result))

(defpackage :goo
  (:use #:cl #:goo-page)
  (:export
   #:*words*
   #:lookup-and-show-new-word))



