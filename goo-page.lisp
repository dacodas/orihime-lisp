(in-package :goo-page)

(defun page-log-information (page)
  (let* ((entry (page-entry page))
         (title (entry-title entry))
         (contents-html (lquery-funcs:serialize (entry-contents entry))))

    (multiple-value-bind (word readings old-readings)
        (title-match-readings title)
      (log:debug "Path to this word: ~a" (format nil "~{~a~^ → ~}" (navbar-to-list (page-navbar page))))
      (log:debug "Word: ~a (~a) ~a" word readings (if old-readings (format nil "(~a)" old-readings) ""))
      (log:debug "Contents through html2text: ~a" (format-html-as-text contents-html))
      ;; (log:debug "Contents (raw html): ~a" contents-html)
      )))

(defun page-html-title-regex-matcher (regex page)
  (let ((page-html-title (lquery:$1 page "head title" (lquery-funcs:text))))
    (cl-ppcre:scan regex page-html-title)))

(defun page-show-result (page)
  (if (not (page-is-single page))
      (error "This page was called for show result but it doesn't have just one result!"))
  (let* ((entry (page-entry page))
         (title (entry-title entry))
         (contents (entry-contents entry)))
    (log:debug "Got the following title: ~a" (lquery-funcs:text title))
    (log:debug "Got the following contents: ~a" (lquery-funcs:text contents))
    (log:debug "Here's the navbar: ~a" (navbar-to-list (page-navbar page)))
    (format t (format-html-as-text (lquery-funcs:serialize contents)))))

(defun page-navbar (page)
  "Take in a goo page and return the navbar describing where in the dictionary you are"
  (lquery:$1 page "div#NR-wrapper nav"))

(defun page-entry (page)
  (lquery:$1 page "div#NR-main div#NR-main-in section div.section"))

(defun page-is-single (page)
  (page-html-title-regex-matcher ".*の意味.*" page))

(defun page-is-multiple (page)
  (page-html-title-regex-matcher ".*の検索結果.*" page))

(defun page-select-result (page)
  (if (not (page-is-multiple page))
      (error "This page was called for select-result but doesn't have multiple results!"))
  (princ (format nil "~a~%~%" "Select from the following results:"))
  (let ((links-and-texts (lquery:$ page ".section .search ul li a"
                                   (lquery-macros:combine (lquery-funcs:attr :href) (lquery:$1 "dt.title" (lquery-funcs:text))))))
    (map nil
         (lambda (entry number) (princ (format nil "~a ~a~%" number (cadr entry))))
         links-and-texts
         (loop for i from 1 to (length links-and-texts) collect i))
    (let* ((user-selection (parse-integer (read-line)))
           (relative-link (car (aref links-and-texts (- user-selection 1))))
           (absolute-link (format nil "~a~a" "https://dictionary.goo.ne.jp" relative-link)))
      (log:debug "Grabbing the following page and returning: ~a" absolute-link)
      (plump:parse (drakma:http-request absolute-link)))))

(defun navbar-to-list (navbar)
  (map 'list
       (lambda (navbar-item) (strip (lquery-funcs:text navbar-item)))
       (lquery:$ navbar "ol li")))

(defun entry-title (entry)
  (lquery:$1 entry "div.basic_title.nolink.jn h1"))

(defun entry-contents (entry)
  (lquery:$1 entry "div.contents_area div.contents"))

(defun title-match-readings (title)
  (cl-ppcre:register-groups-bind
      (readings nil old-readings word)
      ("(.*?)(〔(.*?)〕)?【(.*?)】" (lquery-funcs:text title))
    (values-list (list word readings old-readings))))

(defun page-title (page)
  (entry-title (page-entry page)))

(defun title-prettify (title)
  (if (eq nil title)
      "This title is bunk as fuck, bro"
      (multiple-value-bind (word readings old-readings) (title-match-readings title)
        (format nil
                "~A (~A) ~@[[~A]~]"
                word
                readings
                old-readings))))
