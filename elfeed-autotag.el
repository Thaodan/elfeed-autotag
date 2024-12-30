;;; elfeed-autotag.el --- Easy auto-tagging for elfeed -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2021 Paul Elms
;; Copyright (C) 2024 Björn Bidar
;; Derived from elfeed-org by Remy Honig
;;
;; Author: Paul Elms <https://paul.elms.pro>
;; 	Björn Bidar <me@thaodan.de>
;; Maintainer: Paul Elms <paul@elms.pro>
;; Version: 0.1.1
;; Keywords: news
;; Homepage: https://github.com/paulelms/elfeed-autotag
;; Package-Requires: ((emacs "27.1") (elfeed "3.4.1") (elfeed-protocol "0.8.0") (org "8.2.7") (dash "2.10.0") (s "1.9.0"))
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;  Easy auto-tagging for elfeed-protocol (and elfeed in general).
;;  Thanks to elfeed-org by Remy Honig for starting point.
;;
;;; Code:

(require 'elfeed)
(require 'elfeed-protocol)
(require 'org)
(require 'org-element)
(require 'cl-lib)

(defgroup elfeed-autotag nil
  "Configure the Elfeed RSS reader with an Orgmode file."
  :group 'comm)

(defcustom elfeed-autotag-tree-id "elfeed"
  "The tag or ID property on the trees containing the RSS feeds."
  :group 'elfeed-autotag
  :type 'string)

(defcustom elfeed-autotag-ignore-tag "ignore"
  "The tag on the feed trees that will be ignored."
  :group 'elfeed-autotag
  :type 'string)

(defcustom elfeed-autotag-files (list (locate-user-emacs-file "elfeed.org"))
  "The files where we look to find trees with the `elfeed-autotag-tree-id'."
  :group 'elfeed-autotag
  :type '(repeat (file :tag "org-mode file")))

;; (defcustom elfeed-autotag-protocol-used nil
;;   "Is elfeed-protocol used as source for `elfeed-feeds'."
;;   :group 'elfeed-autotag
;;   :type 'boolean)

(defvar elfeed-autotag-protocol-used t
  "Is elfeed-protocol used as source for `elfeed-feeds'.")

(defvar elfeed-autotag--new-entry-hook nil
  "List of new-entry tagger hooks created by `elfeed-autotag'.")


(defconst elfeed-autotag--filter-keywords
   ;; From elfeed/README.md:: # Org-store-link and Org-capture
   '("feed-title" ;; The title of the feed supplied by the feed
     "feed-url" ;; The URL of the feed
     "feed-author" ;; List of feed authors names
     "entry-title" ;; Title of the feed
     "entry-link" ;; Feed entry external link
     "entry-content-type" ;; Feed entry content type as in e.g. HTML
     "entry-enclosure" ;; The feed enclosure such as for example a preview picture
     )
   "List of possible keywords.")

(defconst elfeed-autotag--filter-regexp
  (regexp-opt
   (cl-union
    elfeed-autotag--filter-keywords
    '("http")))
  "Regular expression to match possible filters.")

(defun elfeed-autotag--check-configuration-file (file)
  "Make sure FILE exists."
  (when (not (file-exists-p file))
    (error "Elfeed-autotag cannot open %s.  Make sure it exists or customize the variable \'elfeed-autotag-files\'"
           (abbreviate-file-name file))))

(defun elfeed-autotag--import-trees (tree-id)
  "Get trees with \":ID:\" property or tag of value TREE-ID.
Return trees with TREE-ID as the value of the id property or
with a tag of the same value.  Setting an \":ID:\" property is not
recommended but I support it for backward compatibility of
current users."
  (org-element-map
      (org-element-parse-buffer)
      'headline
    (lambda (h)
      (when (or (member tree-id (org-element-property :tags h))
                (equal tree-id (org-element-property :ID h))) h))))

(defun elfeed-autotag--convert-tree-to-headlines (parsed-org)
  "Get the inherited tags from PARSED-ORG structure if MATCH-FUNC is t.
The algorithm to gather inherited tags depends on the tree being
visited depth first by `org-element-map'.  The reason I don't use
`org-get-tags-at' for this is that I can reuse the parsed org
structure and I am not dependent on the setting of
`org-use-tag-inheritance' or an org buffer being present at
all.  Which in my opinion makes the process more traceable."
  (let* ((tags '())
         (level 1))
    (org-element-map parsed-org 'headline
      (lambda (h)
        (let* ((current-level (org-element-property :level h))
               (delta-level (- current-level level))
               (delta-tags (mapcar (lambda (elmnt)(intern (substring-no-properties elmnt)))
                                  (org-element-property :tags h)))
               (heading (org-element-property :raw-value h)))
          ;; update the tags stack when we visit a parent or sibling
          (unless (> delta-level 0)
            (let ((drop-num (+ 1 (- delta-level))))
              (setq tags (nthcdr drop-num tags))))
          ;; save current level to compare with next heading that will be visited
          (setq level current-level)
          ;; save the tags that might apply to potential children of the current heading
          (push (append (car tags) delta-tags) tags)
          ;; return the heading and inherited tags
          (append (list heading)
                   (car tags)))))))

(defun elfeed-autotag--filter-relevant (list)
  "Filter relevant entries from the LIST."
  (seq-filter
   (lambda (entry)
     (and
      (string-match elfeed-autotag--filter-regexp (car entry))
      (not (member (intern elfeed-autotag-ignore-tag) entry))))
   list))

(defun elfeed-autotag--cleanup-headlines (headlines tree-id)
  "In all HEADLINES given remove the TREE-ID."
  (mapcar (lambda (e) (delete tree-id e)) headlines))

(defun elfeed-autotag--import-headlines-from-files (files tree-id)
  "Visit FILES and return headlines tagged or property ID containing TREE-ID."
  (seq-uniq (seq-mapcat (lambda (file)
                        (with-current-buffer (find-file-noselect (expand-file-name file))
                          (org-mode)
                          (elfeed-autotag--cleanup-headlines
                           (elfeed-autotag--filter-relevant
                            (elfeed-autotag--convert-tree-to-headlines
                             (elfeed-autotag--import-trees tree-id)))
                           (intern tree-id))))
                      files)))


(defun elfeed-autotag--convert-headline-to-tagger-params (tagger-headline)
  "Add new entry hooks for tags configured in TAGGER-HEADLINE."
   (when-let* ((first-in-headline (car tagger-headline))
               (matched-keyword (seq-find
                                 (lambda (kw) (string-prefix-p
                                               kw
                                               first-in-headline))
                                 elfeed-autotag--filter-keywords))
               (matched-keyword (concat matched-keyword ":"))
               (matched-keyword-parameter
                (string-trim (string-remove-prefix
                              matched-keyword
                              first-in-headline))))
     (unless (string-empty-p matched-keyword-parameter)
       (list matched-keyword-parameter
             (cdr tagger-headline)))))

(defun elfeed-autotag--export-keyword-hook (keyword tagger-params)
  "Make tagger for TAGGER-PARAMS WITH KEYWORD."
  (add-hook 'elfeed-autotag--new-entry-hook
            (elfeed-make-tagger
             ;; We have to use `intern-soft' here so `keywordp'
             ;; is true that the keyword is valid.
             ;; `make-symbol' would not be enough as the created
             ;; symbol has to be in the symbol table.
             (intern-soft (concat ":" keyword)) (nth 0 tagger-params)
             :add (nth 1 tagger-params))))

(defun elfeed-autotag--export-headline-hook (headline)
  "Export HEADLINE to the proper `elfeed' structure."
  (let* ((feed-url (nth 0 headline))
         (tags-and-title (cdr headline))
         (tags (if (stringp (car (last tags-and-title)))
                   (butlast tags-and-title)
                 tags-and-title)))
    ;; TODO raw url sometimes not suitable for this function
    (add-hook 'elfeed-autotag--new-entry-hook
              (elfeed-make-tagger
               :feed-url feed-url
               :add tags))))

(defun elfeed-autotag--export-titles (headline)
  "Export HEADLINE to elfeed titles."
  (if (and (stringp (car (last headline)))
           (> (length headline) 1))
      (progn
        (let* ((feed-id (if elfeed-autotag-protocol-used
                            (elfeed-protocol-format-subfeed-id (caar elfeed-feeds) (car headline))
                          (car headline)))
               (feed (elfeed-db-get-feed feed-id)))
          (setf (elfeed-meta feed :title) (car (last headline)))
          (elfeed-meta feed :title)))))

(defun elfeed-autotag--filter-keyword-taggers (keyword headlines)
  "Filter tagging rules for KEYWORD from the HEADLINES in the tree."
  (seq-filter (lambda (headline)
                (string-prefix-p keyword (car headline)))
             headlines))


(defun elfeed-autotag--filter-subscriptions (headlines)
  "Filter subscriptions to rss feeds from the HEADLINES in the tree."
  (seq-filter (lambda (headline)
                (let* ((text (car headline))
                       (link-and-title (string-prefix-p "^\\[\\[\\(http.+?\\)\\]\\[\\(.+?\\)\\]\\]" text))
                       (hyperlink (string-prefix-p "^\\[\\[\\(http.+?\\)\\]\\(?:\\[.+?\\]\\)?\\]" text)))
                  (cond ((string-prefix-p "http" text) headline)
                        (link-and-title (append (list (nth 1 hyperlink))
                                                (cdr headline)
                                                (list (nth 2 link-and-title))))
                        (hyperlink (append (list (nth 1 hyperlink)) (cdr headline))))))
              headlines))


(defun elfeed-autotag--run-new-entry-hook (entry)
  "Run ENTRY through `elfeed-autotag' taggers."
  (dolist (hook elfeed-autotag--new-entry-hook)
    (funcall hook entry)))

(defun elfeed-autotag-process (files tree-id)
  "Process headlines and taggers from FILES with org headlines with TREE-ID."

  ;; Warn if configuration files are missing
  (mapc #'elfeed-autotag--check-configuration-file files)

  ;; Clear elfeed structures
  (setq elfeed-autotag--new-entry-hook nil)

  ;; Convert org structure to elfeed structure and register taggers
  (let* ((headlines (elfeed-autotag--import-headlines-from-files
                     files tree-id))
         (feeds (elfeed-autotag--filter-subscriptions headlines))
         (keywords elfeed-autotag--filter-keywords))
    (dolist (keyword keywords)
      (when-let*
          ((taggers (elfeed-autotag--filter-keyword-taggers
                     keyword headlines))
           (taggers (mapcar #'elfeed-autotag--convert-headline-to-tagger-params
                            taggers)))
        (dolist (tagger taggers)
                  (elfeed-autotag--export-keyword-hook keyword tagger))))

      (mapc #'elfeed-autotag--export-headline-hook feeds)
      (mapc #'elfeed-autotag--export-titles feeds))

  (elfeed-log 'info "elfeed-autotag loaded %i rules"
              (length elfeed-autotag--new-entry-hook)))

(defun elfeed-autotag-load-before-configure ()
  "Load all feed rules before Elfeed is started."
  (elfeed-autotag-process elfeed-autotag-files elfeed-autotag-tree-id))

;;;###autoload
(defun elfeed-autotag ()
  "Setup auto-tagging rules."
  (interactive)
  (elfeed-log 'info "elfeed-autotag initialized")
  (advice-add 'elfeed :before #'elfeed-autotag-load-before-configure)
  (add-hook 'elfeed-new-entry-hook #'elfeed-autotag--run-new-entry-hook))

(provide 'elfeed-autotag)
;;; elfeed-autotag.el ends here
