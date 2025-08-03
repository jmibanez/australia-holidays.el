;;; australia-holidays.el --- Australian holidays for calendar -*- lexical-binding: t -*-

;; Copyright (C) 2025 JM Ibañez

;; Author: JM Ibañez <jm@jmibanez.com>
;; URL: https://github.com/jmibanez/australia-holidays.el
;; Version: 1.0.0
;; Package-Requires: ((emacs "24.3"))
;; Keywords: calendar

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Replace holidays with holidays valid across all Australian states
;; and territories; e.g.:
;;
;; (setq calendar-holidays australia-holidays)
;;
;; Or append holidays:
;;
;; (setq calendar-holidays (append calendar-holidays australia-holidays))
;;
;; You can also use a more specific list tailored to your state; e.g.
;; for New South Wales use australia-holidays-for-nsw.
;;
;;; Code:

(eval-when-compile
  (require 'calendar)
  (require 'holidays)
  (require 'cl-lib))

;;;###autoload
(defcustom australia-holidays-january-26-label "Australia Day"
  "What to call the holiday celebrated on January 26."
  :type 'string
  :group 'calendar)

;;;###autoload
(defcustom australia-holidays-include-january-26 t
  "Whether to include January 26 in the list of holidays."
  :type 'boolean
  :group 'calendar)

;;;###autoload
(defcustom australia-holidays-states-to-include nil
  "List of Australian states/territories to include in `australia-holidays`.
Possible values are symbols: :act, :nsw, :nt, :qld, :sa, :tas, :vic, :wa.
If nil, only national holidays are included."
  :type '(repeat (choice
                  (const :tag "Australian Capital Territory" :act)
                  (const :tag "New South Wales" :nsw)
                  (const :tag "Northern Territory" :nt)
                  (const :tag "Queensland" :qld)
                  (const :tag "South Australia" :sa)
                  (const :tag "Tasmania" :tas)
                  (const :tag "Victoria" :vic)
                  (const :tag "Western Australia" :wa)))
  :group 'calendar)

(defvar australia-holidays-state-alist
  '((:act . australia-holidays-for-act)
    (:nsw . australia-holidays-for-nsw)
    (:nt  . australia-holidays-for-nt)
    (:qld . australia-holidays-for-qld)
    (:sa  . australia-holidays-for-sa)
    (:tas . australia-holidays-for-tas)
    (:vic . australia-holidays-for-vic)
    (:wa  . australia-holidays-for-wa))
  "Alist mapping Australian state/territory symbols to their holiday variables.")

(defun australia-holidays--resolve (sym)
  "Resolve SYM (a variable or list) to a holiday list."
  (let ((val (symbol-value sym)))
    (if (and (listp val) (symbolp (car val)))
        (mapcan #'australia-holidays--resolve val)
      val)))

(defun australia-holidays--for-states (states)
  "Return a merged list of holidays for STATES (list of symbols)."
  (let ((holidays
         (mapcan (lambda (state)
                   (let ((var (cdr (assoc state australia-holidays-state-alist))))
                     (when var (australia-holidays--resolve var))))
                 states)))
    ;; Remove duplicate holidays by label and main date
    (cl-remove-duplicates holidays
                         :test (lambda (h1 h2)
                                 (equal (list (nth 0 h1) (nth 1 h1) (nth 2 h1))
                                        (list (nth 0 h2) (nth 1 h2) (nth 2 h2)))))))

(defvar australia-holidays--national
  '((holiday-fixed 1 1 "New Year")
    (if australia-holidays-include-january-26
        (holiday-fixed 1 26 australia-holidays-january-26-label))
    (holiday-easter-etc -2 "Good Friday")
    (holiday-easter-etc 1 "Easter Monday")
    (holiday-fixed 4 25 "ANZAC Day")
    (holiday-fixed 12 25 "Christmas Day"))
  "Holidays valid in all states and territories.")

;;;###autoload
(defvar australia-holidays nil
  "Australian holidays based on `australia-holidays-states-to-include`.")

(defun australia-holidays--update ()
  "Update `australia-holidays` according to `australia-holidays-states-to-include`."
  (setq australia-holidays
        (if (and australia-holidays-states-to-include
                 (listp australia-holidays-states-to-include)
                 australia-holidays-states-to-include)
            (australia-holidays--for-states australia-holidays-states-to-include)
          (let ((national (symbol-value 'australia-holidays--national)))
            (or national
                '((holiday-fixed 1 1 "New Year")
                  (if australia-holidays-include-january-26
                      (holiday-fixed 1 26 australia-holidays-january-26-label))
                  (holiday-easter-etc -2 "Good Friday")
                  (holiday-easter-etc 1 "Easter Monday")
                  (holiday-fixed 4 25 "ANZAC Day")
                  (holiday-fixed 12 25 "Christmas Day")))))))

(defun australia-holidays--states-setter (sym val)
  (set-default sym val)
  (australia-holidays--update))
(put 'australia-holidays-states-to-include 'custom-set #'australia-holidays--states-setter)

;;;###autoload
(defvar australia-holidays-for-act
  '(australia-holidays
    (holiday-float 3 1 2 "Canberra Day")
    (holiday-easter-etc -1 "Easter Saturday")
    (holiday-easter-etc 0 "Easter Sunday")
    (holiday-float 5 1 1 "Reconciliation Day" 26)
    (holiday-float 6 1 2 "King's Birthday")
    (holiday-float 10 1 1 "Labour Day")
    (holiday-fixed 12 26 "Boxing Day"))
  "Holidays in the Australian Capital Territory.")

;;;###autoload
(defvar australia-holidays-for-nsw
  (append australia-holidays
          '((holiday-easter-etc -1 "Easter Saturday")
            (holiday-easter-etc 0 "Easter Sunday")
            (holiday-fixed 4 25 "ANZAC Day")
            (holiday-float 6 1 2 "King's Birthday")
            (holiday-float 10 1 1 "Labour Day")
            (holiday-fixed 12 26 "Boxing Day")))
  "Holidays in New South Wales.")

;;;###autoload
(defvar australia-holidays-for-nt
  (append australia-holidays
          '((holiday-easter-etc -1 "Easter Saturday")
            (holiday-easter-etc 0 "Easter Sunday")
            (holiday-float 5 1 1 "May Day")
            (holiday-float 6 1 2 "King's Birthday")
            (holiday-float 8 1 1 "Picnic Day")
            (holiday-fixed 12 24 "Christmas Eve")
            (holiday-fixed 12 26 "Boxing Day")
            (holiday-fixed 12 31 "New Year's Eve")))
  "Holidays in the Northern Territory.")

;;;###autoload
(defvar australia-holidays-for-qld
  (append australia-holidays
          '((holiday-easter-etc -1 "The Day After Good Friday")
            (holiday-easter-etc 0 "Easter Sunday")
            (holiday-float 5 1 1 "Labour Day")
            (holiday-float 8 3 1 "Royal Queensland Show" 9)
            (holiday-float 10 1 1 "King's Birthday")
            (holiday-fixed 12 24 "Christmas Eve")
            (holiday-fixed 12 26 "Boxing Day")))
  "Holidays in Queensland.")

;;;###autoload
(defvar australia-holidays-for-sa
  (append australia-holidays
          '((holiday-float 3 1 2 "Adelaide Cup Day")
            (holiday-easter-etc -1 "Easter Saturday")
            (holiday-easter-etc 0 "Easter Sunday")
            (holiday-float 6 1 2 "King's Birthday")
            (holiday-float 10 1 1 "Labour Day")
            (holiday-fixed 12 24 "Christmas Eve")
            (holiday-fixed 12 26 "Proclamation Day")
            (holiday-fixed 12 31 "New Year's Eve")))
  "Holidays in South Australia.")

;;;###autoload
(defvar australia-holidays-for-tas
  (append australia-holidays
          '((holiday-float 2 1 2 "Royal Hobart Regatta")
            (holiday-float 3 1 2 "Eight Hours Day")
            (holiday-easter-etc 2 "Easter Tuesday")
            (holiday-float 6 1 2 "King's Birthday")
            (holiday-float 11 1 1 "Recreation Day")
            (holiday-fixed 12 26 "Boxing Day")))
  "Holidays in Tasmania.")

;;;###autoload
(defvar australia-holidays-for-vic
  (append australia-holidays
          '((holiday-float 3 1 2 "Labour Day")
            (holiday-easter-etc -1 "Saturday Before Easter Sunday")
            (holiday-easter-etc 0 "Easter Sunday")
            (holiday-float 6 1 2 "King's Birthday")
            (holiday-float 11 2 1 "Melbourne Cup")
            (holiday-float 9 5 -1
                           "Friday before AFL Grand Final"
                           29)
            (holiday-fixed 12 26 "Boxing Day")))
  "Holidays in Victoria.")

;;;###autoload
(defvar australia-holidays-for-wa
  (append australia-holidays
          '((holiday-float 3 1 1 "Labour Day")
            (holiday-easter-etc 0 "Easter Sunday")
            (holiday-float 6 1 1 "Western Australia Day")
            (holiday-fixed 12 26 "Boxing Day")))
  "Holidays in Western Australia.")

(provide 'australia-holidays)

;;; australia-holidays.el ends here
