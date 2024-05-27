;;; timed-actions.el --- Schedule and perform timed actions -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/timed-actions
;; Version: 0.1.0
;; Keywords: calendar
;; Package-Requires: ((emacs "25.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to schedule and perform timed actions,
;; such as opening URLs at specified times and days of the week.

;; Usage:

;; 1. Customize your timed actions in the `timed-actions-events' variable.
;; 2. Use the `timed-actions-schedule-events' function to schedule these actions.

;; Example:

;; (require 'timed-actions)
;; (add-to-list 'timed-actions-events '(:time ("14:10" "Mon" "Wed")
;;                                      :action browse-url
;;                                      :args ("https://gnu.org")))
;; (timed-actions-schedule-events)

;;; Code:

(require 'notifications)
(defcustom timed-actions-events nil
  "List of timed actions with specified times, actions, and arguments.

A list of timed actions to be executed at specified times and days.

Each element in the list is a property list representing an event.
The `:time' key should be a cons cell where the car is a string
representing the time (e.g., \"10:00\") and the cdr is a set of days
\\=(e.g., \"Mon\", \"Tue\").

The `:action' key specifies the function to be called when the event
is triggered. Common actions include `browse-url', `timed-actions-alert',
and `timed-actions-alert-open-url'.

The `:args' key is a list of arguments to be passed to the action
function. Each argument can be a string, integer, or any valid
sexp.

Example:

  (:time (\"10:00\" \"Mon\")
   :action `browse-url'
   :args (\"https://gnu.org\"))"
  :type '(repeat (plist
                  :tag "Event"
                  :value (:time
                          ("10:00" "Mon")
                          :action browse-url
                          :args
                          ("https://gnu.org"))
                  :options
                  (((const
                     :format "%v "
                     :time)
                    (cons (string
                           :tag "Time (e.g. 10:20)"
                           :value "10:00")
                     (set :greedy t
                      (const :tag "Monday" "Mon")
                      (const :tag "Tuesday" "Tue")
                      (const :tag "Wednesday" "Wed")
                      (const :tag "Thursday" "Thu")
                      (const :tag "Friday" "Fri")
                      (const :tag "Saturday" "Sat")
                      (const :tag "Sunday" "Sun"))))
                   ((const
                     :format "%v "
                     :action)
                    (choice
                     :value browse-url
                     (const :tag "Show notification with URL"
                      timed-actions-alert-open-url)
                     (const :tag "Show notification" timed-actions-alert)
                     (const :tag "Browse URL" browse-url)
                     (function :tag "Other")))
                   ((const
                     :format "%v "
                     :args)
                    (repeat
                     (choice
                      string
                      integer
                      sexp))))))
  :group 'timed-actions)


(defvar timed-actions-dow-time-days '(("Monday" . 1)
                                      ("Tuesday" . 2)
                                      ("Wednesday" . 3)
                                      ("Thursday" . 4)
                                      ("Friday" . 5)
                                      ("Saturday" . 6)
                                      ("Sunday" . 0))
  "Days of the week mapped to their respective time offsets.")

(defvar timed-actions-dow-time-days-short
  '(("Mon" . 1)
    ("Tue" . 2)
    ("Wed" . 3)
    ("Thu" . 4)
    ("Fri" . 5)
    ("Sat" . 6)
    ("Sun" . 0))
  "Abbreviated day names mapped to their numeric weekday counterparts.")


(defun timed-actions-dow-read-day (day-input)
  "Prompt for a day of the week and calculate the date for that day.

Argument DAY-INPUT is a number representing the day of the week or a string that
can be associated with a day in `timed-actions-dow-time-days'."
  (interactive (list
                (completing-read "Day of week: " timed-actions-dow-time-days nil
                                 t)))
  (let* ((day (if (numberp day-input) day-input
                (cdr (assoc day-input timed-actions-dow-time-days))))
         (time (decode-time))
         (dow (nth 6 time))
         (day-of-month (nth 3 time))
         (new-dow (if (> day dow)       ; Check what's the new dow's index
                      (- day dow)       ; In the same week
                    (+ (- 7 dow) day))) ; In the next week
         (new-time (encode-time 0 0 0
                                (+ new-dow day-of-month)
                                (nth 4 time)
                                (nth 5 time))))
    (if (called-interactively-p 'any)
        (message (current-time-string new-time))
      new-time)))


(defun timed-actions-dow-time (day-input)
  "Calculate future date based on input day of week.

Argument DAY-INPUT is a number representing the day of the week or a string that
can be associated with a day of the week."
  (interactive (list
                (completing-read "Day of week: " timed-actions-dow-time-days nil t)))
  (let* ((day (if (numberp day-input) day-input
                (cdr (assoc day-input timed-actions-dow-time-days))))
         (time (decode-time))
         (dow (nth 6 time))
         (day-of-month (nth 3 time))
         (new-dow (if (> day dow)       ; Check what's the new dow's index
                      (- day dow)       ; In the same week
                    (+ (- 7 dow) day))) ; In the next week
         (new-time (encode-time 0 0 0
                                (+ new-dow day-of-month)
                                (nth 4 time)
                                (nth 5 time))))
    (if (called-interactively-p 'any)
        (message (current-time-string new-time)) ; Print nicely if interactive
      new-time)))

(defun timed-actions--date-part (time-decoded)
  "Extract the time portion from a decoded time list.

Argument TIME-DECODED is a list representing a decoded time value."
  (nthcdr 3 time-decoded))

(defun timed-actions--today ()
  "Decode the current time into a human-readable list."
  (decode-time (current-time)))

(defun timed-actions--tomorrow ()
  "Add 86400 seconds to current time and decode it."
  (decode-time (time-add 86400 (current-time))))


(defun timed-actions-convert-hh-mm-to-decoded (timestring)
  "Convert \"HH:MM\" string to a list with time components.

Argument TIMESTRING is a string representing time in HH:MM format."
  (interactive "sHH:MM: ")
  (let* ((split (split-string timestring "[^0-9]"))
         (h (string-to-number (nth 0 split)))
         (m (string-to-number (nth 1 split))))
    (append (list 0 m h)
            (timed-actions--date-part (timed-actions--today)))))

(defun timed-actions--decoded-time-day-short (decoded-time)
  "Return the short day name for a given DECODED-TIME.

Argument DECODED-TIME is a list representing a decoded time, which is suitable
for `encode-time'."
  (car (rassoc (string-to-number
                (format-time-string "%u"
                                    (apply #'encode-time decoded-time)))
               timed-actions-dow-time-days-short)))

(defun timed-actions--shift-day (days)
  "Shift to the next day in DAYS list.

Argument DAYS is a list of short day names (e.g., `Mon', `Tue') to find the next
matching day."
  (let ((curr (time-add 86400 (current-time))))
    (while (not (member (timed-actions--decoded-time-day-short (decode-time curr)) days))
      (setq curr (time-add 86400 curr)))
    (decode-time curr)))

(defun timed-actions--shift-to-tomorrow (time-date)
  "Shift the date in TIME-DATE to tomorrow.

Argument TIME-DATE is a list representing a date and time, where the date part
is to be shifted to tomorrow."
  (let ((td time-date))
    (setf (nthcdr 3 td) nil)
    (append td (timed-actions--date-part (timed-actions--tomorrow)))))

(defun timed-actions--shift-to-date (time-date target-date)
  "Change TIME-DATE list to match TARGET-DATE.

Argument TIME-DATE is a list representing a date and time.

Argument TARGET-DATE is a list representing a date."
  (let ((td time-date))
    (setf (nthcdr 3 td) nil)
    (append td (timed-actions--date-part target-date))))

(defun timed-actions--shift-to-tomorrow-maybe (time-date)
  "Shift date to tomorrow if it's in the past.

Argument TIME-DATE is a list representing a decoded time, which can be in the
format of `current-time' or `encode-time' functions output."
  (let ((td (encode-time time-date)))
    (if (time-less-p td nil)
        (timed-actions--shift-to-tomorrow time-date)
      time-date)))

(defun timed-actions--time-to-tomorrow-encoded (time)
  "Calculate encoded TIME for tomorrow from a given time string.

Argument TIME is a string representing a time in \"HH:MM\" format."
  (encode-time
   (timed-actions--shift-to-tomorrow-maybe
    (timed-actions-convert-hh-mm-to-decoded time))))

(defun timed-actions-run-at-time-today-or-tomorrow (time fn &rest args)
  "Schedule FN to run at TIME today or tomorrow.

Argument TIME is a string representing the time of day in the format \"HH:MM\".

Argument FN is the function to be called at the specified time.

Remaining arguments ARGS are the arguments to pass to FN when it is called."
  (let ((targs (append (list (timed-actions--time-to-tomorrow-encoded time) nil fn)
                       args)))
    (apply #'run-at-time targs)))

(defun timed-actions--read-time ()
  "Parse and format a time string as \"HH:MM\"."
  (format-time-string "%H:%M"
                      (if (fboundp 'idate-read)
                          (idate-read "Time: " (time-add
                                                (current-time)
                                                (seconds-to-time 70)))
                        (read-string "HH:MM"
                                     (format-time-string
                                      "%H:%M"
                                      (time-add
                                       (current-time)
                                       (seconds-to-time 70)))))))


(defun timed-actions--current-day-number ()
  "Calculate the current day of the week as a number."
  (nth 6 (decode-time (current-time))))

(defvar timed-actions-scheduled-timers nil
  "List of scheduled timers for keyboard macros.")

(defun timed-actions--schedule-action-with-callback (action actions-args
                                                            callback &rest
                                                            callback-args)
  "Schedule an ACTION with arguments and optionally execute a callback.

Argument ACTION is the function to be executed.

Argument ACTIONS-ARGS is a list of arguments to be passed to ACTION.

Argument CALLBACK is the function to be called after ACTION.

Remaining arguments CALLBACK-ARGS are the arguments to be passed to CALLBACK."
  (setq timed-actions-scheduled-timers (seq-remove #'timer--triggered timed-actions-scheduled-timers))
  (apply action actions-args)
  (when callback
    (apply callback callback-args)))

(defun timed-actions--schedule-action-event (event-info)
  "Schedule browsing a URL at a specified time and day.

Argument EVENT-INFO is a list where the first element is a URL string, the
second element is a time string in \"HH:MM\" format, and the remaining elements
are day identifiers."
  (pcase-let ((`(,hh-mm . ,days-weeks)
               (plist-get event-info :time))
              (action (plist-get event-info :action))
              (args (plist-get event-info :args))
              (short-days)
              (endoded-hh-mm)
              (encoded-result)
              (time-less-then-today)
              (is-current-day)
              (timer)
              (days-numbers))
    (setq days-numbers (mapcar
                        (lambda (it)
                          (if (numberp it)
                              it
                            (cdr (assoc it timed-actions-dow-time-days-short))))
                        days-weeks))
    (setq short-days (mapcar
                      (lambda (it) (car (rassoc it timed-actions-dow-time-days-short)))
                      days-numbers))
    (setq endoded-hh-mm (timed-actions-convert-hh-mm-to-decoded hh-mm))
    (setq is-current-day (member (timed-actions--current-day-number) days-numbers))
    (setq time-less-then-today (time-less-p (encode-time endoded-hh-mm) nil))
    (setq encoded-result (if (or time-less-then-today
                                 (not is-current-day))
                             (encode-time
                              (timed-actions--shift-to-date
                               endoded-hh-mm
                               (timed-actions--shift-day
                                short-days)))
                           (encode-time endoded-hh-mm)))
    (message (format-time-string "Scheduled event at %a %H:%M, %d %b "
                                 encoded-result))
    (setq timer (run-at-time encoded-result nil
                             #'timed-actions--schedule-action-with-callback
                             action
                             args
                             #'timed-actions--schedule-action-event event-info))
    (setq timed-actions-scheduled-timers (push timer timed-actions-scheduled-timers))))

(defun timed-actions-alert-on-close-function (id reason)
  "Display a message with ID and REASON for closure.

Argument ID is an identifier for the message being closed.

Argument REASON is a string describing why the message is being closed."
  (message "Message %d, closed due to \"%s\"" id reason))


(defun timed-actions-alert-plist-merge (plist-a plist-b)
  "Add props from PLIST-B to PLIST-A."
  (dotimes (idx (length plist-b))
    (when (eq (logand idx 1) 0)
      (let ((prop-name (nth idx plist-b)))
        (let ((val (plist-get plist-b prop-name)))
          (plist-put plist-a prop-name val)))))
  plist-a)

(defun timed-actions-alert-open-url (url)
  "Display a notification asking to open a URL.

Argument URL is a string representing the web address to be opened."
  (notifications-notify
   :title "Title"
   :body "Open URL?"
   :actions '("Confirm" "I agree" "Refuse" "I disagree")
   :urgency 'critical
   :on-action (lambda (_id key)
                (pcase key
                  ("Confirm"
                   (browse-url url))))
   :on-close 'timed-actions-alert-on-close-function))

(defun timed-actions-run-at-time (time fn &rest args)
  "Schedule FN to run after TIME with ARGS.

Argument TIME specifies when to run FN; it can be a time string or a number of
seconds from now.

Argument FN is the function to be called when TIME elapses.

Remaining arguments ARGS are the arguments to pass to FN when it is called."
  (apply #'run-at-time time nil fn args))


(defvar timed-actions-alerts nil)

;;;###autoload
(defun timed-actions-alert-schedule ()
  "Prompt for a message and schedule an alert or URL opening based on input."
  (interactive)
  (let ((msg (read-string "Message: ")))
    (if (string-match-p "^https://" msg)
        (timed-actions-run-at-time-today-or-tomorrow
         (timed-actions--read-time)
         #'timed-actions-alert-open-url msg)
      (timed-actions-run-at-time
       (when (fboundp 'idate-read)
         (idate-read "Message at: "))
       #'timed-actions-alert msg))))

;;;###autoload
(defun timed-actions-alert (status-message &rest rest)
  "Display a notification with an optional status message.

Optional argument STATUS-MESSAGE is a string for the notification message.

Remaining arguments REST are additional properties for the notification."
  (interactive "sMessage: ")
  (let ((args (list
               :title status-message
               :urgency 'critical)))
    (let ((id))
      (while (setq id (pop timed-actions-alerts))
        (when (fboundp 'notifications-close-notification)
          (notifications-close-notification id)))
      (if (fboundp 'notifications-notify)
          (setq id (apply #'notifications-notify
                          (timed-actions-alert-plist-merge args rest)))
        (message (plist-get args :title)))
      (when id (push id timed-actions-alerts)))))

(defun timed-actions-schedule-events ()
  "Schedule browsing URLs at specified times."
  (dolist (it timed-actions-events)
    (timed-actions--schedule-action-event it)))

(provide 'timed-actions)
;;; timed-actions.el ends here
