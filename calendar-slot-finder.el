;;; calendar-slot-finder.el --- Find empty time slots in your calendar -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Assistant
;; Keywords: calendar, org-mode, scheduling
;; Version: 1.0
;; Package-Requires: ((emacs "26.1") (org "9.0"))

;;; Commentary:

;; This package provides functionality to find empty time slots in your
;; calendar by integrating org-agenda-files and org-gcal entries.
;; It uses a Python backend for the heavy lifting.

;;; Code:

(require 'org)
(require 'org-agenda)
(require 'json)
(require 'seq)
(require 'subr-x)

;;; Customization

(defgroup calendar-slot-finder nil
  "Find empty calendar slots."
  :group 'org
  :prefix "csf-")

(defcustom csf-python-script-path nil
  "Path to the Python calendar slot finder script.
If nil, assumes the script is in the same directory as this file."
  :type '(choice (const nil) file)
  :group 'calendar-slot-finder)

(defcustom csf-default-week-offset 0
  "Default week offset (0 = current week, 1 = next week)."
  :type 'integer
  :group 'calendar-slot-finder)

(defcustom csf-gcal-data-file nil
  "Path to JSON file containing Google Calendar data from org-gcal."
  :type '(choice (const nil) file)
  :group 'calendar-slot-finder)

(defcustom csf-working-hours '("09:00" "17:00")
  "Working hours as (start-time end-time) in HH:MM format."
  :type '(list string string)
  :group 'calendar-slot-finder)

(defcustom csf-min-slot-duration 60
  "Minimum duration for a free slot in minutes."
  :type 'integer
  :group 'calendar-slot-finder)

(defcustom csf-slot-duration 30
  "Default slot duration in minutes to split each free gap into.
Prompted for on every interactive `csf-find-slots' call (30, 45, or a
custom value); this is only the fallback used by non-interactive
callers such as `csf-find-slots-quick'."
  :type 'integer
  :group 'calendar-slot-finder)

(defcustom csf-meeting-cushion 15
  "Buffer time in minutes before and after meetings."
  :type 'integer
  :group 'calendar-slot-finder)

(defcustom csf-default-meeting-duration 30
  "Default meeting duration in minutes for email suggestions."
  :type 'integer
  :group 'calendar-slot-finder)

(defcustom csf-num-weeks 1
  "Number of weeks to display slots for.
Set to 2 or more to select slots across multiple weeks."
  :type 'integer
  :group 'calendar-slot-finder)

(defcustom csf-inbox-file "~/Documents/0Inbox/index.org"
  "Path to the inbox org file where tentative meetings are stored."
  :type 'file
  :group 'calendar-slot-finder)

;;; Google Calendar exceptions

(defcustom csf-gcal-calendar-aliases nil
  "Alist of (SYMBOL . GOOGLE-CALENDAR-ID) naming calendars.
Lets `csf-excluded-gcal-calendars' refer to calendars by a short symbol
instead of their long ID.  May also be a variable holding such an alist,
e.g. `modern-emacs/gcal-calendar-ids'."
  :type '(choice (alist :key-type symbol :value-type string)
                 (variable :tag "Variable holding an alist"))
  :group 'calendar-slot-finder)

(defcustom csf-excluded-gcal-calendars nil
  "Google calendars whose events never block a free slot.
Each element is either
 - a string: a regexp matched (`string-match-p') against the calendar ID
   stored in the org-gcal `entry-id' property (EVENTID/CALENDARID).  An
   exact ID works as-is; anchor with ^ and $ to be strict; or
 - a symbol: looked up in `csf-gcal-calendar-aliases'.
Use this to skip calendars that mirror your own Emacs tasks, e.g.
  (setq csf-excluded-gcal-calendars \='(work personal clocked done))"
  :type '(repeat (choice symbol string))
  :group 'calendar-slot-finder)

(defcustom csf-include-plain-org-entries t
  "Whether org entries without a Google Calendar entry-id block time.
Set to nil so only real Google Calendar events (minus the calendars in
`csf-excluded-gcal-calendars') count as busy."
  :type 'boolean
  :group 'calendar-slot-finder)

(defun csf--excluded-calendar-patterns ()
  "Resolve `csf-excluded-gcal-calendars' to a list of regexp strings."
  (let ((aliases (if (and (symbolp csf-gcal-calendar-aliases)
                          (boundp csf-gcal-calendar-aliases))
                     (symbol-value csf-gcal-calendar-aliases)
                   csf-gcal-calendar-aliases))
        patterns)
    (dolist (entry csf-excluded-gcal-calendars)
      (cond
       ((stringp entry) (push entry patterns))
       ((symbolp entry)
        (let ((id (cdr (assq entry aliases))))
          (if id
              (push id patterns)
            (message "csf: no calendar alias for `%s', ignoring" entry))))))
    (nreverse patterns)))

;;; Variables

(defvar csf-weekly-restrictions nil
  "List of weekly restrictions.")

(defvar csf-current-week-offset 0
  "Current week offset being displayed.")

(defvar csf-last-search-params nil
  "Parameters from the last search for navigation.")

(defvar csf-current-slot-duration nil
  "Slot duration in minutes used for the current/last search.")

(defvar csf-free-slots nil
  "List of free slots from the last search.")

(defvar csf-last-slot-count 0
  "Number of slots in the last search.")

;;; Keymap and Mode

(defvar csf-results-mode-map (make-sparse-keymap)
  "Keymap for Calendar Slot Finder results buffer.")

(define-key csf-results-mode-map "q" 'quit-window)
(define-key csf-results-mode-map "n" 'csf-next-week)
(define-key csf-results-mode-map "p" 'csf-previous-week)
(define-key csf-results-mode-map "r" 'csf-refresh-current-week)
(define-key csf-results-mode-map "g" 'csf-goto-week)
(define-key csf-results-mode-map "c" 'csf-copy-email-suggestion)
(define-key csf-results-mode-map "w" 'csf-set-num-weeks)
(define-key csf-results-mode-map "?" 'csf-show-help)

(define-derived-mode csf-results-mode fundamental-mode "Calendar-Slots"
  "Major mode for Calendar Slot Finder results buffer."
  (use-local-map csf-results-mode-map)
  (setq buffer-read-only t)
  (setq truncate-lines nil))

;;; Navigation Functions

(defun csf-next-week ()
  "Show calendar slots for the next week."
  (interactive)
  (when csf-last-search-params
    (setq csf-current-week-offset (1+ csf-current-week-offset))
    (csf--refresh-with-current-params)))

(defun csf-previous-week ()
  "Show calendar slots for the previous week."
  (interactive)
  (when csf-last-search-params
    (if (<= csf-current-week-offset 0)
        (message "❌ Cannot navigate before the current week")
      (setq csf-current-week-offset (1- csf-current-week-offset))
      (csf--refresh-with-current-params))))

(defun csf-refresh-current-week ()
  "Refresh the current week's calendar slots."
  (interactive)
  (when csf-last-search-params
    (csf--refresh-with-current-params)))

(defun csf-goto-week (offset)
  "Go to a specific week offset."
  (interactive "nWeek offset (0=current, 1=next, -1=previous): ")
  (when csf-last-search-params
    (if (< offset 0)
        (message "❌ Cannot navigate before the current week")
      (setq csf-current-week-offset offset)
      (csf--refresh-with-current-params))))

(defun csf-set-num-weeks (weeks)
  "Set the number of weeks to display and refresh."
  (interactive "nNumber of weeks to show: ")
  (setq csf-num-weeks (max 1 weeks))
  (when csf-last-search-params
    (csf--refresh-with-current-params))
  (message "Showing %d week(s)" csf-num-weeks))

(defun csf-show-help ()
  "Show help for Calendar Slot Finder."
  (interactive)
  (message "Calendar Slot Finder: q=quit, n=next week, p=previous week, r=refresh, g=goto week, w=set weeks, c=copy email+org, ?=help"))

;;; Email Suggestion Functions

(defun csf--first-slot-shorter-than (slots-vec duration)
  "Return the first slot-info in SLOTS-VEC whose duration_minutes is
less than DURATION, or nil if none are shorter (or duration is missing)."
  (catch 'found
    (dotimes (i (length slots-vec))
      (let* ((slot-info (aref slots-vec i))
             (slot-dur (gethash "duration_minutes" slot-info)))
        (when (and slot-dur (< slot-dur duration))
          (throw 'found slot-info))))
    nil))

(defun csf--create-org-timestamp (date-str time-str duration)
  "Create org timestamp from date, time, and duration.
DATE-STR: 'Wed, Nov 05, 2025'
TIME-STR: '2:30pm' or '2:30PM'
DURATION: meeting duration in minutes
Returns: '<2025-11-05 Wed 14:30-15:00>'"
  (let* (;; Parse date: "Wed, Nov 05, 2025"
         (date-parts (split-string date-str ", "))
         (day-name (nth 0 date-parts))
         (month-day-str (nth 1 date-parts))
         (year (nth 2 date-parts))
         (month-day-parts (split-string month-day-str))
         (month-str (nth 0 month-day-parts))
         (day-num (nth 1 month-day-parts))
         
         ;; Convert month to number
         (month-num (cond
                     ((string= month-str "Jan") "01") ((string= month-str "Feb") "02")
                     ((string= month-str "Mar") "03") ((string= month-str "Apr") "04")
                     ((string= month-str "May") "05") ((string= month-str "Jun") "06")
                     ((string= month-str "Jul") "07") ((string= month-str "Aug") "08")
                     ((string= month-str "Sep") "09") ((string= month-str "Oct") "10")
                     ((string= month-str "Nov") "11") ((string= month-str "Dec") "12")
                     (t "01")))
         
         ;; Parse time: "2:30pm" or "2:30PM" (case-insensitive)
         (time-lower (downcase time-str))
         (time-match (string-match "\\([0-9]+\\):\\([0-9]+\\)\\(am\\|pm\\)" time-lower))
         (hour-str (if time-match (match-string 1 time-lower) "0"))
         (minute-str (if time-match (match-string 2 time-lower) "0"))
         (ampm (if time-match (match-string 3 time-lower) "am"))
         (hour (string-to-number hour-str))
         (minute (string-to-number minute-str))
         
         ;; Convert to 24-hour format
         (hour-24 (if (string= ampm "pm")
                      (if (= hour 12) 12 (+ hour 12))
                    (if (= hour 12) 0 hour)))
         
         ;; Calculate end time
         (total-minutes (+ (* hour-24 60) minute duration))
         (end-hour (/ total-minutes 60))
         (end-minute (mod total-minutes 60)))
    
    ;; Return org timestamp in correct format
    (format "<%s-%s-%s %s %02d:%02d-%02d:%02d>" 
            year month-num day-num day-name
            hour-24 minute end-hour end-minute)))

(defun csf--create-org-entries (slots-info duration person)
  "Create tentative meeting entries in inbox org file.
SLOTS-INFO: list of hash tables (1 or 2) with 'date' and 'time' keys
DURATION: Meeting duration in minutes
PERSON: Who the meeting is with"
  (let* ((inbox-path (expand-file-name csf-inbox-file)))
    (if (file-exists-p inbox-path)
        (with-current-buffer (find-file-noselect inbox-path)
          (goto-char (point-max))
          (dolist (slot-info slots-info)
            (let* ((slot-date (gethash "date" slot-info))
                   (slot-time (gethash "time" slot-info))
                   (timestamp (csf--create-org-timestamp slot-date slot-time duration)))
              (insert "\n")
              (insert (format "* Tentative Meeting with %s\n" person))
              (insert (format "SCHEDULED: %s\n" timestamp))))
          (save-buffer)
          (message "✅ Added %d tentative meeting%s to %s"
                   (length slots-info)
                   (if (= (length slots-info) 1) "" "s")
                   inbox-path))
      (message "⚠️  Inbox file not found at %s" inbox-path))))

(defun csf-copy-email-suggestion ()
  "Copy selected meeting slots as an email suggestion to clipboard.
Enter one or two slot indices as comma-separated values (e.g. \"1\" or \"1,3\")."
  (interactive)
  (if (= csf-last-slot-count 0)
      (message "⚠️  No free slots available to select from")
    (let* ((input (read-string
                   (format "One or two slot indices, comma-separated (0-%d), e.g. 1 or 1,3: "
                           (1- csf-last-slot-count))))
           (indices (mapcar #'string-to-number
                            (split-string input "," t "[ \t]+")))
           (meeting-duration (read-number
                              "Meeting duration in minutes (default 30): "
                              csf-default-meeting-duration)))
      (cond
       ((not (memq (length indices) '(1 2)))
        (message "❌ Please enter 1 or 2 indices (e.g. \"1\" or \"1,3\")"))
       ((and (= (length indices) 2) (= (nth 0 indices) (nth 1 indices)))
        (message "❌ The two indices must be different"))
       ((seq-some (lambda (idx) (or (< idx 0) (>= idx csf-last-slot-count))) indices)
        (message "❌ Indices must be between 0 and %d" (1- csf-last-slot-count)))
       (t
        (csf--format-and-copy-slots indices meeting-duration))))))

(defun csf--format-and-copy-slots (slot-indices duration)
  "Format selected slots into email suggestion and copy to clipboard.
SLOT-INDICES is a list of 1 or 2 slot indices. DURATION is the
requested meeting length in minutes."
  (let* ((script-path (csf--get-python-script-path))
         (python-cmd (or (executable-find "python3") (executable-find "python")))
         (config-file (csf--create-config-file))
         (org-files (nth 0 csf-last-search-params))
         (gcal-file (nth 1 csf-last-search-params))
         (args (append
                (list script-path
                      "--week-offset" (number-to-string csf-current-week-offset)
                      "--num-weeks" (number-to-string csf-num-weeks)
                      "--config-file" config-file
                      "--non-interactive"
                      "--format-slots")
                (mapcar #'number-to-string slot-indices)
                (list "--duration" (number-to-string duration)))))

    (when org-files
      (setq args (append args (cons "--org-files" org-files))))
    
    (when gcal-file
      (setq args (append args (list "--gcal-file" gcal-file))))
    
    (message "⏳ Generating email suggestion...")
    
    (let ((output (with-output-to-string
                    (with-current-buffer standard-output
                      (let ((exit-code (apply #'call-process python-cmd nil t nil args)))
                        (when (file-exists-p config-file)
                          (delete-file config-file))
                        
                        (if (/= exit-code 0)
                            (error "❌ Failed to generate email suggestion")))))))
      
      (condition-case err
          (let* ((json-data (json-parse-string output))
                 (success (gethash "success" json-data))
                 (suggestion (gethash "suggestion" json-data))
                 (slots-vec (gethash "slots" json-data)))

            (if (not success)
                (error "Failed to generate suggestion")
              (let* ((short-slot (and slots-vec
                                       (csf--first-slot-shorter-than slots-vec duration)))
                     (proceed (or (not short-slot)
                                  (y-or-n-p
                                   (format "Slot on %s at %s is only %d min, shorter than the requested %d min. Proceed with the shorter duration? "
                                           (gethash "date" short-slot)
                                           (gethash "time" short-slot)
                                           (gethash "duration_minutes" short-slot)
                                           duration)))))
                (if (not proceed)
                    (message "❌ Cancelled - adjust the requested duration or choose a different slot")
                  (kill-new suggestion)
                  (message "✅ Email suggestion copied to clipboard!")
                  (if (= (length slots-vec) 2)
                      (let ((slot1-info (aref slots-vec 0))
                            (slot2-info (aref slots-vec 1)))
                        (message "📧 Slots: %s at %s, %s at %s"
                                 (gethash "date" slot1-info)
                                 (gethash "time" slot1-info)
                                 (gethash "date" slot2-info)
                                 (gethash "time" slot2-info)))
                    (let ((slot1-info (aref slots-vec 0)))
                      (message "📧 Slot: %s at %s"
                               (gethash "date" slot1-info)
                               (gethash "time" slot1-info))))

                  ;; Ask if user wants to create org entries
                  (when (y-or-n-p "Create tentative meeting entries in org file? ")
                    (let ((person (read-string "Who is this meeting with? ")))
                      (when (and person (not (string-empty-p person)))
                        (csf--create-org-entries (append slots-vec nil) duration person))))))))
        (error (message "❌ Error parsing response: %s" err))))))

;;; Helper Functions

(defun csf--get-python-script-path ()
  "Get the path to the Python script."
  (or csf-python-script-path
      (let ((base-dir (cond
                       (load-file-name (file-name-directory load-file-name))
                       (buffer-file-name (file-name-directory buffer-file-name))
                       ((boundp 'user-emacs-directory) user-emacs-directory)
                       (t "~/"))))
        (expand-file-name "calendar_slot_finder.py" base-dir))))

(defun csf--get-org-agenda-files ()
  "Get list of org agenda files."
  (cond
   ((and (boundp 'org-agenda-files) 
         (org-agenda-files)
         (not (equal (org-agenda-files) '(nil))))
    (org-agenda-files))
   ((boundp 'org-directory)
    (let ((common-files (list (expand-file-name "agenda.org" org-directory)
                              (expand-file-name "schedule.org" org-directory))))
      (seq-filter #'file-exists-p common-files)))
   (t
    (let ((common-files (list (expand-file-name "~/org/agenda.org"))))
      (seq-filter #'file-exists-p common-files)))))

(defun csf--get-week-description (offset)
  "Get a human-readable description of the week offset."
  (cond
   ((= offset 0) "Current week")
   ((= offset 1) "Next week") 
   ((= offset -1) "Previous week")
   ((> offset 1) (format "%d weeks from now" offset))
   ((< offset -1) (format "%d weeks ago" (- offset)))
   (t (format "Week offset %d" offset))))

(defun csf--extract-done-keywords ()
  "Extract done state keywords from `org-todo-keywords'.
In org-todo-keywords, everything after the \"|\" is a done state."
  (let ((done-keywords '()))
    (when (boundp 'org-todo-keywords)
      (dolist (sequence org-todo-keywords)
        (when (listp sequence)
          (let ((after-pipe nil))
            (dolist (item (cdr sequence))  ; skip 'sequence symbol
              (cond
               ((string= item "|")
                (setq after-pipe t))
               (after-pipe
                ;; Extract keyword, stripping (x) or (x@/!) notation
                (let ((keyword (replace-regexp-in-string "(.*)" "" item)))
                  (push keyword done-keywords)))))))))
    (delete-dups done-keywords)))

(defun csf--prompt-slot-duration ()
  "Prompt the user to choose a slot duration: 30, 45, or a custom value."
  (let ((choice (completing-read "Slot duration? "
                                  '("30 minutes" "45 minutes" "Custom...")
                                  nil t)))
    (cond
     ((string= choice "30 minutes") 30)
     ((string= choice "45 minutes") 45)
     (t (read-number "Custom slot duration (minutes): " csf-slot-duration)))))

(defun csf--create-config-file ()
  "Create a temporary configuration file for the Python script."
  (let ((config-file (make-temp-file "csf-config" nil ".json"))
        (config-data (list)))

    (push (cons "working_hours" (vector (car csf-working-hours) (cadr csf-working-hours))) config-data)
    (push (cons "min_slot_duration" csf-min-slot-duration) config-data)
    (push (cons "slot_duration" (or csf-current-slot-duration csf-slot-duration)) config-data)
    (push (cons "meeting_cushion" csf-meeting-cushion) config-data)

    (when-let* ((excluded (csf--excluded-calendar-patterns)))
      (push (cons "excluded_calendars" (vconcat excluded)) config-data))
    (push (cons "include_plain_entries"
                (if csf-include-plain-org-entries t :json-false))
          config-data)

    ;; Add done keywords from org-todo-keywords
    (let ((done-keywords (csf--extract-done-keywords)))
      (when done-keywords
        (push (cons "done_keywords" (vconcat done-keywords)) config-data)))

    (when csf-weekly-restrictions
      (let ((restrictions-json '()))
        (dolist (restriction csf-weekly-restrictions)
          (let ((restriction-json (list)))
            (push (cons "days" (vconcat (plist-get restriction :days))) restriction-json)
            (push (cons "start-time" (plist-get restriction :start-time)) restriction-json)
            (push (cons "end-time" (plist-get restriction :end-time)) restriction-json)
            (push (cons "description" (plist-get restriction :description)) restriction-json)
            (push restriction-json restrictions-json)))
        (push (cons "restrictions" (vconcat (reverse restrictions-json))) config-data)))

    (with-temp-file config-file
      (insert (json-encode (reverse config-data))))

    config-file))

(defun csf--refresh-with-current-params ()
  "Refresh the results buffer with current parameters."
  (when csf-last-search-params
    (let ((org-files (nth 0 csf-last-search-params))
          (gcal-file (nth 1 csf-last-search-params))
          (slot-duration (nth 2 csf-last-search-params)))
      (when slot-duration
        (setq csf-current-slot-duration slot-duration))
      (csf--run-python-script csf-current-week-offset org-files gcal-file))))

(defun csf--run-python-script (week-offset org-files gcal-file)
  "Run the Python script with the given parameters."
  (let* ((script-path (csf--get-python-script-path))
         (python-cmd (or (executable-find "python3") (executable-find "python")))
         (config-file (csf--create-config-file))
         (args (list script-path
                     "--week-offset" (number-to-string week-offset)
                     "--num-weeks" (number-to-string csf-num-weeks)
                     "--config-file" config-file
                     "--non-interactive")))

    (setq csf-last-search-params (list org-files gcal-file csf-current-slot-duration))
    (setq csf-current-week-offset week-offset)
    
    (when org-files
      (setq args (append args (cons "--org-files" org-files))))
    
    (when gcal-file
      (setq args (append args (list "--gcal-file" gcal-file))))
    
    (message "🔍 Searching for free slots...")
    
    (let ((output-buffer (get-buffer-create "*Calendar Slot Finder*")))
      (switch-to-buffer output-buffer)
      
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize
                 (format "=== Calendar Slot Finder - %s%s ===\n"
                         (csf--get-week-description week-offset)
                         (if (> csf-num-weeks 1) (format " (%d weeks)" csf-num-weeks) ""))
                 'face 'bold))
        (insert (propertize
                 "📖 Navigation: n=next, p=prev, g=goto, w=set weeks, r=refresh, c=copy email+org, q=quit\n\n"
                 'face 'italic))
        
        (let ((exit-code (apply #'call-process python-cmd nil t nil args)))
          (when (file-exists-p config-file)
            (delete-file config-file))
          
          (if (= exit-code 0)
              (progn
                ;; Count the number of slots [0], [1], etc.
                (let ((slot-count 0))
                  (save-excursion
                    (goto-char (point-min))
                    (while (re-search-forward "^   \\[\\([0-9]+\\)\\]" nil t)
                      (setq slot-count (1+ slot-count))))
                  (setq csf-last-slot-count slot-count))
                
                (setq buffer-read-only nil)
                (csf-results-mode)
                (goto-char (point-min))
                (message "✅ Found available time slots! Press c to copy email + create org entries, use n/p to navigate weeks, q to quit."))
            (error "❌ Search failed. Check *Calendar Slot Finder* buffer for details")))))))

;;; Interactive Commands

;;;###autoload
(defun csf-find-slots (&optional week-offset slot-duration)
  "Find empty calendar slots for the specified week.
With prefix argument, prompt for which week to search.
Free gaps are split into SLOT-DURATION-minute blocks; when called
interactively, always prompts for the slot duration (30, 45, or a
custom value)."
  (interactive
   (list (if current-prefix-arg
             (let ((week-choice (completing-read "Which week? "
                                               '("Current week (0)" "Next week (1)" "Previous week (-1)" "Custom...")
                                               nil nil nil nil "Current week (0)")))
               (cond
                ((string-match "Current week" week-choice) 0)
                ((string-match "Next week" week-choice) 1)
                ((string-match "Previous week" week-choice) -1)
                ((string-match "Custom" week-choice)
                 (read-number "Week offset: " csf-default-week-offset))
                (t csf-default-week-offset)))
           csf-default-week-offset)
         (csf--prompt-slot-duration)))

  (setq csf-current-slot-duration (or slot-duration csf-slot-duration))

  (let ((script-path (csf--get-python-script-path)))
    (unless (file-exists-p script-path)
      (error "Python script not found at %s. Please set csf-python-script-path" script-path))

    (let ((org-files (csf--get-org-agenda-files))
          (gcal-file csf-gcal-data-file))

      (csf--run-python-script week-offset org-files gcal-file))))

;;;###autoload
(defun csf-find-slots-quick ()
  "Quickly find empty calendar slots for default week."
  (interactive)
  (csf-find-slots csf-default-week-offset))

;;;###autoload
(defun csf-find-slots-this-week ()
  "Find empty calendar slots for the current week."
  (interactive)
  (csf-find-slots 0))

;;;###autoload
(defun csf-find-slots-next-week ()
  "Find empty calendar slots for next week."
  (interactive)
  (csf-find-slots 1))

;;;###autoload
(defun csf-setup-restrictions ()
  "Set up weekly restrictions interactively."
  (interactive)
  (setq csf-weekly-restrictions nil)
  
  (while (y-or-n-p "Add a weekly restriction? ")
    (let* ((description (read-string "Description: "))
           (days-input (read-string "Days (weekdays/weekend or 0,1,2): "))
           (days (cond
                  ((string= days-input "weekdays") '(0 1 2 3 4))
                  ((string= days-input "weekend") '(5 6))
                  (t (mapcar #'string-to-number (split-string days-input "," t "\\s+")))))
           (start-time (read-string "Start time (HH:MM): "))
           (end-time (read-string "End time (HH:MM): ")))
      
      (when (and days start-time end-time)
        (push (list :days days
                    :start-time start-time
                    :end-time end-time
                    :description description)
              csf-weekly-restrictions)
        (message "Added restriction: %s" description))))
  
  (message "Restrictions configured. Run csf-find-slots to search for free slots."))

;;;###autoload
(defun csf-set-meeting-cushion (minutes)
  "Set the meeting cushion to MINUTES."
  (interactive (list (read-number "Meeting cushion in minutes: " csf-meeting-cushion)))
  (setq csf-meeting-cushion minutes)
  (message "Meeting cushion set to %d minutes" minutes))

;;; Integration

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x s") 'csf-find-slots))

(provide 'calendar-slot-finder)

;;; calendar-slot-finder.el ends here
