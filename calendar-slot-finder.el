(defcustom csf-min-slot-duration 60
  "Minimum duration for a free slot in minutes."
  :type 'integer
  :group 'calendar-slot-finder);;; calendar-slot-finder.el --- Find empty time slots in your calendar -*- lexical-binding: t; -*-

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

(defgroup calendar-slot-finder nil
  "Find empty calendar slots."
  :group 'org
  :prefix "csf-")

(defcustom csf-python-script-path nil
  "Path to the Python calendar slot finder script.
If nil, assumes the script is in the same directory as this file."
  :type '(choice (const nil) file)
  :group 'calendar-slot-finder)

(defcustom csf-default-week-offset 1
  "Default week offset (1 = next week, 0 = current week)."
  :type 'integer
  :group 'calendar-slot-finder)

(defcustom csf-gcal-data-file nil
  "Path to JSON file containing Google Calendar data from org-gcal.
If nil, will try to extract from org-gcal cache."
  :type '(choice (const nil) file)
  :group 'calendar-slot-finder)

(defcustom csf-working-hours '("09:00" "17:00")
  "Default working hours as list of (start-time end-time) in HH:MM format."
  :type '(list string string)
  :group 'calendar-slot-finder)

(defcustom csf-meeting-cushion 15
  "Buffer time in minutes to add before and after existing meetings.
This ensures you have transition time between appointments."
  :type 'integer
  :group 'calendar-slot-finder)

(defvar csf-weekly-restrictions nil
  "List of weekly restrictions.
Each restriction is a plist with keys:
  :days - list of weekday numbers (0=Monday, 6=Sunday)
  :start-time - start time as HH:MM string
  :end-time - end time as HH:MM string
  :description - human readable description")

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
   ;; Use org-agenda-files if available and not empty
   ((and (boundp 'org-agenda-files) 
         (org-agenda-files)
         (not (equal (org-agenda-files) '(nil))))
    (org-agenda-files))
   ;; Fall back to common org files
   ((boundp 'org-directory)
    (let ((common-files (list (expand-file-name "agenda.org" org-directory)
                              (expand-file-name "schedule.org" org-directory)
                              (expand-file-name "tasks.org" org-directory))))
      (seq-filter #'file-exists-p common-files)))
   ;; Last resort - try home directory
   (t
    (let ((common-files (list (expand-file-name "~/org/agenda.org")
                              (expand-file-name "~/org/schedule.org") 
                              (expand-file-name "~/org/tasks.org"))))
      (seq-filter #'file-exists-p common-files)))))

(defun csf--export-gcal-data ()
  "Export org-gcal data to JSON format for the Python script.
Returns path to temporary JSON file or nil if no data available."
  (when (and (featurep 'org-gcal)
             (boundp 'org-gcal-fetch-file-alist)
             org-gcal-fetch-file-alist)
    (let ((gcal-entries '())
          (temp-file (make-temp-file "org-gcal-data" nil ".json")))
      
      ;; Extract entries from org-gcal files
      (dolist (calendar-file org-gcal-fetch-file-alist)
        (let ((file-path (cdr calendar-file)))
          (when (and file-path (file-exists-p file-path))
            (with-temp-buffer
              (insert-file-contents file-path)
              (goto-char (point-min))
              ;; Look for entries with timestamps and gcal IDs
              (while (re-search-forward "^\\*\\+.*" nil t)
                (let* ((headline (match-string 0))
                       (entry-start (point-at-bol))
                       (entry-end (save-excursion
                                   (outline-next-heading)
                                   (point)))
                       (entry-content (buffer-substring entry-start entry-end)))
                  
                  ;; Look for timestamps and gcal properties
                  (when (string-match "<[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}[^>]*>" entry-content)
                    (let ((timestamp (match-string 0 entry-content))
                          (title (replace-regexp-in-string "^\\*+" "" headline)))
                      (push `((summary . ,title)
                              (timestamp . ,timestamp)
                              (source . "org-gcal"))
                            gcal-entries)))))))))
      
      ;; Write to JSON file if we have entries
      (if gcal-entries
          (progn
            (with-temp-file temp-file
              (insert (json-encode gcal-entries)))
            temp-file)
        ;; Clean up temp file if no entries
        (delete-file temp-file)
        nil))))

(defun csf--parse-weekday-input (input)
  "Parse weekday input string into list of numbers.
INPUT can be like 'mon,tue,wed' or '0,1,2' or 'weekdays' or 'weekend'."
  (cond
   ((string= input "weekdays") '(0 1 2 3 4))
   ((string= input "weekend") '(5 6))
   ((string-match-p "^[0-6,\\s]*$" input)
    ;; Numbers separated by commas
    (mapcar #'string-to-number (split-string input "," t "\\s+")))
   (t
    ;; Day names
    (let ((day-map '(("mon" . 0) ("tue" . 1) ("wed" . 2) ("thu" . 3)
                     ("fri" . 4) ("sat" . 5) ("sun" . 6)
                     ("monday" . 0) ("tuesday" . 1) ("wednesday" . 2)
                     ("thursday" . 3) ("friday" . 4) ("saturday" . 5)
                     ("sunday" . 6)))
          (days '()))
      (dolist (day-name (split-string input "," t "\\s+"))
        (let ((day-num (cdr (assoc (downcase day-name) day-map))))
          (when day-num
            (push day-num days))))
      (reverse days)))))

(defun csf--read-time (prompt default)
  "Read a time string with PROMPT, using DEFAULT if empty."
  (let ((input (read-string (format "%s (HH:MM, default %s): " prompt default))))
    (if (string-empty-p (string-trim input))
        default
      input)))

(defun csf--setup-restrictions ()
  "Interactively set up weekly restrictions."
  (setq csf-weekly-restrictions nil)
  
  (while (y-or-n-p "Add a weekly restriction (e.g., meeting hours)? ")
    (let* ((description (read-string "Description (e.g., 'Team meeting'): "))
           (days-input (read-string "Days (weekdays/weekend or mon,tue,wed or 0,1,2): "))
           (days (csf--parse-weekday-input days-input))
           (start-time (csf--read-time "Start time" "14:30"))
           (end-time (csf--read-time "End time" "17:30")))
      
      (when (and days start-time end-time)
        (push (list :days days
                    :start-time start-time
                    :end-time end-time
                    :description description)
              csf-weekly-restrictions)
        (message "Added restriction: %s" description)))))

(defun csf--create-config-file ()
  "Create a temporary configuration file for the Python script."
  (let ((config-file (make-temp-file "csf-config" nil ".json"))
        (config-data (list)))
    
    ;; Add working hours
    (push (cons "working_hours" (vector (car csf-working-hours) (cadr csf-working-hours))) config-data)
    
    ;; Add minimum slot duration
    (push (cons "min_slot_duration" csf-min-slot-duration) config-data)
    
    ;; Add meeting cushion
    (push (cons "meeting_cushion" csf-meeting-cushion) config-data)
    
    ;; Add restrictions
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
    
    ;; Write config file
    (with-temp-file config-file
      (insert (json-encode (reverse config-data))))
    
    ;; Debug: show config file content
    (message "Created config file with restrictions: %s" 
             (if csf-weekly-restrictions
                 (mapconcat (lambda (r) (plist-get r :description)) csf-weekly-restrictions ", ")
               "none"))
    
    config-file))

(defun csf--run-python-script (week-offset org-files gcal-file interactive-p)
  "Run the Python script with the given parameters."
  (let* ((script-path (csf--get-python-script-path))
         (python-cmd (or (executable-find "python3") (executable-find "python")))
         (config-file (csf--create-config-file))
         (args (list script-path
                     "--week-offset" (number-to-string week-offset)
                     "--config-file" config-file)))
    
    ;; Add org files
    (when org-files
      (setq args (append args (cons "--org-files" org-files))))
    
    ;; Add gcal file
    (when gcal-file
      (setq args (append args (list "--gcal-file" gcal-file))))
    
    ;; Add interactive flag - use config file instead of non-interactive
    (unless interactive-p
      (setq args (append args (list "--non-interactive"))))
    
    ;; Debug: show what we're running
    (message "Running: %s %s" python-cmd (string-join args " "))
    (message "Config file: %s" config-file)
    
    ;; Run the script
    (let ((output-buffer (get-buffer-create "*Calendar Slot Finder*")))
      (with-current-buffer output-buffer
        (erase-buffer)
        (let ((exit-code (apply #'call-process python-cmd nil t nil args)))
          ;; Clean up config file
          (when (file-exists-p config-file)
            (delete-file config-file))
          
          (if (= exit-code 0)
              (progn
                (goto-char (point-min))
                (display-buffer output-buffer)
                (message "Calendar slot search completed. See results in *Calendar Slot Finder* buffer."))
            (error "Python script failed with exit code %d. See *Calendar Slot Finder* buffer for details" exit-code)))))))

;;;###autoload
(defun csf-set-meeting-cushion (minutes)
  "Set the meeting cushion to MINUTES.
This adds buffer time before and after each meeting to account for
transition time, preparation, or overruns."
  (interactive
   (list (read-number "Meeting cushion in minutes: " csf-meeting-cushion)))
  (setq csf-meeting-cushion minutes)
  (message "Meeting cushion set to %d minutes" minutes))

;;;###autoload
(defun csf-find-slots (&optional week-offset interactive-setup)
  "Find empty calendar slots for the specified week.

WEEK-OFFSET specifies which week to search (default: next week).
If INTERACTIVE-SETUP is non-nil, run interactive configuration first.

The function integrates with org-agenda-files and org-gcal entries
to find free time slots based on your restrictions."
  (interactive
   (list (if current-prefix-arg
             (read-number "Week offset (0=current, 1=next): " csf-default-week-offset)
           csf-default-week-offset)
         (y-or-n-p "Run interactive setup? ")))
  
  (let ((script-path (csf--get-python-script-path)))
    (unless (file-exists-p script-path)
      (error "Python script not found at %s. Please set csf-python-script-path" script-path))
    
    ;; Interactive setup if requested
    (when interactive-setup
      (csf--setup-restrictions))
    
    ;; Get org agenda files
    (let ((org-files (csf--get-org-agenda-files))
          (gcal-file (or csf-gcal-data-file (csf--export-gcal-data))))
      
      (unless org-files
        (message "Warning: No org agenda files found. Please check your org-agenda-files configuration."))
      
      (message "Searching for calendar slots...")
      (when org-files
        (message "Org files: %s" (string-join org-files ", ")))
      (when gcal-file
        (message "Using gcal data from: %s" gcal-file))
      
      ;; Run the Python script
      (csf--run-python-script (or week-offset csf-default-week-offset)
                              org-files
                              gcal-file
                              interactive-setup))))

;;;###autoload
(defun csf-find-slots-quick ()
  "Quickly find empty calendar slots for next week using current settings."
  (interactive)
  (csf-find-slots csf-default-week-offset nil))

;;;###autoload
(defun csf-setup-restrictions ()
  "Set up weekly restrictions interactively."
  (interactive)
  (csf--setup-restrictions)
  (message "Restrictions configured. Run csf-find-slots to search for free slots."))

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

;; Add to org-mode menu if available
(when (featurep 'org)
  (define-key org-mode-map (kbd "C-c C-x s") 'csf-find-slots)
  
  ;; Add to org agenda dispatcher if available
  (when (boundp 'org-agenda-custom-commands)
    (add-to-list 'org-agenda-custom-commands
                 '("s" "Find empty slots" csf-find-slots))))

(provide 'calendar-slot-finder)

;;; calendar-slot-finder.el ends here
