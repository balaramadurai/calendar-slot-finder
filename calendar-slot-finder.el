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

(defcustom csf-meeting-cushion 15
  "Buffer time in minutes before and after meetings."
  :type 'integer
  :group 'calendar-slot-finder)

;;; Variables

(defvar csf-weekly-restrictions nil
  "List of weekly restrictions.")

(defvar csf-current-week-offset 0
  "Current week offset being displayed.")

(defvar csf-last-search-params nil
  "Parameters from the last search for navigation.")

;;; Keymap and Mode

(defvar csf-results-mode-map (make-sparse-keymap)
  "Keymap for Calendar Slot Finder results buffer.")

(define-key csf-results-mode-map "q" 'quit-window)
(define-key csf-results-mode-map "n" 'csf-next-week)
(define-key csf-results-mode-map "p" 'csf-previous-week)
(define-key csf-results-mode-map "r" 'csf-refresh-current-week)
(define-key csf-results-mode-map "g" 'csf-goto-week)
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
    (setq csf-current-week-offset (1- csf-current-week-offset))
    (csf--refresh-with-current-params)))

(defun csf-refresh-current-week ()
  "Refresh the current week's calendar slots."
  (interactive)
  (when csf-last-search-params
    (csf--refresh-with-current-params)))

(defun csf-goto-week (offset)
  "Go to a specific week offset."
  (interactive "nWeek offset (0=current, 1=next, -1=previous): ")
  (when csf-last-search-params
    (setq csf-current-week-offset offset)
    (csf--refresh-with-current-params)))

(defun csf-show-help ()
  "Show help for Calendar Slot Finder."
  (interactive)
  (message "Calendar Slot Finder: q=quit, n=next week, p=previous week, r=refresh, g=goto week"))

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

(defun csf--create-config-file ()
  "Create a temporary configuration file for the Python script."
  (let ((config-file (make-temp-file "csf-config" nil ".json"))
        (config-data (list)))
    
    (push (cons "working_hours" (vector (car csf-working-hours) (cadr csf-working-hours))) config-data)
    (push (cons "min_slot_duration" csf-min-slot-duration) config-data)
    (push (cons "meeting_cushion" csf-meeting-cushion) config-data)
    
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
          (gcal-file (nth 1 csf-last-search-params)))
      (csf--run-python-script csf-current-week-offset org-files gcal-file))))

(defun csf--run-python-script (week-offset org-files gcal-file)
  "Run the Python script with the given parameters."
  (let* ((script-path (csf--get-python-script-path))
         (python-cmd (or (executable-find "python3") (executable-find "python")))
         (config-file (csf--create-config-file))
         (args (list script-path
                     "--week-offset" (number-to-string week-offset)
                     "--config-file" config-file
                     "--non-interactive")))
    
    (setq csf-last-search-params (list org-files gcal-file))
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
                 (format "=== Calendar Slot Finder - %s ===\n" 
                         (csf--get-week-description week-offset))
                 'face 'bold))
        (insert (propertize 
                 "📖 Navigation: n=next week, p=previous week, g=goto week, r=refresh, q=quit\n\n"
                 'face 'italic))
        
        (let ((exit-code (apply #'call-process python-cmd nil t nil args)))
          (when (file-exists-p config-file)
            (delete-file config-file))
          
          (if (= exit-code 0)
              (progn
                (setq buffer-read-only nil)
                (csf-results-mode)
                (goto-char (point-min))
                (message "✅ Found available time slots! Use n/p to navigate weeks, q to quit."))
            (error "❌ Search failed. Check *Calendar Slot Finder* buffer for details")))))))

;;; Interactive Commands

;;;###autoload
(defun csf-find-slots (&optional week-offset)
  "Find empty calendar slots for the specified week.
With prefix argument, prompt for which week to search."
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
           csf-default-week-offset)))
  
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
