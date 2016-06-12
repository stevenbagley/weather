;; A simple interface to the Weather Underground API. Influenced by
;; forecast.el and sunshine.el. May 2016, Steve Bagley.

;; M-x weather to see a textual weather report in the *weather*
;; buffer. Type g to refresh, G to force refresh, b to bury, q to
;; quit. Give an argument to M-x weather to see brief summary in
;; minibuffer.

(require 'url)
(require 'json)

(defvar weather-api-url "http://api.wunderground.com/api")
;; you need to set this with your own key:
(defvar weather-underground-key nil)
(defvar weather-data)                   ;stores retrieved weather data
(defvar weather-data-last-update-time nil)     ;time of last update
(defvar weather-data-cache-lifetime (* 30 60)) ;time in seconds before requesting from server

(defface weather-title-face
  '((t (:foreground "gray90" :background "cyan4" :height 2.0 :slant italic :box (:line-width 2 :style released-button))))
  "Face for title in weather buffer.")

(defface weather-heading-face
  '((t (:foreground "#646464" :background "DarkSlateGray2" :height 1.3 :slant italic :box (:line-width 2 :style released-button))))
  "Face for headings in weather buffer.")

;; issues request, puts results in buffer in JSON format
(defun weather-get-data ()
  (unless weather-underground-key
    (error "You need to set weather-underground-key to your WU key (see https://www.wunderground.com/weather/api/)"))
  (let ((request (format "%s/%s/conditions/forecast/q/%f,%f.json"
                         weather-api-url weather-underground-key
                         calendar-latitude calendar-longitude)))
    (url-retrieve-synchronously request)))

;; set global with all the weather data, as nested alists.
(defun weather-update-data ()
  (setq weather-data (with-current-buffer (weather-get-data)
                       (goto-char (point-min))
                       ;; move past header to beginning of JSON object
                       (search-forward "{")
                       (forward-char -1)
                       (let ((json-array-type 'list)
                             (json-key-type 'symbol))
                         (json-read-from-string (buffer-substring (point) (point-max))))))
  (setq weather-data-last-update-time (current-time))
  (message "Weather data updated."))

;; get weather data if no cache or cache has expired
(defun weather-update ()
  (when (or (null weather-data-last-update-time)
            (time-less-p (time-add weather-data-last-update-time
                                   (seconds-to-time weather-data-cache-lifetime))
                         (current-time)))
    (weather-update-data)))

(defun assoc-get (symbol alist)
  (let ((val (assoc symbol alist)))
    (or (cdr val)
        (format "%s (missing)" (symbol-name symbol)))))

(defun format-brief-weather-summary (weather-data)
  (let ((summary (assoc-get 'current_observation weather-data)))
    ;; double the percent sign for message call later.
    (format "%s, Temp %dF, Humidity %s%%, Wind %s (retrieved %s)"
            (assoc-get 'weather summary)
            (assoc-get 'temp_f summary)
            (assoc-get 'relative_humidity summary)
            (assoc-get 'wind_string summary)
            (format-time-string "%k:%M:%S" (seconds-to-time (string-to-int (assoc-get 'local_epoch summary)))))))

(defun format-summary (weather-data)
  (let* ((summary (assoc-get 'current_observation weather-data))
         (display-location (assoc-get 'display_location summary))
         (city (assoc-get 'city display-location)))
    (concat (propertize (concat " "city " Weather ") 'font-lock-face 'weather-title-face)
            "\n\n"
            (propertize (concat "Current conditions (retrieved "
                                (format-time-string "%A %k:%M:%S" (seconds-to-time (string-to-int (assoc-get 'local_epoch summary))))
                                ")") 'font-lock-face 'weather-heading-face)
            (format "\n%s\nTemp %dF\nHumidity %s\nPrecipitation rate %s in/hr\nWind %s\n"
                    (assoc-get 'weather summary)
                    (assoc-get 'temp_f summary)
                    (assoc-get 'relative_humidity summary)
                    (assoc-get 'precip_1hr_in summary)
                    (assoc-get 'wind_string summary)))))

(defun format-forecast (weather-data)
  (let ((forecast (assoc-get 'forecastday (assoc-get 'txt_forecast (assoc-get 'forecast weather-data)))))
    (apply #'concat
           (loop for entry in forecast
                 collect (concat "\n"
                                 (propertize (assoc-get 'title entry) 'font-lock-face 'weather-heading-face)
                                 "\n"
                                 (assoc-get 'fcttext entry)
                                 "\n")))))

(defvar weather-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map "g" 'weather-redisplay-buffer)
    (define-key map "G" 'weather-force-redisplay-buffer)
    (define-key map "b" 'weather-bury)
    (define-key map "q" 'weather-quit)
    map))

;; this is built on org-mode to get the heading formatting for free.
(define-derived-mode weather-mode org-mode "Weather"
  "A major mode for displaying the weather.")

(defun weather-redisplay-buffer ()
  (interactive)
  (weather-update)
  (let* ((buf (get-buffer-create "*weather*"))
         (new (not (eq buf (current-buffer)))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (weather-mode)
      (erase-buffer)
      (insert (format-summary weather-data))
      (insert (format-forecast weather-data))
      (make-variable-buffer-local 'cursor-type)
      (setq cursor-type nil)            ;hide the cursor
      (setq buffer-read-only t)
      (when new
        ;; if we are already in the weather buffer, then we don't need
        ;; to switch to other window.
        (switch-to-buffer-other-window (current-buffer))))))

(defun weather-force-redisplay-buffer ()
  (interactive)
  (setq weather-data-last-update-time nil)
  (weather-redisplay-buffer))

;; quit, just bury the buffer
(defun weather-bury ()
  (interactive)
  (quit-window nil (selected-window)))

;; quit, killing window
(defun weather-quit ()
  (interactive)
  (quit-window t (selected-window)))

;; show summary in minibuffer
(defun weather-brief-summary ()
  (interactive)
  (weather-update)
  (message (format-brief-weather-summary weather-data)))

;; top level function
(defun weather (&optional arg)
  (interactive "P")
  (if arg
      (weather-brief-summary)
    (weather-redisplay-buffer)))

(provide 'weather)
