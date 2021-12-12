(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(bidi-paragraph-direction 'left-to-right)
 '(blink-cursor-mode nil)
 '(browse-url-browser-function 'browse-url-firefox)
 '(elfeed-search-title-max-width 100)
 '(elfeed-search-title-min-width 100)
 '(newsticker-url-list '(("Kontanta-42" "www.konstanta.lt/rss" nil nil nil)))
 '(org-archive-location "~/org/archive.org::")
 '(w3m-search-default-engine "duckduckgo"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; desktop save session ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(desktop-save-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-drill settings                                                 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-drill-save-buffers-after-drill-sessions-p nil)

;;  To enable random "noise" for item intervals in org-drill
(setq org-drill-add-random-noise-to-intervals-p t)



(global-set-key (kbd "C-x g") 'magit-status)

(setq org-todo-keywords
     '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))

(display-time-mode 1)

(toggle-scroll-bar -1)

(tool-bar-mode -1)

;; (setq org-clock-persist 'history)
;; (eval-after-load "org"
;;  '(progn 
;;   (org-clock-persistence-insinuate)))

;; lietuviškos kabutės
(add-hook 'org-mode-hook 'electric-quote-mode)
(setq electric-quote-replace-double t 
      electric-quote-chars '(8216 8217 8222 8220))

;; data is stored in ~/.elfeed
(setq elfeed-feeds
      '(
        ;; openhardware movement
        ("https://linuxsmartphones.com/feed/" linux smartphones)
	("https://tuxphones.com/rss/" linux smartphones)

        ;; science
        ("http://www.konstanta.lt/feed/atom" astrofizika)

	;; politics
	("https://www.svoboda.org/api/z_jqpperyop_" svoboda)
	("http://feeds2.feedburner.com/racas" racas)
	("http://racas.lt/feed/rss/" racas)
	
	;; security
	("https://www.schneier.com/feed/atom/" security)
	("https://krebsonsecurity.com/feed/" security)

	;; psichologija
	("https://www.advancedassessments.co.uk/Blog/files/feed.xml" psichologija teisė)
	("https://www.criminalthinking.net/feed/" psichologija teisė)
	("https://www.bulletpsych.com/blog-feed.xml" psichologija psichiatrija)
	("http://popsych.org/feed/" psichologija)
	("https://danariely.com/feed/" psichologija)
	("http://humanfactorsblog.org/feed/" psichologija)
	("https://mindhacks.com/feed/" psichologija)

	;; IT
	("https://sachachua.com/blog/feed/" emacs)
	("https://functor.tokyo/blog/feed/" nixos)
	("https://karl-voit.at/feeds/lazyblorg-all.atom_1.0.links-and-content.xml" emacs)
	
))

;; (setq-default elfeed-search-filter "@2-days-ago +unread")
(setq-default elfeed-search-title-max-width 100)
(setq-default elfeed-search-title-min-width 100)


;; emms controls keybindings
(global-set-key (kbd "C-c e <up>") 'emms-start)
(global-set-key (kbd "C-c e <down>") 'emms-stop)
(global-set-key (kbd "C-c e <left>") 'emms-previous)
(global-set-key (kbd "C-c e <right>") 'emms-next)
(global-set-key (kbd "<XF86AudioNext>") 'emms-seek-forward)
(global-set-key (kbd "<XF86AudioPrev>") 'emms-seek-backward)
(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause) 

;; emacs window to always show your system-name and the full path of the buffer you're currently editing

(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
            '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; copy command automatically select the target dir in a split pane
(setq dired-dwim-target t)
