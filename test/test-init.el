
;; totally manually install
;; (add-to-list 'load-path "~/org/org-mode/testing/")
;; (add-to-list 'load-path (file-name-concat user-emacs-directory "elpa" "maplev-3.2.0"))

;; Install using use-package from git-hub
(use-package ob-maple
  :vc (:url "https://github.com/BoJakobsen/ob-maple" :branch "my-fix")
  :ensure t
  :after org
  :config
  (add-to-list 'org-babel-load-languages '(maple . t))
  ;(setq org-babel-maple-command
  ;      "/opt/maple2026/bin/maple -q -c 'interface(prettyprint=0)'")
  ;; Some keys which might be usefull
  ;;(keymap-set org-babel-map "S" #'org-babel-maple-kill-session)
  ;;(keymap-set org-babel-map "K" #'org-babel-maple-kill-all-sessions)
  )

;; Update vc installed
;;M-x package-vc-upgrade


;; Install from local checkout (sym-link) useful for development
;; M-x package-vc-install-from-checkout
;; M-x package-vc-rebuild
