;; This Emacs minor mode is intended for use with nuweb.py[1].
;;
;; Copyright Simon Wright <simon@pushface.org>
;;
;; This package is free software, just as GNU Emacs; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by ;the Free Software Foundation;
;; either version 3, or (at your option) any later version.  GNU Emacs
;; is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;; It's based on polymode, a framework for multiple major modes.
;;
;; Unlike noweb, nuweb's scraps don't have any associated language; it
;; might be possible to work out the language of a scrap by looking
;; for an output scrap that uses this one, but this seems like a
;; tedious (if not hard) problem, so I've gone with fundamental mode.
;;
;; This code relies on polymode features that aren't in the version
;; currently at melpa, so you need to download polymode's git repo[2]
;; and include it on your load-path, e.g.
;;   (add-to-list 'load-path (expand-file-name "~/poly/polymode"))
;;
;; Because special input characters are buffer-wide, I've suppressed
;; electric-indent-mode locally (LaTeX indentation isn't appropriate
;; for most code).

;; [1] nuweb.py: https://github.com/simonjwright/nuweb.py
;; [2] polymode: https://github.com/polymode/polymode

(require 'polymode)

(add-hook 'poly-nuweb-mode-hook (lambda () (setq electric-indent-inhibit t)))

(define-hostmode poly-nuweb-hostmode
  :mode 'latex-mode)

(define-innermode poly-nuweb-fundamental-innermode
  :mode 'fundamental-mode
  :head-matcher "^@[dDoO]"
  :tail-matcher "@}$"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-nuweb-mode
  :hostmode 'pm-host/latex
  :innermodes '(poly-nuweb-fundamental-innermode))

(add-to-list 'auto-mode-alist '("\\.w$" . poly-nuweb-mode))

(provide 'poly-nuweb)
