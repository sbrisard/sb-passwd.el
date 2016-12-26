;;; sb-passwd.el --- blabla

;;; Commentary:
;;

;;; Code:

(random t)

(defgroup sb-passwd nil "Password management.")

(defcustom sb-passwd-symbols "!#$%&‘()*+,-./:;<=>?@[\]^{}~"
  "List of symbols that are allowed in passwords.

By default, some symbols are prohibited. For example, the
vertical bar | interferes with Org-mode table delimiters. Also,
the double quote causes `org-table-read' to raise the following
error \"org-babel-read: End of file during parsing\"."
  :type 'string :group 'sb-passwd :tag "Symbols")

(defun sb-passwd-create-password(n &optional use-digits use-letters use-symbols)
  "Return a new password.

N is the total length of the password (number of signs). When
USE-DIGITS is true, the returned password may include digits
[0-9]. When USE-LETTERS is true, the returned password may
include letters [a-zA-Z]. Finally, when USE-SYMBOLS is true, the
returned password is allowed to include the additional symbols
defined by `sb-passwd-symbols'.

When called interactively, N can be passed as a prefix argument."
  (interactive (list
                ;; From the source of goto-line
                ;; (M-x find-function goto-line RET).
                (if (and current-prefix-arg
                         (not (consp current-prefix-arg)))
                    (prefix-numeric-value current-prefix-arg)
                  (read-number "Length of password:" 10))
                (y-or-n-p "Use digits? ")
                (y-or-n-p "Use letters? ")
                (y-or-n-p "Use symbols? ")))
  (let ((signs)
        (password))
    (progn
      (when use-digits (setq signs "0123456789"))
      (when use-letters (setq signs (concat signs
                                            "abcdefghijklmnopqrstuvwxyz"
                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
      (when use-symbols (setq signs (concat signs sb-passwd-symbols)))
      (unless signs (setq signs "0123456789"))
      (dotimes (i n (concat password))
        (setq password (cons (elt signs (random (length signs))) password))))))

(defun sb-passwd-insert-new-password ()
  "Insert new password at point.

Calls `sb-passwd-create-password' interactively."
  (interactive)
  (insert (call-interactively 'sb-passwd-create-password)))

(provide 'sb-passwd)

;;; sb-passwd.el ends here



;; (setq passwords '(("site1" "passwd1") ("site2" "passwd2")))
;; (assoc "site1" passwords)
;; (completing-read "site?" '("x" "y" "z"))
;; (map 'list 'car passwords)
