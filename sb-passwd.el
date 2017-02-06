;;; sb-passwd.el --- blabla

;;; Commentary:
;;

;;; Code:

(random t)

(defvar sb-passwd-passwords ())

(defgroup sb-passwd nil "Password management.")

(defcustom sb-passwd-symbols "!#$%&‘()*+,-./:;<=>?@[\]^{}~"
  "List of symbols that are allowed in passwords.

By default, some symbols are prohibited.  For example, the vertical
bar | interferes with Org-mode table delimiters.  Also, the double
quote causes `org-table-read' to raise the following error
\"org-babel-read: End of file during parsing\"."
  :type 'string :group 'sb-passwd :tag "Symbols")

(defcustom sb-passwd-org-file-name ""
  "Name of the Org file that contains the password table."
  :type 'string :group 'sb-passwd :tag "File name")

(defcustom sb-passwd-org-table-name "passwords"
  "Name of the Org table that holds the passwords.

The table name is specified by a #+NAME directive."
  :type 'string :group 'sb-passwd :tag "Table name")

(defcustom sb-passwd-org-table-key-index 0
  "Index (0-based) of the column that holds the key.

This is used when loading passwords from e.g. an Org table."
  :type 'integer :group 'sb-passwd :tag "Key index")

(defcustom sb-passwd-org-table-login-index 1
  "Index (0-based) of the column that holds the login.

This is used when loading passwords from e.g. an Org table."
  :type 'integer :group 'sb-passwd :tag "Login index")

(defcustom sb-passwd-org-table-password-index 2
  "Index (0-based) of the column that holds the password.

This is used when loading passwords from e.g. an Org table."
  :type 'integer :group 'sb-passwd :tag "Password index")

(defun sb-passwd-create-password(n &optional use-digits use-letters use-symbols)
  "Return a new password.

N is the total length of the password (number of signs).

When USE-DIGITS is true, the returned password may include digits [0-9].

When USE-LETTERS is true, the returned password may include letters
[a-zA-Z].

When USE-SYMBOLS is true, the returned password is allowed to include
the additional symbols defined by `sb-passwd-symbols'.

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
        (password (make-string n ?x)))
    (progn
      (when use-digits (setq signs "0123456789"))
      (when use-letters (setq signs (concat signs
                                            "abcdefghijklmnopqrstuvwxyz"
                                            "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
      (when use-symbols (setq signs (concat signs sb-passwd-symbols)))
      (unless signs (setq signs "0123456789"))
      (dotimes (i n password) (aset password i (aref signs (random (length signs))))))))

(defun sb-passwd-insert-new-password ()
  "Insert new password at point.

Calls `sb-passwd-create-password' interactively."
  (interactive)
  (insert (call-interactively 'sb-passwd-create-password)))

(defun sb-passwd-new-password-to-kill-ring ()
  "Create a new password and make it the latest kill in the kill ring.

Calls `sb-passwd-create-password' interactively."
  (interactive)
  (kill-new (call-interactively 'sb-passwd-create-password)))

(defun sb-passwd-append (key login password)
  "Append new password to the global list of passwords.

This function adds a new entry associated to KEY in
`sb-passwd-passwords'.  LOGIN and PASSWORD are the login and password
associated with KEY.

If KEY is a valid Org-mode link, the actual key associated with the
specified password is the description of the link.

If a mapping for KEY already exists, a warning is issued and the
function returns nil.  Otherwise, the value of `sb-passwd-passwords' is
returned."
  (let* ((key-and-link (sb-passwd--parse-org-link key))
         (link (cdr key-and-link)))
    (setq key (car key-and-link))
    (if (assoc-string key sb-passwd-passwords)
        (progn (display-warning 'sb-passwd
                                (format-message "Key already exists: %s" key))
               nil)
      (setq sb-passwd-passwords
            (cons (append (list key :login login :password password)
                          (if link (list :link link) nil))
                  sb-passwd-passwords)))))

(defun sb-passwd-get (key what)
  "Return the value associated with the password KEY.

WHAT specifies the information that must be returned.  It must be one of
:login, :password or :link.

The function returns nil if KEY is not present, or WHAT is not specified
for KEY."
  (plist-get (cdr (assoc-string key sb-passwd-passwords)) what))

(defun sb-passwd--parse-org-link(link)
  "Parse an org-link and returns a (description . link) cons cell.

If LINK is not an org hyperlink, then it is returned as a one-element
list."
  (if (string-match "\\[\\[\\(.*\\)\\]\\[\\(.*\\)\\]\\]" link)
      (cons (match-string 2 link) (match-string 1 link))
    (cons link ())))

(defun sb-passwd--org-babel-ref-resolve (ref)
  "Resolve the reference REF and return its value.

Same as `org-babel-ref-resolve', but for numbers that are not parsed.

The Org-Mode function `org-babel-ref-resolve' uses
`org-babel--string-to-number' to convert number-like table cells to
numbers.  This can lead long, digits only passwords, to be parsed as
floats.

To avoid this issue, the present function locally binds
`org-babel--string-to-number' to `identity' before calling
`org-babel-ref-resolve'."
  (cl-letf (((symbol-function 'org-babel--string-to-number) 'identity))
    (org-babel-ref-resolve ref)))

(defun sb-passwd-load-table-from-file (&optional filename tablename)
  "Return the Org table from file FILENAME, named TABLENAME.

Table should be named by a #+NAME directive.

Uses `sb-passwd--org-babel-ref-resolve' internally.

The default values of FILENAME and TABLENAME are
`sb-passwd-org-file-name' and `sb-passwd-org-table-name', respectively."
  (let ((buffer)
        (kill-buffer-on-return)
        (table))
    (unless filename (setq filename sb-passwd-org-file-name))
    (unless tablename (setq tablename sb-passwd-org-table-name))
    (unless (setq buffer (find-buffer-visiting filename))
      (setq buffer (find-file filename))
      (setq kill-buffer-on-return t))
    (with-current-buffer buffer
      (setq table (sb-passwd--org-babel-ref-resolve tablename)))
    (when kill-buffer-on-return (kill-buffer buffer))
    table))

(defun sb-passwd-append-from-table (table &optional key-index login-index password-index)
  "Populate `sb-passwd-passwords' with the table TABLE.

The function returns the updated value of `sb-passwd-passwords'.

TABLE should be a list (rows) of lists (columns).  Each row of the table
corresponds to a new password.  The optional arguments KEY-INDEX,
LOGIN-INDEX and PASSWORD-INDEX specify the 0-based column indices of the
key, login and password, respectively.

Default values for these optional arguments are defined by

  - `sb-passwd-org-table-key-index',
  - `sb-passwd-org-table-login-index',
  - `sb-passwd-org-table-password-index',

respectively.  In order to avoid issues with number-like passwords,
`sb-passwd--org-babel-ref-resolve' should be used to parse the table
from an Org file.

An example of Org file would read like this

-----begin org file-----

#+NAME: passwords
| Key   | Login  | Password |
|-------+--------+----------|
| site1 | login1 | passwd1  |
| site2 | login2 | passwd2  |
| site3 | login3 | passwd3  |

#+HEADER: :var table=(sb-passwd--org-babel-ref-resolve \"table20170119\")
#+BEGIN_SRC emacs-lisp :colnames yes :results none
  (sb-passwd-append-from-table table)
#+END_SRC

-----end org file-----"
  (unless key-index (setq key-index
                          sb-passwd-org-table-key-index))
  (unless login-index (setq login-index
                            sb-passwd-org-table-login-index))
  (unless password-index (setq password-index
                               sb-passwd-org-table-password-index))
  (mapc (lambda (row) (sb-passwd-append (nth key-index row)
                                        (nth login-index row)
                                        (nth password-index row)))
        table)
  sb-passwd-passwords)

(defun sb-passwd-password-to-kill-ring (key)
  "Make the password for site KEY the latest kill in the kill ring.

The function returns the password (\"\" if KEY does not exist in
`sb-passwd-passwords').

When called interactively, it also echoes a message recalling the
login."
  (interactive (list (completing-read "Password for site: "
                                      (sort (map 'list
                                                 'car
                                                 sb-passwd-passwords)
                                            'string<))))
  (let ((login (sb-passwd-get key :login)))
    (if (not login) ""
      (when (interactive-p)
        (message "Your login for site \"%s\" is: %s" key login))
      (kill-new (sb-passwd-get key :password)))))

(defun sb-passwd--setup-menu-buffer ()
  "Set up a menu buffer for the selection of actions.

The function returns a read-only buffer *sb-passwd menu*, which
is created if it does not exist."
  (with-current-buffer (get-buffer-create "*sb-passwd menu*")
    (read-only-mode -1)
    (erase-buffer)
    (setq cursor-type nil)
    (insert "Existing passwords       New password\n")
    (insert "------------------       ------------\n\n")
    (insert (apply 'format (concat "[%s] Insert at point      "
                                   "[%s] Insert at point\n"
                                   "[%s] Save to kill ring    "
                                   "[%s] Save to kill ring\n")
                   (mapcar (lambda (text) (propertize text 'face 'warning))
                           '("p" "P" "k" "K"))))
    (insert (format "\n[%s] Quit" (propertize "q/Q" 'face 'success)))
    (read-only-mode 1)
    (current-buffer)))

(defun sb-passwd--get-menu-selection ()
  "Display the menu buffer and returns the users selection."
  (with-current-buffer (sb-passwd--setup-menu-buffer)
    (display-buffer-below-selected (current-buffer)
                                   '((window-height . fit-window-to-buffer)))
    (read-char-choice "sb-passwd command: " '(?p ?P ?k ?K ?q ?Q))))

(defun sb-passwd-menu ()
  "Display the menu buffer and call the selected action."
  (let ((actions '((?p . (lambda () (message "p")))
                   (?P . sb-passwd-insert-new-password)
                   (?k . sb-passwd-password-to-kill-ring))))
    (call-interactively (cdr (assoc (sb-passwd--get-menu-selection) actions)))))

(provide 'sb-passwd)

;;; sb-passwd.el ends here
