(defun kec:install-packages ()
  (dolist (pkg package-selected-packages)
    (if (package-installed-p pkg)
        (message "Package %S is already installed" pkg)
      (message "Installing package %S..." pkg)
      (package-install pkg))))

(kec:install-packages)
