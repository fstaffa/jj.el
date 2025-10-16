(package "jj"
         "0.0.1"
         "Integration with jujutsu version control system")

(website-url "https://github.com/fstaffa/jj.el")
(keywords "jj" "jujutsu" "vcs")

(package-file "jj.el")

(script "test" "eask exec buttercup -L . -L tests tests/")
(script "coverage" "eask emacs --batch --load scripts/run-coverage-proper.el")

(source "gnu")
(source "melpa")

(depends-on "emacs" "29.1")
(depends-on "s")
(depends-on "transient" )
(development
 (depends-on "buttercup")
 (depends-on "undercover"))
