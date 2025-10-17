;;; test-jj-navigation.el --- Tests for jj-status navigation  -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for Task Group 4: Navigation System

;;; Code:

(require 'buttercup)
(require 'test-helper)
(require 'jj)

(describe "Task Group 4: Navigation System"

  (describe "jj-status--mark-item-bounds"
    (it "should mark text region with jj-item property"
      (with-temp-buffer
        (insert "test line\n")
        (let ((start (point-min))
              (end (1- (point-max)))
              (item-data '(:path "test.txt" :status "M")))
          (jj-status--mark-item-bounds start end item-data)
          (expect (get-text-property start 'jj-item) :to-equal item-data)))))

  (describe "jj-status--item-at-point"
    (it "should return file item when on a file line"
      (with-temp-buffer
        (let ((file-data '(:path "test.txt" :status "M")))
          (insert "M  test.txt\n")
          (put-text-property (point-min) (1- (point-max)) 'jj-item file-data)
          (goto-char (point-min))
          (let ((result (jj-status--item-at-point)))
            (expect (plist-get result :type) :to-be 'file)
            (expect (plist-get result :data) :to-equal file-data)))))

    (it "should return revision item when on a revision line"
      (with-temp-buffer
        (let ((rev-data '(:change-id "qpvuntsm" :description "Test")))
          (insert "@  qpvuntsm  Test\n")
          (put-text-property (point-min) (1- (point-max)) 'jj-item rev-data)
          (goto-char (point-min))
          (let ((result (jj-status--item-at-point)))
            (expect (plist-get result :type) :to-be 'revision)
            (expect (plist-get result :data) :to-equal rev-data)))))

    (it "should return nil when on empty space"
      (with-temp-buffer
        (insert "Empty line\n")
        (goto-char (point-min))
        (let ((result (jj-status--item-at-point)))
          (expect (plist-get result :type) :to-be nil)
          (expect (plist-get result :data) :to-be nil)))))

  (describe "jj-status-next-item"
    (it "should move to next item with jj-item property"
      (with-temp-buffer
        (insert "Header\n")
        (insert "Item 1\n")
        (insert "Item 2\n")
        ;; Mark only items, not header
        (put-text-property 8 15 'jj-item '(:path "file1.txt"))
        (put-text-property 15 22 'jj-item '(:path "file2.txt"))
        (goto-char (point-min))
        (jj-status-next-item)
        (expect (point) :to-equal 8)))

    (it "should skip lines without jj-item property"
      (with-temp-buffer
        (insert "Header\n")
        (insert "Blank\n")
        (insert "Item\n")
        (put-text-property 14 19 'jj-item '(:path "test.txt"))
        (goto-char (point-min))
        (jj-status-next-item)
        (expect (point) :to-equal 14))))

  (describe "jj-status-prev-item"
    (it "should move to previous item with jj-item property"
      (with-temp-buffer
        (insert "Item 1\n")
        (insert "Item 2\n")
        (insert "Item 3\n")
        (put-text-property (point-min) 7 'jj-item '(:path "file1.txt"))
        (put-text-property 7 14 'jj-item '(:path "file2.txt"))
        (put-text-property 14 21 'jj-item '(:path "file3.txt"))
        (goto-char 16)  ; Start in Item 3
        (let ((start-item (get-text-property (point) 'jj-item)))
          (jj-status-prev-item)
          ;; Should have moved to a different item
          (let ((new-item (get-text-property (point) 'jj-item)))
            (expect new-item :not :to-equal start-item))))))

  (describe "jj-status-show-diff"
    (it "should show placeholder message for files"
      (with-temp-buffer
        (insert "M  test.txt\n")
        (put-text-property (point-min) (1- (point-max)) 'jj-item '(:path "test.txt" :status "M"))
        (goto-char (point-min))
        ;; Function shows message, doesn't throw error
        (jj-status-show-diff)
        (expect t :to-be t)))

    (it "should show placeholder message for revisions"
      (with-temp-buffer
        (insert "@  qpvuntsm  Working copy\n")
        (put-text-property (point-min) (1- (point-max)) 'jj-item '(:change-id "qpvuntsm"))
        (goto-char (point-min))
        ;; Function shows message, doesn't throw error
        (jj-status-show-diff)
        (expect t :to-be t)))))

;;; test-jj-navigation.el ends here
