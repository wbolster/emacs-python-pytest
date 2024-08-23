(defmacro pytest-test-with-temp-text (text &rest body)
  (declare (indent 1) (debug t))
  `(let ((inside-text (if (stringp ,text) ,text (eval ,text))))
     (with-temp-buffer
       (setq python-indent-offset 2
             python-indent-guess-indent-offset nil)
       (python-mode)
       (let ((point (string-match "<point>" inside-text)))
	 (if point
	     (progn
	       (insert (replace-match "" nil nil inside-text))
	       (goto-char (1+ (match-beginning 0))))
	   (insert inside-text)
	   (goto-char (point-min))))
       (font-lock-ensure (point-min) (point-max))
       ,@body)))

(ert-deftest get-current-def-outside-class ()
  (pytest-test-with-temp-text (concat
                               "def foo():<point>\n"
                               "  pass\n"
                               "def bar():\n"
                               "  pass\n")
    (should (equal (python-pytest--node-id-def-at-point-treesit) "foo"))
    (forward-line 1)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "foo"))
    (forward-line 1)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "bar"))
    (forward-line 1)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "bar"))
    ;; when the buffer is narrowed, we should get the same result.
    (goto-char (point-min))
    (search-forward "foo")
    (save-restriction
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "foo")))
    (forward-line 1)
    (save-restriction
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "foo")))
    (forward-line 1)
    (save-restriction
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "bar")))
    (forward-line 1)
    (save-restriction
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "bar")))))

(ert-deftest get-current-def-inside-class ()
  (pytest-test-with-temp-text (concat
                               "class TestGroup:\n"
                               "  def foo():<point>\n"
                               "    pass\n"
                               "  def bar():\n"
                               "    pass\n")
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestGroup::foo"))
    (forward-line 1)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestGroup::foo"))
    (forward-line 1)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestGroup::bar"))
    (forward-line 1)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestGroup::bar"))
    ;; when the buffer is narrowed, we should get the same result
    (goto-char (point-min))
    (search-forward "foo")
    (save-restriction
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestGroup::foo")))
    (forward-line 1)
    (save-restriction
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestGroup::foo")))
    (forward-line 1)
    (save-restriction
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestGroup::bar")))
    (forward-line 1)
    (save-restriction
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestGroup::bar")))))

(ert-deftest get-current-def-inside-multiple-classes ()
  (pytest-test-with-temp-text (string-join
                               '("class TestDepthOne:"
                                 "  class TestDepthTwo:"
                                 "    class TestDepthThree:"
                                 "      def foo():<point>"
                                 "        pass"
                                 "      def bar():"
                                 "        pass")
                               "\n")
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::foo"))
    (forward-line 1)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::foo"))
    (forward-line 1)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::bar"))
    (forward-line 1)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::bar"))
    (forward-line 1)
    ;; when the buffer is narrowed, we should get the same result.
    (goto-char (point-min))
    (save-restriction
      (search-forward "foo")
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::foo")))
    (save-restriction
      (forward-line 1)
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::foo")))
    (save-restriction
      (forward-line 1)
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::bar")))
    (save-restriction
      (forward-line 1)
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::bar"))))
  (pytest-test-with-temp-text (string-join
                               '("class TestDepthOne:"
                                 "  def test_depth_one():<point>"
                                 "    pass"
                                 "  class TestDepthTwo:"
                                 "    def test_depth_two():"
                                 "      pass"
                                 "    class TestDepthThree:"
                                 "      def test_depth_three():"
                                 "        pass")
                               "\n")
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::test_depth_one"))
    (search-forward "test_depth_two")
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::test_depth_two"))
    (search-forward "test_depth_three")
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::test_depth_three"))
    ;; when the buffer is narrowed, we should get the same result.
    (goto-char (point-min))
    (save-restriction
      (search-forward "test_depth_one")
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::test_depth_one")))
    (save-restriction
      (search-forward "test_depth_two")
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::test_depth_two")))
    (save-restriction
      (search-forward "test_depth_three")
      (narrow-to-defun)
      (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::test_depth_three")))))

(ert-deftest get-current-def-inside-def ()
  (pytest-test-with-temp-text (string-join
                               '("def foo():"
                                 "  def bar():"
                                 "    pass<point>")
                               "\n")
    (should (equal (python-pytest--node-id-def-at-point-treesit) "foo"))
    (narrow-to-defun)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "foo")))
  (pytest-test-with-temp-text (string-join
                               '("class TestDepthOne:"
                                 "  class TestDepthTwo:"
                                 "    class TestDepthThree:"
                                 "      def foo():"
                                 "        def bar():<point>"
                                 "          pass")
                               "\n")
    ;; We want to get the outermost def because pytest can't
    ;; identify defs inside defs. In other words, pytest can
    ;; only identify those defs that are not contained within
    ;; other defs.
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::foo"))
    ;; when the buffer is narrowed, we should get the same result.
    (narrow-to-defun)
    (should (equal (python-pytest--node-id-def-at-point-treesit) "TestDepthOne::TestDepthTwo::TestDepthThree::foo"))))

(ert-deftest get-current-class-outside-class ()
  (pytest-test-with-temp-text (string-join
                               '("class Test:"
                                 "  def foo():"
                                 "    pass<point>")
                               "\n")
    (should (equal (python-pytest--node-id-class-at-point-treesit) "Test"))
    ;; when the buffer is narrowed, we should get the same result.
    (narrow-to-defun)
    (should (equal (python-pytest--node-id-class-at-point-treesit) "Test"))))

(ert-deftest get-current-class-inside-class ()
  ;; when the buffer is not narrowed
  (pytest-test-with-temp-text (string-join
                               '("class TestDepthOne:"
                                 "  class TestDepthTwo:"
                                 "    def foo():"
                                 "      pass<point>")
                               "\n")
    (should (equal
             (python-pytest--node-id-class-at-point-treesit)
             "TestDepthOne::TestDepthTwo"))
    ;; when the buffer is narrowed, we should get the same result.
    (narrow-to-defun)
    (should (equal
             (python-pytest--node-id-class-at-point-treesit)
             "TestDepthOne::TestDepthTwo"))))

(ert-deftest get-current-class-inside-multiple-classes ()
  (pytest-test-with-temp-text (string-join
                               '("class TestDepthOne:"
                                 "  class TestDepthTwo:"
                                 "    class TestDepthThree:"
                                 "      class TestDepthFour:"
                                 "        class TestDepthFive:"
                                 "          def foo():"
                                 "            pass<point>")
                               "\n")
    (should (equal
             (python-pytest--node-id-class-at-point-treesit)
             "TestDepthOne::TestDepthTwo::TestDepthThree::TestDepthFour::TestDepthFive"))
    ;; when the buffer is narrowed, we should get the same result.
    (narrow-to-defun)
    (should (equal
             (python-pytest--node-id-class-at-point-treesit)
             "TestDepthOne::TestDepthTwo::TestDepthThree::TestDepthFour::TestDepthFive"))))
