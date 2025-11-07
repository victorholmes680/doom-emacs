;;; config.el --- Doom Emacs configuration for Java + LSP + Corfu

;; ------------------------------
;; LSP Java / jdtls 最终配置
;; ------------------------------
(after! lsp-java
  ;; 指定 Java 可执行文件路径
  (setq lsp-java-java-path "/opt/homebrew/opt/openjdk/bin/java")

  ;; jdtls server 安装目录（Homebrew 安装）
  (setq lsp-java-server-install-dir "/opt/homebrew/Cellar/jdtls/1.47.0/libexec")

  ;; ⚠️ 每个项目独立的 workspace 目录，避免 “Internal error”
  (setq lsp-java-workspace-dir
        (expand-file-name
         (format "~/.emacs.d/.cache/lsp-java/%s/"
                 (md5 (or (projectile-project-root) default-directory)))))

  ;; Maven 可执行路径
  (setq lsp-java-maven-path "/Users/wangzhixiong/Downloads/apache-maven-3.9.9/bin/mvn")

  ;; Maven 本地仓库
  ;;(setq lsp-java-maven-local-repository "/Users/wangzhixiong/repo")

  ;; Lombok 支持（确保路径存在）
  (let ((lombok-jar
         (expand-file-name
          "/Users/wangzhixiong/.config/doom/lombok-1.18.24.jar")))
    (when (file-exists-p lombok-jar)
      ;; 在 jdtls 启动参数中加入 lombok agent
      (setq lsp-java-vmargs
            (list
             "-noverify"
             "-Xmx2G"
             "-XX:+UseG1GC"
             (concat "-javaagent:" lombok-jar)
             (concat "-Xbootclasspath/a:" lombok-jar)
             "--add-modules=ALL-SYSTEM"
             "--add-opens=java.base/java.util=ALL-UNNAMED"
             "--add-opens=java.base/java.lang=ALL-UNNAMED"))))

  ;; 保存前自动格式化
  (add-hook 'java-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'lsp-format-buffer nil t)))

  ;; 自动导入 + Organize Imports
  (setq lsp-java-save-action-organize-imports t)

  ;; 使用 UTF-8 编码（防止部分 JDTLS 字符集错误）
  (setenv "JAVA_TOOL_OPTIONS" "-Dfile.encoding=UTF-8")
)



;; ------------------------------
;; Company-mode 补全配置
;; ------------------------------
(use-package! company
  :config
  (global-company-mode 1)
  ;; 等待 0.2 秒触发补全
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1)
  ;; LSP 使用 company
  (setq lsp-completion-provider :capf)
  (add-hook 'lsp-mode-hook #'company-mode))

;; ------------------------------
;; Flycheck 自动语法检查
;; ------------------------------
(after! flycheck
  (global-flycheck-mode 1))

;; ------------------------------
;; Evil / M-. 风格快捷键绑定
;; ------------------------------
(after! lsp-mode
  (define-key lsp-mode-map (kbd "M-.") #'lsp-find-definition)
  (define-key lsp-mode-map (kbd "M-,") #'pop-tag-mark)   ;; 回到上一个位置
  (define-key lsp-mode-map (kbd "M-?") #'lsp-find-references)
  (define-key lsp-mode-map (kbd "C-c j k") #'lsp-describe-thing-at-point))

(setq scroll-conservatively 101)
(setq scroll-margin 3)
(setq scroll-step 1)

(after! lsp-mode
  (setq lsp-clients-clangd-args
        '("--background-index"
          "--clang-tidy"
          "--header-insertion=never"
          "--completion-style=detailed"
          "--compile-commands-dir=build"
          "--query-driver=/usr/bin/g++"))
  (setq lsp-enable-symbol-highlighting t
        lsp-enable-snippet t
        lsp-auto-guess-root t
        lsp-idle-delay 0.1
        lsp-completion-provider :capf))

;; 终端下开启鼠标支持
(unless (display-graphic-p)
  (xterm-mouse-mode 1))

  ;; ------------------------------
;; Tree-sitter grammar sources
;; ------------------------------
(setq treesit-language-source-alist
      '((c     . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp   . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (java  . ("https://github.com/tree-sitter/tree-sitter-java"))
        (json  . ("https://github.com/tree-sitter/tree-sitter-json"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))))

