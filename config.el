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

;; ------------------------------
;; Remote C++ Development Configuration
;; ------------------------------

;; TRAMP optimization for remote development
(after! tramp
  ;; Use ssh compression for better performance
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  ;; Reduce connection timeouts
  (setq tramp-connection-timeout 10
        tramp-chunksize 500)

  ;; Disable version control on remote hosts for better performance
  (setq tramp-remote-process-environment
        '("GIT_CONFIG=/dev/null" "GIT_AUTHOR_NAME=tramp" "GIT_AUTHOR_EMAIL=tramp"))

  ;; Optimize file operations
  (setq tramp-use-ssh-controlmaster-options t)
  (setq tramp-default-method "ssh"))

;; Remote C++ build configuration
(setq remote-compile-command "make -C %s build")
(setq remote-run-command "%s/build/%s")

;; Function to compile C++ projects on remote server
(defun remote-c++-compile ()
  "Compile C++ project on remote server"
  (interactive)
  (if (file-remote-p default-directory)
      (let* ((project-root (projectile-project-root))
             (compile-cmd (format remote-compile-command project-root)))
        (compile compile-cmd))
    (message "Not in a remote directory")))

;; Function to run C++ executable on remote server
(defun remote-c++-run ()
  "Run C++ executable on remote server"
  (interactive)
  (if (file-remote-p default-directory)
      (let* ((project-root (projectile-project-root))
             (project-name (file-name-base (directory-file-name project-root)))
             (run-cmd (format remote-run-command project-root project-name)))
        (async-shell-command run-cmd))
    (message "Not in a remote directory")))

;; Enhanced LSP configuration for remote C++
(after! lsp-clangd
  ;; Configure clangd for remote development
  (setq lsp-clients-clangd-args
        '("--background-index"
          "--clang-tidy"
          "--header-insertion=never"
          "--completion-style=detailed"
          "--compile-commands-dir=build"
          "--query-driver=/usr/bin/g++"
          "--all-scopes-completion"
          "--cross-file-rename"))

  ;; Reduce LSP latency for remote connections
  (setq lsp-idle-delay 0.2
        lsp-completion-provider :capf
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-snippet t
        lsp-enable-symbol-highlighting t))

;; Remote file editing optimizations
(after! cc-mode
  ;; Enable syntax highlighting for remote C++ files
  (setq c-basic-offset 4
        c-default-style "stroustrup"
        tab-width 4)

  ;; Auto-reload remote files when changed
  (auto-revert-mode 1)
  (setq auto-revert-interval 1))

;; Keybindings for remote C++ development
(map! :leader
      :desc "Remote compile" "c r" #'remote-c++-compile
      :desc "Remote run" "c R" #'remote-c++-run
      :desc "Open remote file" "f o" #'find-file
      :desc "Open remote Dired" "f d" #'dired)

;; ------------------------------
;; Additional Remote Development Utilities
;; ------------------------------

;; Function to connect to remote server with specific project
(defun connect-remote-cpp-project (hostname project-path)
  "Connect to remote C++ project on HOSTNAME at PROJECT-PATH"
  (interactive
   (list (read-string "Remote hostname: " )
         (read-string "Remote project path: ")))
  (find-file (format "/ssh:%s:%s" hostname project-path)))

;; Function to generate compile_commands.json for C++ projects
(defun generate-compile-commands ()
  "Generate compile_commands.json using Bear or compiledb"
  (interactive)
  (if (file-remote-p default-directory)
      (progn
        (message "Generating compile_commands.json on remote server...")
        (async-shell-command
         "(which bear && bear -- make) || (which compiledb && compiledb make) || echo 'Bear/compiledb not found. Install with: sudo apt install bear'"))
    (message "This function only works on remote directories")))

;; Function to sync local changes to remote (basic implementation)
(defun sync-to-remote (local-file remote-host remote-path)
  "Sync LOCAL-FILE to REMOTE-HOST:REMOTE-PATH"
  (interactive
   (list (read-file-name "Local file: " )
         (read-string "Remote host: " )
         (read-string "Remote path: ")))
  (async-shell-command
   (format "scp \"%s\" \"%s:%s\"" local-file remote-host remote-path)))

;; Add remote development commands to leader key
(map! :leader
      :prefix ("p" . "project")
      :desc "Connect to remote project" "c" #'connect-remote-cpp-project
      :desc "Generate compile commands" "g" #'generate-compile-commands
      :desc "Sync to remote" "s" #'sync-to-remote)

