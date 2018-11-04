(el-get-bundle! dockerfile-mode
  (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(el-get-bundle docker
  (when (equal system-type 'darwin)

    (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
    (setq exec-path (append exec-path '("/usr/local/bin")))
    ;; Use "docker-machine env box" command to find out your environment variables
    (setenv "DOCKER_TLS_VERIFY" "1")
    (setenv "DOCKER_HOST" "tcp://10.11.12.13:2376")
    (setenv "DOCKER_CERT_PATH" "/Users/foo/.docker/machine/machines/box")
    (setenv "DOCKER_MACHINE_NAME" "box")))
