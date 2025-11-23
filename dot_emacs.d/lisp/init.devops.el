;; Enhanced Kubernetes integration for Emacs
;; Integrates with zsh/bash shell configuration and provides better tools

;; Core kubernetes package with better settings
(use-package kubernetes
  :commands (kubernetes-overview)
  :init
  (setq kubernetes-poll-frequency 3600
        kubernetes-redraw-frequency 3600
        kubernetes-default-overview-view 'overview
        kubernetes-default-overview-namespace "default"
        kubernetes-commands-display-buffer-function 'display-buffer)
  :bind (:map kubernetes-mode-map
         ("C-c C-k" . kubernetes-set-namespace)
         ("C-c C-c" . kubernetes-copy-pod-name)))

;; Get kubectl config from shell (zsh/bash compatible)
(defun get-kubectl-context-from-shell ()
  "Get current kubectl context from shell."
  (string-trim
   (shell-command-to-string "kubectl config current-context")))

(defun get-kubectl-namespace-from-shell ()
  "Get current kubectl namespace from shell."
  (string-trim
   (shell-command-to-string "kubectl config view --minify --output \"jsonpath={..namespace}\"")))

;; Function to sync kubectl context from shell
(defun kubectl-sync-from-shell ()
  "Sync kubectl context and namespace from shell."
  (interactive)
  (let ((context (get-kubectl-context-from-shell))
        (namespace (get-kubectl-namespace-from-shell)))
    (when (and context (not (string= context "")))
      (message "Setting kubectl context to %s" context)
      (shell-command (format "kubectl config use-context %s" context)))
    (when (and namespace (not (string= namespace "")))
      (message "Setting kubectl namespace to %s" namespace)
      (shell-command (format "kubectl config set-context --current --namespace=%s" namespace)))))

;; Enhanced kubectl with Emacs integration
(defun kubectl-get-resources (&optional resource namespace)
  "Run kubectl get for the specified RESOURCE in NAMESPACE."
  (interactive
   (list
    (completing-read "Resource: " '("pods" "deployments" "services" "ingress" "configmaps" "secrets" "nodes" "pv" "pvc" "namespaces"))
    (completing-read "Namespace: " (split-string (shell-command-to-string "kubectl get namespaces -o name") "\n" t))))
  (let ((resource (or resource "pods"))
        (namespace (or namespace "default")))
    (when (string-prefix-p "namespace/" namespace)
      (setq namespace (substring namespace 10)))
    (switch-to-buffer (format "*kubectl-%s-%s*" namespace resource))
    (erase-buffer)
    (insert (shell-command-to-string (format "kubectl get %s -n %s" resource namespace)))
    (special-mode)))

;; Support for kubectl logs
(defun kubectl-logs (pod &optional namespace container follow)
  "View logs for a POD in NAMESPACE and optionally CONTAINER with FOLLOW flag."
  (interactive
   (let* ((namespace (completing-read "Namespace: " (split-string (shell-command-to-string "kubectl get namespaces -o name") "\n" t)))
          (pod-list (split-string (shell-command-to-string (format "kubectl get pods -n %s -o name" namespace)) "\n" t))
          (pod (completing-read "Pod: " pod-list))
          (follow (y-or-n-p "Follow logs? ")))
     (list pod namespace nil follow)))
  
  (when (string-prefix-p "pod/" pod)
    (setq pod (substring pod 4)))
  
  (when (string-prefix-p "namespace/" namespace)
    (setq namespace (substring namespace 10)))
  
  (let* ((containers (split-string (shell-command-to-string 
                                  (format "kubectl get pod %s -n %s -o jsonpath='{.spec.containers[*].name}'" pod namespace)) " " t))
         (container (if (= (length containers) 1)
                        (car containers)
                      (completing-read "Container: " containers)))
         (buffer-name (format "*kubectl-logs-%s-%s*" namespace pod))
         (cmd (format "kubectl logs %s %s -n %s -c %s"
                      (if follow "-f" "")
                      pod
                      namespace
                      container)))
    
    (if follow
        (async-shell-command cmd buffer-name)
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert (shell-command-to-string cmd))
        (goto-char (point-min))
        (special-mode)
        (switch-to-buffer buffer-name)))))

;; Describe Kubernetes resources
(defun kubectl-describe (resource &optional namespace)
  "Describe a Kubernetes RESOURCE in NAMESPACE."
  (interactive
   (let* ((res-type (completing-read "Resource type: " 
                                  '("pod" "deployment" "service" "ingress" "configmap" "secret" "node" "pv" "pvc" "namespace")))
          (namespace (unless (member res-type '("node" "pv" "namespace"))
                       (completing-read "Namespace: " (split-string (shell-command-to-string 
                                                                   "kubectl get namespaces -o name") "\n" t))))
          (resources (if namespace
                         (split-string (shell-command-to-string 
                                      (format "kubectl get %ss -n %s -o name" res-type namespace)) "\n" t)
                       (split-string (shell-command-to-string 
                                    (format "kubectl get %ss -o name" res-type)) "\n" t)))
          (resource (completing-read (format "%s: " res-type) resources)))
     (list resource namespace)))
  
  (let* ((buffer-name (format "*kubectl-describe-%s*" resource))
         (cmd (if namespace
                  (format "kubectl describe %s -n %s" resource namespace)
                (format "kubectl describe %s" resource))))
    
    (with-current-buffer (get-buffer-create buffer-name)
      (erase-buffer)
      (insert (shell-command-to-string cmd))
      (goto-char (point-min))
      (special-mode)
      (switch-to-buffer buffer-name))))

;; Exec into a pod
(defun kubectl-exec-pod (pod &optional namespace)
  "Execute a shell in POD in NAMESPACE."
  (interactive
   (let* ((namespace (completing-read "Namespace: " (split-string (shell-command-to-string 
                                                               "kubectl get namespaces -o name") "\n" t)))
          (pod-list (split-string (shell-command-to-string 
                                 (format "kubectl get pods -n %s -o name" namespace)) "\n" t))
          (pod (completing-read "Pod: " pod-list)))
     (list pod namespace)))
  
  (when (string-prefix-p "pod/" pod)
    (setq pod (substring pod 4)))
  
  (when (string-prefix-p "namespace/" namespace)
    (setq namespace (substring namespace 10)))
  
  (let* ((containers (split-string (shell-command-to-string 
                                  (format "kubectl get pod %s -n %s -o jsonpath='{.spec.containers[*].name}'" pod namespace)) " " t))
         (container (if (= (length containers) 1)
                        (car containers)
                      (completing-read "Container: " containers)))
         (cmd (format "kubectl exec -it %s -n %s -c %s -- bash" pod namespace container)))
    (with-current-buffer (get-buffer-create "*kubectl-exec*")
      (async-shell-command cmd "*kubectl-exec*"))))

;; Port-forward to pod
(defun kubectl-port-forward (resource &optional namespace local-port remote-port)
  "Forward LOCAL-PORT to REMOTE-PORT on RESOURCE in NAMESPACE."
  (interactive
   (let* ((res-type (completing-read "Resource type: " '("pod" "service" "deployment")))
          (namespace (completing-read "Namespace: " (split-string (shell-command-to-string 
                                                               "kubectl get namespaces -o name") "\n" t)))
          (resources (split-string (shell-command-to-string 
                                  (format "kubectl get %ss -n %s -o name" res-type namespace)) "\n" t))
          (resource (completing-read (format "%s: " res-type) resources))
          (port-spec (read-string "Port (local:remote): " "8080:80"))
          (ports (split-string port-spec ":")))
     (list resource namespace (car ports) (cadr ports))))
  
  (when (and (stringp resource) (string-match "/" resource))
    (setq resource (substring resource (1+ (string-match "/" resource)))))
  
  (when (string-prefix-p "namespace/" namespace)
    (setq namespace (substring namespace 10)))
  
  (let ((cmd (format "kubectl port-forward %s %s:%s -n %s" 
                     resource local-port remote-port namespace))
        (buffer-name (format "*kubectl-portforward-%s-%s*" resource local-port)))
    (async-shell-command cmd buffer-name)
    (message "Port forwarding started. Access at localhost:%s" local-port)))

;; Function to browse kubectl services in browser
(defun kubectl-browse-service (service &optional namespace)
  "Open a service from kubectl in the web browser."
  (interactive
   (let* ((namespace (completing-read "Namespace: " (split-string (shell-command-to-string 
                                                               "kubectl get namespaces -o name") "\n" t)))
          (service-list (split-string (shell-command-to-string 
                                     (format "kubectl get services -n %s -o name" namespace)) "\n" t))
          (service (completing-read "Service: " service-list)))
     (list service namespace)))
  
  (when (string-prefix-p "service/" service)
    (setq service (substring service 8)))
  
  (when (string-prefix-p "namespace/" namespace)
    (setq namespace (substring namespace 10)))
  
  ;; Get the port
  (let* ((port-data (shell-command-to-string
                   (format "kubectl get service %s -n %s -o jsonpath='{.spec.ports[0].port}'" service namespace)))
         (port (if (and port-data (not (string-empty-p port-data)))
                   (string-to-number port-data)
                 80))
         (local-port (+ 8000 (random 1000))))
    
    ;; Start port-forwarding
    (let ((proc (start-process 
               (format "kubectl-port-forward-%s" service)
               (format "*kubectl-port-forward-%s*" service)
               "kubectl" "port-forward" 
               (format "service/%s" service) 
               (format "%d:%d" local-port port)
               "-n" namespace)))
      
      ;; Give it a moment to start
      (sleep-for 1)
      
      ;; Open browser
      (browse-url (format "http://localhost:%d" local-port))
      
      ;; Set up process sentinel to clean up
      (set-process-sentinel 
       proc
       (lambda (process event)
         (when (string-match "finished" event)
           (message "Port forwarding for %s stopped." service)))))))

;; Create a transient menu for kubectl commands (like magit)
(use-package transient
  :after kubernetes
  :config
  (transient-define-prefix kubectl-menu ()
    "Kubectl menu"
    ["Kubectl commands"
     ["View resources"
      ("p" "Get pods" kubectl-get-resources)
      ("d" "Get deployments" (lambda () (interactive) (kubectl-get-resources "deployments")))
      ("s" "Get services" (lambda () (interactive) (kubectl-get-resources "services")))
      ("i" "Get ingress" (lambda () (interactive) (kubectl-get-resources "ingress")))
      ("c" "Get configmaps" (lambda () (interactive) (kubectl-get-resources "configmaps")))
      ("S" "Get secrets" (lambda () (interactive) (kubectl-get-resources "secrets")))
      ("n" "Get nodes" (lambda () (interactive) (kubectl-get-resources "nodes")))
      ("N" "Get namespaces" (lambda () (interactive) (kubectl-get-resources "namespaces")))]
     ["Describe & Logs"
      ("D" "Describe resource" kubectl-describe)
      ("l" "View logs" kubectl-logs)]
     ["Actions"
      ("e" "Exec into pod" kubectl-exec-pod)
      ("f" "Port forward" kubectl-port-forward)
      ("b" "Browse service" kubectl-browse-service)
      ("C" "Copy resource name" kubernetes-copy-thing-at-point)]]
    ["Context"
     ("k" "Kubernetes Overview" kubernetes-overview)
     ("x" "Set context" kubernetes-set-context)
     ("X" "Sync from shell" kubectl-sync-from-shell)
     ("ns" "Set namespace" kubernetes-set-namespace)])
  
  ;; Bind the menu to a key
  (global-set-key (kbd "C-c k") 'kubectl-menu))

;; Function to easily switch kubectl contexts with completion
(defun kubectl-use-context ()
  "Switch kubectl context using completion."
  (interactive)
  (let* ((contexts (split-string (shell-command-to-string "kubectl config get-contexts -o name") "\n" t))
         (context (completing-read "Context: " contexts)))
    (shell-command (format "kubectl config use-context %s" context))
    (message "Switched to context: %s" context)))

;; Function to easily switch kubectl namespaces with completion
(defun kubectl-use-namespace ()
  "Switch kubectl namespace using completion."
  (interactive)
  (let* ((namespaces (split-string (shell-command-to-string "kubectl get namespaces -o name") "\n" t))
         (namespace (completing-read "Namespace: " namespaces)))
    (when (string-prefix-p "namespace/" namespace)
      (setq namespace (substring namespace 10)))
    (shell-command (format "kubectl config set-context --current --namespace=%s" namespace))
    (message "Switched to namespace: %s" namespace)))

;; Add shell integration in Eshell for kubectl
(with-eval-after-load 'eshell
  (defun eshell/kc (&rest args)
    "Run kubectl commands in eshell, with completion when possible."
    (let ((cmd (string-join (cons "kubectl" args) " ")))
      (eshell-print (shell-command-to-string cmd)))))

;; General.el integration for kubernetes commands
(with-eval-after-load 'general
  (when (fboundp 'my/leader-keys)
    (my/leader-keys
      ;; Kubernetes operations
      "k"  '(:ignore t :which-key "kubernetes")
      "ko" '(kubernetes-overview :which-key "kubernetes overview")
      "km" '(kubectl-menu :which-key "kubectl menu")
      "kl" '(kubectl-logs :which-key "kubectl logs")
      "ke" '(kubectl-exec-pod :which-key "kubectl exec")
      "kf" '(kubectl-port-forward :which-key "kubectl port-forward")
      "kc" '(kubectl-use-context :which-key "kubectl context")
      "kn" '(kubectl-use-namespace :which-key "kubectl namespace")
      "ks" '(kubectl-sync-from-shell :which-key "kubectl sync from shell"))))

;; Update which-key descriptions
(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "SPC k" "kubernetes"))

(provide 'init.devops)
