;; -*- lexical-binding: t; -*-
;; DevOps: Kubernetes + AWS + GCP + Vault + Docker + Terraform/OpenTofu

;; ============================================================
;; Terraform / OpenTofu Mode
;; ============================================================

(use-package terraform-mode
  :mode ("\\.tf\\'" "\\.tfvars\\'")
  :hook ((terraform-mode . terraform-format-on-save-mode)
         (terraform-mode . eglot-ensure))
  :custom
  (terraform-indent-level 2)
  :config
  ;; Use tofu instead of terraform if available
  (when (executable-find "tofu")
    (setq terraform-format-on-save-mode-command "tofu"))

  ;; Add terraform-ls to eglot
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
                 '(terraform-mode . ("terraform-ls" "serve")))))

;; HCL mode for other HashiCorp files (Packer, Nomad, etc.)
(use-package hcl-mode
  :mode ("\\.hcl\\'" "\\.nomad\\'" "\\.pkr\\.hcl\\'"))

;; Terraform documentation lookup
(defun terraform-doc-at-point ()
  "Look up Terraform documentation for resource at point."
  (interactive)
  (let* ((word (thing-at-point 'symbol t))
         (parts (and word (split-string word "_")))
         (provider (car parts))
         (resource (string-join (cdr parts) "_")))
    (if (and provider resource)
        (browse-url (format "https://registry.terraform.io/providers/hashicorp/%s/latest/docs/resources/%s"
                            provider resource))
      (browse-url "https://registry.terraform.io/"))))

;; Terraform helper functions
(defun terraform-init ()
  "Run terraform/tofu init in current directory."
  (interactive)
  (let ((cmd (if (executable-find "tofu") "tofu" "terraform")))
    (compile (format "%s init" cmd))))

(defun terraform-plan ()
  "Run terraform/tofu plan in current directory."
  (interactive)
  (let ((cmd (if (executable-find "tofu") "tofu" "terraform")))
    (compile (format "%s plan" cmd))))

(defun terraform-apply ()
  "Run terraform/tofu apply in current directory."
  (interactive)
  (when (y-or-n-p "Run terraform apply? ")
    (let ((cmd (if (executable-find "tofu") "tofu" "terraform")))
      (compile (format "%s apply" cmd)))))

(defun terraform-validate ()
  "Run terraform/tofu validate in current directory."
  (interactive)
  (let ((cmd (if (executable-find "tofu") "tofu" "terraform")))
    (compile (format "%s validate" cmd))))

(defun terraform-state-list ()
  "Show terraform state list."
  (interactive)
  (let* ((cmd (if (executable-find "tofu") "tofu" "terraform"))
         (buffer "*terraform-state*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string (format "%s state list" cmd)))
      (special-mode)
      (switch-to-buffer buffer))))

;; Terraform transient menu
(with-eval-after-load 'transient
  (transient-define-prefix terraform-menu ()
    "Terraform/OpenTofu commands"
    ["Terraform"
     ("i" "Init" terraform-init)
     ("p" "Plan" terraform-plan)
     ("a" "Apply" terraform-apply)
     ("v" "Validate" terraform-validate)]
    ["State"
     ("s" "State list" terraform-state-list)
     ("d" "Documentation" terraform-doc-at-point)]
    ["Format"
     ("f" "Format buffer" terraform-format-buffer)]))

;; Keybinding for terraform files
(with-eval-after-load 'terraform-mode
  (define-key terraform-mode-map (kbd "C-c C-c") 'terraform-menu)
  (define-key terraform-mode-map (kbd "C-c C-d") 'terraform-doc-at-point))

;; ============================================================
;; Kubel - Interactive Kubernetes Management
;; ============================================================

(use-package kubel
  :commands (kubel kubel-namespaces)
  :custom
  (kubel-use-namespace-list 'on)
  :config
  ;; Use vterm for exec if available
  (when (featurep 'vterm)
    (setq kubel-exec-shell 'vterm)))

;; Additional kubernetes.el for overview
(use-package kubernetes
  :commands (kubernetes-overview)
  :custom
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))

;; ============================================================
;; Kubectl Helper Functions
;; ============================================================

(defun kubectl-get-contexts ()
  "Get list of kubectl contexts."
  (split-string (shell-command-to-string "kubectl config get-contexts -o name 2>/dev/null") "\n" t))

(defun kubectl-get-namespaces ()
  "Get list of namespaces in current context."
  (mapcar (lambda (s) (string-trim-left s "namespace/"))
          (split-string (shell-command-to-string "kubectl get namespaces -o name 2>/dev/null") "\n" t)))

(defun kubectl-get-pods (&optional namespace)
  "Get list of pods in NAMESPACE."
  (let ((ns (or namespace "default")))
    (mapcar (lambda (s) (string-trim-left s "pod/"))
            (split-string (shell-command-to-string
                           (format "kubectl get pods -n %s -o name 2>/dev/null" ns)) "\n" t))))

(defun kubectl-use-context ()
  "Switch kubectl context with completion."
  (interactive)
  (let ((ctx (completing-read "Context: " (kubectl-get-contexts) nil t)))
    (shell-command (format "kubectl config use-context %s" ctx))
    (message "Context: %s" ctx)))

(defun kubectl-use-namespace ()
  "Switch kubectl namespace with completion."
  (interactive)
  (let ((ns (completing-read "Namespace: " (kubectl-get-namespaces) nil t)))
    (shell-command (format "kubectl config set-context --current --namespace=%s" ns))
    (message "Namespace: %s" ns)))

(defun kubectl-logs ()
  "View pod logs with completion."
  (interactive)
  (let* ((ns (completing-read "Namespace: " (kubectl-get-namespaces) nil t))
         (pod (completing-read "Pod: " (kubectl-get-pods ns) nil t))
         (follow (y-or-n-p "Follow? "))
         (buffer (format "*kubectl-logs-%s/%s*" ns pod))
         (cmd (format "kubectl logs %s %s -n %s" (if follow "-f" "") pod ns)))
    (if follow
        (async-shell-command cmd buffer)
      (with-current-buffer (get-buffer-create buffer)
        (erase-buffer)
        (insert (shell-command-to-string cmd))
        (goto-char (point-min))
        (special-mode)
        (switch-to-buffer buffer)))))

(defun kubectl-exec ()
  "Exec into pod with completion."
  (interactive)
  (let* ((ns (completing-read "Namespace: " (kubectl-get-namespaces) nil t))
         (pod (completing-read "Pod: " (kubectl-get-pods ns) nil t))
         (shell (completing-read "Shell: " '("bash" "sh" "/bin/bash" "/bin/sh") nil nil "bash")))
    (if (featurep 'vterm)
        (vterm-other-window (format "*kubectl-exec-%s*" pod))
      (ansi-term "/bin/bash" (format "kubectl-exec-%s" pod)))
    (let ((cmd (format "kubectl exec -it %s -n %s -- %s" pod ns shell)))
      (if (featurep 'vterm)
          (vterm-send-string (concat cmd "\n"))
        (term-send-raw-string (concat cmd "\n"))))))

(defun kubectl-port-forward ()
  "Port forward to a pod/service."
  (interactive)
  (let* ((ns (completing-read "Namespace: " (kubectl-get-namespaces) nil t))
         (type (completing-read "Type: " '("pod" "service" "deployment") nil t "pod"))
         (resources (mapcar (lambda (s) (string-trim-left s (concat type "/")))
                            (split-string (shell-command-to-string
                                           (format "kubectl get %ss -n %s -o name 2>/dev/null" type ns)) "\n" t)))
         (resource (completing-read (format "%s: " (capitalize type)) resources nil t))
         (ports (read-string "Ports (local:remote): " "8080:80"))
         (buffer (format "*kubectl-pf-%s/%s*" ns resource)))
    (async-shell-command
     (format "kubectl port-forward %s/%s %s -n %s" type resource ports ns)
     buffer)
    (message "Port forwarding: localhost:%s" (car (split-string ports ":")))))

;; ============================================================
;; AWS CLI Integration
;; ============================================================

(defun aws-get-profiles ()
  "Get list of AWS profiles."
  (split-string (shell-command-to-string
                 "grep '\\[' ~/.aws/credentials 2>/dev/null | tr -d '[]'") "\n" t))

(defun aws-get-regions ()
  "Common AWS regions."
  '("us-east-1" "us-east-2" "us-west-1" "us-west-2"
    "eu-west-1" "eu-west-2" "eu-central-1"
    "ap-southeast-1" "ap-southeast-2" "ap-northeast-1"))

(defun aws-set-profile ()
  "Set AWS profile for current session."
  (interactive)
  (let ((profile (completing-read "AWS Profile: " (aws-get-profiles) nil t)))
    (setenv "AWS_PROFILE" profile)
    (message "AWS_PROFILE=%s" profile)))

(defun aws-set-region ()
  "Set AWS region for current session."
  (interactive)
  (let ((region (completing-read "AWS Region: " (aws-get-regions) nil t)))
    (setenv "AWS_DEFAULT_REGION" region)
    (message "AWS_DEFAULT_REGION=%s" region)))

(defun aws-whoami ()
  "Show current AWS identity."
  (interactive)
  (message "%s" (string-trim (shell-command-to-string "aws sts get-caller-identity 2>/dev/null"))))

(defun aws-ec2-instances ()
  "List EC2 instances in a buffer."
  (interactive)
  (let ((buffer "*aws-ec2*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string
               "aws ec2 describe-instances --query 'Reservations[*].Instances[*].[InstanceId,State.Name,InstanceType,PrivateIpAddress,Tags[?Key==`Name`].Value|[0]]' --output table 2>/dev/null"))
      (special-mode)
      (switch-to-buffer buffer))))

(defun aws-s3-buckets ()
  "List S3 buckets."
  (interactive)
  (let ((buffer "*aws-s3*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string "aws s3 ls 2>/dev/null"))
      (special-mode)
      (switch-to-buffer buffer))))

(defun aws-ssm-connect ()
  "SSM connect to an EC2 instance."
  (interactive)
  (let* ((instances (split-string
                     (shell-command-to-string
                      "aws ec2 describe-instances --query 'Reservations[*].Instances[*].[InstanceId,Tags[?Key==`Name`].Value|[0]]' --output text 2>/dev/null")
                     "\n" t))
         (instance (completing-read "Instance: " instances nil t))
         (instance-id (car (split-string instance "\t"))))
    (if (featurep 'vterm)
        (progn
          (vterm-other-window "*aws-ssm*")
          (vterm-send-string (format "aws ssm start-session --target %s\n" instance-id)))
      (async-shell-command (format "aws ssm start-session --target %s" instance-id) "*aws-ssm*"))))

(defun aws-logs-groups ()
  "List CloudWatch log groups."
  (interactive)
  (let ((buffer "*aws-logs*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string
               "aws logs describe-log-groups --query 'logGroups[*].logGroupName' --output table 2>/dev/null"))
      (special-mode)
      (switch-to-buffer buffer))))

;; ============================================================
;; GCP CLI Integration
;; ============================================================

(defun gcp-get-projects ()
  "Get list of GCP projects."
  (split-string (shell-command-to-string
                 "gcloud projects list --format='value(projectId)' 2>/dev/null") "\n" t))

(defun gcp-get-regions ()
  "Common GCP regions."
  '("us-central1" "us-east1" "us-east4" "us-west1" "us-west2"
    "europe-west1" "europe-west3" "europe-north1"
    "asia-east1" "asia-southeast1" "asia-northeast1"))

(defun gcp-set-project ()
  "Set GCP project for current session."
  (interactive)
  (let ((project (completing-read "GCP Project: " (gcp-get-projects) nil t)))
    (shell-command (format "gcloud config set project %s" project))
    (message "GCP project: %s" project)))

(defun gcp-set-region ()
  "Set GCP compute region for current session."
  (interactive)
  (let ((region (completing-read "GCP Region: " (gcp-get-regions) nil t)))
    (shell-command (format "gcloud config set compute/region %s" region))
    (message "GCP region: %s" region)))

(defun gcp-whoami ()
  "Show current GCP account and project."
  (interactive)
  (message "%s" (string-trim (shell-command-to-string "gcloud config list 2>/dev/null"))))

(defun gcp-get-clusters ()
  "Get list of GKE clusters as \"name\\tlocation\" entries."
  (split-string (shell-command-to-string
                 "gcloud container clusters list --format='value(name,location)' 2>/dev/null") "\n" t))

(defun gcp-gke-clusters ()
  "List GKE clusters in a buffer."
  (interactive)
  (let ((buffer "*gcp-gke*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string
               "gcloud container clusters list --format='table(name,location,status,currentMasterVersion)' 2>/dev/null"))
      (special-mode)
      (switch-to-buffer buffer))))

(defun gcp-gke-get-credentials ()
  "Fetch kubectl credentials for a GKE cluster."
  (interactive)
  (let* ((cluster (completing-read "Cluster: " (gcp-get-clusters) nil t))
         (parts (split-string cluster "\t"))
         (name (car parts))
         (location (cadr parts)))
    (shell-command (format "gcloud container clusters get-credentials %s --region %s" name location))
    (message "kubectl configured for GKE cluster: %s" name)))

(defun gcp-storage-buckets ()
  "List GCS buckets."
  (interactive)
  (let ((buffer "*gcp-storage*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string "gcloud storage ls 2>/dev/null"))
      (special-mode)
      (switch-to-buffer buffer))))

(defun gcp-logs ()
  "List Cloud Logging log names."
  (interactive)
  (let ((buffer "*gcp-logs*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string "gcloud logging logs list --format='table(name)' 2>/dev/null"))
      (special-mode)
      (switch-to-buffer buffer))))

;; ============================================================
;; Vault Integration
;; ============================================================

(defun vault-set-address ()
  "Set VAULT_ADDR for current session."
  (interactive)
  (let ((addr (read-string "Vault address: " (or (getenv "VAULT_ADDR") "https://vault:8200"))))
    (setenv "VAULT_ADDR" addr)
    (message "VAULT_ADDR=%s" addr)))

(defun vault-status ()
  "Show vault status in a buffer."
  (interactive)
  (let ((buffer "*vault-status*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string "vault status 2>&1"))
      (special-mode)
      (switch-to-buffer buffer))))

(defun vault-whoami ()
  "Show current vault token info in a buffer."
  (interactive)
  (let ((buffer "*vault-whoami*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string "vault token lookup 2>&1"))
      (special-mode)
      (switch-to-buffer buffer))))

(defun vault-login ()
  "Log in to vault with a chosen auth method."
  (interactive)
  (let ((method (completing-read "Auth method: " '("oidc" "token" "userpass" "ldap") nil t "oidc")))
    (if (featurep 'vterm)
        (progn
          (vterm-other-window "*vault-login*")
          (vterm-send-string (format "vault login -method=%s\n" method)))
      (async-shell-command (format "vault login -method=%s" method) "*vault-login*"))))

(defun vault-kv-list (path)
  "List secrets under PATH."
  (interactive "sVault path: ")
  (let ((buffer "*vault-kv-list*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string (format "vault kv list %s 2>&1" (shell-quote-argument path))))
      (special-mode)
      (switch-to-buffer buffer))))

(defun vault-kv-get (path)
  "Get secret at PATH."
  (interactive "sVault path: ")
  (let ((buffer "*vault-kv-get*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string (format "vault kv get %s 2>&1" (shell-quote-argument path))))
      (special-mode)
      (switch-to-buffer buffer))))

;; ============================================================
;; Docker Integration
;; ============================================================

(use-package docker
  :commands docker
  :bind ("C-c D" . docker)
  :custom
  (docker-command "podman")
  (docker-compose-command "podman-compose")
  (docker-container-tramp-method "podman"))

(defun docker-ps ()
  "List running containers."
  (interactive)
  (let ((buffer "*podman-ps*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string "podman ps --format 'table {{.ID}}\t{{.Image}}\t{{.Status}}\t{{.Names}}'"))
      (special-mode)
      (switch-to-buffer buffer))))

;; ============================================================
;; Transient Menus (Magit-style)
;; ============================================================

(use-package transient
  :config

  ;; Kubernetes menu
  (transient-define-prefix kubectl-menu ()
    "Kubectl commands"
    ["Context & Namespace"
     ("c" "Switch context" kubectl-use-context)
     ("n" "Switch namespace" kubectl-use-namespace)]
    ["Resources"
     ("k" "Kubel (interactive)" kubel)
     ("K" "Kubernetes overview" kubernetes-overview)]
    ["Pod Operations"
     ("l" "Logs" kubectl-logs)
     ("e" "Exec into pod" kubectl-exec)
     ("p" "Port forward" kubectl-port-forward)])

  ;; AWS menu
  (transient-define-prefix aws-menu ()
    "AWS commands"
    ["Profile & Region"
     ("p" "Set profile" aws-set-profile)
     ("r" "Set region" aws-set-region)
     ("w" "Whoami" aws-whoami)]
    ["Services"
     ("e" "EC2 instances" aws-ec2-instances)
     ("s" "S3 buckets" aws-s3-buckets)
     ("l" "CloudWatch logs" aws-logs-groups)]
    ["Connect"
     ("c" "SSM connect" aws-ssm-connect)])

  ;; GCP menu
  (transient-define-prefix gcp-menu ()
    "GCP commands"
    ["Project & Region"
     ("p" "Set project" gcp-set-project)
     ("r" "Set region" gcp-set-region)
     ("w" "Whoami" gcp-whoami)]
    ["Services"
     ("k" "GKE clusters" gcp-gke-clusters)
     ("c" "GKE get-credentials" gcp-gke-get-credentials)
     ("s" "Storage buckets" gcp-storage-buckets)
     ("l" "Logs" gcp-logs)])

  ;; Vault menu
  (transient-define-prefix vault-menu ()
    "Vault commands"
    ["Connection"
     ("a" "Set address" vault-set-address)
     ("s" "Status" vault-status)
     ("w" "Whoami" vault-whoami)
     ("l" "Login" vault-login)]
    ["KV Secrets"
     ("k" "List path" vault-kv-list)
     ("g" "Get secret" vault-kv-get)])

  ;; DevOps master menu
  (transient-define-prefix devops-menu ()
    "DevOps commands"
    [["Kubernetes"
      ("k" "Kubectl menu" kubectl-menu)
      ("K" "Kubel" kubel)]
     ["AWS"
      ("a" "AWS menu" aws-menu)]
     ["GCP"
      ("g" "GCP menu" gcp-menu)]
     ["Vault"
      ("v" "Vault menu" vault-menu)]
     ["Terraform"
      ("t" "Terraform menu" terraform-menu)]
     ["Helm"
      ("h" "Template" helm-template)
      ("H" "Lint" helm-lint)]
     ["Mise"
      ("m" "Current" mise-current)
      ("M" "List" mise-list)
      ("n" "New project" mise-new-project)]
     ["Docker"
      ("d" "Docker" docker)
      ("D" "Docker ps" docker-ps)]]))

;; ============================================================
;; Helm Charts
;; ============================================================

(use-package k8s-mode
  :hook (k8s-mode . yas-minor-mode))

(defun helm-template ()
  "Run helm template on the current chart directory."
  (interactive)
  (let* ((chart-dir (locate-dominating-file default-directory "Chart.yaml"))
         (buffer "*helm-template*"))
    (if chart-dir
        (progn
          (with-current-buffer (get-buffer-create buffer)
            (erase-buffer)
            (insert (shell-command-to-string (format "helm template %s 2>&1" chart-dir)))
            (yaml-mode)
            (goto-char (point-min))
            (switch-to-buffer buffer)))
      (message "No Chart.yaml found in parent directories"))))

(defun helm-lint ()
  "Run helm lint on the current chart."
  (interactive)
  (let ((chart-dir (locate-dominating-file default-directory "Chart.yaml")))
    (if chart-dir
        (compile (format "helm lint %s" chart-dir))
      (message "No Chart.yaml found in parent directories"))))

(defun helm-dry-run ()
  "Run helm install --dry-run on the current chart."
  (interactive)
  (let ((chart-dir (locate-dominating-file default-directory "Chart.yaml")))
    (if chart-dir
        (let* ((name (read-string "Release name: " "test"))
               (buffer "*helm-dry-run*"))
          (with-current-buffer (get-buffer-create buffer)
            (erase-buffer)
            (insert (shell-command-to-string
                     (format "helm install %s %s --dry-run 2>&1" name chart-dir)))
            (yaml-mode)
            (goto-char (point-min))
            (switch-to-buffer buffer)))
      (message "No Chart.yaml found in parent directories"))))

;; ============================================================
;; Mise (polyglot runtime manager)
;; ============================================================

(defun mise-install ()
  "Run mise install in current project."
  (interactive)
  (compile "mise install"))

(defun mise-use (tool)
  "Run mise use TOOL in current project."
  (interactive "sTool (e.g. python@3.12, node@20): ")
  (compile (format "mise use %s" tool)))

(defun mise-list ()
  "Show installed mise tools."
  (interactive)
  (let ((buffer "*mise-list*"))
    (with-current-buffer (get-buffer-create buffer)
      (erase-buffer)
      (insert (shell-command-to-string "mise list 2>&1"))
      (special-mode)
      (switch-to-buffer buffer))))

(defun mise-current ()
  "Show active mise tool versions."
  (interactive)
  (message "%s" (string-trim (shell-command-to-string "mise current 2>&1"))))

(defun mise-new-project (dir)
  "Scaffold a per-project mise.toml + .envrc + identity dirs in DIR.
Mirrors the rtl-style setup: isolated GNUPGHOME, AWS creds, kube
configs, OCI config and pass store, all rooted under DIR so they
never bleed into the global ones."
  (interactive "DProject directory: ")
  (unless (file-exists-p dir) (make-directory dir t))
  (let* ((default-directory dir)
         (email (read-string "Git author email: " user-mail-address))
         (vault-addr (read-string "Vault address (blank to skip): "))
         (gnupg-dir (expand-file-name ".gnupg" dir)))
    (dolist (sub '(".gnupg" ".aws" ".kube" ".oci" "pass"))
      (make-directory (expand-file-name sub dir) t))
    (set-file-modes gnupg-dir #o700)
    (with-temp-file (expand-file-name "mise.toml" dir)
      (insert "[env]\n")
      (insert (format "GIT_AUTHOR_EMAIL = \"%s\"\n" email))
      (insert (format "GIT_COMMITTER_EMAIL = \"%s\"\n" email))
      (insert "GNUPGHOME = \"{{config_root}}/.gnupg\"\n")
      (insert "PASSWORD_STORE_DIR = \"{{config_root}}/pass\"\n")
      (insert "AWS_DIR = \"{{config_root}}/.aws\"\n")
      (insert "AWS_SHARED_CREDENTIALS_FILE = \"{{env.AWS_DIR}}/credentials\"\n")
      (insert "AWS_CONFIG_FILE = \"{{env.AWS_DIR}}/config\"\n")
      (insert "KUBECONFIG = '''{{ exec(command=\"echo \" ~ config_root ~ \"/.kube/*.yaml | tr ' ' ':'\") }}'''\n")
      (insert "OCI_CONFIG_FILE = \"{{config_root}}/.oci/config\"\n")
      (insert "OCI_PRIVATE_KEY_PATH = \"{{config_root}}/.oci/id_rsa\"\n")
      (unless (string-empty-p vault-addr)
        (insert (format "VAULT_ADDR = \"%s\"\n" vault-addr))))
    (with-temp-file (expand-file-name ".envrc" dir)
      (insert "use mise\n"))
    (shell-command (format "direnv allow %s" (shell-quote-argument dir)))
    (message "Scaffolded %s - drop kubeconfigs into .kube/, aws creds into .aws/" dir)))

;; Global bindings
(global-set-key (kbd "C-c k") 'kubectl-menu)
(global-set-key (kbd "C-c a") 'aws-menu)
(global-set-key (kbd "C-c G") 'gcp-menu)
(global-set-key (kbd "C-c v") 'vault-menu)
(global-set-key (kbd "C-c K") 'devops-menu)

;; ============================================================
;; Meow Leader Integration
;; ============================================================

(with-eval-after-load 'meow
  (meow-leader-define-key
   ;; Kubernetes
   '("k" . (keymap))
   '("kk" . kubel)
   '("ko" . kubernetes-overview)
   '("km" . kubectl-menu)
   '("kc" . kubectl-use-context)
   '("kn" . kubectl-use-namespace)
   '("kl" . kubectl-logs)
   '("ke" . kubectl-exec)
   '("kp" . kubectl-port-forward)

   ;; AWS
   '("K" . (keymap))
   '("Ka" . aws-menu)
   '("Kp" . aws-set-profile)
   '("Kr" . aws-set-region)
   '("Ke" . aws-ec2-instances)
   '("Ks" . aws-s3-buckets)
   '("Kc" . aws-ssm-connect)
   '("Kw" . aws-whoami)

   ;; GCP
   '("G" . (keymap))
   '("Gg" . gcp-menu)
   '("Gp" . gcp-set-project)
   '("Gr" . gcp-set-region)
   '("Gw" . gcp-whoami)
   '("Gk" . gcp-gke-clusters)
   '("Gc" . gcp-gke-get-credentials)
   '("Gs" . gcp-storage-buckets)
   '("Gl" . gcp-logs)

   ;; Vault
   '("v" . (keymap))
   '("vv" . vault-menu)
   '("va" . vault-set-address)
   '("vs" . vault-status)
   '("vw" . vault-whoami)
   '("vl" . vault-login)
   '("vk" . vault-kv-list)
   '("vg" . vault-kv-get)

   ;; Terraform
   '("T" . (keymap))
   '("Tm" . terraform-menu)
   '("Ti" . terraform-init)
   '("Tp" . terraform-plan)
   '("Ta" . terraform-apply)
   '("Tv" . terraform-validate)
   '("Ts" . terraform-state-list)

   ;; Helm
   '("H" . (keymap))
   '("Ht" . helm-template)
   '("Hl" . helm-lint)
   '("Hd" . helm-dry-run)

   ;; Mise
   '("M" . (keymap))
   '("Mc" . mise-current)
   '("Ml" . mise-list)
   '("Mi" . mise-install)
   '("Mu" . mise-use)
   '("Mn" . mise-new-project)))

(provide 'init.devops)
