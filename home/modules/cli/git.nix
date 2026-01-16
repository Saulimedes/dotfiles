# Git configuration
{ config, pkgs, lib, userConfig, ... }:

{
  programs.git = {
    enable = true;

    # Git settings (new format)
    settings = {
      user = {
        name = userConfig.fullName;
        email = userConfig.email;
      };

      # Aliases
      alias = {
        A = "add -A .";
        aa = "add --all";
        amend = "commit --amend --no-edit";
        br = "branch --format='%(HEAD) %(color:yellow)%(refname:short)%(color:reset) - %(contents:subject) %(color:green)(%(committerdate:relative)) [%(authorname)]' --sort=-committerdate";
        branch-name = "!git for-each-ref --format='%(refname:short)' 'git symbolic-ref HEAD'";
        bx = "branch -D";
        c = "clone";
        ck = "checkout";
        clean = "clean -fdx";
        done = "!git push origin HEAD";
        force = "push --force";
        harder = "reset --hard";
        last = "log -1 HEAD --stat";
        lg1 = "log --graph --all --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%cr)%C(reset) %C(white)%s%C(reset) %C(bold white)- %cn%C(reset)%C(bold yellow)%d%C(reset)' --abbrev-commit --date=relative";
        lg2 = "log --graph --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%cr)%C(reset) %C(white)%s%C(reset) %C(bold white)- %cn%C(reset)%C(bold yellow)%d%C(reset)' --abbrev-commit --date=relative";
        lg3 = "!git --no-pager log -15 --reverse --oneline --decorate --all --pretty=tformat:'%C(bold blue)%h%C(reset) %C(bold green)(%cr)%C(reset) %C(white)%s%C(reset) %C(bold yellow)- %cn%C(reset)'";
        m = "merge";
        me = "config user.name";
        mail = "config user.email";
        open = "!f() { url=$(git remote get-url \${1:-origin} | sed 's/git@\\([^:]*\\):/https:\\/\\/\\1\\//' | sed 's/\\.git$//'); xdg-open \"$url\"; }; f";
        original = "!git fetch origin && git reset --hard origin/master";
        panic = "!tar cvf ../git_panic.tar *";
        publish = "push -u origin HEAD";
        repo = "!git remote -v | grep fetch | awk '{print $2}'";
        root = "rev-parse --show-toplevel";
        rev = "log --format='%H' -n 1";
        s = "status -sb .";
        se = "!git rev-list --all | xargs git grep -F";
        tags = "tag -l";
        undo = "reset --soft HEAD";
        yoda = "push --force-with-lease";
      };

      core = {
        whitespace = "fix,-indent-with-non-tab,trailing-space,cr-at-eol";
        quotepath = false;
        autocrlf = "input";
        fileMode = false;
        editor = userConfig.editor;
        ignorecase = false;
        precomposeUnicode = true;
        preloadIndex = true;
      };

      init.defaultBranch = "main";

      status.submoduleSummary = true;

      push = {
        default = "current";
        followTags = true;
      };

      pull.rebase = true;
      fetch.prune = true;
      fetch.recurseSubmodules = "on-demand";

      submodule.recurse = true;

      rebase = {
        autosquash = true;
        autoStash = true;
      };

      help.autocorrect = 20;

      gc.auto = 256;
      clean.requireForce = false;
      tag.sort = "version:refname";

      color = {
        ui = "always";
        pager = true;
      };

      "color \"branch\"" = {
        current = "yellow bold";
        local = "green bold";
        remote = "cyan bold";
      };

      "color \"diff\"" = {
        meta = "yellow bold";
        frag = "magenta bold";
        old = "red bold";
        new = "green bold";
        whitespace = "red reverse";
      };

      "color \"status\"" = {
        added = "green bold";
        changed = "yellow bold";
        untracked = "red bold";
      };

      diff = {
        prompt = false;
        mnemonicprefix = true;
        indentHeuristic = true;
      };

      mergetool = {
        keepBackup = false;
        keepTemporaries = false;
        writeToTemp = true;
        prompt = false;
      };

      gpg.program = "/usr/bin/gpg2";
      log.decorate = "auto";
      status.relativePaths = true;
    };

    ignores = [
      ".DS_Store"
      "*.swp"
      "*.swo"
      "*~"
      ".direnv/"
      ".envrc"
      "result"
      "result-*"
      ".idea/"
      ".vscode/"
      "*.log"
      ".env.local"
    ];
  };

}
