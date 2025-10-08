# Aliases
  $ show_workspace() { gitst "$1" workspace.out; cat workspace.out; }
# Setup
  $ echo "workspace.*" > .gitignore
  $ git init
  Initialized empty Git repository in $TESTCASE_ROOT/.git/
  $ touch workspace.out
# Modify worktree
  $ touch README.md
  $ touch CONTRIBUTE.md
  $ touch RELEASE_NOTES.txt
  $ ls | sort
  CONTRIBUTE.md
  README.md
  RELEASE_NOTES.txt
  ends_with.sexp
  starts_with.sexp
  workspace.out
  $ show_workspace starts_with.sexp
  Changed files in worktree starting with RE:
  (((:status Untracked) (:file README.md))
   ((:status Untracked) (:file RELEASE_NOTES.txt)))
  $ show_workspace ends_with.sexp
  Changed '.md' files in worktree:
  (((:status Untracked) (:file CONTRIBUTE.md))
   ((:status Untracked) (:file README.md)))
