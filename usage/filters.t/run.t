# Aliases
  $ show_workspace() { gitst "$1" workspace.out; cat workspace.out; }
# Setup
  $ echo "workspace.*" > .gitignore
  $ git init -b main
  Initialized empty Git repository in $TESTCASE_ROOT/.git/
  $ git config user.name "Fitz Chivalry"
  $ git config user.email "fitz@castelcerf.com"
  $ touch workspace.out
# Modify worktree
  $ touch README.md
  $ touch CONTRIBUTE.md
  $ touch RELEASE_NOTES.txt
  $ ls | sort
  CONTRIBUTE.md
  README.md
  RELEASE_NOTES.txt
  contains.sexp
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
  $ git commit --allow-empty -m "Root commit" > /dev/null
  $ git commit --allow-empty -m "Awesome commit" > /dev/null
  $ git commit --allow-empty -m "Superbissima feature" > /dev/null
  $ git commit --allow-empty -m "fixup this above" > /dev/null
  $ show_workspace contains.sexp
  Last 3 commits:
  ("Fitz Chivalry | fixup this above")
