# Aliases
  $ show_workspace() { gitst workspace.sexp workspace.out; cat workspace.out; }
# Setup
  $ echo "workspace.*" > .gitignore
  $ git init
  Initialized empty Git repository in $TESTCASE_ROOT/.git/
  $ cat workspace.sexp
  (
      (projection ((describe "Changed '.md' files in worktree") (repo .)
          (show (filter (ends_with .md) worktree))))
      (projection ((describe "Changed files in worktree starting with RE") (repo .)
          (show (filter (starts_with RE) worktree))))
  )
  $ touch workspace.out
# Modify worktree
  $ touch README.md
  $ touch CONTRIBUTE.md
  $ touch RELEASE_NOTES.txt
  $ ls | sort
  CONTRIBUTE.md
  README.md
  RELEASE_NOTES.txt
  workspace.out
  workspace.sexp
  $ show_workspace
  Changed '.md' files in worktree:
  ((Untracked CONTRIBUTE.md) (Untracked README.md))
  
  Changed files in worktree starting with RE:
  ((Untracked README.md) (Untracked RELEASE_NOTES.txt))

