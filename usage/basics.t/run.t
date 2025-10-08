# Aliases
  $ add_and_commit() { local msg=$1; shift; git add $@; git commit -m "$msg" > /dev/null; }
  $ show_workspace() { gitst $1 workspace.out; cat workspace.out; }

# Single repo examples
  $ git init
  Initialized empty Git repository in $TESTCASE_ROOT/.git/
  $ echo "workspace.*" > .gitignore
  $ cat workspace.sexp
  (
      (projection ((describe "Worktree") (repo .) (show worktree)))
      (projection ((describe "Changed files relative to reference") (repo .) (show (union worktree (select :file (diff_files (merge_base HEAD main) HEAD))))))
  )
  $ add_and_commit "Root commit" .gitignore
# No difference
  $ show_workspace workspace.sexp
  Worktree:
  ()
  
  Changed files relative to reference:
  ()
# Change worktree but no commit
  $ touch a.txt
  $ show_workspace workspace.sexp
  Worktree:
  (((:status Untracked) (:file a.txt)))
  
  Changed files relative to reference:
  (((:status Untracked) (:file a.txt)))
  $ git checkout -b my_branch
  Switched to a new branch 'my_branch'
# Commit changes
  $ add_and_commit "Add a.txt" a.txt
  $ show_workspace workspace.sexp
  Worktree:
  ()
  
  Changed files relative to reference:
  (a.txt)
# Cleanup
  $ rm *.txt
  $ rm -rf .git
  $ rm workspace.out
  $ ls -a
  .
  ..
  .gitignore
  workspace.sexp

# Multi repo test
  $ mkdir A
  $ git init A
  Initialized empty Git repository in $TESTCASE_ROOT/A/.git/
  $ touch A/a.txt
  $ cd A
  $ echo "workspace.*" > .gitignore
  $ add_and_commit "Root" .gitignore
  $ git mkb my_branch
  Switched to a new branch 'my_branch'
  $ add_and_commit "A" a.txt
  $ cd ..

  $ mkdir B
  $ git init B
  Initialized empty Git repository in $TESTCASE_ROOT/B/.git/
  $ touch B/b.txt
  $ cd B
  $ echo "workspace.*" > .gitignore
  $ add_and_commit "Root" .gitignore
  $ git mkb my_branch
  Switched to a new branch 'my_branch'
  $ add_and_commit "B" b.txt
  $ cd ..

  $ cp workspace.sexp A/
  $ cp workspace.sexp B/
  $ show_workspace A/workspace.sexp
  Worktree:
  ()
  
  Changed files relative to reference:
  (a.txt)
  $ show_workspace B/workspace.sexp
  Worktree:
  ()
  
  Changed files relative to reference:
  (b.txt)
