# Aliases
  $ add_and_commit() { local msg=$1; shift; git add $@; git commit -m "$msg" > /dev/null; }
  $ show_workspace() { gitst $1 workspace.out; cat workspace.out; }

# Single repo examples
  $ git init -b main
  Initialized empty Git repository in $TESTCASE_ROOT/.git/
  $ git config user.name "Legolas"
  $ git config user.email "legolas@mid.earth"
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
  $ git init -b main A
  Initialized empty Git repository in $TESTCASE_ROOT/A/.git/
  $ git -C A config user.name "Aragorn"
  $ git -C A config user.email "aragorn@mid.earth"
  $ touch A/a.txt
  $ cd A
  $ echo "workspace.*" > .gitignore
  $ add_and_commit "Root" .gitignore
  $ git checkout -b my_branch
  Switched to a new branch 'my_branch'
  $ add_and_commit "A" a.txt
  $ cd ..

  $ mkdir B
  $ git init -b main B
  Initialized empty Git repository in $TESTCASE_ROOT/B/.git/
  $ git -C B config user.name "Gimli"
  $ git -C B config user.email "gimli@mid.earth"
  $ touch B/b.txt
  $ cd B
  $ echo "workspace.*" > .gitignore
  $ add_and_commit "Root" .gitignore
  $ git checkout -b my_branch
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
