# Aliases
  $ add_and_commit() { local msg=$1; shift; git add $@; git commit -m "$msg" > /dev/null; }
  $ show_workspace() { gitst $1 workspace.out; cat workspace.out; }

# Single repo examples
  $ git init
  Initialized empty Git repository in $TESTCASE_ROOT/.git/
  $ cat workspace.sexp
  (
      (projection ((describe "Changed files relative to reference") (repo .) (show (diff_files (merge_base HEAD main) HEAD))))
  )
  $ touch a.txt
  $ add_and_commit "Root commit" a.txt
# No difference
  $ show_workspace workspace.sexp
  ((Projection
    ((repo ./.) (describe "Changed files relative to reference")
     (show (Diff_files (Merge_base HEAD main) HEAD)))))
  
  Changed files relative to reference:
  ()
  $ git checkout -b my_branch
  Switched to a new branch 'my_branch'
# Change worktree but no commit
  $ touch b.txt
  $ show_workspace workspace.sexp
  ((Projection
    ((repo ./.) (describe "Changed files relative to reference")
     (show (Diff_files (Merge_base HEAD main) HEAD)))))
  
  Changed files relative to reference:
  ()
# Commit changes
  $ add_and_commit "Add b.txt" b.txt
  $ show_workspace workspace.sexp
  ((Projection
    ((repo ./.) (describe "Changed files relative to reference")
     (show (Diff_files (Merge_base HEAD main) HEAD)))))
  
  Changed files relative to reference:
  (b.txt)
# Cleanup
  $ rm *.txt
  $ rm -rf .git
  $ rm workspace.out
  $ ls -a
  .
  ..
  workspace.sexp


# Multi repo test
  $ mkdir A
  $ git init A
  Initialized empty Git repository in $TESTCASE_ROOT/A/.git/
  $ touch A/root.txt
  $ touch A/a.txt
  $ cd A
  $ add_and_commit "Root" root.txt
  $ git mkb my_branch
  Switched to a new branch 'my_branch'
  $ add_and_commit "A" a.txt
  $ cd ..

  $ mkdir B
  $ git init B
  Initialized empty Git repository in $TESTCASE_ROOT/B/.git/
  $ touch B/root.txt
  $ touch B/b.txt
  $ cd B
  $ add_and_commit "Root" root.txt
  $ git mkb my_branch
  Switched to a new branch 'my_branch'
  $ add_and_commit "B" b.txt
  $ cd ..

  $ cp workspace.sexp A/
  $ cp workspace.sexp B/
  $ show_workspace A/workspace.sexp
  ((Projection
    ((repo A/.) (describe "Changed files relative to reference")
     (show (Diff_files (Merge_base HEAD main) HEAD)))))
  
  Changed files relative to reference:
  (a.txt)
  $ show_workspace B/workspace.sexp
  ((Projection
    ((repo B/.) (describe "Changed files relative to reference")
     (show (Diff_files (Merge_base HEAD main) HEAD)))))
  
  Changed files relative to reference:
  (b.txt)

