(
    (projection ((describe "Changed files in last commit") (repo .)
        (show (select :file (diff_files HEAD~ HEAD)))))
    (projection ((describe "Current worktree") (repo .)
        (show worktree)))
    (projection ((describe "All diff VS ref") (repo .)
        (show (select :file
            (union worktree (diff_files main HEAD))))))
    (projection ((describe "Last 3 commits") (repo .)
        (show (format (:hash (s |) :message (s |) :author)
            (commit_range HEAD~3 HEAD)))))
)
