(
    (projection ((describe "Worktree") (repo .) (show worktree)))
    (projection ((describe "Changed files relative to reference") (repo .) (show (union worktree (select (get :file) (diff_files (merge_base HEAD main) HEAD))))))
)
