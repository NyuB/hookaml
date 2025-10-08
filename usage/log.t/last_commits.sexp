(
    (projection ((describe "Last 3 commits") (repo .) (show (commit_range HEAD~3 HEAD))))
    (projection ((describe "Commits since reference") (repo .)
        (show (format (:hash (s |) :message (s |) :author)
            (commit_range (merge_base main HEAD) HEAD)))))
)
