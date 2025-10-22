(
    (defn commits (since) 
        (projection ((describe ("Commits since " since)) (repo .) (show (format (:message) (commit_range since HEAD)))))
    )

    (apply (commits HEAD~1))
    (apply (commits HEAD~2))
)