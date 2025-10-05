(
    (projection ((describe "Changed files relative to reference") (repo .) (show (diff_files (merge_base HEAD main) HEAD))))
)
