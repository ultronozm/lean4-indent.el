#!/usr/bin/env nu

# Validate tree-sitter corpus tests against the Lean compiler and check for duplicates.
# Usage: nu scripts/validate-corpus.nu [--fix]
#
# For each test case in test/corpus/*.txt:
#   1. Extracts the Lean source code
#   2. Passes it through `lean --stdin` to check validity
#   3. Detects duplicate test code across all files
#
# With --fix: removes duplicate test cases (keeps first occurrence).

# Run lean on code and capture result without propagating the error
def run-lean [code: string]: nothing -> record {
    $code | ^lean --stdin o+e>| complete
}

# Extract individual test cases from a corpus file.
# Returns a table of {file, name, code} records.
def parse-corpus-file [file: string]: nothing -> table {
    let lines = (open --raw $file | lines)
    let n = ($lines | length)
    mut tests = []
    mut i = 0

    while $i < $n {
        let line = ($lines | get $i)
        if ($line =~ '^=+$') and ($i + 2) < $n {
            let name = ($lines | get ($i + 1))
            let closing = ($lines | get ($i + 2))
            if ($closing =~ '^=+$') {
                # Found a test header; extract code until ---
                mut j = ($i + 3)
                # skip blank line after header
                if $j < $n and ($lines | get $j) == "" {
                    $j = $j + 1
                }
                let code_start = $j
                while $j < $n and ($lines | get $j) != "---" {
                    $j = $j + 1
                }
                # code is lines [code_start, j) minus trailing blanks
                let code = ($lines | slice $code_start..<$j | str join "\n" | str trim)

                $tests = ($tests | append {
                    file: ($file | path basename)
                    name: $name
                    code: $code
                })

                # skip past AST to next header
                $i = $j + 1
                while $i < $n and not (($lines | get $i) =~ '^=+$') {
                    $i = $i + 1
                }
            } else {
                $i = $i + 1
            }
        } else {
            $i = $i + 1
        }
    }
    $tests
}

def main [--fix] {
    let corpus_dir = "test/corpus"
    let files = (glob $"($corpus_dir)/*.txt" | sort)

    let all_tests = ($files | each { |f| parse-corpus-file $f } | flatten)
    let total = ($all_tests | length)
    print $"Found ($total) test cases across ($files | length) files\n"

    # --- Duplicate detection ---
    print "=== Duplicate detection ==="
    let grouped = ($all_tests | group-by code)
    let dupes = ($grouped | transpose key value | where { |r| ($r.value | length) > 1 })

    if ($dupes | length) > 0 {
        print $"Found ($dupes | length) set\(s\) of duplicates:\n"
        for dup in $dupes {
            let locs = ($dup.value | each { |t| $"    ($t.file) :: ($t.name)" } | str join "\n")
            print $locs
            print ""
        }
        if $fix {
            print "Removing duplicates (keeping first occurrence)...\n"
            remove-duplicates $all_tests $dupes $corpus_dir
        }
    } else {
        print "  No duplicates found."
    }
    print ""

    # --- Lean validation ---
    print "=== Lean validation ==="
    let results = ($all_tests | each { |test|
        let r = (run-lean $test.code)
        let errs = ($r.stdout | lines | where { |l| $l =~ 'error' })
        {
            file: $test.file
            name: $test.name
            valid: (($errs | length) == 0)
            errors: ($errs | str join "\n")
        }
    })

    let valid = ($results | where valid)
    let invalid = ($results | where { |r| not $r.valid })

    print $"  Valid:   ($valid | length)"
    print $"  Invalid: ($invalid | length)\n"

    if ($invalid | length) > 0 {
        print "Invalid test cases:"
        for t in $invalid {
            print $"  ($t.file) :: ($t.name)"
            for line in ($t.errors | lines) {
                print $"    ($line)"
            }
        }
    }
}

# Remove duplicate test cases from corpus files (keeps first occurrence).
def remove-duplicates [all_tests: table, dupes: table, corpus_dir: string] {
    # Collect (file, name) pairs to remove — skip first occurrence of each group
    let to_remove = ($dupes
        | each { |d| $d.value | skip 1 }
        | flatten)

    let by_file = ($to_remove | group-by file)

    for entry in ($by_file | transpose file tests) {
        let names = ($entry.tests | get name)
        print $"  Removing from ($entry.file): ($names | str join ', ')"
        let path = ($corpus_dir | path join $entry.file)
        remove-tests-from-file $path $names
    }
}

# Remove named tests from a corpus file by rewriting it.
def remove-tests-from-file [file: string, names: list<string>] {
    let lines = (open --raw $file | lines)
    let n = ($lines | length)
    mut keep = []
    mut i = 0

    while $i < $n {
        let line = ($lines | get $i)
        if ($line =~ '^=+$') and ($i + 1) < $n {
            let name = ($lines | get ($i + 1))
            if ($name in $names) {
                # Skip entire test block: header (3 lines) + code + --- + ast
                $i = $i + 3
                while $i < $n and ($lines | get $i) != "---" { $i = $i + 1 }
                $i = $i + 1  # skip ---
                # Skip AST until next header or EOF
                while $i < $n and not (($lines | get $i) =~ '^=+$') { $i = $i + 1 }
                # Also skip a preceding blank line if present
                if ($keep | length) > 0 and ($keep | last) == "" {
                    $keep = ($keep | drop 1)
                }
                continue
            }
        }
        $keep = ($keep | append $line)
        $i = $i + 1
    }

    $keep | str join "\n" | save --force $file
}
