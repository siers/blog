---
title: Grepping scala transitive dependencies
---
If you're looking for some string in scala dependencies (which the IDE doesn't index as far as I'm aware), and you have bloop build server available,
you can use this `jq` query:

```
# with xargs
jq -r < .bloop/*.json '(.project.resolution.modules // []) | map(.artifacts | map(select(.classifier == "sources") | .path)[])[]' | sort -u | xargs -rL1 sh -c 'zipgrep PATTERN "$1" && echo from: "$1"' sh

# with GNU parallel
jq -r < .bloop/*.json '(.project.resolution.modules // []) | map(.artifacts | map(select(.classifier == "sources") | .path)[])[]' | sort -u | parallel -P4 -N1 'sh -c "zipgrep PATTERN \$1 && echo from: \$1" sh'

# or as a function:
bloopgrep() { jq -r < .bloop/*.json '(.project.resolution.modules // []) | map(.artifacts | map(select(.classifier == "sources") | .path)[])[]' | sort -u | pattern="$1" parallel -P8 -N1 'sh -c "zipgrep \"\$pattern\" \"\$1\" && echo -e \"from: \$1\n\"" sh' 2&>1 | grep -v zip.bomb }
```

This may prove useful for finding rougue print statements or something else, possibly... Good luck!
