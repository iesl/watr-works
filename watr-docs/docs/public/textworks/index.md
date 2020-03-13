---
layout: docs
title: TextWorks
---

```tut:invisible
import org.watrworks
import watr.apps._
```

### Quick Start

Download the latest release:

```plain
curl -o textworks.tar.gz -L https://github.com/iesl/watr-works/releases/download/v0.8/textworks-0.8.tar.gz
tar xzvf textworks.tar.gz
```

and run

```plain
path/to/textworks/bin/textworks --help
```

for option syntax.

### Examples

```plain
# Single file
textworks --input input.pdf --output-file output.json

# Multiple files specified in list, output will be input file name + ext
textworks --input-list ./files-to-process.txt --output-ext ".wtxt.json"
```



### [Corpus Management](/watr-works/docs/textworks/corpus-management.html)

```plain
# Run on all files in corpus (see Corpus Management for explanation) 
textworks --corpus --output-file "extracted-text.json"
```

### [Output Formats](/watr-works/docs/textworks/output-formats.html)


### Text layout and analysis options

```plain
# Single file
textworks --input input.pdf --output-file output.json --options dehyphenate
```

