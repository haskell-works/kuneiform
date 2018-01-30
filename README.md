# kuneiform

## Installation

### OS X
```
$ brew tap haskell-works/homebrew-haskell-works git@github.com:haskell-works/homebrew-haskell-works.git
$ brew install haskell-works/homebrew-haskell-works/kuneiform
```

### Build from source
```
$ git clone https://github.com/haskell-works/kuneiform
$ cd kuneiform
$ stack install
```

## Examples

### Delete all versioned objects and delete markers from a bucket

```
$ kuneiform-exe s3 remove-all --bucket jky-delimiter --versions
```
