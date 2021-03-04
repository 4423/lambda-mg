## Usage
```
$ omake
$ ./src/lambda-mg [<option>] <file0> <file1> ...
```

## Benchmarks
### fix_functor
```
$ cd benchmarks/fix_functor/
$ make codegen
$ ./bench.out <number of functor applications>
```

### set_functor
```
$ cd benchmarks/set_functor/
$ make codegen
$ ./bench.out <length of a set>
```