Some benchmarks of different List implementations in Pascal.

# Usage with Perl 5.10+

```
./run.pl [TYPE]
```

where `TYPE` is one of:
- `arrays`, using native arrays
- `fgl`, using TFPGList and TFPGObjectList from FGL
- `fglc`, modified fgl by paweld
- `generics`, using TList and TObjectList from Generics.Collections

# Manual usage

```
time fpc -dfgl bench.pas
cat input.txt | time ./bench
```

