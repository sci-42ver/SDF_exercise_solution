Title: How to overload one function with compatible types in C?

Recently I tried to install [siod][1] to use [Pratt-Parser for SIOD][2]. While `make`, it shows:
```bash
slibu.c:775:6: error: conflicting types for ‘lchmod’; have ‘struct obj *(struct obj *, struct obj *)’
  775 | LISP lchmod(LISP path,LISP mode)
      |      ^~~~~~
/usr/include/sys/stat.h:359:12: note: previous declaration of ‘lchmod’ with type ‘int(const char *, __mode_t)’ {aka ‘int(const char *, unsigned int)’}
  359 | extern int lchmod (const char *__file, __mode_t __mode)
      |            ^~~~~~
```

[Most of related answers][3] for "conflicting types" says we should add one declaration, but that will still throw the error
```
slibu.c:774:6: error: conflicting types for ‘lchmod’; have ‘struct obj *(struct obj *, struct obj *)’
  774 | LISP lchmod(LISP path,LISP mode);
      |      ^~~~~~
In file included from slibu.c:28:
/usr/include/sys/stat.h:359:12: note: previous declaration of ‘lchmod’ with type ‘int(const char *, __mode_t)’ {aka ‘int(const char *, unsigned int)’}
  359 | extern int lchmod (const char *__file, __mode_t __mode)
      |            ^~~~~~
```

This siod


  [1]: https://aur.archlinux.org/packages/siod#comment-872653
  [2]: https://www.cs.cmu.edu/Groups//AI/util/lang/scheme/code/parsing/pratt/pratt.scm
  [3]: https://stackoverflow.com/a/22437005/21294350

---

https://www.nongnu.org/muesli/ulsiod.html#ext solves this by
> The main difference from the original SIOD is that each function name in the C source code has the string "siod_" prepended to it, to *avoid clashes with other language-implementation libraries that we link in* to the same program.
