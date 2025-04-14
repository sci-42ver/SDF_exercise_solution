Recently I tried to implement one tokenizer following [this Python doc example][1]. It uses iterator for efficiency IMHO, [see QA1][2]. This efficiency matters for big files but not for small expression string in the book exercise to be done which I am currently working with.

But Both the space efficiency and the time efficiency comes from space re-usage. See [the mailing list][3] in QA1.
> Iterators have a tiny constant size while lists take space proportional to the length of the list. The part that is not obvious is that looping over the iterator re-uses the same memory location again an again.

> It takes time to allocate Python objects, but the smart allocator *saves some of that time* when it can *reuse* recently discarded objects.

So `stream` in Scheme can't directly do that since after all possible elements (if finite stream) are forced, all these are  located in the memory/registers etc because maybe they may be used in the latter part of the program. For Python, iterator is defined to be *different* from list so that iterated elements can't be used later *directly* except that we bookkeep them explicitly maybe with extra information attached.

Is there one way to achieve that efficiency due to space re-usage?

  [1]: https://docs.python.org/3/library/re.html#writing-a-tokenizer
  [2]: https://stackoverflow.com/a/631619/21294350
  [3]: https://web.archive.org/web/20220324173409/https://markmail.org/message/t2a6tp33n5lddzvy
  [4]: https://srfi.schemers.org/srfi-115/srfi-115.html#proc-regexp-fold
  [5]: https://docs.racket-lang.org/reference/regexp.html#%28part._.Regexp_.Matching%29
  [6]: https://srfi.schemers.org/srfi-115/srfi-115.html#proc-regexp-partition

---

See p.s. in https://stackoverflow.com/q/79570000/21294350
