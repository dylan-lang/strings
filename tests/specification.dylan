Module:    strings-test-suite
Synopsis:  Test suite for strings library
Author:    Carl Gay


define constant <string-or-char> = type-union(<string>, <character>);

define interface-specification-suite strings-specification-suite ()
  sealed generic function alphabetic? (<string-or-char>) => (<boolean>);
  sealed generic function alphanumeric? (<string-or-char>) => (<boolean>);
  sealed generic function control? (<string-or-char>) => (<boolean>);
  sealed generic function graphic? (<string-or-char>) => (<boolean>);
  sealed generic function printable? (<string-or-char>) => (<boolean>);

  sealed generic function lowercase? (<string-or-char>) => (<boolean>);
  sealed generic function uppercase? (<string-or-char>) => (<boolean>);
  sealed generic function whitespace? (<string-or-char>) => (<boolean>);

  sealed generic function decimal-digit? (<string-or-char>) => (<boolean>);
  sealed generic function hexadecimal-digit? (<string-or-char>) => (<boolean>);
  sealed generic function octal-digit? (<string-or-char>) => (<boolean>);

  function char-compare (<character>, <character>) => (one-of(-1, 0, 1));
  function char-compare-ic (<character>, <character>) => (one-of(-1, 0, 1));
  function char-equal-ic? (<character>, <character>) => (<boolean>);

  sealed generic function string-compare
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2", #"test")
   => (one-of(-1, 0, 1));
  sealed generic function string-equal?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic function string-greater?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic function string-less?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic function string-equal-ic?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic function string-greater-ic?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic function string-less-ic?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic function starts-with?
      (<string>, <string>, #"key", #"test")
   => (<boolean>);
  sealed generic function ends-with?
      (<string>, <string>, #"key", #"test")
   => (<boolean>);

  sealed generic function lowercase  (<string-or-char>, #"key") => (<string-or-char>);
  sealed generic function lowercase! (<string-or-char>, #"key") => (<string-or-char>);
  sealed generic function uppercase  (<string-or-char>, #"key") => (<string-or-char>);
  sealed generic function uppercase! (<string-or-char>, #"key") => (<string-or-char>);

  sealed generic function strip
      (<string>, #"key", #"test", #"start", #"end")
   => (<string>);
  sealed generic function strip-left
      (<string>, #"key", #"test", #"start", #"end")
   => (<string>);
  sealed generic function strip-right
      (<string>, #"key", #"test", #"start", #"end")
   => (<string>);

  sealed generic function pad       (<string>, <integer>, #"key", #"fill") => (<string>);
  sealed generic function pad-left  (<string>, <integer>, #"key", #"fill") => (<string>);
  sealed generic function pad-right (<string>, <integer>, #"key", #"fill") => (<string>);

  sealed generic function find-substring
      (<string>, <string>, #"key", #"start", #"end", #"ignore-case?")
   => (false-or(<integer>));
  sealed generic function replace-substrings
      (<string>, <string>, <string>, #"key", #"start", #"end", #"count", #"ignore-case?")
   => (<string>);
  sealed generic function count-substrings
      (<string>, <string>, #"key", #"start", #"end", #"ignore-case?")
   => (<integer>);

  function split-lines (<string>, #"key", #"remove-if-empty?") => (<sequence>);
end strings-specification-suite;

ignore(strings-specification-suite);
