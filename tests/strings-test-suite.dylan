Module:    strings-test-suite
Synopsis:  Test suite for strings library
Author:    Carl Gay


define constant fmt = format-to-string;
define constant <string-or-char> = type-union(<string>, <character>);



define library-spec strings ()
  module strings;
end library-spec strings;


define module-spec strings ()
  sealed generic-function alphabetic? (<string-or-char>) => (<boolean>);
  sealed generic-function alphanumeric? (<string-or-char>) => (<boolean>);
  sealed generic-function control? (<string-or-char>) => (<boolean>);
  sealed generic-function graphic? (<string-or-char>) => (<boolean>);
  sealed generic-function printable? (<string-or-char>) => (<boolean>);

  sealed generic-function lowercase? (<string-or-char>) => (<boolean>);
  sealed generic-function uppercase? (<string-or-char>) => (<boolean>);
  sealed generic-function whitespace? (<string-or-char>) => (<boolean>);

  sealed generic-function decimal-digit? (<string-or-char>) => (<boolean>);
  sealed generic-function hexadecimal-digit? (<string-or-char>) => (<boolean>);
  sealed generic-function octal-digit? (<string-or-char>) => (<boolean>);

  function char-compare (<character>, <character>) => (one-of(-1, 0, 1));
  function char-compare-ic (<character>, <character>) => (one-of(-1, 0, 1));
  function char-equal-ic? (<character>, <character>) => (<boolean>);

  sealed generic-function string-compare
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2", #"test")
   => (one-of(-1, 0, 1));
  sealed generic-function string-equal?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic-function string-greater?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic-function string-less?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic-function string-equal-ic?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic-function string-greater-ic?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic-function string-less-ic?
      (<string>, <string>, #"key", #"start1", #"end1", #"start2", #"end2")
   => (<boolean>);
  sealed generic-function starts-with?
      (<string>, <string>, #"key", #"test")
   => (<boolean>);
  sealed generic-function ends-with?
      (<string>, <string>, #"key", #"test")
   => (<boolean>);

  sealed generic-function lowercase  (<string-or-char>, #"key") => (<string-or-char>);
  sealed generic-function lowercase! (<string-or-char>, #"key") => (<string-or-char>);
  sealed generic-function uppercase  (<string-or-char>, #"key") => (<string-or-char>);
  sealed generic-function uppercase! (<string-or-char>, #"key") => (<string-or-char>);

  sealed generic-function strip
      (<string>, #"key", #"test", #"start", #"end")
   => (<string>);
  sealed generic-function strip-left
      (<string>, #"key", #"test", #"start", #"end")
   => (<string>);
  sealed generic-function strip-right
      (<string>, #"key", #"test", #"start", #"end")
   => (<string>);

  sealed generic-function pad       (<string>, <integer>, #"key", #"fill") => (<string>);
  sealed generic-function pad-left  (<string>, <integer>, #"key", #"fill") => (<string>);
  sealed generic-function pad-right (<string>, <integer>, #"key", #"fill") => (<string>);

  sealed generic-function find-substring
      (<string>, <string>, #"key", #"start", #"end", #"ignore-case?")
   => (false-or(<integer>));
  sealed generic-function replace-substrings
      (<string>, <string>, <string>, #"key", #"start", #"end", #"count", #"ignore-case?")
   => (<string>);
  sealed generic-function count-substrings
      (<string>, <string>, #"key", #"start", #"end", #"ignore-case?")
   => (<integer>);

  function split-lines (<string>, #"key", #"remove-if-empty?") => (<sequence>);
end module-spec strings;


////
//// Character comparisons
////

define strings function-test char-compare ()
  for (item in #(#('a', 'a', 0),
                 #('A', 'A', 0),
                 #('a', 'b', -1),
                 #('b', 'a', 1),
                 #('a', 'B', 1),
                 #('B', 'a', -1)))
    let (c1, c2, expected) = apply(values, item);
    check-equal(fmt("char-compare(%c, %c) = %d", c1, c2, expected),
                char-compare(c1, c2),
                expected);
  end;
end;

define strings function-test char-compare-ic ()
  for (item in #(#('a', 'a', 0),
                 #('A', 'A', 0),
                 #('a', 'b', -1),
                 #('b', 'a', 1),
                 #('a', 'B', -1),
                 #('B', 'a', 1)))
    let (c1, c2, expected) = apply(values, item);
    check-equal(fmt("char-compare-ic(%c, %c) = %d", c1, c2, expected),
                char-compare-ic(c1, c2),
                expected);
  end;
end;

define strings function-test char-equal-ic? ()
  check-true("a", char-equal-ic?('a', 'a'));
  check-true("b", char-equal-ic?('a', 'A'));
end;



////
//// Strings comparisons
////

define strings function-test string-compare ()
  for (item in #(#(0, "", ""),
                 #(0, "abc", "abc"),
                 #(0, "xabcx", "yabcy", start1:, 1, end1:, 4, start2:, 1, end2:, 4),
                 #(-1, "the", "them"),
                 #(1, "beer", "bee")))
    let (expected, #rest args) = apply(values, item);
    check-equal(fmt("%s", item), expected, apply(string-compare, args));
  end;
end function-test string-compare;

define strings function-test string-equal? ()
  local method eq (s1, s2, #rest args)
          check-true(fmt("string-equal?(%=, %=, ,@%=)", s1, s2, args),
                     apply(string-equal?, s1, s2, args))
        end;
  eq("", "");
  eq("abc", "abc");
  eq("xaaax", "yaaay", start1: 1, end1: 4, start2: 1, end2: 4);
  eq("a", "", end1: 0);
  eq("a", "", start1: 1);
  eq("", "a", end2: 0);
  eq("", "a", start2: 1);
  eq("abcd", "ab", end1: 2);
  eq("abcd", "ab", end1: 1, end2: 1);
  eq("abcd", "bc", start1: 1, end1: 3);
  eq("abcd", "cd", start1: 2);
  eq("ab", "abcd", end2: 2);
  eq("ab", "abcd", end1: 1, end2: 1);
  eq("bc", "abcd", start2: 1, end2: 3);
  eq("cd", "abcd", start2: 2);
  check-false("a", string-equal?("a", "b"));
  check-false("a", string-equal?("a", "b", start1: 1));
end function-test string-equal?;

define strings function-test string-equal-ic? ()
  // This is basically a copy of the test for string-equal? with some
  // arguments changed to uppercase.
  local method eq (s1, s2, #rest args)
          check-true(fmt("string-equal-ic?(%=, %=, ,@%=)", s1, s2, args),
                     apply(string-equal-ic?, s1, s2, args))
        end;
  eq("", "");
  eq("ABC", "abc");
  eq("XAAAX", "yaaay", start1: 1, end1: 4, start2: 1, end2: 4);
  eq("A", "", end1: 0);
  eq("A", "", start1: 1);
  eq("", "a", end2: 0);
  eq("", "a", start2: 1);
  eq("ABCD", "ab", end1: 2);
  eq("abcd", "AB", end1: 1, end2: 1);
  eq("ABCD", "bc", start1: 1, end1: 3);
  eq("abcd", "CD", start1: 2);
  eq("AB", "abcd", end2: 2);
  eq("ab", "ABCD", end1: 1, end2: 1);
  eq("BC", "abcd", start2: 1, end2: 3);
  eq("cd", "ABCD", start2: 2);
  check-false("a", string-equal-ic?("A", "b"));
  check-false("b", string-equal-ic?("A", "b", start1: 1));
end function-test string-equal-ic?;


define strings function-test string-less? ()
  local method less (s1, s2, #rest args)
          check-true(fmt("string-less?(%=, %=, ,@%=)", s1, s2, args),
                     apply(string-less?, s1, s2, args))
        end;
  less("", "a");
  less("a", "ab");
  less("a", "b");
  less("B", "a");
  less("aBc", "abc");
  less("aaa", "b");
  less("aaa", "bbbb");
end function-test string-less?;

define strings function-test string-less-ic? ()
  check-true("a", string-less-ic?("a", "B"));
  check-true("b", string-less-ic?("A", "B"));
  check-true("c", string-less-ic?("A", "b"));
end function-test string-less-ic?;


define strings function-test string-greater? ()
  local method gr8r (s1, s2, #rest args)
          check-true(fmt("string-greater?(%=, %=, ,@%=)", s1, s2, args),
                     apply(string-greater?, s1, s2, args))
        end;
  gr8r("a", "");
  gr8r("ab", "a");
  gr8r("b", "a");
  gr8r("a", "B");
  gr8r("abc", "aBc");
  gr8r("b", "aaa");
  gr8r("bbbb", "aaa");
end function-test string-greater?;

define strings function-test string-greater-ic? ()
  check-true("a", string-greater-ic?("B", "a"));
  check-true("b", string-greater-ic?("B", "A"));
  check-true("c", string-greater-ic?("b", "A"));
end function-test string-greater-ic?;


define strings function-test ends-with? ()
  local method ew (s1, s2, #rest args)
          check-true(fmt("ends-with?(%=, %=, ,@%=)", s1, s2, args),
                     apply(ends-with?, s1, s2, args))
        end;
  ew("abc", "");
  ew("abc", "abc");
  ew("abc", "bc");
  ew("abc", "ABC", test: char-compare-ic);
  ew("abc", "bC", test: char-compare-ic);
end function-test ends-with?;

define strings function-test starts-with? ()
  local method sw (s1, s2, #rest args)
          check-true(fmt("starts-with?(%=, %=, ,@%=)", s1, s2, args),
                     apply(starts-with?, s1, s2, args))
        end;
  sw("abc", "");
  sw("abc", "abc");
  sw("abc", "ab");
  sw("abc", "ABC", test: char-compare-ic);
  sw("abc", "Ab", test: char-compare-ic);
end function-test starts-with?;


////
//// Character classes
////

define strings function-test graphic? ()
  for (code from 0 to 255)
    let char = as(<character>, code);
    if (char >= ' ' & char <= '~')
      check-true(fmt("graphic?(%c)", char), graphic?(char));
    else
      check-false(fmt("~graphic?(%c)", char), graphic?(char));
    end;
  end;
end function-test graphic?;

define strings function-test alphanumeric? ()
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  for (code from 0 to 255)
    let char = as(<character>, code);
    if (member?(char, chars))
      check-true(fmt("alphanumeric?(%=)", char), alphanumeric?(char));
    else
      check-false(fmt("~alphanumeric?(%=)", char), alphanumeric?(char));
    end;
  end;
end function-test alphanumeric?;

define strings function-test alphabetic? ()
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  for (code from 0 to 255)
    let char = as(<character>, code);
    if (member?(char, chars))
      check-true(fmt("alphabetic??(%=)", char), alphabetic?(char));
    else
      check-false(fmt("~alphabetic?(%=)", char), alphabetic?(char));
    end;
  end;
end function-test alphabetic?;

define strings function-test control? ()
  // control = ~(white | printable)
  for (code from 0 to 31)
    let char = as(<character>, code);
    if (whitespace?(char))
      check-false(fmt("~control?(%=)", char), control?(char));
    else
      check-true(fmt("control?(%=)", char), control?(char));
    end;
  end;
  for (code from 127 to 255)
    let char = as(<character>, code);
    check-true(fmt("control?(%=)", char), control?(char));
  end;
end function-test control?;

define strings function-test printable? ()
  // printable = (whitespace | graphic)
  for (code from 0 to 31)
    let char = as(<character>, code);
    if (whitespace?(char))
      check-true(fmt("printable?(%=)", char), printable?(char));
    else
      check-false(fmt("~printable?(%=)", char), printable?(char));
    end;
  end;
  for (code from 127 to 255)
    let char = as(<character>, code);
    check-false(fmt("~printable?(%=)", char), printable?(char));
  end;
end function-test printable?;


////
//// Substrings
////

define strings function-test find-substring ()
  let data = #(#(0, "", ""),
               #(0, "x", ""),
               #(0, "x", "x"),
               #(1, "abcde", "bc"),
               #(#f, "abcde", "bc", start:, 2),
               #(#f, "abcde", "bc", end:, 2),
               #(#f, "abcde", "BC"),
               #(1,  "abcde", "BC", ignore-case?:, #t));
  for (item in data)
    let (expected, #rest args) = apply(values, item);
    check-equal(fmt("%s", args), apply(find-substring, args), expected);
  end;
end function-test find-substring;

define strings function-test replace-substrings ()
  check-equal("simple", replace-substrings("zig", "i", "a"), "zag");
  check-equal("count = #f", replace-substrings("zig zig", "i", "a"), "zag zag");
  check-equal("case sensitive", replace-substrings("zIg", "i", "a"), "zIg");
  check-equal("ignore case", replace-substrings("zIg", "i", "a", ignore-case?: #t), "zag");
  check-equal("count = 1", replace-substrings("zig zig", "ig", "ipped", count: 1), "zipped zig");
  check-equal("start", replace-substrings("zig zig", "zi", "pi", start: 1), "zig pig");
  check-equal("end", replace-substrings("zig zig", "zi", "pi", end: 5), "pig zig");
end function-test replace-substrings;

define strings function-test count-substrings ()
  let data = #(#(1, "", ""),
               #(2, "x", ""),
               #(1, "x", "x"),
               #(2, "xxxxx", "xx"), // check non-overlap
               #(1, "abcabc", "abc", end:, 5),  // check non-overlap
               #(1, "xxxxx", "xx", start:, 1, end:, 4),
               #(0, "xxx", "X"),
               #(3, "xxx", "X", ignore-case?:, #t));
  for (item in data)
    let (expected, #rest args) = apply(values, item);
    check-equal(fmt("%s", args), apply(count-substrings, args), expected);
  end;
end function-test count-substrings;


////
//// Digits
////

define strings function-test octal-digit? ()
  for (char in "01234567")
    check-true(fmt("%s", char), octal-digit?(char));
  end;
  check-false("8", octal-digit?('8'));
  check-false("a", octal-digit?('a'));
end function-test octal-digit?;

define strings function-test decimal-digit? ()
  for (char in "0123456789")
    check-true(fmt("%s", char), decimal-digit?(char));
  end;
  check-false("a", decimal-digit?('a'));
  check-false("A", decimal-digit?('A'));
end function-test decimal-digit?;

define strings function-test hexadecimal-digit? ()
  for (char in "0123456789abcdfeABCDEF")
    check-true(fmt("%s", char), hexadecimal-digit?(char));
  end;
  check-false("g", hexadecimal-digit?('g'));
  check-false("G", hexadecimal-digit?('G'));
end function-test hexadecimal-digit?;


////
//// Padding
////

define strings function-test pad-left ()
  check-equal("a", pad-left("x", 3), "  x");
  check-equal("b", pad-left("xx", 1), "xx");
  check-equal("c", pad-left("x", 2, fill: '-'), "-x");
end function-test pad-left;

define strings function-test pad ()
  check-equal("a", pad("x", 3), " x ");
  check-equal("b", pad("xx", 1), "xx");
  check-equal("c", pad("x", 3, fill: '-'), "-x-");
end function-test pad;

define strings function-test pad-right ()
  check-equal("a", pad-right("x", 3), "x  ");
  check-equal("b", pad-right("xx", 1), "xx");
  check-equal("c", pad-right("x", 2, fill: '-'), "x-");
end function-test pad-right;


////
//// Character case
////

define strings function-test lowercase! ()
  for (item in #(#("", ""),
                 #("a", "a"),
                 #("E", "e"),
                 #("ABC", "abc"),
                 #("ONE TWO", "one two"),
                 #("_oNe,Two", "_one,two")))
    let (before, expected) = apply(values, map(copy-sequence, item));
    check-equal(fmt("lowercase! %=", before), expected, lowercase!(before));
    check-true(fmt("lowercase! %= retains identity", before),
               lowercase!(before) == before);
  end;
end function-test lowercase!;

define strings function-test lowercase ()
  for (item in #(#("", ""),
                 #("a", "a"),
                 #("A", "a"),
                 #("ABC", "abc"),
                 #("ONE TWO", "one two"),
                 #("_oNe,Two", "_one,two")))
    let (before, expected) = apply(values, item);
    check-equal(fmt("lowercase %=", before), expected, lowercase(before));
  end;
end function-test lowercase;

define strings function-test uppercase! ()
  for (item in #(#("", ""),
                 #("A", "A"),
                 #("a", "A"),
                 #("abc", "ABC"),
                 #("one two", "ONE TWO"),
                 #("_oNe,Two", "_ONE,TWO")))
    let (before, expected) = apply(values, map(copy-sequence, item));
    check-equal(fmt("uppercase! %=", before), expected, uppercase!(before));
    check-true(fmt("uppercase! %= retains identity", before),
               uppercase!(before) == before);
  end;
end function-test uppercase!;

define strings function-test uppercase ()
  for (item in #(#("", ""),
                 #("a", "A"),
                 #("A", "A"),
                 #("abc", "ABC"),
                 #("one two", "ONE TWO"),
                 #("_oNe,Two", "_ONE,TWO")))
    let (before, expected) = apply(values, item);
    check-equal(fmt("uppercase %=", before), expected, uppercase(before));
  end;
end function-test uppercase;

define strings function-test lowercase? ()
  check-true("a", lowercase?('a'));
  check-true("b", lowercase?("abc-$#^^10"));
  check-false("c", lowercase?('A'));
  check-false("d", lowercase?("aBc-$#^^10"));
end function-test lowercase?;

define strings function-test uppercase? ()
  check-true("a", uppercase?('X'));
  check-true("b", uppercase?("ABC-$#^^10"));
  check-false("c", uppercase?('b'));
  check-false("d", uppercase?("aBc-$#^^10"));
end function-test uppercase?;


////
//// Stripping
////

define strings function-test strip ()
  for (item in list(#("", ""),
                    #("a", "a"),
                    #("a", " a "),
                    list(" a ", " a ", test:, method (c) #f end),
                    list("o", "xox", test:, method (c) c == 'x' end)))
    let (expected, before, #rest strip-args) = apply(values, item);
    check-equal(fmt("strip %=", before),
                expected,
                apply(strip, before, strip-args));
  end for;
end function-test strip;

define strings function-test strip-left ()
  for (item in list(#("", ""),
                    #("a", "a"),
                    #("a", " a"),
                    list(" a ", " a ", test:, method (c) #f end),
                    list("ox", "xox", test:, method (c) c == 'x' end)))
    let (expected, before, #rest strip-args) = apply(values, item);
    check-equal(fmt("strip-left %=", before),
                expected,
                apply(strip-left, before, strip-args));
  end for;
end function-test strip-left;

define strings function-test strip-right ()
  for (item in list(#("", ""),
                    #("a", "a"),
                    #("a", "a "),
                    list(" a ", " a ", test:, method (c) #f end),
                    list("xo", "xox", test:, method (c) c == 'x' end)))
    let (expected, before, #rest strip-args) = apply(values, item);
    check-equal(fmt("strip-right %=", before),
                expected,
                apply(strip-right, before, strip-args));
  end for;
end function-test strip-right;


////
//// Miscellaneous
////

define strings function-test whitespace? ()
  for (char in " \t\n\r\f")
    check-true(fmt("%=", char), whitespace?(char));
  end;
  check-true("a", whitespace?(" \t \n \r \f "));
  check-false("b", whitespace?('a'));
  check-false("c", whitespace?("  b  "));
end function-test whitespace?;

define strings function-test split-lines ()
  check-equal("a", split-lines("a\nb\nc"), #["a", "b", "c"]);
  check-equal("b", split-lines("a\r\nb"), #["a", "b"]);
  check-equal("c", split-lines("a\rb"), #["a", "b"]);
  check-equal("d", split-lines(""), #[""]);
  check-equal("e", split-lines("a\n"), #["a"]);
  check-equal("f", split-lines("a"), #["a"]);
  check-equal("g", split-lines("a\r\rb"), #["a", "", "b"]);
  check-equal("h", split-lines("a\r\rb", remove-if-empty?: #t), #["a", "b"]);
  check-equal("i", split-lines("a\n\rb"), #["a", "", "b"]);
  check-equal("j", split-lines("\na"), #["", "a"]);
end function-test split-lines;


define method main () => ()
  let filename = locator-name(as(<file-locator>, application-name()));
  if (split(filename, ".")[0] = "strings-test-suite")
    run-test-application(strings-test-suite);
  end;
end method main;

begin
  main()
end;

