Module:    strings-test-suite
Synopsis:  Test suite for strings library
Author:    Carl Gay


define constant fmt = format-to-string;

////
//// Character comparisons
////

define test test-char-compare ()
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
end test;

define test test-char-compare-ic ()
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
end test;

define test test-char-equal-ic? ()
  check-true("a", char-equal-ic?('a', 'a'));
  check-true("b", char-equal-ic?('a', 'A'));
end test;



////
//// Strings comparisons
////

define test test-string-compare ()
  for (item in #(#(0, "", ""),
                 #(0, "abc", "abc"),
                 #(0, "xabcx", "yabcy", start1:, 1, end1:, 4, start2:, 1, end2:, 4),
                 #(-1, "the", "them"),
                 #(1, "beer", "bee")))
    let (expected, #rest args) = apply(values, item);
    check-equal(fmt("%s", item), expected, apply(string-compare, args));
  end;
end test;

define test test-string-equal? ()
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
end test;

define test test-string-equal-ic? ()
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
end test;


define test test-string-less? ()
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
end test;

define test test-string-less-ic? ()
  check-true("a", string-less-ic?("a", "B"));
  check-true("b", string-less-ic?("A", "B"));
  check-true("c", string-less-ic?("A", "b"));
end test;


define test test-string-greater? ()
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
end test;

define test test-string-greater-ic? ()
  check-true("a", string-greater-ic?("B", "a"));
  check-true("b", string-greater-ic?("B", "A"));
  check-true("c", string-greater-ic?("b", "A"));
end test;


define test test-ends-with? ()
  local method ew (s1, s2, #rest args)
          check-true(fmt("ends-with?(%=, %=, ,@%=)", s1, s2, args),
                     apply(ends-with?, s1, s2, args))
        end;
  ew("abc", "");
  ew("abc", "abc");
  ew("abc", "bc");
  ew("abc", "ABC", test: char-compare-ic);
  ew("abc", "bC", test: char-compare-ic);
end test;

define test test-starts-with? ()
  local method sw (s1, s2, #rest args)
          check-true(fmt("starts-with?(%=, %=, ,@%=)", s1, s2, args),
                     apply(starts-with?, s1, s2, args))
        end;
  sw("abc", "");
  sw("abc", "abc");
  sw("abc", "ab");
  sw("abc", "ABC", test: char-compare-ic);
  sw("abc", "Ab", test: char-compare-ic);
end test;


////
//// Character classes
////

define test test-graphic? ()
  for (code from 0 to 255)
    let char = as(<character>, code);
    if (char >= ' ' & char <= '~')
      check-true(fmt("graphic?(%=)", code), graphic?(char));
    else
      check-false(fmt("~graphic?(%=)", code), graphic?(char));
    end;
  end;
end test;

define test test-alphanumeric? ()
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
  for (code from 0 to 255)
    let char = as(<character>, code);
    if (member?(char, chars))
      check-true(fmt("alphanumeric?(%=)", char), alphanumeric?(char));
    else
      check-false(fmt("~alphanumeric?(%=)", char), alphanumeric?(char));
    end;
  end;
end test;

define test test-alphabetic? ()
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
  for (code from 0 to 255)
    let char = as(<character>, code);
    if (member?(char, chars))
      check-true(fmt("alphabetic??(%=)", char), alphabetic?(char));
    else
      check-false(fmt("~alphabetic?(%=)", char), alphabetic?(char));
    end;
  end;
end test;

define test test-control? ()
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
end test;

define test test-printable? ()
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
end test;


////
//// Substrings
////

define test test-find-substring ()
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
end test;

define test test-replace-substrings ()
  check-equal("empty", replace-substrings("", "a", "b"), "");
  check-equal("simple", replace-substrings("zig", "i", "a"), "zag");
  check-equal("count = #f", replace-substrings("zig zig", "i", "a"), "zag zag");
  check-equal("case sensitive", replace-substrings("zIg", "i", "a"), "zIg");
  check-equal("ignore case", replace-substrings("zIg", "i", "a", ignore-case?: #t), "zag");
  check-equal("count = 1", replace-substrings("zig zig", "ig", "ipped", count: 1), "zipped zig");
  check-equal("start", replace-substrings("zig zig", "zi", "pi", start: 1), "zig pig");
  check-equal("end", replace-substrings("zig zig", "zi", "pi", end: 5), "pig zig");
end test;

define test test-count-substrings ()
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
end test;


////
//// Digits
////

define test test-octal-digit? ()
  for (char in "01234567")
    check-true(fmt("%s", char), octal-digit?(char));
  end;
  check-false("8", octal-digit?('8'));
  check-false("a", octal-digit?('a'));
end test;

define test test-decimal-digit? ()
  for (char in "0123456789")
    check-true(fmt("%s", char), decimal-digit?(char));
  end;
  check-false("a", decimal-digit?('a'));
  check-false("A", decimal-digit?('A'));
end test;

define test test-hexadecimal-digit? ()
  for (char in "0123456789abcdfeABCDEF")
    check-true(fmt("%s", char), hexadecimal-digit?(char));
  end;
  check-false("g", hexadecimal-digit?('g'));
  check-false("G", hexadecimal-digit?('G'));
end test;


////
//// Padding
////

define test test-pad-left ()
  check-equal("a", pad-left("x", 3), "  x");
  check-equal("b", pad-left("xx", 1), "xx");
  check-equal("c", pad-left("x", 2, fill: '-'), "-x");
end test;

define test test-pad ()
  check-equal("a", pad("x", 3), " x ");
  check-equal("b", pad("xx", 1), "xx");
  check-equal("c", pad("x", 3, fill: '-'), "-x-");
end test;

define test test-pad-right ()
  check-equal("a", pad-right("x", 3), "x  ");
  check-equal("b", pad-right("xx", 1), "xx");
  check-equal("c", pad-right("x", 2, fill: '-'), "x-");
end test;


////
//// Character case
////

define test test-lowercase! ()
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
end test;

define test test-lowercase ()
  for (item in #(#("", ""),
                 #("a", "a"),
                 #("A", "a"),
                 #("ABC", "abc"),
                 #("ONE TWO", "one two"),
                 #("_oNe,Two", "_one,two")))
    let (before, expected) = apply(values, item);
    check-equal(fmt("lowercase %=", before), expected, lowercase(before));
  end;
end test;

define test test-uppercase! ()
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
end test;

define test test-uppercase ()
  for (item in #(#("", ""),
                 #("a", "A"),
                 #("A", "A"),
                 #("abc", "ABC"),
                 #("one two", "ONE TWO"),
                 #("_oNe,Two", "_ONE,TWO")))
    let (before, expected) = apply(values, item);
    check-equal(fmt("uppercase %=", before), expected, uppercase(before));
  end;
end test;

define test test-lowercase? ()
  check-true("a", lowercase?('a'));
  check-true("b", lowercase?("abc-$#^^10"));
  check-false("c", lowercase?('A'));
  check-false("d", lowercase?("aBc-$#^^10"));
end test;

define test test-uppercase? ()
  check-true("a", uppercase?('X'));
  check-true("b", uppercase?("ABC-$#^^10"));
  check-false("c", uppercase?('b'));
  check-false("d", uppercase?("aBc-$#^^10"));
end test;


////
//// Stripping
////

define test test-strip ()
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
end test;

define test test-strip-left ()
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
end test;

define test test-strip-right ()
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
end test;


////
//// Miscellaneous
////

define test test-whitespace? ()
  for (char in " \t\n\r\f")
    check-true(fmt("%=", char), whitespace?(char));
  end;
  check-true("a", whitespace?(" \t \n \r \f "));
  check-false("b", whitespace?('a'));
  check-false("c", whitespace?("  b  "));
end test;

define test test-split-lines ()
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
end test;

define test test-find-any ()
  assert-false(find-any("abc", whitespace?));
  assert-equal(1, find-any("a b c", whitespace?));
  assert-equal(3, find-any("a b c", whitespace?, from-end?: #t));
  assert-equal(0, find-any("a b c", complement(whitespace?)));
  assert-equal(4, find-any("a b c", complement(whitespace?), from-end?: #t));
  assert-equal(3, find-any("a b c", whitespace?, start: 2));
  assert-equal(#f, find-any("a b c", whitespace?, start: 2, end: 2));
  assert-equal(1, find-any("a b c", whitespace?, end: 2, from-end?: #t));
  assert-equal(#f, find-any("a b c", whitespace?, start: 2, end: 2, from-end?: #t));
end test;

////
//// Benchmarks
////

define benchmark string-compare-benchmark ()
  let string = make(<string>, size: 10000, fill: 'x');
  benchmark-repeat(iterations: 200)
    string-compare(string, string);
  end;
end benchmark;

define benchmark string-compare-ic-benchmark ()
  let string = make(<string>, size: 10000, fill: 'x');
  benchmark-repeat(iterations: 200)
    string-compare(string, string, test: char-compare-ic);
  end;
end benchmark;

define benchmark string-equal?-benchmark ()
  let string = make(<string>, size: 10000, fill: 'x');
  benchmark-repeat(iterations: 200)
    string-equal?(string, string);
  end;
end benchmark string-equal?-benchmark;

define benchmark string-equal-ic?-benchmark ()
  let string = make(<string>, size: 10000, fill: 'x');
  benchmark-repeat(iterations: 200)
    string-equal-ic?(string, string);
  end;
end benchmark;
