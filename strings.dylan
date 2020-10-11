Module: %strings

// TODO(cgay): Anything related to unicode.


// eXpression IF.  (Not that the built-in IF isn't an expression.)
define macro xif
    { xif(?test:expression, ?true:expression, ?false:expression) }
 => { if (?test) ?true else ?false end }

    { xif(?test:expression, ?true:expression) }
 => { if (?test) ?true end }
end;


define constant <string-or-char> = type-union(<string>, <character>);



//// Character-type predicates that apply to characters or strings. 

define sealed generic alphabetic?
    (string-or-character :: <string-or-char>, #key) => (alphabetic? :: <boolean>);

define method alphabetic?
    (char :: <character>, #key) => (b :: <boolean>)
  let code = as(<integer>, char);
  uppercase-code?(code) | lowercase-code?(code)
end;

define inline method alphabetic?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(alphabetic?, string, start, epos)
end method alphabetic?;


define sealed generic alphanumeric?
    (string-or-character :: <string-or-char>, #key) => (alphanumeric? :: <boolean>);

define method alphanumeric?
    (char :: <character>, #key) => (b :: <boolean>)
  let code = as(<integer>, char);
  uppercase-code?(code) | lowercase-code?(code) | decimal-digit?(char)
end;

define method alphanumeric?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(alphanumeric?, string, start, epos)
end;


define sealed generic graphic?
    (string-or-character :: <string-or-char>, #key) => (graphic? :: <boolean>);

define method graphic?
    (char :: <character>, #key) => (b :: <boolean>)
  let code = as(<integer>, char);
  as(<integer>, ' ') <= code & code <= as(<integer>, '~')
end;

define method graphic?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(graphic?, string, start, epos)
end;



define sealed generic printable?
    (string-or-character :: <string-or-char>, #key) => (printable? :: <boolean>);

define method printable?
    (char :: <character>, #key) => (b :: <boolean>)
  graphic?(char) | whitespace?(char)
end;

define method printable?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(printable?, string, start, epos)
end;


define sealed generic control?
    (string-or-character :: <string-or-char>, #key) => (control? :: <boolean>);

define method control?
    (char :: <character>, #key) => (b :: <boolean>)
  ~ printable?(char)
end;

define method control?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(control?, string, start, epos)
end;


define sealed generic whitespace?
    (string-or-character :: <string-or-char>, #key) => (whitespace? :: <boolean>);

define method whitespace?
    (char :: <character>, #key) => (b :: <boolean>)
  select (char)
    // Space, tab, newline, formfeed, carriage return
    ' ', '\t', '\n', '\f', '\r' => #t;
    otherwise => #f;
  end select
end;

define method whitespace?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(whitespace?, string, start, epos)
end;


// Returns #t if the required argument could be a value returned from
// 'as-uppercase'.  In other words, if the argument does NOT contain
// any lowercase characters.
define sealed generic uppercase?
    (string-or-character :: <string-or-char>, #key) => (uppercase? :: <boolean>);

define inline method uppercase?
    (char :: <character>, #key) => (b :: <boolean>)
  ~lowercase-code?(as(<integer>, char))
end;

define method uppercase?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(uppercase?, string, start, epos)
end;


// Returns #t if the required argument could be a value returned from
// 'as-lowercase'.  In other words, if the argument does NOT contain
// any uppercase characters.
define sealed generic lowercase?
    (string-or-character :: <string-or-char>, #key) => (lowercase? :: <boolean>);

define inline method lowercase?
    (char :: <character>, #key) => (b :: <boolean>)
  ~uppercase-code?(as(<integer>, char))
end;

define method lowercase?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(lowercase?, string, start, epos)
end;


define sealed generic octal-digit?
    (string-or-character :: <string-or-char>, #key) => (octal? :: <boolean>);

define method octal-digit?
    (char :: <character>, #key) => (b :: <boolean>)
  let code = as(<integer>, char);
  as(<integer>, '0') <= code & code <= as(<integer>, '7')
end;

define method octal-digit?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(octal-digit?, string, start, epos)
end;


define sealed generic decimal-digit?
    (string-or-character :: <string-or-char>, #key) => (decimal? :: <boolean>);

define method decimal-digit?
    (char :: <character>, #key) => (b :: <boolean>)
  let code = as(<integer>, char);
  as(<integer>, '0') <= code & code <= as(<integer>, '9')
end;

define method decimal-digit?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(decimal-digit?, string, start, epos)
end;


define sealed generic hexadecimal-digit?
    (string-or-character :: <string-or-char>, #key) => (hexadecimal? :: <boolean>);

define method hexadecimal-digit?
    (char :: <character>, #key) => (b :: <boolean>)
  let code = as(<integer>, as-lowercase(char));
  (as(<integer>, '0') <= code & code <= as(<integer>, '9')) |
  (as(<integer>, 'a') <= code & code <= as(<integer>, 'f'))
end;

define method hexadecimal-digit?
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (b :: <boolean>)
  %every?(hexadecimal-digit?, string, start, epos)
end;



//// Character comparisons

// Note that the one-of result type causes the compiler to insert a
// check-type(result, <integer>) in some places.  Might want to type
// these as <integer> instead.
define inline function char-compare
    (char1 :: <character>, char2 :: <character>)
 => (result :: one-of(-1, 0, 1))
  let c1 :: <integer> = as(<integer>, char1);
  let c2 :: <integer> = as(<integer>, char2);
  xif(c1 == c2, 0, xif(c1 < c2, -1, 1))
end;

define inline function char-compare-ic
    (char1 :: <character>, char2 :: <character>) => (result :: one-of(-1, 0, 1))
  let c1 :: <integer> = as(<integer>, as-lowercase(char1));
  let c2 :: <integer> = as(<integer>, as-lowercase(char2));
  xif(c1 == c2, 0, xif(c1 < c2, -1, 1))
end;

define inline function char-equal-ic?
    (char1 :: <character>, char2 :: <character>) => (eq? :: <boolean>)
  as-lowercase(char1) == as-lowercase(char2)
end;



//// String comparisons

// Compare string1 and string2, returning -1 if string1 < string2, 0
// if string1 = string2, and 1 if string1 > string2.
define sealed generic string-compare
    (string1 :: <string>, string2 :: <string>,
     #key start1, end1, start2, end2, test)
 => (result :: one-of(-1, 0, 1));

define method string-compare
    (string1 :: <string>, string2 :: <string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
          start2 :: <integer> = 0, end2 :: <integer> = string2.size,
          test :: <function> = char-compare)
 => (result :: one-of(-1, 0, 1))
  %string-compare(string1, string2, start1, end1, start2, end2, test)
end;


define sealed generic %string-compare
    (string1 :: <string>, string2 :: <string>,
     start1 :: <integer>, end1 :: <integer>,
     start2 :: <integer>, end2 :: <integer>,
     test :: <function>)
 => (result :: one-of(-1, 0, 1));

// A macro to inline specific comparator functions for the fast path,
// namely char-compare and char-compare-ic.
define macro %string-compare-body
  { %string-compare-body(?test:name,
                         ?string1:expression, ?string2:expression,
                         ?start1:expression, ?end1:expression,
                         ?start2:expression, ?end2:expression)
  } => {
    iterate loop (i1 :: <integer> = ?start1, i2 :: <integer> = ?start2)
      case
        i1 = ?end1 =>
          xif(i2 = ?end2, 0, -1);
        i2 = ?end2 =>
          1;
        otherwise =>
          let result :: <integer> = ?test(?string1[i1], ?string2[i2]);
          xif(result == 0,
              loop(i1 + 1, i2 + 1),
              result);
      end case
    end iterate
  }
end macro %string-compare-body;

define method %string-compare
    (string1 :: <string>, string2 :: <string>,
     start1 :: <integer>, end1 :: <integer>,
     start2 :: <integer>, end2 :: <integer>,
     test :: <function>)
 => (result :: one-of(-1, 0, 1))
  range-check(string1, string1.size, start1, end1);
  range-check(string2, string2.size, start2, end2);
  %string-compare-body(test, string1, string2, start1, end1, start2, end2)
end method %string-compare;

define method %string-compare
    (string1 :: <byte-string>, string2 :: <byte-string>,
     start1 :: <integer>, end1 :: <integer>,
     start2 :: <integer>, end2 :: <integer>,
     test :: <function>)
 => (result :: one-of(-1, 0, 1))
  range-check(string1, string1.size, start1, end1);
  range-check(string2, string2.size, start2, end2);
  select (test)
    char-compare =>
      %string-compare-body(char-compare,
                           string1, string2, start1, end1, start2, end2);
    char-compare-ic =>
      %string-compare-body(char-compare-ic,
                           string1, string2, start1, end1, start2, end2);
    otherwise =>
      %string-compare-body(test, string1, string2, start1, end1, start2, end2);
  end select
end method %string-compare;


define sealed generic  string-equal?
    (string1 :: <string>, string2 :: <string>, #key start1, end1, start2, end2, test)
 => (equal? :: <boolean>);

define method string-equal?
    (string1 :: <string>, string2 :: <string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
          start2 :: <integer> = 0, end2 :: <integer> = string2.size,
          test :: <function> = char-compare)
 => (equal? :: <boolean>)
  (end2 - start2) == (end1 - start1)
    & (%string-compare(string1, string2, start1, end1, start2, end2, test) == 0)
end method string-equal?;


// Is string1 < string2?
define sealed generic string-less?
    (string1 :: <string>, string2 :: <string>, #key start1, end1, start2, end2, test)
 => (less? :: <boolean>);

define method string-less?
    (string1 :: <string>, string2 :: <string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
          start2 :: <integer> = 0, end2 :: <integer> = string2.size,
          test :: <function> = char-compare)
 => (less? :: <boolean>)
  %string-compare(string1, string2, start1, end1, start2, end2, test) == -1
end method string-less?;


// Is string1 > string2?
define sealed generic string-greater?
    (string1 :: <string>, string2 :: <string>, #key start1, end1, start2, end2, test)
 => (greater? :: <boolean>);

define method string-greater?
    (string1 :: <string>, string2 :: <string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
          start2 :: <integer> = 0, end2 :: <integer> = string2.size,
          test :: <function> = char-compare)
 => (greater? :: <boolean>)
  %string-compare(string1, string2, start1, end1, start2, end2, test) == 1
end method string-greater?;


define sealed generic string-equal-ic?
    (string1 :: <string>, string2 :: <string>, #key start1, end1, start2, end2)
 => (equal? :: <boolean>);

define method string-equal-ic?
    (string1 :: <string>, string2 :: <string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
          start2 :: <integer> = 0, end2 :: <integer> = string2.size)
 => (equal? :: <boolean>)
  (end2 - start2) == (end1 - start1)
    & (%string-compare(string1, string2, start1, end1, start2, end2, char-compare-ic) == 0)
end method string-equal-ic?;


// Is string1 < string2, ignoring case?
define sealed generic string-less-ic?
    (string1 :: <string>, string2 :: <string>, #key start1, end1, start2, end2)
 => (less? :: <boolean>);

define method string-less-ic?
    (string1 :: <string>, string2 :: <string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
          start2 :: <integer> = 0, end2 :: <integer> = string2.size)
 => (less? :: <boolean>)
  %string-compare(string1, string2, start1, end1, start2, end2, char-compare-ic) == -1
end method string-less-ic?;


// Is string1 > string2, ignoring case?
define sealed generic string-greater-ic?
    (string1 :: <string>, string2 :: <string>, #key start1, end1, start2, end2)
 => (greater? :: <boolean>);

define method string-greater-ic?
    (string1 :: <string>, string2 :: <string>,
     #key start1 :: <integer> = 0, end1 :: <integer> = string1.size,
          start2 :: <integer> = 0, end2 :: <integer> = string2.size)
 => (greater? :: <boolean>)
  %string-compare(string1, string2, start1, end1, start2, end2, char-compare-ic) = 1
end method string-greater-ic?;



// Does string start with pattern?  (Is pattern a prefix of string?)
define sealed generic starts-with?
    (string :: <string>, pattern :: <string>, #key test)
 => (starts-with? :: <boolean>);

define method starts-with?
    (string :: <string>, pattern :: <string>, #key test :: <function> = char-compare)
 => (starts-with? :: <boolean>)
  let plen :: <integer> = pattern.size;
  if (plen <= string.size)
    %string-compare(string, pattern, 0, plen, 0, plen, test) = 0
  end
end method starts-with?;

// Does string end with pattern?  (Is pattern a suffix of string?)
define sealed generic ends-with?
    (string :: <string>, pattern :: <string>, #key test)
 => (ends-with? :: <boolean>);

define method ends-with?
    (string :: <string>, pattern :: <string>, #key test :: <function> = char-compare)
 => (ends-with? :: <boolean>)
  let slen :: <integer> = string.size;
  let plen :: <integer> = pattern.size;
  if (plen <= slen)
    %string-compare(string, pattern, slen - plen, slen, 0, plen, test) = 0
  end
end method ends-with?;



//// Case conversions

define sealed generic lowercase
    (string-or-character :: <string-or-char>, #key)
 => (new-string-or-character :: <string-or-char>);

define method lowercase
    (char :: <character>, #key) => (new-char :: <character>)
  as-lowercase(char)
end;

define method lowercase
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (new-string :: <string>)
  lowercase!(copy-sequence(string, start: start, end: epos))
end method lowercase;
  

define sealed generic lowercase!
    (string-or-character :: <string-or-char>, #key)
 => (string-or-character :: <string-or-char>);

define method lowercase!
    (char :: <character>, #key) => (new-char :: <character>)
  as-lowercase(char)
end;

define method lowercase!
    (string :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (string :: <string>)
  range-check(string, string.size, start, epos);
  for (i from start below epos)
    string[i] := lowercase(string[i])
  end;
  string
end method lowercase!;


define sealed generic uppercase
    (string-or-character :: <string-or-char>, #key)
 => (new-string-or-character :: <string-or-char>);

define method uppercase
    (char :: <character>, #key) => (new-char :: <character>)
  as-uppercase(char)
end;

define method uppercase
    (string :: <string>,
     #key start :: <integer> = 0, end: epos :: <integer> = string.size)
 => (new-string :: <string>)
  uppercase!(copy-sequence(string, start: start, end: epos))
end method uppercase;


define sealed generic uppercase!
    (string-or-character :: <string-or-char>, #key)
 => (string-or-character :: <string-or-char>);

define method uppercase!
    (char :: <character>, #key) => (new-char :: <character>)
  as-uppercase(char)
end;

define method uppercase!
    (string :: <string>,
     #key start :: <integer> = 0, end: epos :: <integer> = string.size)
 => (string :: <string>)
  range-check(string, string.size, start, epos);
  for (i from start below epos)
    string[i] := uppercase(string[i])
  end;
  string
end method uppercase!;



//// String padding (or alignment)

define sealed generic pad
    (string :: <string>, width :: <integer>, #key fill)
 => (new-string :: <string>);

define method pad
    (string :: <string>, width :: <integer>, #key fill :: <character> = ' ')
 => (new-string :: <string>)
  let slen = string.size;
  if (slen >= width)
    string
  else
    let start = floor/(width - slen, 2);       // start of "string" in "new"
    let new = make(<string>, size: width);
    for (i :: <integer> from 0 below start)
      new[i] := fill;
    end;
    for (c in string,
         i :: <integer> from start)
      new[i] := c;
    end;
    for (i :: <integer> from start + slen below width)
      new[i] := fill;
    end;
    new
  end
end method pad;


// Return a string of size 'width', padded on the right with 'fill' characters.
define sealed generic pad-right
    (string :: <string>, width :: <integer>, #key fill)
 => (new-string :: <string>);

define method pad-right
    (string :: <string>, width :: <integer>, #key fill :: <character> = ' ')
 => (new-string :: <string>)
  let slen :: <integer> = string.size;
  if (slen >= width)
    string
  else
    let new = make(<string>, size: width);
    for (c in string,
         i :: <integer> from 0)
      new[i] := c;
    end;
    for (i :: <integer> from slen below width)
      new[i] := fill;
    end;
    new
  end
end method pad-right;


// Return a string of size 'width', padded on the left with 'fill' characters.
define sealed generic pad-left
    (string :: <string>, width :: <integer>, #key fill)
 => (new-string :: <string>);

define method pad-left
      (string :: <string>, width :: <integer>, #key fill :: <character> = ' ')
   => (new-string :: <string>)
  let slen :: <integer> = string.size;
  if (slen >= width)
    string
  else
    let new = make(<string>, size: width);
    for (i :: <integer> from 0 below width - slen)
      new[i] := fill;
    end;
    for (i :: <integer> from width - slen,
         c in string)
      new[i] := c;
    end;
    new
  end
end method pad-left;



//// Stripping (or trimming)

define sealed generic strip
    (string :: <string>, #key test, start, end: epos) => (new-string :: <string>);

define method strip
    (string :: <string>,
     #key test :: <function> = whitespace?,
          start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (new-string :: <string>)
  let untest = complement(test);
  let left = find-any(string, untest, start: start, end: epos);
  if (~left)
    ""
  else
    // If left is not #f then right will also not be #f.
    let right :: <integer>
      = find-any(string, untest, start: start, end: epos, from-end?: #t);
    if (left == 0 & right == (string.size - 1))
      string
    else
      copy-sequence(string, start: left, end: right + 1)
    end
  end if
end method strip;

define sealed generic strip-left
    (string :: <string>, #key test, start, end: epos) => (new-string :: <string>);

define method strip-left
    (string :: <string>,
     #key test :: <function> = whitespace?,
          start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (new-string :: <string>)
  let left :: <integer>
    = find-any(string, complement(test), start: start, end: epos) | epos;
  if (left == 0 & epos == string.size)
    string
  else
    copy-sequence(string, start: left, end: epos)
  end
end method strip-left;

define sealed generic strip-right
    (string :: <string>, #key test, start, end: epos) => (new-string :: <string>);

define method strip-right
    (string :: <string>,
     #key test :: <function> = whitespace?,
          start :: <integer> = 0,
          end: epos :: <integer> = string.size)
 => (new-string :: <string>)
  let right = find-any(string, complement(test), from-end?: #t, start: start, end: epos);
  case
    ~right
      => "";
    start == 0 & right == (string.size - 1)
      => string;
    otherwise
      => copy-sequence(string, start: start, end: right + 1)
  end
end method strip-right;



//// Split lines

define function split-lines
    (string :: <string>, #key remove-if-empty? :: <boolean>)
 => (lines :: <sequence>)
  let epos :: <integer> = string.size;
  reverse!(iterate loop (bpos :: <integer> = 0, parts :: <list> = #())
             let (sep-start, sep-end)
               = iterate find-eol (i = bpos) // Recognize CR alone, CRLF, or LF alone.
                   if (i < epos)
                     select (string[i])
                       '\r' =>
                         let i2 = i + 1;
                         let char2 = (i2 < epos) & string[i2];
                         values(i, (char2 = '\n' & i + 2) | i2);
                       '\n' =>
                         values(i, i + 1);
                       otherwise =>
                         find-eol(i + 1);
                     end select
                   end if
                 end iterate;
             let part = copy-sequence(string, start: bpos, end: sep-start | epos);
             case
               ~sep-start =>
                 xif(remove-if-empty? & empty?(part),
                     parts,
                     pair(part, parts));
               sep-end = epos =>     // trailing EOL
                 pair(part, parts);
               remove-if-empty? & empty?(part) =>
                 loop(sep-end, parts);
               otherwise =>
                 loop(sep-end, pair(part, parts));
             end
           end)
end function split-lines;


//// Utilities

define inline function uppercase-code?
    (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'A') <= code & code <= as(<integer>, 'Z')
end;


define inline function lowercase-code?
    (code :: <integer>) => (true? :: <boolean>)
  as(<integer>, 'a') <= code & code <= as(<integer>, 'z')
end;


define inline function range-check
    (sequence :: <sequence>, _size :: <integer>, _start :: <integer>, _end :: <integer>) => ()
  when (_start < 0 | _start > _size)
    element-range-error(sequence, _start)
  end;
  when (_end < 0 | _end > _size)
    element-range-error(sequence, _end)
  end
end;


// Because every? doesn't have start/end parameters.
define inline function %every?
    (test :: <function>, string :: <string>, bpos :: <integer>, epos :: <integer>)
  iterate loop (pos :: <integer> = bpos)
    xif(pos < epos,
        xif(test(string[pos]), loop(pos + 1), #f),
        #t)
  end
end function %every?;


// Find the index of the first char in `string` that is a member of `charset`.
define generic find-any (string :: <string>, test :: <function>,
                         #key start, end: epos, from-end?)
 => (index :: false-or(<integer>));

define method find-any (string :: <string>, test :: <function>,
                        #key start: bpos :: <integer> = 0,
                             end: epos :: <integer> = string.size,
                             from-end? :: <boolean>)
 => (index :: false-or(<integer>))
  block (return)
    if (from-end?)
      for (i from epos - 1 to bpos by -1)
        if (test(string[i]))
          return(i);
        end;
      end;
    else
      for (i from bpos below epos)
        if (test(string[i]))
          return(i);
        end;
      end;
    end;
  end
end method;
