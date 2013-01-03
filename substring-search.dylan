Module: 	%strings
Author: 	Robert Stockton (rgs@cs.cmu.edu)
Synopsis:	Provides a small assortment of specialized operations for
		searching and modifying <byte-string>s.  These
		operations are analogous to existing collection operations but
		provide keywords and efficiency improvements which are
		meaningful only within the more limited domain.
                (used to be strsearch.dylan in module string-search library 
		collection-extensions)
Copyright: see below

// TODO(cgay): standardize on "ignore-case?" rather than "case-sensitive"

//======================================================================
//
// Copyright (c) 1994  Carnegie Mellon University
// Copyright (c) 1998, 1999, 2000  Gwydion Dylan Maintainers
// All rights reserved.
// 
// Use and copying of this software and preparation of derivative
// works based on this software are permitted, including commercial
// use, provided that the following conditions are observed:
// 
// 1. This copyright notice must be retained in full on any copies
//    and on appropriate parts of any derivative works.
// 2. Documentation (paper or online) accompanying any system that
//    incorporates this software, or any part of it, must acknowledge
//    the contribution of the Gwydion Project at Carnegie Mellon
//    University, and the Gwydion Dylan Maintainers.
// 
// This software is made available "as is".  Neither the authors nor
// Carnegie Mellon University make any warranty about the software,
// its performance, or its conformity to any specification.
// 
//======================================================================

//======================================================================
// The "string-search" module provides basic search and replace
// capabilities upon <byte-string>.  Exploiting the known properties
// of these types yields substantially better performance than can be
// achieved for sequences in general.
//======================================================================

define generic find-substring
    (big :: <string>, pattern :: <string>, #key start, end: _end, ignore-case?)
 => (index :: false-or(<integer>));

//     This is a specialized version of subsequence-position which works only
//     on <byte-strings>.  Since this routine only handles byte-characters and
//     \== tests, it can do a "Boyer-Moore-ish" search.  (If the pattern is
//     too small for B-M to pay off, find-substring will fall back upon a
//     simpler search strategy -- this function should never be slower than
//     subsequence-position.) 
//
define sealed inline method find-substring
    (big :: <string>, pattern :: <byte-string>,
     #key start = 0,
          end: big-end = size(big),
          ignore-case? :: <boolean>)
 => (position :: false-or(<integer>))
  let compiled-pattern = compile-substring(pattern, ~ignore-case?);
  %find-substring(big, pattern, start, big-end, ~ignore-case?,
                  compiled-pattern)
end method find-substring;


define generic count-substrings
    (big :: <string>, pattern :: <string>, #key start, end: epos, ignore-case?)
 => (count :: <integer>);

define method count-substrings
    (big :: <string>, pattern :: <string>,
     #key start :: <integer> = 0,
          end: epos :: <integer> = big.size,
          ignore-case? :: <boolean>)
 => (count :: <integer>)
  let patlen = pattern.size;
  if (patlen == 0)
    big.size + 1
  else
    iterate loop (bpos = start, count = 0)
      if (bpos < epos)
        let index = find-substring(big, pattern,
                                   start: bpos, end: epos, ignore-case?: ignore-case?);
        xif(index,
            loop(index + patlen, count + 1),
            count)
      else
        count
      end
    end
  end
end method count-substrings;


define sealed method make-substring-positioner 
    (pattern :: <byte-string>, case-sensitive :: <boolean>)
 => (positioner :: <function>)
  let compiled-pattern = compile-substring(pattern, case-sensitive);
  local method compiled-matcher
	    (big :: <string>, #key start = 0, end: big-end = size(big))
	 => (position :: false-or(<integer>))
	  %find-substring(big, pattern, start, big-end, case-sensitive,
                          compiled-pattern);
	end method compiled-matcher;
  compiled-matcher;
end method make-substring-positioner;

define inline function equal?
    (char1 :: <character>, char2 :: <character>, case-sensitive :: <boolean>)
 => (result :: <boolean>)
  if (case-sensitive)
    char1 == char2
  else
    char-equal-ic?(char1, char2)
  end if
end function equal?;

// Does the real work of find-substring.  Not exported.
//
// Specialized version of "subsequence-position" for <byte-string>
// patterns.  Since this routine only handles <byte-string>s it can do
// a Boyer-Moore-ish search.  As a further optimization, you may
// pre-compile the pattern with "compile-substring" and pass it in as
// the "compiled-pattern" argument.  This will save both time and
// space if you are searching for the same pattern repeatedly.
//
// Note: By specializing on <string> instead of <byte-string>, we increase our
// generality while decreasing efficiency.  This may be a good candidate for
// providing an "out-lined" implementation on <byte-string>.
//
define sealed method %find-substring
    (big :: <string>, pattern :: <byte-string>, start :: <integer>,
     big-end :: <integer>, case-sensitive :: <boolean>,
     compiled-pattern :: <simple-object-vector>)
 => (position :: false-or(<integer>))
  let pat-sz :: <integer> = pattern.size;
  select (pat-sz)
    0 =>			// empty string always matches
      start;
    1 =>			// simple character search
      let ch = pattern[0];
      iterate loop (i = start)
        if (i < big-end)
          if (equal?(big[i], ch, case-sensitive)) i else loop(i + 1) end
        end
      end;
    2 =>			// pairs of characters -- starting to get
      let ch1 = pattern[0];	// marginal 
      let ch2 = pattern[1];
      for (key from start below big-end - 1,
           until: equal?(big[key], ch1, case-sensitive)
                  & equal?(big[key + 1], ch2, case-sensitive))
      finally
        if (key < (big-end - 1)) key end
      end for;
    otherwise =>		// It's worth doing something Boyer-Moore-ish
      let pat-last = pat-sz - 1;
      let last-char = pattern[pat-last];
      let skip = compiled-pattern;
      iterate search (index :: <integer> = start + pat-last)
        if (index < big-end)
          let char = big[index];
          if (equal?(char, last-char, case-sensitive)) 
            // maybe it's here -- we'd better check
            for (pat-key from 0 below pat-last,
                 big-key from index - pat-last,
                 while: equal?(big[big-key], pattern[pat-key], case-sensitive))
            finally
              if (pat-key == pat-last) // fell off end -- found it.
                index - pat-last
              else
                search(index + 1) // no luck -- try further down
              end if
            end for
          else
            // last character didn't match, so we can use
            // the "skip table" to optimize
            let incr :: <integer>
              = element(skip, as(<integer>, char), default: pat-sz);
            search(index + incr)
          end if
        end if
      end iterate;
  end select
end method %find-substring;


// Used by positioners.  Not exported.
//
// Produce a skip table for Boyer-Moore-ish searching.  By splitting this off
// into a separate routine we allow people to pre-compile heavily used
// strings, thus avoiding one of the more expensive parts of the search.
//
define method compile-substring
    (pattern :: <byte-string>, case-sensitive :: <boolean>)
 => (compiled :: <simple-object-vector>);
  let sz = size(pattern);
  if (sz < 3)
    make(<simple-object-vector>, size: 0)
  else
    let result = make(<simple-object-vector>, size: 256, fill: sz);
    for (index from 0 below sz - 1,
         skip from sz - 1 by -1)
      if (case-sensitive)
	result[as(<integer>, pattern[index])] := skip;
      else
	result[as(<integer>, as-lowercase(pattern[index]))] := skip;
	result[as(<integer>, as-uppercase(pattern[index]))] := skip;
      end if;
    end for;
    result;
  end if;
end method compile-substring;


define generic replace-substrings
    (big :: <string>, pattern :: <string>, replacement :: <string>,
     #key count, start, end: _end, ignore-case?)
 => (new-string :: <string>);

define sealed method replace-substrings
    (big :: <string>, pattern :: <string>, replacement :: <string>,
     #key count :: false-or(<integer>),
          start :: <integer> = 0,
          end: _end :: <integer> = big.size,
          ignore-case? :: <boolean>)
 => (new-string :: <string>)
  let positioner = make-substring-positioner(pattern, ~ignore-case?);
  do-replacement(positioner, big, pattern, replacement, start, _end, count, #f, ignore-case?)
end method replace-substrings;

// The local method expand-replace-sequence probably generates
// excessive garbage for replace-with's that involve backslashes.  One
// might try to allocate the largest newest-piece that'll fit between
// backslashes, rather than turn each string into a character every
// time.
//
define inline function do-replacement 
    (positioner :: <function>, input :: <string>, pattern :: <string>, replacement :: <string>,
     start :: <integer>, input-end :: <integer>, count :: false-or(<integer>),
     expand-backreferences :: <boolean>,
     ignore-case? :: <boolean>)
 => (new-string :: <string>)
  local method digit-to-integer (digit :: <character>)
          as(<integer>, digit) - as(<integer>, '0')
        end;
  local method expand-replace-sequence (marks :: <sequence>)
	  if (expand-backreferences & member?('\\', replacement))
	    let return-string = "";
	    let index = 0;
	    while (index < size(replacement))
	      let newest-piece 
		= if (replacement[index] ~= '\\')
		    as(<string>, replacement[index]);
		  else
		    index := index + 1;
		    if (~decimal-digit?(replacement[index]))
		      as(<string>, replacement[index]);
		    else
		      let ref-number 
			= digit-to-integer(replacement[index]);
		      if (marks[2 * ref-number] = #f)
			"";
		      else
			copy-sequence(input, start: marks[2 * ref-number],
				      end: marks[1 + 2 * ref-number]);
		      end if;
		    end if;
		  end if;
	      return-string := concatenate(return-string, newest-piece);
	      index := index + 1;
	    end while;
	    return-string
	  else
	    replacement
	  end if
	end method expand-replace-sequence;

  let result-string = copy-sequence(input, end: start);
  let index = start;
  let num-matches = 0;
  block (done)
    while (~count | num-matches < count)
      let (#rest marks) = positioner(input, start: index, end: input-end);
      if (marks[0] = #f)
        done()
      end;
      let new = expand-replace-sequence(marks);
      let chunk = copy-sequence(input, start: index, end: marks[0]);
      result-string := concatenate(result-string, chunk, new);
      index := marks[0] + pattern.size;
      num-matches := num-matches + 1;
    end while;
  end block;
  concatenate(result-string, copy-sequence(input, start: index))
end function do-replacement;
