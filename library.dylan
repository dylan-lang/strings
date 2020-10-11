Module:    dylan-user
Synopsis:  Basic string manipulation library
Author:    Carl Gay
Copyright: This code is in the public domain.


define library strings
  use common-dylan;
  use dylan,
    import: { dylan-extensions };

  export
    strings,
    %strings;    // for regular-expressions
end library strings;


// Interface module
//
define module strings
  // Possible addtions...
  // translate
  // make-translation-table

  create
    alphabetic?,
    alphanumeric?,
    control?,
    graphic?,
    printable?,

    lowercase?,
    uppercase?,
    whitespace?,

    octal-digit?,
    decimal-digit?,
    hexadecimal-digit?,

    char-compare,
    char-compare-ic,
    char-equal-ic?,

    string-compare,
    string-equal-ic?,
    string-equal?,
    string-greater-ic?,
    string-greater?,
    string-less-ic?,
    string-less?,

    starts-with?,
    ends-with?,

    lowercase!,
    lowercase,
    uppercase!,
    uppercase,

    strip,
    strip-left,
    strip-right,

    pad,
    pad-left,
    pad-right,

    find-any,
    find-substring,
    replace-substrings,
    count-substrings,

    // override find(...) in common-extensions

    split-lines;

    /* Should have all these basic conversion functions in common-dylan 
    character-to-string,
    string-to-integer, integer-to-string,
    string-to-float, float-to-string,
    
    */

end module strings;


// Implementation module
//
define module %strings
  use common-dylan;
  use dylan-extensions,
    import: { element-no-bounds-check,
              element-no-bounds-check-setter,
              element-range-check,
              element-range-error,
              // make-symbol,
              // case-insensitive-equal,
              // case-insensitive-string-hash
              <format-string-condition>
              };
  use strings;            // Use API module
  export make-substring-positioner;
end module %strings;

