Module:    dylan-user
Synopsis:  Test suite for the strings library
Author:    Carl Gay


define library strings-test-suite
  use common-dylan;
  use io,
    import: { format };
  use regular-expressions;
  use strings;
  use system,
    import: { locators };
  use testworks;
end;

define module strings-test-suite
  use common-dylan;
  use format;
  use regular-expressions,
    import: { compile-regex,
              match-group };
  use strings;
  use locators,
    import: { locator-name,
              <file-locator> };
  use testworks;
end;

