acme-php-types
==============

Everyone knows that Haskell type system is too restrictive and dull for real world applications.
Millions of users cannot be wrong: hail to industrial-strength PHP typing, now in Haskell!

For years Haskell developers were bursting with envy, looking at the easy-as-a-pie type juggling in PHP. But today you can do all this amazing stuff yourself:

```
> stack ghci --ghc-options="-XOverloadedStrings"
Ok, modules loaded: Data.PHP.
>
> "foo" == true
True
> "foo" != 0
False
> true  != 0
True
>
>
> 123 != "123foo"
False
> "123" != "123foo"
True
>
>
> "6" != " 6"
False
> "6" != "6 "
True
>
>
> "4.2" != "4.20"
False
> "133" != "0133"
False
>
>
> "false" == true
True
> "false" == false
False
> "false" != 0
False
```
