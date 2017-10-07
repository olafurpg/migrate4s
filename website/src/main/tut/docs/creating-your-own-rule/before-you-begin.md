---
layout: docs
title: Before your begin
---

# Before your begin
Before you dive right into the code of your rule, it might be good to answer the following questions first.

### What diff do you want to make?
Scalafix is a tool to automatically produce diffs. Before implementing a rule, it's good to manually migrate/refactor a few examples first. Manually refactoring code is helpful to estimate how complicated the rule is.

### Is the expected output unambiguous?
Does the rule require manual intervention or do you always know what output the rule should produce? Scalafix currently does not yet support interactive refactoring. However, Scalafix has support for configuration, which makes it possible to leave some choice to the user on how the rule should behave.

### Who will use your rule?
The target audience/users of your rule can impact the implementation the rule. If you are the only end-user of the rule, then you can maybe take shortcuts and worry less about rare corner cases that may be easier to fix manually. If your rule is intended to be used by the entire Scala community, then you might want to be more careful with corner cases.

### What code will your rule fix?
Is your rule specific to a particular codebase? Or is the rule intended to be used on codebases that you don't have access to? If your rule is specific to one codebase, then it's easier to validate if your rule is ready. You may not even need tests, since your codebase is your only test. If your rule is intended to be used in any random codebase, you may want to have tests and put more effort into handling corner cases. In general, the smaller the target domain of your rule, the easier it is to implement a rule.

### How often will your rule run?
Are you writing a one-off migration script or will your rule run on every pull request? A rule that runs on every pull request should ideally have some unit tests and be documented so that other people can help maintain the rule.
