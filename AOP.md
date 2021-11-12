# AOP - A Spec Oriented Programming

AOP is a way to transform code using the following mechanism:

**Point cut:** defines where some code can be modified (eg before or after a function is called)

**Join point:** defines which code should be altered (eg all, project code only, a namespace, any fn with 3 params, etc)

**Advice:** the code that will be executed at the specific pointcut based on the results of the join point.

All the above are language specific.

In our case we are looking at data from conforming Clojure namespaces against the core language specs.

We don't define pointcuts because that is more limiting than giving each transforming function the conformed data for any given form.

In place of pre-defined pointcuts we provide a library - that we can evolve - of functions that manipulate the conformed data to meet the most obvious and common needs for code transformation.

Join points are defined as a map of data that define over what scope the transforming function is called.

Transforming functions operate in a one-pass pipeline:
 - the join points are realized into a sequence
 - each transforming function is passed the form
 - the function transforms the form or pass it on unchanged

Two examples: one to demonstrate a function transformer that requires one advice function and a second that requires several advice functions.

1. Print or tap function parameters
- add an expression at the start of each function to emit the parameters passed into the function. Emit:
  - the function name
  - the parameter names (or `:none`)
  - the parameter values (or `:none`)

2. Require a tracing library and trace each function call
- add a `require` at the start of each namespace to require the tracing library. 
- add an expression before each function and var definition to enable tracing.



