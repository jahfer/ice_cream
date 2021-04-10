# Tasks

- [ ] Parse <.rbs> files to get the interface of custom objects
  - Only store input arity and output type (drop input types) since we only
    want to see what methods are invoked on the params, not the nominal
    type of the input
  - This implicitly makes <.rbs> files the public export interface

- [ ] Analyze <.rb> files for method invocations
  - [ ] Start with def blocks only

- [ ] Verify output types match inferred output values from methods 

```ocaml
(<+ : a -> b>, a) -> b
def foo(a, b)
  a + b
end
```