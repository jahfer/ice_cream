# foo : (<.thing : () -> T0>) -> T0
def foo(x)
  x.thing
end

# bar : (<.thing : T1 -> T0> & T1) -> T0
def bar(x)
  x.thing(x)
end

