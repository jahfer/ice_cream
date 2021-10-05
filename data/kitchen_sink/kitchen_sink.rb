foo
45

attr_reader :bar

# comment

params = {
  "key": true,
  "another": "value"
}

old_hash = {
  "foo" => true,
  "another" => "value"
}

@x = :my_symbol
@x

FooBar = 151.56
::FooBar
M1::M2::Baz
::M1::M2

stmt1 = 3; stmt2 = 1

def sum0; false end

def sum1(); true end

def sum2(thing)
  45
end

def sum3(thing1, thing2) end

def maybe_sum(a, b, should_do_thing) end

def named_args(foo: true, bar: 3) end
def default_args(x, foo = true, bar = false) end
def mixed_args(foo, bar = 3, baz: nil) end

false

y = [1,2,3]
y.first
n = y[0]
z = y.first

func1 = -> { x = 45 }
func2 = -> (local) { }
func3 = -> (local, _x) {
  local.first
}
func3.call(y)

b = 3
a = b

sum1(5)
sum2(1,2)
maybe_sum(3, 5, false)

-> (local, _x) {
  local.first
}

class Foo; end
class Bar < Foo; end

module Foo::Bar::Baz; end

module M1
  class M2
    def sum1(thing)
      45
    end
  end
end

class Foo::Bar
  def sum1(thing)
    45
  end

  class << self
    def thing; end
  end
end

y.each_with_object({}) do |x, hash|
  hash[x]
end

# z.without_parens n ## FAILURE

r.foo = (true)