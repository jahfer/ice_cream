## Ice Cream - A Ruby Type Inference Engine

### Todo List

- [x] Parser
- [ ] Compute algebraic subtyping annotations (+/- flows)
- [ ] Parse RBS files to build library of type interfaces
- [ ] Apply constraints to type annotations
- [ ] ????
- [ ] Solve! 💰

### Current Status: Parsing (mostly) works!

#### Unsupported Syntax (for now)

```ruby
# string interpolation
"Hello, #{name}"

# curly-braced do-blocks 
foo { |x| x.bar }

# method assignment
x.foo = bar

# array/hash assignment
hash[:foo] = bar

# named parameters
def foo(bar:); end

# default positional argument values
def foo(bar = true); end

# default named parameter values
def foo(bar: true); end

# heredoc
<<~MSG
Hello, name!
MSG

# heredoc method call
<<~MSG.trim
Hello
world!
MSG

# ambiguous do-block
foo bar do |x| # block belongs to foo, not bar!
  x.baz
end

# class inheritence
class Foo < Bar; end

# hash rockets
{ "foo" => :bar }

# operators
x | 0
a + b

# ...and a lot more!
```

#### Input
```ruby
module ChatApp
  VERSION = "1.0.0"

  class User
    attr_reader :login
    attr_reader :email

    def initialize(login, email); end
  end

  class Bot
    attr_reader :name
    attr_reader :email
    attr_reader :owner

    def initialize(name, owner); end
  end

  class Message
    attr_reader :id
    attr_reader :string
    attr_reader :from
    attr_reader :reply_to

    def initialize(from, string)
      @from = from
      @string = string
    end

    def reply(from, string)
      m = Message.new
      m
    end
  end

  class Channel
    attr_reader :name
    attr_reader :messages
    attr_reader :users
    attr_reader :bots

    def initialize(name); end
    def each_member; end
  end
end
```


#### Output
```clj
(casgn 
    (const 
        (nesting []) `ChatApp) 
    (module 
        (casgn 
            (const 
                (nesting []) `VERSION) "1.0.0") 
        (casgn 
            (const 
                (nesting []) `User) 
            (class 
                (send nil `attr_reader 
                    (args :login)) 
                (send nil `attr_reader 
                    (args :email)) 
                (def `initialize 
                    (params 
                        (param `login) 
                        (param `email)) nil) 
                ())) 
        (casgn 
            (const 
                (nesting []) `Bot) 
            (class 
                (send nil `attr_reader 
                    (args :name)) 
                (send nil `attr_reader 
                    (args :email)) 
                (send nil `attr_reader 
                    (args :owner)) 
                (def `initialize 
                    (params 
                        (param `name) 
                        (param `owner)) nil) 
                ())) 
        (casgn 
            (const 
                (nesting []) `Message) 
            (class 
                (send nil `attr_reader 
                    (args :id)) 
                (send nil `attr_reader 
                    (args :string)) 
                (send nil `attr_reader 
                    (args :from)) 
                (send nil `attr_reader 
                    (args :reply_to)) 
                (def `initialize 
                    (params 
                        (param `from) 
                        (param `string)) 
                    (ivasgn @from 
                        (lvar `from)) 
                    (ivasgn @string 
                        (lvar `string)) 
                    ()) 
                (def `reply 
                    (params 
                        (param `from) 
                        (param `string)) 
                    (lvasgn `m 
                        (send 
                            (const 
                                (nesting []) `Message) `new 
                            (args ))) 
                    (lvar `m) 
                    ()) 
                ())) 
        (casgn 
            (const 
                (nesting []) `Channel) 
            (class 
                (send nil `attr_reader 
                    (args :name)) 
                (send nil `attr_reader 
                    (args :messages)) 
                (send nil `attr_reader 
                    (args :users)) 
                (send nil `attr_reader 
                    (args :bots)) 
                (def `initialize 
                    (params 
                        (param `name)) nil) 
                (def `each_member 
                    () nil) 
                ())) 
        ()))
```

#### Index Querying

**OCaml Script**

```ocaml
let index = Ast_index.create ast in
  index
  |> Query.query_all ~f:(fun node ->
    (Node.node_type node) = "AssignmentNode"
  )
  |> List.iter (fun node ->
    print_endline @@ Location.loc_as_string (Node.location node);
    print_endline @@ Node.pretty_print node
  );
```

**Output**
```xml
     ...
     8| params = {
      | ^^^^^^

<AssignmentNode>
  <RefNode name="params" />
  <ValueNode type="Hash" value="{ key: true, another: value }" />
</AssignmentNode>

==========================
     ...
    13| @x = :my_symbol
      | ^^

<AssignmentNode>
  <RefNode name="@x" />
  <ValueNode type="Symbol" value=":my_symbol" />
</AssignmentNode>

==========================
     ...
    16| FooBar = 151.56
      | ^^^^^^

<AssignmentNode>
  <RefNode name="FooBar" />
  <ValueNode type="Float" value="151.560000" />
</AssignmentNode>

==========================
     ...
    21| stmt1 = 3; stmt2 = 1
      | ^^^^^

<AssignmentNode>
  <RefNode name="stmt1" />
  <ValueNode type="Integer" value="3" />
</AssignmentNode>

==========================
     ...
    21| stmt1 = 3; stmt2 = 1
      |            ^^^^^

<AssignmentNode>
  <RefNode name="stmt2" />
  <ValueNode type="Integer" value="1" />
</AssignmentNode>

==========================
     ...
    37| y = [1,2,3]
      | ^

<AssignmentNode>
  <RefNode name="y" />
  <ValueNode type="Array" value="[1 2 3]" />
</AssignmentNode>

==========================
     ...
    39| n = y[0]
      | ^

<AssignmentNode>
  <RefNode name="n" />
  <CallNode method="[]" >
    <ValueNode type="Integer" value="0" />
  </CallNode>
</AssignmentNode>

==========================
     ...
    40| z = y.first
      | ^

<AssignmentNode>
  <RefNode name="z" />
  <CallNode method="first" >

  </CallNode>
</AssignmentNode>

==========================
     ...
    42| func1 = -> { x = 45 }
      | ^^^^^

<AssignmentNode>
  <RefNode name="func1" />
</AssignmentNode>

==========================
     ...
    43| func2 = -> (local) { }
      | ^^^^^

<AssignmentNode>
  <RefNode name="func2" />
</AssignmentNode>

==========================
     ...
    44| func3 = -> (local, _x) {
      | ^^^^^

<AssignmentNode>
  <RefNode name="func3" />
</AssignmentNode>

==========================
     ...
    49| b = 3
      | ^

<AssignmentNode>
  <RefNode name="b" />
  <ValueNode type="Integer" value="3" />
</AssignmentNode>

==========================
     ...
    50| a = b
      | ^

<AssignmentNode>
  <RefNode name="a" />
  <RefNode name="b" />
</AssignmentNode>

==========================
     ...
    60| class Foo; end
      |       ^^^

<AssignmentNode>
  <RefNode name="Foo" />
</AssignmentNode>

==========================
     ...
    61| class Bar < Foo; end
      |       ^^^

<AssignmentNode>
  <RefNode name="Bar" />
</AssignmentNode>

==========================
     ...
    63| module Foo::Bar::Baz; end
      |        ^^^^^^^^^^^^^

<AssignmentNode>
  <RefNode name="Baz" />
</AssignmentNode>

==========================
     ...
    65| module M1
      |        ^^

<AssignmentNode>
  <RefNode name="M1" />
  <ScopingNode>
    <AssignmentNode>
      <RefNode name="M2" />
      <ScopingNode>
        <MethodNode name="sum1" />
      </ScopingNode>
    </AssignmentNode>
  </ScopingNode>
</AssignmentNode>

==========================
     ...
    66|   class M2
      |         ^^

<AssignmentNode>
  <RefNode name="M2" />
  <ScopingNode>
    <MethodNode name="sum1" />
  </ScopingNode>
</AssignmentNode>

==========================
     ...
    73| class Foo::Bar
      |       ^^^^^^^^

<AssignmentNode>
  <RefNode name="Bar" />
  <ScopingNode>
    <MethodNode name="sum1" />
    <ScopingNode>
      <AssignmentNode>
        <RefNode name="<<EIGENCLASS>>" />
        <ScopingNode>
          <MethodNode name="thing" />
        </ScopingNode>
      </AssignmentNode>
    </ScopingNode>
  </ScopingNode>
</AssignmentNode>

==========================
     ...
    78|   class << self
      |   ^^^

<AssignmentNode>
  <RefNode name="<<EIGENCLASS>>" />
  <ScopingNode>
    <MethodNode name="thing" />
  </ScopingNode>
</AssignmentNode>

==========================
```
