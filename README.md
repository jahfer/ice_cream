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

#### Indexed Data
```
CONST ASSIGNMENTS

Definition of ::ChatApp::VERSION
     ...
     2|   VERSION = "1.0.0"
      |   ^^^^^^^

Definition of ::ChatApp::User
     ...
     4|   class User
      |   ^^^^^^^^^^

Definition of ::ChatApp::Bot
     ...
    11|   class Bot
      |   ^^^^^^^^^

Definition of ::ChatApp::Message
     ...
    19|   class Message
      |   ^^^^^^^^^^^^^

Definition of ::ChatApp::Channel
     ...
    36|   class Channel
      |   ^^^^^^^^^^^^^

Definition of ::ChatApp
     ...
     1| module ChatApp
      | ^^^^^^^^^^^^^^

LOCAL VARIABLE ASSIGNMENTS

Scope: ::ChatApp::Message
     ...
    31|       m = Message.new
      |       ^

INSTANCE VARIABLE ASSIGNMENTS

Scope: ::ChatApp::Message
     ...
    26|       @from = from
      |       ^^^^^

Scope: ::ChatApp::Message
     ...
    27|       @string = string
      |       ^^^^^^^

FUNCTION DEFINITIONS

Scope: ::ChatApp::User
     ...
     8|     def initialize(login, email); end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Scope: ::ChatApp::Bot
     ...
    16|     def initialize(name, owner); end
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^

Scope: ::ChatApp::Message
     ...
    25|     def initialize(from, string)
      |     ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

Scope: ::ChatApp::Message
     ...
    30|     def reply(from, string)
      |     ^^^^^^^^^^^^^^^^^^^^^^^

Scope: ::ChatApp::Channel
     ...
    42|     def initialize(name); end
      |     ^^^^^^^^^^^^^^^^^^^^

Scope: ::ChatApp::Channel
     ...
    43|     def each_member; end
      |     ^^^^^^^^^^^^^^^
```
