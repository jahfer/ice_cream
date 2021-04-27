## Ice Cream - A Ruby Type Inference Engine

### Todo List

- [x] Parser
- [ ] Compute algebraic subtyping annotations (+/- flows)
- [ ] Parse RBS files to build library of type interfaces
- [ ] Apply constraints to type annotations
- [ ] ????
- [ ] Solve! 💰

### Current Status: Parsing (mostly) works!

**Input**
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

    def initialize(from, string); end

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


**Output**
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
                        (param `string)) nil) 
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