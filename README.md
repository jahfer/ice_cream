## Ice Cream - A Ruby Type Inference Engine

### Todo List

- [x] Ruby parser
- [ ] Parse RBS files to build library of type interfaces
- [ ] Compute algebraic subtyping annotations (+/- flows)
- [ ] Apply constraints to type annotations

### Current Status

#### Parsing _(kinda-sorta)_ works!

```sh
$ make build
dune build bin/cli.exe

$ make run
OCAMLRUNPARAM=b dune exec bin/cli.exe -- --dir=data/example
Importing file:  `data/example/example.rb`
Importing file:  `data/example/rbs/example.rbs`
...
```

#### …and support for RBS!

```sh
$ make build
dune build bin/cli.exe

$ make check
OCAMLRUNPARAM=b dune exec bin/cli.exe -- --dir=data/example --check
Importing file:  `data/example/rbs/example.rbs`
class Example #empty? (bool) -> bool
```

#### There's even some error messaging! 

![Error messages](images/errors.png)

#### (Un)Supported Syntax

See [Kitchen Sink demo](data/kitchen_sink/kitchen_sink.rb) for supported syntax, but this is a non-exhaustive rough list of what doesn't work yet:

```ruby
# string interpolation
"Hello, #{name}"

# curly-braced do-blocks 
foo { |x| x.bar }

# object method calls without parens
x.foo true

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

# trailing commas
{ 
  foo: :bar,
}

# operators
x | 0
a + b
```

### Examples

#### AST Generation

<details>
<summary>Input</summary>

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
</details>

<details open=true>
<summary>Output</summary>

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
</details>

#### API

The following examples use [kitchen_sink.rb](data/kitchen_sink/kitchen_sink.rb)

##### Basic Script

<details open=true>
<summary>Script</summary>

```ocaml
(* basic_ast_query.ml *)

let ast_index = Ast_index.create in

ast_index
|> Query.(query_all ~f:(is_a Method)) ~flatten:true
|> List.iter (fun node ->
  print_endline @@ "# Original code:";
  print_endline @@ Location.loc_as_string (Node.location node);

  print_endline @@ "# RBS:";
  print_endline @@ Node.to_rbs node;

  print_endline @@ "\n# AST node:";
  print_endline @@ Node.pretty_print node;
  
  print_endline "\n==========================\n";
);
```
</details>

<details>
<summary>Output</summary>

```clj
# Original code:
     ...
     1| class Example
      | ^^^^^^^^^^^^^

# RBS:


# AST node:
(Assignment (:label Example) (:node_type Assignment)
  (Ref (:name Example) (:node_type Ref))
  (Scoping (:type class) (:node_type Scoping)
    (Method (:name empty?) (:node_type Method) (:parameters [])
      (Value (:type bool) (:value true) (:node_type Value)))
    (Method (:name thing) (:node_type Method) (:parameters [x])
      (Call (:receiver (Ref (:name x) (:node_type Ref))) (:method foo) (:node_type Call)
        (Ref (:name x) (:node_type Ref))))))

==========================

# Original code:
     ...
     2|   def empty?
      |   ^^^^^^^^^^

# RBS:
def empty?: () -> untyped

# AST node:
(Method (:name empty?) (:node_type Method) (:parameters [])
  (Value (:type bool) (:value true) (:node_type Value)))

==========================

# Original code:
     ...
     6|   def thing(x)
      |   ^^^^^^^^^^^^

# RBS:
def thing: (untyped x) -> untyped

# AST node:
(Method (:name thing) (:node_type Method) (:parameters [x])
  (Call (:receiver (Ref (:name x) (:node_type Ref))) (:method foo) (:node_type Call)
    (Ref (:name x) (:node_type Ref))))

==========================
```
</details>

##### Advanced Script

This script locates all method definitions (`def foo ...`), then traverses their bodies to locate all places the method's parameters are used.

<details open=true>
<summary>Script</summary>

```ocaml
(* advanced_ast_query.ml *)
let ast_index = Ast_index.create in

ast_index
|> Query.(query_all ~f:(is_a Method)) ~flatten:true
|> List.iter @@ fun node ->
  let method_name = Query.string_attr "name" node
  and param_names = Query.string_list_attr "parameters" node in

  let usages = node
  |> Node.children
  |> Option.get
  |> Query.(query_all ~f:(is_a Ref)) ~flatten:true
  |> List.filter @@ fun n ->
    List.mem (Query.string_attr "name" n) param_names in

  List.iter (fun n -> 
    let name = Query.string_attr "name" n in
    Printf.printf "In method `#%s`, param `%s` used:\n%s\n%s\n"
      method_name
      name
      (Location.loc_as_docstr (Node.location n))
      (Location.loc_as_string (Node.location n))
  ) usages
```
</details>

<details open=true>
<summary>Output</summary>

```
In method `#sum1`, param `thing` used:
data/kitchen_sink/kitchen_sink.rb:84:5
     ...
    84|     thing.call(some)
      |     ^^^^^

In method `#sum1`, param `some` used:
data/kitchen_sink/kitchen_sink.rb:84:16
     ...
    84|     thing.call(some)
      |                ^^^^
```
</details>