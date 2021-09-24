module Space
  class Foo
    attr_reader :thing
    attr_accessor :another
    
    def initialize(a, b)
      @thing = a
      @another = b
    end

    def do_thing(x)
      x.call(@thing)
    end
  end
end

spacefoo = Space::Foo.new(4, "Yes")
spacefoo.do_thing(-> (x) { puts(x) })