# Converts Life inputs in the "Plaintext" format into a snippet of DNC code to initialise a buffer
# that state.
#
# Plaintext format description: https://conwaylife.com/wiki/Plaintext

input_file = ARGV[0] or abort "Usage: #{$0} <file>"
lines = File.read(ARGV[0]).split("\n")

# Handle leading comments
# Those starting with `!` are just ignored, but we use `!!` for magic directives
# (Comments aren't permitted to appear elsewhere)
rotate = false
origin = [0, 0]
while lines.first.start_with?('!')
    comment = lines.shift
    if comment.start_with?('!!')
        case comment
        when '!!Rotate'
            rotate = true
        when /^!!Origin (\d+) (\d+)/
            origin = [$1.to_i, $2.to_i]
        end
    end
end

# Parse cells
Cell = Struct.new('Cell', :x, :y) do
    def rotate
        Cell.new(y, x)
    end
    def translate(dx, dy)
        Cell.new(x + dx, y + dy)
    end
end
cells = []
lines.each_with_index do |line, y|
    line.chars.each_with_index do |char, x|
        case char
        when 'O'
            # There's a live cell here
            cells << Cell.new(x, y)
        when '.'
            # No cell here
        else
            raise "unknown character: #{char}"
        end
    end
end

# Apply modifiers
cells.map!(&:rotate) if rotate
cells.map! { |cell| cell.translate(*origin) }

# Generate code
LIFE_ROWS = 45
LIFE_COLS = 32
WORD_SIZE = 16
raise 'assumed LIFE_COLS / WORD_SIZE = 2' unless (LIFE_COLS.to_f / WORD_SIZE.to_f) == 2.0

if cells.any? { |cell| cell.x >= LIFE_COLS || cell.y >= LIFE_ROWS }
    raise "does not fit"
end

puts "fn init_buffer(buffer: *u16) {"
LIFE_ROWS.times do |y|
    print "    "
    2.times do |word|
        index = y * 2 + word
        print "*(buffer + #{index.to_s.rjust(2, '0')}) = 0b"
        WORD_SIZE.times do |word_index|
            x = word * WORD_SIZE + word_index
            if cells.find { |cell| cell.x == x && cell.y == y }
                print '1'
            else
                print '0'
            end
        end
        print "; "
    end
    puts
end
puts "}"
