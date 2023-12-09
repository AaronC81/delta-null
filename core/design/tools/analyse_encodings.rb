# Analyses all available instructions, by scanning through the design in `instructions.md` and
# looking for encodings.
#
# Reports any statistics or problems with the encoding set.

require 'set'

INSTRUCTIONS_DOC = File.join(__dir__, "..", "instructions.md")

Instruction = Struct.new('Instruction', :mnemonic, :encoding)

# Finds all used instruction encodings.
# Returns them as a 16-element array, from most- to least-significant bit, where each item is either
# 1, 0 or nil if can be either.
def all_instructions
    File.read(INSTRUCTIONS_DOC)
        .scan(/`.... .... .... ....` - `[a-z0-9_]+`/)
        .map do |ins|
            encoding, mnemonic = ins.split(" - ")

            encoding = encoding
                .gsub(/\s+|`/, '')
                .chars
                .map do |c|
                    case c
                    when '1'
                        1
                    when '0'
                        0
                    when /[a-z]/
                        nil
                    else
                        raise "unexpected character in encoding: #{c}"
                    end 
                end
            mnemonic = mnemonic.gsub('`', '')

            Instruction.new(mnemonic, encoding)
        end
end

# Expands an encoding (from `all_encodings`) into an array of integers representing all possible
# numbers which match that encoding.
def expand_encoding(encoding)
    result = []

    # This logic was far too complicated for 9pm on a Saturday - so here's some ChatGPT code :(
    def generate_combinations(binary_list, index, result)
        if index == binary_list.length
            result << binary_list.dup
            return
        end
  
        if binary_list[index].nil?
            binary_list[index] = 0
            generate_combinations(binary_list, index + 1, result)
    
            binary_list[index] = 1
            generate_combinations(binary_list, index + 1, result)
    
            binary_list[index] = nil  # Reset for backtracking
        else
            generate_combinations(binary_list, index + 1, result)
        end
    end
    generate_combinations(encoding, 0, result)

    # Convert to integers
    result.map { _1.map(&:to_s).join.to_i(2) }
end

# Figure out how much of the encoding space is free
occupied_encodings = {}
all_instructions.each do |ins|
    values = expand_encoding(ins.encoding)
    values.each do |value|
        occupied_encodings[value] ||= []
        occupied_encodings[value] << ins.mnemonic
    end
end

# Check for conflicts
occupied_encodings.each do |value, mnemonics|
    if mnemonics.length > 1
        puts "Warning: #{value.to_s(2).rjust(16, '0')} is covered by multiple instructions: #{mnemonics.join(', ')}"
    end
end

# Print percentage used
percentage_covered = (occupied_encodings.length.to_f / 0xFFFF) * 100
puts "#{percentage_covered.round(2)}% of available encodings are used." 
