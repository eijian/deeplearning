#!/usr/bin/ruby
#
# MNIST data converter
#

USAGE = "Usage:"
SKIP = 16
LEN = 28*28

def init
  if ARGV.size != 1
    STDERR.puts USAGE
    exit 1
  end

  $FILE = ARGV[0]
end

def main
  init
  dat = File.binread($FILE)
  dat = dat.unpack("C*").drop(SKIP)
  dat.each_slice(LEN).each_with_index do |img, i|
    fn = sprintf("%08d.pgm", i)
    File.open(fn, 'w') do |fp|
      fp.puts "P5"
      fp.puts "# MNIST data (#{i})"
      fp.puts "28 28"
      fp.puts "255"
      fp.write(img.pack("C*"))
    end
    puts fn
  end

end

main


