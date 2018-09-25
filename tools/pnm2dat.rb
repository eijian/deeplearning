#!/usr/bin/ruby
#
# pnm2dat.rb: convert from PNM to dat format
#
# Usage: pnm2dat.rb < (input PNM(pgm or ppm) file) > (output dat format)
#
# cf. ImageMagik command:
#  convert image.jpg -thumbnail 100x100 -background gray  -gravity center -extent 100x100 image.ppm
#  convert image.jpg -thumbnail 100x100 -background gray  -gravity center -extent 100x100 image.pgm

UNIT = 1.00 / 256   # unit of pixel value

def read_header
  @ch = case STDIN.readline.chomp
  when "P5" then 1
  when  "P6" then 3
  else 0
  end
  while (sz0 = STDIN.readline) =~ /^#/ do
  end
  sz = sz0.chomp.split
  @xreso = sz[0].to_i
  @yreso = sz[1].to_i
  @col = STDIN.readline.chomp.to_i
end

def read_body
  bd = STDIN.read(@xreso * @yreso * @ch).unpack("C*")
  @body = Array.new
  @ch.times do |i|
    @body[i] = Array.new
  end
  (@xreso * @yreso).times do |i|
    @ch.times do |j|
      @body[j] << bd[i * @ch + j]
    end
  end
=begin
  @ch.times do |i|
    File.open("test-#{i}.ppm", 'w') do |fp|
      fp.write @body[i].pack("C*")
    end
  end
=end
end

def c2f(c)
  "#{c * UNIT}"
end

def mat2str(body)
  body = <<-EOF
(#{@xreso}><#{@yreso})
[
#{body.map {|c| c2f(c)}.join(", ")}
]
  EOF
  body
end

def output_data
  puts "["
  puts @body.map {|b| mat2str(b)}.join(",")
  puts "]"
end

def main
  read_header
  read_body

  #puts "#{@xreso} x #{@yreso}"
  output_data
end

main
