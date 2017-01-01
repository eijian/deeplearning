#!/usr/bin/ruby
# back prop count check
#

image = [5, 5]
ksize = [2, 2]
s0 = image[0] - ksize[0] + 1
s1 = image[1] - ksize[1] + 1
w = Hash.new

s0.times do |i|
  s1.times do |j|

    ksize[0].times do |s|
      ksize[1].times do |t|
        ws = "W[#{s},#{t}]"
        if w[ws] == nil then w[ws] = "" end
        w[ws] += "+ d[#{i},#{j}]*a[#{i+s},#{j+t}]"
      end
    end
  end
end

w.each_key do |k|
  puts k
  puts w[k]
end

