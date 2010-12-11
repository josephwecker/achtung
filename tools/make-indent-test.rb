#!/usr/bin/env ruby
$KCODE = 'u'
require 'iconv'

times = ARGV[0] ? ARGV[0].to_i : 1
alpha =  [('a'..'z'),('A'..'Z')].map{|i| i.to_a}.flatten;
ws = rand(3)*2 # Starting indent level
(1..(times * 100)).each do
  puts('') if rand(100) > 90
  puts(' ' * rand(16)) if rand(100) == 100
  pivot = rand(20)
  ws = case pivot
       when 1..5 then ws - (2 * pivot)
       when 6..8 then ws + 2
       else ws
       end
  ws = 0 if ws < 0
  text = 'x'
  (0..rand(80)).each do
    text += case rand(100)
            when 1..10 then ' '
            when 11..80 then alpha[rand(alpha.length)]
            when 81..95 then (rand(94) + 32).chr
            else # Put some random unicode in there...
              txt2 = ''
              (1..3).each { arr = [rand(0xffff)]; txt2 += arr.pack("S")  }
              Iconv.conv("UTF8//IGNORE", "UNICODE", txt2)
            end
  end
  puts((' ' * ws) + text)
end
