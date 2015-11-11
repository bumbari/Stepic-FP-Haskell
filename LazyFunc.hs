foo a = a

bar = const foo

baz x = const True

quux = let x = x in x

corge = 10

grault x 0 = x
grault x y = x

garply = grault 'q'

waldo = foo