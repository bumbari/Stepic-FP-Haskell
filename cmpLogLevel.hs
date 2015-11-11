data LogLevel = Error | Warning | Info

cmp :: LogLevel -> LogLevel -> Ordering
cmp Info Info = EQ
cmp Error Error = EQ
cmp Warning Warning = EQ
cmp Info _ = LT
cmp Error _ = GT
cmp Warning Error = LT
cmp Warning _ = GT