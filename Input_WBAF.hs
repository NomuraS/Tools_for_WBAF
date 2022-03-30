module Input_WBAF
where
import WBAF

input_arg=[
  "a","b","c","d","e","f","g",
  "h","h1","i","i1"
  ,"j","j1","k","k1"
  ,"l","l1","m","m1"
              ]


input_att = [
                ("m","d"),
                ("l","m"),
                ("j","i"),
                ("k","j")
                ]

input_sup = [
                (["b","c"],"a"),
                (["e","d"],"b"),
                (["f"],"c"),
                (["i"],"c"),
                (["h"],"e"),
                (["g"],"d"),
                (["m1"],"m"),
                (["l1"],"l"),
                (["j1"],"j"),
                (["k1"],"k")
                ]


input_weight x = case x of 
  "h"->2
  "m1"->2
  "c"->3
  "f"->3
  "l1"->3
  "g"->4
  "e"->4
  "i"->4
  "d"->5
  otherwise -> 1

input_WBAF :: WBAF String
input_WBAF = 
    WBAF input_arg input_att input_sup input_weight



-- module Input_WBAF
-- where
-- import WBAF

-- input_arg=[
--   "a","b","c","d","e","f","g",
--   "h","i"
--   ,"j","k"
--               ]

-- input_att = [
--                 ("h","g"),
--                 ("g","f"),
--                 ("f","e"),
--                 ("e","d"),
--                 ("d","c"),
--                 ("c","b"),
--                 ("b","a"),
--                 ("k","j"),
--                 ("j","e"),
--                 ("i","d")
--                 ]


-- input_sup = [
--                 ]

-- input_weight x = case x of 
--   "f"->2
--   "g"->4
--   "h"->3
--   otherwise -> 1

-- input_WBAF :: WBAF String
-- input_WBAF = 
--     WBAF input_arg input_att input_sup input_weight

