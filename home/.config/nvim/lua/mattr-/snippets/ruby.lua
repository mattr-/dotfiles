local ls = require "luasnip"

-- local snippet = ls.s
local sn = ls.sn
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local d = ls.dynamic_node
local c = ls.choice_node
local r = ls.restore_node

return {
  s({ trig = "def", desc = "method definition"}, {
    t("def "),
    c(1, {
        r(1, "method_name", i(1)),
        sn(nil, {
          t("self."),
          r(1, "method_name", i(1))
        }),
    }),
    i(2),
    c(3, {
      t(""),
      sn(nil, {
        t("("),
        i(1),
        t(")")
      }),
    }),
    t({"", "\t"}),
    i(0),
    t({"", "end"}),
  }),
}

