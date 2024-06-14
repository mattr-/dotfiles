local ls = require("luasnip")

local sn = ls.sn
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local d = ls.dynamic_node
local c = ls.choice_node
local r = ls.restore_node

ls.add_snippets("ruby", {
  s({ trig = "fsl", desc = "frozen_string_literal: true sigil" }, {
   t("frozen_string_literal: true")
  }),
})
