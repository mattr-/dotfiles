local ls = require("luasnip")

-- local sn = ls.sn
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
-- local d = ls.dynamic_node
-- local c = ls.choice_node

local normal_map_snippet = function()
  return
    s({ trig = "nmap", desc = "custom normal map"}, {
      t({"mappings.nmap(", ""}),
      t('"'), i(1, "trigger"), t({'",', ""}),
      t('"'), i(2, "mapping"), t({'",', ""}),
      t({"opts,", ""}),
      t('"'), i(3, "group"), t({'",', ""}),
      t('"'), i(4, "map_name"), t({'",', ""}),
      t('"'), i(5, "Map Description"), t({'"',")"}),
    })
end

ls.add_snippets('lua', {
  normal_map_snippet()
})
