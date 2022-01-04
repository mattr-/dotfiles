local ls = require "luasnip"

-- local snippet = ls.s
local sn = ls.sn
local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local d = ls.dynamic_node
local c = ls.choice_node

-- Adapt the endless list latex snippet from https://github.com/L3MON4D3/LuaSnip/wiki/Cool-Snippets#latex---endless-list
-- for Markdown
local rec_ul
rec_ul = function()
  return sn(nil, {
    c(1, {
      -- important!! Having the sn(...) as the first choice will cause infinite recursion
      t({""}),
      -- The same dynamicNode as in the snippet below (also note: self reference)
      sn(nil, {t({"", "\t - "}), i(1), d(2, rec_ul, {})})
    }),
  });
end

local rec_ol
rec_ol = function()
  return sn(nil, {
    c(1, {
      -- important!! Having the sn(...) as the first choice will cause infinite recursion
      t({""}),
      -- The same dynamicNode as in the snippet below (also note: self reference)
      sn(nil, {t({"", "\t 1. "}), i(1), d(2, rec_ol, {})})
    }),
  });
end

return {
  s({ trig = "ul", desc = "unordered list"}, {
    t({"", "\t - "}), i(1), d(2, rec_ul, {}),
    i(0)
  }),
  s({ trig = "ol", desc = "ordered list"}, {
    t({"", "\t 1. "}), i(1), d(2, rec_ol, {}),
    i(0)
  })
}
