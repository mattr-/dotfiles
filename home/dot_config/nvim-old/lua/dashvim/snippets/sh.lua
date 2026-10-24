local ls = require("luasnip")

local s = ls.snippet
local i = ls.insert_node
local t = ls.text_node
local fmt = require("luasnip.extras.fmt").fmt

ls.add_snippets("sh", {
  s("getopt", fmt([[
   while getopt "{optstring}" opt; do
     case $opt in
       {cases}
       *)
         echo "Usage: $0 [options]" >&2
         exit 1
         ;;
     esac
   done
   shift $((OPTIND - 1))
   ]], {
      optstring = i(1, "hv"),
      cases     = i(0),
    })),

  -- A single case arm.  Only expands when the cursor is inside a live
  -- "getopts" snippet, so it won't fire anywhere else in the file.
  -- i(1) = the flag letter (e.g. "v")
  -- i(2) = body of the case arm
  -- i(0) = final cursor position
  s({ trig = "opt",
    condition = DashVim.util.luasnip.inside("getopts"),
    show_condition = DashVim.util.luasnip.inside("getopts") },
    {
      i(1, "x"),
      t({ ")", "\t" }),
      i(0, ": # handle -x"),
      t({ "", "\t;;" }),
    })
})
