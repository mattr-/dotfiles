local colors = require("mattr-.theme.colors")
local theme = {}

theme.loadSyntax = function()
  -- Set up the old style syntax groups
  local syntaxGroups = {
    Comment        = { fg = colors.comment_grey }, -- any comment
    Constant       = { fg = colors.cyan }, -- any constant
    String         = { fg = colors.green }, -- a string constant: "this is a string"
    Character      = { fg = colors.green }, -- a character constant: 'c', '\n'
    Number         = { fg = colors.dark_yellow }, -- a number constant: 234, 0xff
    Boolean        = { fg = colors.dark_yellow }, -- a boolean constant: TRUE, false
    Float          = { fg = colors.dark_yellow }, -- a floating point constant: 2.3e10
    Identifier     = { fg = colors.red }, -- any variable name
    Function       = { fg = colors.yellow }, -- function name (also: methods for classes)
    Statement      = { fg = colors.purple }, -- any statement
    Conditional    = { fg = colors.purple }, -- if, then, else, endif, switch, etc.
    Repeat         = { fg = colors.purple }, -- for, do, while, etc.
    Label          = { fg = colors.purple }, -- case, default, etc.
    Operator       = { fg = colors.blue }, -- sizeof", "+", "*", etc.
    Keyword        = { fg = colors.red }, -- any other keyword
    Exception      = { fg = colors.purple }, -- try, catch, throw
    PreProc        = { fg = colors.yellow }, -- generic Preprocessor
    Include        = { fg = colors.blue }, -- preprocessor #include
    Define         = { fg = colors.purple }, -- preprocessor #define
    Macro          = { fg = colors.purple }, -- same as Define
    PreCondit      = { fg = colors.yellow }, -- preprocessor #if, #else, #endif, etc.
    Type           = { fg = colors.yellow }, -- int, long, char, etc.
    StorageClass   = { fg = colors.yellow }, -- static, register, volatile, etc.
    Structure      = { fg = colors.yellow }, -- struct, union, enum, etc.
    Typedef        = { fg = colors.yellow }, -- A typedef
    Special        = { fg = colors.blue }, -- any special symbol
    SpecialChar    = {}, -- special character in a constant
    Tag            = {}, -- you can use CTRL-] on this
    Delimiter      = {}, -- character that needs attention
    SpecialComment = {}, -- special things inside a comment
    Debug          = {}, -- debugging statements
    Underlined     = {}, -- text that stands out, HTML links
    Ignore         = {}, -- left blank, hidden
    Error          = { fg = colors.red }, -- any erroneous construct
    Todo           = { fg = colors.purple }, -- anything that needs extra attention; mostly the keywords TODO FIXME and XXX
  }
  return syntaxGroups
end

theme.loadEditor = function()
  -- Editor Highlight Groups
  local editorSyntax = {
    ColorColumn  = { bg = colors.cursor_grey }, -- used for the columns set with 'colorcolumn'
    --Conceal      = {}, -- placeholder characters substituted for concealed text (see 'conceallevel')
    Cursor       = { fg = colors.black, bg = colors.blue }, -- the character under the cursor
    --CursorIM     = {}, -- like Cursor, but used when in IME mode
    CursorColumn = { bg = colors.cursor_grey }, -- the screen column that the cursor is in when 'cursorcolumn' is set
    CursorLine   = { bg = colors.black }, -- the screen line that the cursor is in when 'cursorline' is set
    Directory    = { fg = colors.blue }, -- directory names (and other special names in listings)
    DiffAdd      = { fg = colors.green }, -- diff mode: Added line
    DiffChange   = { fg = colors.dark_yellow }, -- diff mode: Changed line
    DiffDelete   = { fg = colors.red }, -- diff mode: Deleted line
    DiffText     = { fg = colors.blue }, -- diff mode: Changed text within a changed line
    ErrorMsg     = { fg = colors.white, bg = colors.dark_red }, -- error messages on the command line
    VertSplit    = { fg = colors.text }, -- the column separating vertically split windows
    Folded       = { fg = colors.comment_grey }, -- line used for closed folds
    --FoldColumn   = {}, -- 'foldcolumn'
    SignColumn   = { bg = colors.black }, -- column where signs are displayed
    IncSearch    = { fg = colors.black, bg = colors.yellow }, -- 'incsearch' highlighting; also used for the text replaced with ":s///c"
    LineNr       = { fg = colors.gutter_fg_grey }, -- Line number for ":number" and ":#" commands, and when 'number' or 'relativenumber' option is set.
    CursorLineNr = { fg = colors.yellow }, -- Like LineNr when 'cursorline' or 'relativenumber' is set for the cursor line.
    MatchParen   = { fg = colors.blue, style = "underline" }, -- The character under the cursor or just before it, if it is a paired bracket, and its match.
    ModeMsg      = { fg = colors.text }, -- 'showmode' message (e.g., "-- INSERT --")
    --MoreMsg      = {}, -- more-prompt
    NonText      = { fg = colors.special_grey }, -- '~' and '@' at the end of the window, characters from 'showbreak' and other characters that do not really exist in the text (e.g., ">" displayed when a double-wide character doesn't fit at the end of the line).
    Normal       = { fg = colors.text, bg = colors.black }, -- normal text
    Pmenu        = { bg = colors.menu_grey, fg = colors.text }, -- Popup menu: normal item.
    PmenuSel     = { bg = colors.black, fg = colors.white }, -- Popup menu: selected item.
    PmenuSbar    = { bg = colors.special_grey }, -- Popup menu: scrollbar.
    PmenuThumb   = { bg = colors.white }, -- Popup menu: Thumb of the scrollbar.
    Question     = { fg = colors.purple }, -- hit-enter prompt and yes/no questions
    Search       = { fg = colors.black, bg = colors.yellow }, -- Last search pattern highlighting (see 'hlsearch'). Also used for highlighting the current line in the quickfix window and similar items that need to stand out.
    SpecialKey   = { fg = colors.gutter_fg_grey }, -- Meta and special keys listed with ":map", also for text used to show unprintable characters in the text, 'listchars'. Generally: text that is displayed differently from what it really is.
    SpellBad     = { fg = colors.red, style = "underline" }, -- Word that is not recognized by the spellchecker. This will be combined with the highlighting used otherwise.
    SpellCap     = { fg = colors.dark_yellow }, -- Word that should start with a capital. This will be combined with the highlighting used otherwise.
    SpellLocal   = { fg = colors.dark_yellow }, -- Word that is recognized by the spellchecker as one that is used in another region. This will be combined with the highlighting used otherwise.
    SpellRare    = { fg = colors.dark_yellow }, -- Word that is recognized by the spellchecker as one that is hardly ever used. spell This will be combined with the highlighting used otherwise.
    StatusLine   = { fg = colors.white, bg = colors.cursor_grey }, -- status line of current window
    StatusLineNC = { fg = colors.comment_grey }, -- status lines of not-current windows Note: if this is equal to "StatusLine" Vim will use "^^^" in the status line of the current window.
    TabLine      = { fg = colors.text }, -- tab pages line, not active tab page label
    TabLineFill  = { bg = colors.black }, -- tab pages line, where there are no labels
    TabLineSel   = { fg = colors.green }, -- tab pages line, active tab page label
    Title        = { fg = colors.green }, -- titles for output from ":set all", ":autocmd" etc.
    Visual       = { fg = colors.visual_black, bg = colors.visual_grey }, -- Visual mode selection
    VisualNOS    = { bg = colors.visual_grey }, -- Visual mode selection when vim is "Not Owning the Selection". Only X11 Gui's gui-x11 and xterm-clipboard supports this.
    WarningMsg   = { fg = colors.red }, -- warning messages
    --WildMenu     = {}, -- current match in 'wildmenu' completion
  }

  return editorSyntax
end

theme.loadCustomColors = function ()
  local custom_colors = {
    ActiveTerminal  = { bg = colors.ansi_black, fg = colors.ansi_grey },
    HacktheboxBlue  = { fg = colors.blue },
    HacktheboxGreen = { fg = colors.green },
    HacktheboxOrange = { fg = colors.dark_yellow },
  }

  return custom_colors
end

theme.loadTreeSitter = function()
  -- TreeSitter highlight groups

  local treesitter = {

    TSMethod               = { fg = colors.blue },    -- For method calls and definitions.
    TSAnnotation           = { fg = colors.red },    -- For C++/Dart attributes, annotations that can be attached to the code to denote some kind of meta information.
    TSAttribute            = { fg = colors.yellow },    -- (unstable) TODO: docs
    TSBoolean              = { fg = colors.dark_yellow },    -- For booleans.
    TSCharacter            = { fg = colors.green },    -- For characters.
    TSClass                = { fg = colors.purple },
    TSComment              = { fg = colors.comment_grey },    -- For comment blocks.
    TSConditional          = { fg = colors.purple },    -- For keywords related to conditionnals.
    TSConstructor          = { fg = colors.text }, -- For constructor calls and definitions: `= { }` in Lua, and Java constructors.
    TSConstant             = { fg = colors.text },    -- For constants
    TSConstBuiltin         = { fg = colors.cyan },    -- For constant that are built in the language: `nil` in Lua.
    TSConstMacro           = { fg = colors.cyan },    -- For constants that are defined by macros: `NULL` in C.
    TSError                = { fg = colors.red },    -- For syntax/parser errors.
    TSException            = { fg = colors.purple },    -- For exception related keywords.
    TSField                = { fg = colors.text }, -- For fields.
    TSFloat                = { fg = colors.blue },    -- For floats.
    TSFunction             = { fg = colors.yellow },    -- For function (calls and definitions).
    TSFuncBuiltin          = { fg = colors.yellow },    -- For builtin functions: `table.insert` in Lua.
    TSFuncMacro            = { fg = colors.blue },    -- For macro defined fuctions (calls and definitions): each `macro_rules` in Rust.
    TSInclude              = { fg = colors.cyan },    -- For includes: `#include` in C, `use` or `extern crate` in Rust, or `require` in Lua.
    TSKeyword              = { fg = colors.red }, -- For keywords that don't fall in previous categories.
    TSKeywordFunction      = { fg = colors.purple }, -- For keywords used to define a function.
    TSLabel                = { fg = colors.red }, -- For labels: `label:` in C and `:label:` in Lua.
    TSNamespace            = { fg = colors.yellow },    -- For identifiers referring to modules and namespaces.
    TSNone                 = { fg = colors.text },    -- TODO: docs
    TSNumber               = { fg = colors.blue },    -- For all numbers
    TSOperator             = { fg = colors.red }, -- For any operator: `+`, but also `->` and `*` in C.
    TSParameter            = { fg = colors.blue }, -- For parameters of a function.
    --TSParameterReference = { fg = colors.paleblue },    -- For references to parameters of a function.
    TSProperty             = { fg = colors.text }, -- Same as `TSField`.
    TSPunctDelimiter       = { fg = colors.text }, -- For delimiters ie: `.`
    TSPunctBracket         = { fg = colors.cyan }, -- For brackets and parens.
    TSPunctSpecial         = { fg = colors.cyan }, -- For special punctutation that does not fall in the catagories before.
    TSRepeat               = { fg = colors.cyan },    -- For keywords related to loops.
    TSString               = { fg = colors.green },    -- For strings.
    TSStringRegex          = { fg = colors.blue }, -- For regexes.
    --TSStringEscape       = { fg = colors.disabled }, -- For escape characters within a string.
    TSSymbol               = { fg = colors.blue },    -- For identifiers referring to symbols or atoms.
    TSType                 = { fg = colors.red },    -- For types.
    TSTypeBuiltin          = { fg = colors.yellow },    -- For builtin types.
    TSTag                  = { fg = colors.red },    -- Tags like html tag names.
    TSTagDelimiter         = { fg = colors.yellow },    -- Tag delimiter like `<` `>` `/`
    TSText                 = { fg = colors.text },    -- For strings considered text in a markup language.
    TSTextReference        = { fg = colors.yellow }, -- FIXME
    --TSEmphasis           = { fg = colors.paleblue },    -- For text to be represented with emphasis.
    --TSUnderline          = { fg = colors.fg, bg = colors.none, style = 'underline' },    -- For text to be represented with an underline.
    TSStrike               = { },    -- For strikethrough text.
    --TSTitle              = { fg = colors.paleblue, bg = colors.none, style = 'bold' },    -- Text that is part of a title.
    TSLiteral              = { fg = colors.text },    -- Literal text.
    TSVariable             = { fg = colors.text }, -- Any variable name that does not have another highlight.
    TSVariableBuiltin      = { fg = colors.gray }, -- Variable names that are defined by the languages, like `this` or `self`.
    --TSURI                = { fg = colors.link },    -- Any URI like a link or email.
  }


  return treesitter

end

theme.loadLSP = function ()
  -- Lsp highlight groups

 local lsp = {
   DiagnosticDefaultError           = { fg = colors.dark_red }, -- used for "Error" diagnostic virtual text
   DiagnosticSignError              = { fg = colors.dark_red }, -- used for "Error" diagnostic signs in sign column
   DiagnosticFloatingError          = { fg = colors.dark_red }, -- used for "Error" diagnostic messages in the diagnostics float
   DiagnosticVirtualTextError       = { fg = colors.dark_red }, -- Virtual text "Error"
   DiagnosticUnderlineError         = { style = 'undercurl', sp = colors.dark_red }, -- used to underline "Error" diagnostics.
   DiagnosticDefaultWarning         = { fg = colors.yellow }, -- used for "Warning" diagnostic signs in sign column
   DiagnosticSignWarning            = { fg = colors.yellow }, -- used for "Warning" diagnostic signs in sign column
   DiagnosticFloatingWarning        = { fg = colors.yellow }, -- used for "Warning" diagnostic messages in the diagnostics float
   DiagnosticVirtualTextWarning     = { fg = colors.yellow }, -- Virtual text "Warning"
   DiagnosticUnderlineWarning       = { style = 'undercurl', sp = colors.dark_yellow }, -- used to underline "Warning" diagnostics.
   DiagnosticDefaultInformation     = { fg = colors.blue }, -- used for "Information" diagnostic virtual text
   DiagnosticSignInformation        = { fg = colors.blue },  -- used for "Information" diagnostic signs in sign column
   DiagnosticFloatingInformation    = { fg = colors.blue }, -- used for "Information" diagnostic messages in the diagnostics float
   DiagnosticVirtualTextInformation = { fg = colors.blue }, -- Virtual text "Information"
   DiagnosticUnderlineInformation   = { style = 'undercurl', sp = colors.paleblue }, -- used to underline "Information" diagnostics.
   DiagnosticDefaultHint            = { fg = colors.comment_grey },  -- used for "Hint" diagnostic virtual text
   DiagnosticSignHint               = { fg = colors.comment_grey }, -- used for "Hint" diagnostic signs in sign column
   DiagnosticFloatingHint           = { fg = colors.comment_grey }, -- used for "Hint" diagnostic messages in the diagnostics float
   DiagnosticVirtualTextHint        = { fg = colors.comment_grey }, -- Virtual text "Hint"
   DiagnosticUnderlineHint          = { style = 'undercurl', sp = colors.dark_yellow }, -- used to underline "Hint" diagnostics.
   LspReferenceText                 = { fg = colors.cyan, bg = colors.cursor_grey }, -- used for highlighting "text" references
   LspReferenceRead                 = { fg = colors.cyan, bg = colors.cursor_grey }, -- used for highlighting "read" references
   LspReferenceWrite                = { fg = colors.cyan, bg = colors.cursor_grey }, -- used for highlighting "write" references
 }

  return lsp

end

theme.loadPlugins = function()

  local plugins = {
    --nvim-hlslens highlight groups
    HlSearchNear = { fg = colors.black, bg = colors.text },
    HlSearchLens = { fg = colors.black, bg = colors.text },
    HlSearchLensNear = { fg = colors.black, bg = colors.text },
    HlSearchFloat = { fg = colors.black, bg = colors.text },

  }

  return plugins
end

return theme
