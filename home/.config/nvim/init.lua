-- Impatient speeds up loading but we don't want to bail if it's not present
pcall(require, "impatient")

if require "mattr-.first_load"() then
  return
end

require "mattr-.plugins"
