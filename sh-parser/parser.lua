---------
-- Parser of POSIX shell

local lpeg    = require 'lpeg'
local grammar = require 'sh-parser.grammar'


local function inject_tracing (grammar)  --luacheck: ignore 431
  local ok, pegdebug = pcall(require, 'pegdebug')
  if not ok then
    error('You must install Lua module pegdebug to use trace mode!')
  end

  return pegdebug.trace(grammar)
end

-- XXX: temporary
local function create_node (name, captures, start_pos, end_pos, subject) --luacheck: no unused
  return { tag = name, children = captures }
end


local M = {}

--- Parses the given shell script into AST.
--
-- @tparam string input The script to parse.
-- @tparam table|nil opts Options.
-- @treturn ASTNode
function M.parse (input, opts)
  opts = opts or {}

  -- TODO: cache initialized parser
  local parser = opts.trace
    and lpeg.P(inject_tracing(grammar.build()))
    or lpeg.P(grammar.build())

  local ast = parser:match(input, 1, create_node, input, {})
  if not ast then
    return nil
  end

  return ast
end

return M
