---------
-- Parser of POSIX shell

local lpeg       = require 'lpeg'
local simple_ast = require 'sh-parser.ast.simple'
local grammar    = require 'sh-parser.grammar'


local function inject_tracing (grammar)  --luacheck: ignore 431
  local ok, pegdebug = pcall(require, 'pegdebug')
  if not ok then
    error('You must install Lua module pegdebug to use trace mode!')
  end

  return pegdebug.trace(grammar)
end


local M = {}

--- Parses the given shell script into AST.
--
-- @tparam string input The script to parse.
-- @tparam table|nil opts A map of options.
-- @treturn A root node.
function M.parse (input, opts)
  opts = opts or {}

  local gr = grammar.build()

  -- TODO: cache initialized parser
  local parser = opts.trace
    and lpeg.P(inject_tracing(gr))
    or lpeg.P(gr)

  local create_node = simple_ast(input, opts)

  return parser:match(input, 1, create_node, input, {})
end

return M
