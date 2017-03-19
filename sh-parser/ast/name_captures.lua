---------
-- Captures converter

local utils = require 'sh-parser.utils'

local is_string = utils.is_string

local mapping = {
  Program = 'body',
  CompoundList = 'cmds',
  SequentialList = 'cmds',
  AndList = 'cmds',
  OrList = 'cmds',
  Not = 'cmd',
  PipeSequence = 'cmds',
  SimpleCommand = { 'prefix', 'cmd', 'suffix' },
  BraceGroup = { 'body', 'redirs' },
  Subshell = { 'body', 'redirs' },
  If = { 'clauses', 'redirs' },
  IfClause = { 'cond', 'body' },
  ElifClause = { 'cond', 'body' },
  ElseClause = { 'body' },
  For = { 'var', 'items', 'body', 'redirs' },
  Case = { 'var', 'cases', 'redirs' },
  CaseItem = { 'pattern', 'body' },
  While = { 'cond', 'body', 'redirs' },
  Until = { 'cond', 'body', 'redirs' },
  FunctionDef = { 'name', 'body', 'redirs' },
  RedirectFile = { 'fd', 'op', 'file' },
  RedirectHereDoc = { 'fd', 'op', 'delimiter', 'content' },
  HereDocContent = { 'content' },
  Assignments = { 'modifier', 'assignments' },
  Assignment = { 'name', 'value' },
  Name = { 'text' },
  Word = 'content',
  ArithmeticExpansion = { 'text' },
  ParameterExpansion = { 'op_pre', 'param', 'op_in', 'word' },
  CommandSubstitution = 'cmds',
  Comment = { 'text' },
}


--- Converts the given list of positional captures from the specified grammar
-- rule into a map of named captures.
--
-- @function __call
-- @tparam string ttype Type of the grammar rule / AST node.
-- @tparam table captures A list of the captures.
-- @treturn table A new map with the captures under keys.
return function (ttype, captures)
  local keys = mapping[ttype]
  local t = {}

  if keys then
    if is_string(keys) then
      t[keys] = captures
    else
      for i, k in ipairs(keys) do
        t[k] = captures[i]
      end
    end
  else
    t.children = captures
  end

  return t
end
