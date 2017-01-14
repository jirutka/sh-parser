---------
-- Syntactic sugar for building grammar with LPeg.

local lpeg  = require 'lpeg'

local utils = require 'sh-parser.utils'
local fun   = require 'sh-parser.fun_ext'

local iter       = fun.iter
local assert_arg = utils.assert_arg
local is_upper   = utils.is_upper
local lpeg_type  = lpeg.type
local LUA_V      = utils.LUA_V

local Cc = lpeg.Cc
local Cp = lpeg.Cp
local Ct = lpeg.Ct
local V  = lpeg.V


local F = {}

function F.on_define_rule (name, pattern, env)
  local name_init = name:sub(1, 1)

  if name_init ~= '_' and is_upper(name_init) then
    pattern = Cc(name) * Cp() * Ct(pattern) * Cp() / function(...)
      return env.create_ast_node(...)
    end
  end

  env.grammar[name] = pattern
end

function F.on_get_variable (name, env)
  env.used_vars[name] = (env.used_vars[name] or 0) + 1
  return V(name)
end

function F.on_complete (env)
  for name, cnt in pairs(env.used_vars) do
    if not env.grammar[name] then
      error(('Undefined non-terminal "%s" referenced %d times'):format(name, cnt))
    end
  end
end

function F.create_ast_node (rule_name, start_pos, children, end_pos)
  return { tag = rule_name, loc = { start_pos, end_pos }, children = children }
end


local M = {}

function M.build_grammar (func, defs, global_env)
  assert_arg(1, func, 'function')
  defs = defs or {}

  if not global_env then
    global_env = LUA_V == '5.1' and getfenv(func) or _G  --luacheck: std lua51
  end

  local env_index = iter(lpeg)
    :filter(function(k) return is_upper(k:sub(1, 1)) end)
    :chain(F, defs)
    :tomap()

  local init_defined = false

  local env
  env = setmetatable({
      grammar = {},
      used_vars = {},
    }, {
      __index = setmetatable(env_index, {
        __index = setmetatable(global_env, {
          __index = function(_, name)
            return env.on_get_variable(name, env)
          end
        })
      }),
      __newindex = function(tab, name, value)
        if lpeg_type(value) == 'pattern' then
          if not init_defined then
            env.grammar[1] = name
            init_defined = true
          end
          env.on_define_rule(name, value, env)
        else
          rawset(tab, name, value)
        end
      end
  })

  -- Call passed function with custom environment (5.1- and 5.2-style).
  if LUA_V == '5.1' then
    setfenv(func, env)  --luacheck: std lua51
  end
  func(env)

  env.on_complete(env)

  return assert(env.grammar)
end

return M
