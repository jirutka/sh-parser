---------
-- General utility functions.

local fun = require 'sh-parser.fun_ext'

local insert = table.insert
local ipairs = ipairs
local map    = fun.map
local type   = type

-- unpack is not global since Lua 5.3
local unpack = table.unpack or unpack  --luacheck: std lua51


--- Calls the function `func` with the given arguments. This is equivalent to:
--
--     func(unpack(args), ...)
--
-- but in a form that can be highly optimized by LuaJIT (~20x faster) when
-- called with less than 4 arguments in the `args` table. If `#args > 3`, then
-- it fallbacks to `unpack` (that is not JIT-compiled in LuaJIT 2.0).
local function call (func, args, ...)
  local n = #args

  if n == 1 then
    return func(args[1], ...)
  elseif n == 2 then
    return func(args[1], args[2], ...)
  elseif n == 3 then
    return func(args[1], args[2], args[3], ...)
  else
    return func(unpack(args), ...)
  end
end


local M = {}

M.LUA_V = _VERSION:sub(-3)

--- Returns a function that always returns the given `value`.
function M.always (value)
  return function()
    return value
  end
end

--- Asserts that the given argument is of the correct type.
--
-- @tparam number n The argument index.
-- @param value The value to assert.
-- @tparam string ttype The expected type.
-- @raise if the argument n is not the correct type.
function M.assert_arg (n, value, ttype)
  if type(value) ~= ttype then
    error(("bad argument #%d: expected a '%s', got a '%s'")
          :format(n, ttype, type(value)), 2)
  end
end

--- Inserts items from the `src` list at the end of the `dest` list and returns
-- modified `dest` (i.e. it modifies it in-place!).
--
-- @tparam table dest The destination list to extend.
-- @tparam table src The source list to take items from.
-- @treturn table The given `dest` list.
function M.extend (dest, src)
  for _, item in ipairs(src) do
    insert(dest, item)
  end
  return dest
end

--- Returns true if the given str is all upper case.
function M.is_upper (str)
  return str == str:upper()
end

--- Partial application.
-- Takes a function `fn` and arguments, and returns a function *fn2*.
-- When applied, *fn2* returns the result of applying `fn` to the arguments
-- provided initially followed by the arguments provided to *fn2*.
--
-- @tparam function fn
-- @param ... Arguments to pass to the `fn`.
-- @treturn function A partially applied function.
function M.partial (fn, ...)
  local args1 = {...}

  return function(...)
    return call(fn, args1, ...)
  end
end

--- Inserts `value` at the front of the list, moving other elements upwards.
-- This is just a shortcut for `table.insert(list, 1, value)`.
function M.unshift (list, value)
  insert(list, 1, value)
end

--- Returns values from the given table.
--
-- @function values
-- @tparam table tab
-- @treturn table A list of values.
M.values = M.partial(map, function(_, v) return v end)

return M
