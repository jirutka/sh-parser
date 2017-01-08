---------
-- General utility functions.

local type = type


local M = {}

M.LUA_V = _VERSION:sub(-3)

--- Asserts that the given argument is of the correct type.
--
-- @tparam number n The argument index.
-- @param value The value to assert.
-- @tparam string ttype The expected type.
-- @raise if the argument n is not the correct type.
function M.assert_arg(n, value, ttype)
  if type(value) ~= ttype then
    error(("bad argument #%d: expected a '%s', got a '%s'"):format(n, ttype, type(value)), 2)
  end
end

--- Returns true if the given str is all upper case.
function M.is_upper (str)
  return str == str:upper()
end

return M
