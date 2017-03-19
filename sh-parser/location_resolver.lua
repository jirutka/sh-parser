---------
-- Location resolver
--
-- Resolves row (line) and column from position in multiline string.
----
local byte = string.byte
local floor = math.floor
local inf   = math.huge

local NL = byte('\n')


--- Returns a list with starting positions of lines in the given `text` string.
-- Note: only LF (`\n`) is considered as a line separator.
--
-- @tparam string text The string to index.
-- @treturn {int,...} A list of numbers, where index corresponds to a line
--   number and value to a position inside `text` where the line starts.
local function index_lines_pos (text)
  text = '\n'..text

  local t, n = {}, 0
  for i=1, #text do
    if byte(text, i) == NL then
      n = n + 1
      t[n] = i
    end
  end

  return t
end

--- Returns an index of item in the given sorted `list` that is largest from
-- the items smaller or equal to the specified numeric `value`.
-- This implementation uses modified binary search algorithm.
--
-- @tparam {number,...} list A sorted list of numbers.
-- @tparam number value The reference value.
-- @tparam ?int start_idx (default is 1)
-- @tparam ?int end_idx (default is `#list`)
-- @treturn number|nil An index of the item closest to the `value`, or nil if
--   not found.
local function find_nearest_lower (list, value, start_idx, end_idx)
  local low, high = start_idx or 1, end_idx or #list  -- FIXME do not calculate list size each time
  local mid_val

  while low <= high do
    local mid = floor((low + high) / 2)
    mid_val = list[mid]

    if value < mid_val then
      high = mid - 1
    elseif value == mid_val
        or value > mid_val and value < (list[mid + 1] or inf) then
      return mid
    else
      low = mid + 1
    end
  end
end

local function resolve_row_col (lines_index, pos, skip_lines)
  local row = find_nearest_lower(lines_index, pos, skip_lines + 1)
  local col = pos - lines_index[row] + 1

  return row, col
end


--- Initializes the `resolve` function for the given `text`.
--
-- **Type signature:** `string -> int -> int, int, int`
--
-- @function __call
-- @tparam string text
-- @return `resolve`
return function (text)
  -- TODO: try to optimize it, it's quite slow.
  local lines_index
  local last_pos, last_row = 1, 1

  ---
  -- @tparam int pos The position to resolve.
  -- @treturn int A 1-based row (line) number.
  -- @treturn int A 1-based column number.
  -- @treturn int The given `pos`.
  local function resolve (pos)
    if lines_index == nil then
      lines_index = index_lines_pos(text, #text)
    end
    -- Optimize lookup of next position using result of the previous lookup.
    local skip_lines = pos >= last_pos and last_row - 1 or 0
    local row, col = resolve_row_col(lines_index, pos, skip_lines)

    last_pos, last_row = pos, row

    return row, col, pos
  end

  return resolve
end
