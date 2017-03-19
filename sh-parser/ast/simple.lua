---------
-- Simple AST builder
--
-- Builds simple AST suitable for encoding into JSON.
----
local name_captures  = require 'sh-parser.ast.name_captures'

local push = table.insert


--- Creates a function that transforms positional captures produced by the
-- shell parser into AST nodes.
--
-- **Options**:
--
-- * `comments`: Include comments.
-- * `loc`:      Include nodes' location as start and end absolute position in
--               the `subject`.
-- * `source`:   Include raw source in all nodes except the root node.
--
-- @function __call
-- @tparam string subject The parsed input.
-- @tparam {[string]=bool,...} opts The options map.
-- @return `create_node`
return function (subject, opts)
  opts = opts or {}
  local comments = opts.comments and {} or nil
  local with_loc = opts.loc or false
  local with_source = opts.source or false

  --- Creates a new AST node.
  --
  -- @tparam string node_type The node type.
  -- @tparam table captures A list of captures.
  -- @tparam int start_pos Start position in the parsed input.
  -- @tparam int end_pos End position in the parsed input.
  -- @treturn table|nil An AST node, or nil.
  local function create_node (node_type, captures, start_pos, end_pos)

    local node = name_captures(node_type, captures)
    node.type = node_type

    if with_loc then
      node.loc = {
        ['start'] = start_pos,
        ['end'] = end_pos,
      }
    end

    if with_source and node_type ~= 'Program' then
      node.source = subject:sub(start_pos, end_pos)
    end

    if comments ~= nil then
      if node_type == 'Comment' then
        push(comments, node)
        return nil
      elseif node_type == 'Program' then
        node.comments = comments
      end
    end

    return node
  end

  return create_node
end
