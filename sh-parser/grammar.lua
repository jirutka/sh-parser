---------
-- LPeg grammar for POSIX shell

local lpeg       = require 'lpeg'
local fun        = require 'sh-parser.fun_ext'
local lpeg_sugar = require 'sh-parser.lpeg_sugar'
local utils      = require 'sh-parser.utils'

local build_grammar = lpeg_sugar.build_grammar
local chain         = fun.chain
local extend        = utils.extend
local iter          = fun.iter
local op            = fun.op
local par           = utils.partial
local unshift       = utils.unshift
local values        = utils.values

local B    = lpeg.B
local C    = lpeg.C
local Carg = lpeg.Carg
local Cb   = lpeg.Cb
local Cc   = lpeg.Cc
local Cf   = lpeg.Cf
local Cg   = lpeg.Cg
local Cp   = lpeg.Cp
local Cs   = lpeg.Cs
local P    = lpeg.P
local R    = lpeg.R
local S    = lpeg.S


-- Terminals
local ALPHA   = R('AZ', 'az')
local ANY     = P(1)
local BOF     = P(function(_, pos) return pos == 1 end)  -- Beginning Of File
local BQUOTE  = P('`')
local DIGIT   = R('09')
local DOLLAR  = P('$')
local DQUOTE  = P('"')
local EOF     = P(-1)    -- End Of File
local EQUALS  = P('=')
local ESC     = P('\\')  -- escape character
local HASH    = P('#')
local LBRACE  = P('{')
local LDPAREN = P('((')
local LF      = P('\n')
local LPAREN  = P('(')
local RBRACE  = P('}')
local RDPAREN = P('))')
local RPAREN  = P(')')
local SEMI    = P(';')
local SQUOTE  = P("'")
local WORD    = R('AZ', 'az', '09') + P('_')
local WSP     = S(' \t')

-- Shell operators containing single character.
local operators1 = {
  AND_OP       = '&',
  GREAT_OP     = '>',
  LESS_OP      = '<',
  LPAREN_OP    = '(',
  PIPE_OP      = '|',
  RPAREN_OP    = ')',
  SEMI_OP      = ';',
}
-- Shell operators containing more than one character.
local operators2 = {
  AND_IF_OP    = '&&',
  CLOBBER_OP   = '>|',
  DGREAT_OP    = '>>',
  DLESS_OP     = '<<',
  DLESSDASH_OP = '<<-',
  DSEMI_OP     = ';;',
  GREATAND_OP  = '>&',
  LESSAND_OP   = '<&',
  LESSGREAT_OP = '<>',
  OR_IF_OP     = '||',
}

-- Shell reserved words.
local reserved_words = {
  CASE      = 'case',
  DO        = 'do',
  DONE      = 'done',
  ELIF      = 'elif',
  ELSE      = 'else',
  ESAC      = 'esac',
  FI        = 'fi',
  FOR       = 'for',
  IF        = 'if',
  THEN      = 'then',
  UNTIL     = 'until',
  WHILE     = 'while',

  BANG_R    = '!',
  IN        = 'in',
  LBRACE_R  = '{',
  RBRACE_R  = '}',
}

-- Pattern for Special parameters.
local SPECIAL_PARAM = S('@*#?-$!0')

-- Pattern that matches any parameter expansion "operator" that may be used
-- between <parameter-name> and <word>.
local PARAM_EXP_OP = iter({
    ':-', '-', ':=', '=', ':?', '?', ':+', '+', '%%', '%', '##', '#', -- POSIX
    ':', '//', '/'  -- non-POSIX
  }):map(P):reduce(op.add, P(false))

-- Pattern that matches any character used in shell operators.
local OPERATOR_CHARS = values(operators1):map(P):reduce(op.add, P(false))

-- XXX: is this correct?
local WORD_BOUNDARY = S(' \t\n') + BOF + EOF + OPERATOR_CHARS

local reserved_words_patt = iter(reserved_words)
    :map(function(k, v) return k, P(v) * #WORD_BOUNDARY end)

-- Pattern that matches any shell reserved word.
-- XXX: sort them?
local RESERVED_WORD = values(reserved_words_patt):reduce(op.add, P(false))

-- Map of special terminal symbols (patterns).
local terminals = chain(
      iter(operators1):map(function(k, v)
          -- Ensure that operator x does not match xx when xx is valid operator.
          return k, values(operators2):index_of(v..v) and P(v) * -P(v) or P(v)
        end),
      iter(operators2):map(function(k, v)
          return k, P(v)
        end),
      reserved_words_patt
    ):tomap()


--- Creates a pattern that captures escaped `patt`.
--
-- @tparam lpeg.Pattern patt The pattern to escape.
-- @treturn lpeg.Pattern
local function escaped (patt)
  return patt == LF
      and ESC * patt / '' -- produce empty capture
      or ESC / '' * patt  -- omit escaping char from capture
end

--- Creates a pattern that captures any character, except the specified
-- patterns when not preceded by the escape character.
--
-- @usage any_except(P' ') --> escaped(P' ') + 1 - P' '
--
-- @tparam lpeg.Pattern ... The patterns to *not* capture.
-- @treturn lpeg.Pattern
local function any_except (...)
  local patts = iter({...})
  return patts:map(escaped):reduce(op.add, P(false))
       + patts:reduce(op.sub, ANY)
end

--- Transforms captures from *and\_or\_list* into left-associative tree of n-ary
-- nodes *AndList* and *OrList*.
--
-- This function is basically a workaround to create AST for left-associative
-- operators with the same precedence - `&&` and `||`.
--
-- @usage
--   subject = "a && b && c || d || e && f"
--   captures = { {a}, 2, "&&", {b}, 7, "&&", {c}, 12, "||",
--                {d}, 17, "||", {e}, 22, "&&", {f}, 27 }
--
--   capture_and_or(create_node, 1, captures, subject) --> Z
--     ~> create_node("AndList", 1, {a, b, c}, 12) --> X
--     ~> create_node("OrList", 1, {X, d, e}, 22) --> Y
--     ~> create_node("AndList", 1, {Y, f}, 27) --> Z
--
--    Z        Y       X
--   (AndList (OrList (AndList a b c) d e) f)
--
-- @tparam func create_node The function to be called to create AST nodes.
-- @tparam int start_pos Index of the first character of the captured substring.
-- @tparam table captures Table with shape `{table,int,string, table,int,string, ...}`.
--   Element *i* is table of children nodes (pipeline and optional comments),
--   *i + 1* is position of the end of the last child node (int), *i + 2* is
--   operator ("&&", or "||").
-- @tparam string subject The entire subject (i.e. input text).
-- @return Result of the last call of `create_node`.
local function capture_and_or (create_node, start_pos, captures, subject)
  local node_name = { ['&&'] = 'AndList', ['||'] = 'OrList' }
  local node, last_op
  local children = {}

  for i=1, #captures, 3 do
    local caps, end_pos, next_op = captures[i], captures[i + 1], captures[i + 2]

    extend(children, caps)

    if last_op and last_op ~= next_op then
      local name = assert(node_name[last_op], 'invalid operator '..last_op)
      node = create_node(name, children, start_pos, end_pos, subject)
      children = { node }
    end
    last_op = next_op
  end

  return node
end

--- Predicate function that matches start of the here-document's content and
-- returns the corresponding HereDocInfo table.
--
-- This match-time capture function is called by the parser each time when
-- a new line is consumed (see the rule *newline_list*).
--
-- @usage
--   Cg(Cmt(_heredocs_stack, find_heredoc), 'heredoc')
--
-- @tparam string _ The entire subject (unused).
-- @tparam int pos The current position.
-- @tparam {HereDocInfo,...} heredocs (see `capture_heredoc`)
-- @treturn[1] false (no match)
-- @treturn[2] true (match)
-- @treturn[2] HereDocInfo
local function find_heredoc (_, pos, heredocs)
  -- Heredocs list is ordered from latest to earliest to optimize this lookup.
  for _, heredoc in ipairs(heredocs) do
    local cont_start = heredoc.cont_start

    if pos == cont_start then
      return true, heredoc
    elseif pos > cont_start then
      return false
    end
  end

  return false
end

--- Captures here-document redirection.
--
-- @tparam bool strip_tabs Whether to strip leading tabs (for `<<-`).
-- @tparam string subject The entire subject (i.e. input text).
-- @tparam int pos The current position.
-- @tparam string delimiter The captured delimiter string.
-- @tparam bool quoted Is any character in the delimiter word quoted?
-- @tparam {HereDocInfo,...} heredocs The list of parsed here-document
--   redirections into which a new HereDocInfo will be added.
-- @treturn true Match and do not consume any input.
-- @treturn string The delimiter word.
-- @treturn table A "placeholder" for future content.
-- @treturn int ID of the here-document.
-- @raise Error if here-document is not terminated.
local function capture_heredoc (strip_tabs, subject, pos, delimiter, quoted, heredocs)
  local delim_pat = '\n'..(strip_tabs and '\t*' or '')
                        ..delimiter:gsub('%p', '%%%1')  -- escape puncatation chars
                        ..'\n'

  local nl_pos = subject:find('\n', pos, true)
  local cont_start = nl_pos and nl_pos + 1 or #subject

  local cont_end, delim_end = (subject..'\n'):find(delim_pat, nl_pos or #subject)
  if not cont_end then
    -- This is somehow valid in shell implementations, but we can't parse it.
    -- Since this is most likely an error in the script, just raise an error.
    error(('%d: Here-document with delimiter "%s" is not terminated'):format(pos, delimiter))
  end

  -- Skip overlapping heredocs (multiple heredoc redirects on the same line).
  while true do
    local _, entry = find_heredoc(nil, cont_start, heredocs)
    if entry then
      cont_start = entry.delim_end + 1
    else
      break
    end
  end

  local content = {}
  local id = #heredocs + 1

  --- @table HereDocInfo
  local heredoc = {
    id         = id,         -- int: ID of this heredoc.
    cont_start = cont_start, -- int: Position of the first character of the heredoc's content.
    cont_end   = cont_end,   -- int: Position of trailing newline of the heredoc's content.
                             -- It's `cont_start - 1` if there's no content (not even blank line)!
    delim_end  = delim_end,  -- int: Position of a newline after the closing delimiter.
    quoted     = quoted,     -- bool: false if word expansions should be parsed, true otherwise.
    content    = content,    -- table: An empty table that will be mutated into *HereDocContent*.
  }
  unshift(heredocs, heredoc)

  return true, delimiter, content, id
end

--- Predicate function that matches when the `pos` is inside the here-document
-- specified by the `heredoc`. It does not consume any input.
--
-- @usage
--   Cg(Cmt(_heredocs_stack, find_heredoc), 'heredoc')
--   Cmt(Cb'heredoc', inside_heredoc)
--
-- @tparam string _ The entire subject (unused).
-- @tparam int pos The current position.
-- @tparam HereDocInfo heredoc (see `capture_heredoc`)
-- @treturn bool Whether the `pos` is inside the here-document.
local function inside_heredoc (_, pos, heredoc)
  local cont_end = heredoc.cont_end

  if pos == heredoc.cont_start then
    return true
  end

  if pos > cont_end then
    return false
  elseif pos >= heredoc.cont_start then
    return true
  else
    return false
  end
end

--- Captures content of the specified quoted or empty here-document.
--
-- This function is used as a match-time capture to match and capture content
-- of a here-document with quoted delimiter (which means that the content is
-- not expanded).
--
-- @usage
--   Cg(Cmt(_heredocs_stack, find_heredoc), 'heredoc')
--   Cmt(Cb'heredoc', capture_nonexp_heredoc)
--
-- @tparam string subject The entire subject (i.e. input text).
-- @tparam int pos The current position.
-- @tparam HereDocInfo heredoc (see `capture_heredoc`)
-- @treturn[1] boolean true to match without consuming any input,
--   false to not match.
-- @treturn[2] int A new position (at the end of the content).
-- @treturn[2] string The here-document's content.
local function capture_nonexp_heredoc (subject, pos, heredoc)
  local cont_end = heredoc.cont_end

  -- If there's no content (e.g. `<<EOF\nEOF\n`), match without consuming any
  -- input and producing any capture;
  if cont_end == pos - 1 then
    return true
  -- if quoted heredoc (i.e. expansions should *not* be parsed) or the content
  -- is an empty line, consume and capture the content;
  elseif heredoc.quoted or cont_end - pos < 1 then
    return cont_end + 1, subject:sub(pos, cont_end - 1)
  -- else do not match.
  else
    return false
  end
end

local function heredoc_id (heredoc)
  return heredoc.id
end

--- Inject the given *HereDocContent* AST node into the "content placeholder"
-- that has been passed into the *RedirectHereDoc* node.
--
-- The problem here is that here-document's content may not immediatelly follow
-- the redirection (e.g. `<<EOF`); it begins after the next *newline* and there
-- may be anything between the redirection and the newline (e.g.
-- `cat <<EOF; echo "allons-y!"\nContent starts here...`).
--
-- The *RedirectHereDoc* rule produces capture with an empty table as a
-- "content placeholder". This "placeholder" is created in the
-- `capture_heredoc` function and its reference is stored in the `HereDocInfo`
-- table. This function is called after the *HereDocContent* is parsed
-- and produced. It gets the *HereDocContent* (content_node), finds the
-- `HereDocInfo` and copies all keys and metatable from the given
-- `content_node` into the "content placeholder" from the `HereDocInfo`.
--
-- Note that this is an ugly hack that abuses mutability of Lua tables.
--
-- @tparam int pos The current position.
-- @tparam table content_node The *HereDocContent* AST node.
-- @tparam {HereDocInfo,...} heredocs (see `capture_heredoc`)
-- @raise Error if heredoc is not found (should not happen).
local function inject_heredoc (pos, content_node, heredocs)
  local _, heredoc = find_heredoc(nil, pos, heredocs)
  if not heredoc then
    error 'invalid state, this should not happen'
  end

  local placeholder = heredoc.content
  for k, v in pairs(content_node) do
    placeholder[k] = v
  end
  setmetatable(placeholder, getmetatable(content_node))
end


-- Grammar to be processed by `lpeg_sugar`.
local function grammar (_ENV)  --luacheck: no unused args
  --luacheck: allow defined, ignore 113 131

  local _create_node    = Carg(1)  -- callback function for creating AST nodes
  local _subject        = Carg(2)  -- the input being parsed
  local _heredocs_stack = Carg(3)  -- support stack used for parsing heredocs

  local _  = ( WSP + ESC * LF )^0  -- optional whitespace(s)
  local __ = ( (ESC * LF)^0 * WSP )^1  -- at least one whitespace

  Program             = linebreak * ( complete_commands * linebreak )^-1 * EOF
  complete_commands   = complete_command * ( newline_list * complete_command )^0

  -----------------------------  Lists  -----------------------------
  -- Note: Anonymous Cg around named Cg is used only to exclude
  -- the named Cg from captures in AST.

  complete_command    = Cg( Cg(async_cmd, 'async_cmd') * ( CompleteCommand
                                                         + Cb'async_cmd' ) ) * separator_op^-1
  CompleteCommand     = Cb'async_cmd' * ( separator_op * -HASH * async_cmd )^1

  async_cmd           = Cg( Cg(and_or, 'and_or') * ( AsyncCommand
                                                   + Cb'and_or' ) )
  AsyncCommand        = Cb'and_or' * _ * AND_OP

  and_or              = Cg( Cg(Cp(), 'and_or_cp') * Cg(pipeline, 'pipeline') * ( and_or_list
                                                                               + Cb'pipeline' ) )
  and_or_list         = _create_node * Cb'and_or_cp' * Ct(
                          Ct( Cb'pipeline' ) * Cp()
                          * ( _ * and_or_op * Ct( linebreak * pipeline ) * Cp() )^1
                        ) * _subject / capture_and_or
  and_or_op           = C( AND_IF_OP + OR_IF_OP )

  compound_list       = linebreak
                        * Cg( Cg(async_cmd, 'async_cmd2') * ( CompoundList
                                                            + Cb'async_cmd2' ) )
                        * separator^-1
  CompoundList        = Cb'async_cmd2' * ( separator * async_cmd )^1

  separator_op        = _ * SEMI_OP * _
                      + B(AND_OP) * _
  separator           = separator_op * linebreak
                      + newline_list
  sequential_sep      = _ * SEMI_OP * linebreak
                      + newline_list

  ---------------------------  Pipelines  ---------------------------

  pipeline            = Not
                      + pipe_sequence
  Not                 = BANG_R * __ * pipe_sequence
  pipe_sequence       = Cg( Cg(command, 'command') * ( PipeSequence
                                                     + Cb'command' ) )
  PipeSequence        = Cb'command' * ( _ * PIPE_OP * linebreak * command )^1

  command             = compound_command
                      + FunctionDef
                      + SimpleCommand

  -----------------------  Compound Commands  -----------------------

  compound_command    = BraceGroup
                      + Subshell
                      + If
                      + For
                      + Case
                      + While
                      + Until

  BraceGroup          = LBRACE_R * compound_list * RBRACE_R * io_redirects
  Subshell            = LPAREN_OP * compound_list * _ * RPAREN_OP * _ * io_redirects

  If                  = IfClause * ElifClause^0 * ( ElseClause + Cc({}) ) * FI * io_redirects
  IfClause            = IF * compound_list
                        * THEN * compound_list
  ElifClause          = ELIF * compound_list
                        * THEN * compound_list
  ElseClause          = ELSE * compound_list

  For                 = FOR * __ * Name * ( linebreak * IN * Ct( ( __ * Word )^0 ) * sequential_sep
                                          + Cc({}) * ( sequential_sep + _ ) )
                                        * do_group

  Case                = CASE * __ * Word * linebreak
                        * IN * linebreak
                        * Ct( ( CaseItem * _ * DSEMI_OP * linebreak )^0 * CaseItem^-1 )
                        * ESAC * io_redirects
  CaseItem            = ( LPAREN_OP * _ )^-1 * Pattern * _ * RPAREN_OP
                        * ( compound_list + linebreak )
  Pattern             = ( Word - ESAC ) * ( _ * PIPE_OP * _ * Word )^0

  While               = WHILE * compound_list
                        * do_group

  Until               = UNTIL * compound_list
                        * do_group

  do_group            = DO * compound_list * DONE * io_redirects

  ----------------------  Function Definition  ----------------------

  FunctionDef         = ( Name - RESERVED_WORD ) * _ * LPAREN_OP * _ * RPAREN_OP * linebreak
                        * function_body
  function_body       = compound_command * io_redirects

  ------------------------  Simple Commands  ------------------------

  SimpleCommand       = Ct( cmd_prefix ) * ( _ * cmd_name * Ct( cmd_suffix^-1 ) )^-1
                      + Cc({}) * cmd_name * Ct( cmd_suffix^-1 )
  cmd_prefix          = ( io_redirect + Assignment ) * ( _ * cmd_prefix )^-1
  cmd_name            = Word - RESERVED_WORD
  cmd_suffix          = ( _ * ( io_redirect + Word ) )^1

  io_redirects        = Ct( io_redirect^0 )
  io_redirect         = _ * ( RedirectFile
                            + RedirectHereDoc )
  RedirectFile        = ( io_number + Cc(nil) ) * io_file_op * _ * Word
  RedirectHereDoc     = ( io_number + Cc(nil) )
                        * ( C(DLESSDASH_OP) * _ * Cmt(heredoc_delim * _heredocs_stack,
                                                      par(capture_heredoc, true))
                          + C(DLESS_OP) * _ * Cmt(heredoc_delim * _heredocs_stack,
                                                  par(capture_heredoc, false)) )
  io_number           = C( DIGIT^1 ) / tonumber
  io_file_op          = C( GREATAND_OP + DGREAT_OP + CLOBBER_OP + LESSAND_OP
                         + LESSGREAT_OP + GREAT_OP + LESS_OP )
  -- Note: If the delimiter contains any quoted string (even `eo'f'`, `eof""`,
  -- `""eof`, ...), then the shell does not expand the content.
  heredoc_delim       = unquoted_word * Cc(false)
                      + Cf(( squoted_word + dquoted_str + unquoted_word )^1, op.concat) * Cc(true)
  dquoted_str         = DQUOTE * Cs( any_except(DQUOTE, expansion_begin)^0 ) * DQUOTE

  Assignment          = Name * EQUALS * Word^-1

  -----------------------------  Words  -----------------------------

  Name                = C( ( ALPHA + '_' ) * WORD^0 )
  Word                = ( -HASH + #EQUALS ) * ( squoted_word
                                              + dquoted_word
                                              + expansion
                                              + unquoted_word
                                              )^1
  squoted_word        = SQUOTE * Cs( any_except(SQUOTE)^0 ) * SQUOTE
  dquoted_word        = DQUOTE * ( expansion
                                 + Cs( any_except(DQUOTE, expansion_begin)^1 )
                                 )^0 * DQUOTE
  unquoted_word       = Cs( any_except(WSP, LF, SQUOTE, DQUOTE, OPERATOR_CHARS, expansion_begin)^1 )

  -- Cg is used here to force evaluation of the Comment/HereDocContent rule
  -- (and so calling create_node) without producing any capture.
  newline_list        = ( _ * Cg(Comment, '__comment')^-1 * LF
                        * Cg(heredoc_content, '__heredoc')^0
                        )^1 * _
  linebreak           = _ * newline_list^-1

  heredoc_content     = Cp() * HereDocContent * _heredocs_stack / inject_heredoc
  HereDocContent      = Cg( Cmt(_heredocs_stack, find_heredoc), 'heredoc')
                        * Ct( Cmt( Cb'heredoc', capture_nonexp_heredoc)
                          + heredoc_content_exp )
                        * heredoc_end_line * ( Cb'heredoc' / heredoc_id )
  heredoc_content_exp = ( expansion
                        + Cf( heredoc_line_noexp^1, op.concat)
                        )^1 * LF
  heredoc_line_noexp  = Cs( ( any_except(expansion_begin) - LF )^1 )
                      + C(LF) * Cmt( Cb'heredoc', inside_heredoc)
  heredoc_end_line    = ( ANY - LF )^1 * ( LF + EOF )

  ---------------------------  Expansions  --------------------------

  expansion_begin     = DOLLAR * ( LPAREN + LBRACE + WORD + SPECIAL_PARAM )
                      + BQUOTE

  expansion           = ParameterExpansion
                      + ArithmeticExpansion
                      + CommandSubstitution
                      + CommandSubBackquote

  CommandSubstitution = DOLLAR * LPAREN * linebreak
                        * ( complete_commands * linebreak )^-1
                        * RPAREN
  -- XXX: Backquoted command substitution has very odd escaping rules that
  -- makes it complicated to recursively parse. Since this syntax is basically
  -- deprecated, we parse it just as a single quoted text.
  CommandSubBackquote = BQUOTE * Cs( any_except(BQUOTE)^0 ) * BQUOTE

  ParameterExpansion  = DOLLAR * ( encl_param_exp + param_name )
  encl_param_exp      = LBRACE
                        * ( C(HASH) + Cc(nil) )  -- prefix operator
                        * param_name
                        * ( C(PARAM_EXP_OP) * ParamExpWord^-1 )^-1
                        * RBRACE
  -- FIXME: Make it consistent, produce node even for special_param and digit.
  param_name          = C( SPECIAL_PARAM + DIGIT^1 ) + Name
  ParamExpWord        = ( squoted_word
                        + dquoted_word
                        + expansion
                        + Cs( any_except(RBRACE, LF, expansion_begin)^1 )
                        )^1

  ArithmeticExpansion = DOLLAR * LDPAREN * ArithmeticExpr * RDPAREN
  -- XXX: This is incomplete, but should be good enough for now.
  ArithmeticExpr      = Cs( ( any_except(LPAREN, RPAREN)
                            + balanced_parens )^0 )
  balanced_parens     = LPAREN * ( any_except(LPAREN, RPAREN)
                                 + balanced_parens
                                 )^0 * RPAREN

  -----------------------------  Others  ----------------------------

  Comment             = ( B(WSP + LF + SEMI + AND_OP) + #BOF )
                        * HASH * C( ( ANY - LF )^0 )
end


local M = {}

--- Builds LPeg grammar table.
--
-- The grammar expects 3 extra arguments to be passed into the `match` function:
--
-- 1. create_node handler - A function that is called every time the LPeg match
--    a grammar rule for which an AST node should be created. It is called with
--    rule name, a table of captures, start position, end position, subject,
--    and should return the AST node.
-- 2. subject - the string being parsed,
-- 3. an empty table - a table used to store an internal state.
--
-- @usage
--   local function create_ast (name, captures, start_pos, end_pos, subject)
--     return { type = name, children = captures }
--   end
--
--   local parser = lpeg.P(grammar.build())
--   parser:match(subject, 1, create_ast, subject, {})
--
-- @function build
-- @treturn table LPeg grammar table.
M.build = par(build_grammar, grammar, terminals)

return M
