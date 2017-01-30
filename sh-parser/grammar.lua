---------
-- LPeg grammar for POSIX shell.

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
-- Example: any_except(P' ') -> escaped(P' ') + 1 - P' '
--
-- @tparam lpeg.Pattern ... The patterns to *not* capture.
-- @treturn lpeg.Pattern
local function any_except (...)
  local patts = iter({...})
  return patts:map(escaped):reduce(op.add, P(false))
       + patts:reduce(op.sub, ANY)
end

--- Transforms captures from *and_or_list* into left-associative tree of n-ary
-- nodes *AndList* and *OrList*.
--
-- This function is basically a workaround to create AST for left-associative
-- operators with the same precedence - `&&` and `||`.
--
-- ## Example
--
--     subject = "a && b && c || d || e && f"
--
--     capture_and_or([func], 1, {{a}, 2, "&&", {b}, 7, "&&", {c}, 12, "||",
--                                {d}, 17, "||", {e}, 22, "&&", {f}, 27}) -> Z
--     on_match_rule("AndList", 1, {a, b, c}, 12) -> X
--     on_match_rule("OrList", 1, {X, d, e}, 22) -> Y
--     on_match_rule("AndList", 1, {Y, f}, 27) -> Z
--
--      Z        Y       X
--     (AndList (OrList (AndList a b c) d e) f)
--
-- @tparam function on_match_rule Function that creates AST nodes from captures.
-- @tparam int start_pos Index of the first character of the captured substring.
-- @tparam table captures Table with shape `{ table,int,string, table,int,string, ... }`.
--   Element *i* is table of children nodes (pipeline and optional comments),
--   *i + 1* is position of the end of the last child node (int), *i + 2* is
--   operator ("&&", or "||").
-- @return Result of the last call of `on_match_rule`.
local function capture_and_or (on_match_rule, start_pos, captures)
  local node_name = { ['&&'] = 'AndList', ['||'] = 'OrList' }
  local node, last_op
  local children = {}

  for i=1, #captures, 3 do
    local caps, end_pos, next_op = captures[i], captures[i + 1], captures[i + 2]

    extend(children, caps)

    if last_op and last_op ~= next_op then
      local name = assert(node_name[last_op], 'invalid operator '..last_op)
      node = on_match_rule(name, start_pos, children, end_pos)
      children = { node }
    end
    last_op = next_op
  end

  return node
end

--- Skip already captured here-document.
--
-- This is a function for match-time capture that is called from grammar each
-- time when a new line is consumed. When the current position of the parser is
-- inside previously captured heredoc, then it returns position of the end of
-- that heredoc. Basically it teleports parser behind the heredoc.
--
-- @tparam string _ The entire subject (unused).
-- @tparam int pos The current position.
-- @tparam {{int,int},...} heredocs (see `capture_heredoc`)
-- @treturn int The new current position.
local function skip_heredoc (_, pos, heredocs)
  -- Note: Elements are ordered from latest to earliest to optimize this lookup.
  -- Note: We cannot remove skipped heredocs in this function, because the
  --       matched rule may be eventually backtracked!
  for _, range in ipairs(heredocs) do
    local first, last = range[1], range[2]

    if pos > last then
      break
    elseif pos >= first and pos < last then
      return last
    end
  end

  return pos
end

--- Capture here-document.
--
-- @tparam bool strip_tabs Whether to strip leading tabs (for `<<-`).
-- @tparam string subject The entire subject (i.e. input text).
-- @tparam int pos The current position.
-- @tparam string word The captured delimiter word.
-- @tparam {{int,int},...} heredocs The list with positions of captured
--   heredocs. Each element is a list with two integers - position of the first
--   character inside heredoc and position of newline after closing delimiter.
-- @treturn true Consume no subject.
-- @treturn table Heredoc content.
local function capture_heredoc (strip_tabs, subject, pos, word, heredocs)
  local delimiter = word.children[1]

  local delim_pat = '\n'..(strip_tabs and '\t*' or '')
                        ..delimiter:gsub('%p', '%%%1')  -- escape puncatation chars
                        ..'\n'
  local doc_start = subject:find('\n', pos, true) or #subject
  local doc_end, delim_end = (subject..'\n'):find(delim_pat, doc_start)
  if not doc_end then
    doc_end, delim_end = #subject + 1, #subject
  end

  -- Skip overlapping heredocs (multiple heredoc redirects on the same line).
  while true do
    local new_pos = skip_heredoc(nil, doc_start, heredocs)
    if new_pos == doc_start then
      break
    end
    doc_start = new_pos
  end

  unshift(heredocs, { doc_start, delim_end or #subject })

  local content = subject:sub(doc_start, doc_end - 1)  -- keep leading newline
  content = strip_tabs and content:gsub('\n\t+', '\n') or content
  content = content:sub(2)  -- strip leading newline

  return true, content
end


--- Grammar to be processed by `lpeg_sugar`.
local function grammar (_ENV)  --luacheck: no unused args
  --luacheck: allow defined, ignore 113 131

  local _  = ( WSP + ESC * LF )^0  -- optional whitespace(s)
  local __ = ( (ESC * LF)^0 * WSP )^1  -- at least one whitespace
  local heredocs_index = Carg(3)  -- state table used for skipping heredocs

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
  and_or_list         = Cb'and_or_cp' * Ct(
                          Ct( Cb'pipeline' ) * Cp()
                          * ( _ * and_or_op * Ct( linebreak * pipeline ) * Cp() )^1
                        ) / par(capture_and_or, on_match_rule)
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
                        * ( CaseItem * _ * DSEMI_OP * linebreak )^0
                        * CaseItem^-1
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

  SimpleCommand       = Ct( cmd_prefix ) * ( _ * CmdName * Ct( cmd_suffix^-1 ) )^-1
                      + Cc({}) * CmdName * Ct( cmd_suffix^-1 )
  CmdName             = Word - RESERVED_WORD
  cmd_prefix          = ( io_redirect + Assignment ) * ( _ * cmd_prefix )^-1
  cmd_suffix          = ( _ * ( io_redirect + Word ) )^1

  io_redirects        = Ct( io_redirect^0 )
  io_redirect         = _ * ( RedirectFile
                            + RedirectHereDoc )
  RedirectFile        = ( io_number + Cc(nil) ) * io_file_op * _ * Word
  RedirectHereDoc     = ( io_number + Cc(nil) ) * (
                            DLESSDASH_OP * _ * Cmt(Word * heredocs_index, par(capture_heredoc, true))
                          + DLESS_OP * _ * Cmt(Word * heredocs_index, par(capture_heredoc, false))
                        )
  io_number           = C( DIGIT^1 ) / tonumber
  io_file_op          = C( GREATAND_OP + DGREAT_OP + CLOBBER_OP + LESSAND_OP
                         + LESSGREAT_OP + GREAT_OP + LESS_OP )

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

  newline_list        = ( _ * Comment^-1 * LF * Cmt(heredocs_index, skip_heredoc) )^1 * _
  linebreak           = _ * newline_list^-1

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
  encl_param_exp      = LBRACE * ( HASH * param_name
                                 + param_name * ( C(PARAM_EXP_OP) * param_exp_word^-1 )^-1
                                 ) * RBRACE
  param_name          = C( SPECIAL_PARAM + DIGIT^1 ) + Name
  param_exp_word      = ( squoted_word
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


return function ()
  return build_grammar(grammar, terminals)
end
