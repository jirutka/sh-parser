---------
-- LPeg grammar for POSIX shell.

local lpeg       = require 'lpeg'
local fun        = require 'sh-parser.fun_ext'
local lpeg_sugar = require 'sh-parser.lpeg_sugar'
local utils      = require 'sh-parser.utils'

local build_grammar = lpeg_sugar.build_grammar
local chain         = fun.chain
local iter          = fun.iter
local op            = fun.op
local values        = utils.values

local B  = lpeg.B
local C  = lpeg.C
local Cb = lpeg.Cb
local Cg = lpeg.Cg
local Cs = lpeg.Cs
local P  = lpeg.P
local R  = lpeg.R
local S  = lpeg.S


--- Creates a pattern that captures escaped `patt`.
--
-- @tparam lpeg.Pattern patt The pattern to escape.
-- @treturn lpeg.Pattern
local function escaped (patt)
  return P'\\' / '' * patt
end

--- Creates a pattern that captures quoted text.
--
-- @tparam string quote The quotation mark.
-- @treturn lpeg.Pattern
local function quoted (quote)
  return quote * Cs( (escaped(quote) + 1 - quote)^0 ) * quote
end


local BOF = P(function(_, pos) return pos == 1 end)  -- Beginning Of File
local EOF = P(-1)

local BASE_TERMINALS = {
  ALPHA     = R('AZ', 'az'),
  ANY       = P(1),
  BOF       = BOF,
  DIGIT     = R('09'),
  DQUOTE    = P('"'),
  EOF       = EOF,
  EQUALS    = P('='),
  HASH      = P('#'),
  LF        = P('\n'),
  SQUOTE    = P("'"),
  WSP       = S(' \t'),
}

-- Shell operators containing single character.
local OPERATORS_1 = {
  AND       = '&',
  GREAT     = '>',
  LESS      = '<',
  LPAREN    = '(',
  PIPE      = '|',
  RPAREN    = ')',
  SEMI      = ';',
}
-- Shell operators containing more than one character.
local OPERATORS_2 = {
  AND_IF    = '&&',
  CLOBBER   = '>|',
  DGREAT    = '>>',
  DLESS     = '<<',
  DLESSDASH = '<<-',
  DSEMI     = ';;',
  GREATAND  = '>&',
  LESSAND   = '<&',
  LESSGREAT = '<>',
  OR_IF     = '||',
}

-- Shell reserved words.
local RESERVED_WORDS = {
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

  BANG      = '!',
  IN        = 'in',
  LBRACE    = '{',
  RBRACE    = '}',
}

-- Pattern that matches any character used in shell operators.
local operator_chars = values(OPERATORS_1):map(P):reduce(op.add, P(false))

-- XXX: is this correct?
local word_boundary = S(' \t\n') + BOF + EOF + operator_chars

local reserved_words = iter(RESERVED_WORDS)
    :map(function(k, v) return k, P(v) * #word_boundary end)

-- Pattern that matches any shell reserved word.
-- XXX: sort them?
local reserved_word = values(reserved_words):reduce(op.add, P(false))

-- A map of all used terminal symbols (patterns).
local terminals = chain(
      BASE_TERMINALS,
      iter(OPERATORS_1):map(function(k, v)
          -- Ensure that operator x does not match xx when xx is valid operator.
          return k, values(OPERATORS_2):index_of(v..v) and P(v) * -P(v) or P(v)
        end),
      iter(OPERATORS_2):map(function(k, v)
          return k, P(v)
        end),
      reserved_words
    ):tomap()


--- Grammar to be processed by `lpeg_sugar`.
local function grammar (_ENV)  --luacheck: no unused args
  --luacheck: allow defined, ignore 113 131

  local _  = WSP^0  -- optional whitespace(s)
  local __ = WSP^1  -- at least one whitespace

  Program             = linebreak * ( complete_commands * linebreak )^-1 * EOF
  complete_commands   = CompleteCommand * ( newline_list * CompleteCommand )^0
  CompleteCommand     = and_or * ( separator_op * and_or )^0 * separator_op^-1
                      -- Note: Anonymous Cg is here only to exclude named Cg from capture in AST.
  and_or              = Cg( Cg(pipeline, 'pipeline') * ( AndList
                                                       + OrList
                                                       + Cb'pipeline' ) )
  AndList             = Cb'pipeline' * _ * AND_IF * linebreak * and_or
  OrList              = Cb'pipeline' * _ * OR_IF * linebreak * and_or
  pipeline            = Not
                      + pipe_sequence
  Not                 = BANG * __ * pipe_sequence
  pipe_sequence       = Cg( Cg(command, 'command') * ( PipeSequence
                                                     + Cb'command' ) )
  PipeSequence        = Cb'command' * ( _ * PIPE * linebreak * command )^1
  command             = FunctionDefinition
                      + compound_command * io_redirect^0
                      + SimpleCommand
  compound_command    = BraceGroup
                      + Subshell
                      + ForClause
                      + CaseClause
                      + IfClause
                      + WhileClause
                      + UntilClause
  Subshell            = LPAREN * compound_list * _ * RPAREN * _
  compound_list       = linebreak * term * separator^-1
  term                = and_or * ( separator * and_or )^0
  ForClause           = FOR * __ * Name * ( sequential_sep
                                          + linebreak * IN * ( __ * Word )^0 * sequential_sep
                                          + _ )
                                        * do_group
  CaseClause          = CASE * __ * Word * linebreak
                        * IN * linebreak
                        * ( CaseItem * _ * DSEMI * linebreak )^0
                        * CaseItem^-1
                        * ESAC
  CaseItem            = ( LPAREN * _ )^-1 * Pattern * _ * RPAREN
                        * ( compound_list + linebreak )
  Pattern             = ( Word - ESAC ) * ( _ * PIPE * _ * Word )^0
  IfClause            = IF * linebreak
                        * term * separator
                        * THEN * compound_list
                        * elif_part^0
                        * else_part^-1
                        * FI
  elif_part           = ELIF * compound_list
                        * THEN * compound_list
  else_part           = ELSE * compound_list
  WhileClause         = WHILE * compound_list
                        * do_group
  UntilClause         = UNTIL * compound_list
                        * do_group
  FunctionDefinition  = ( Name - reserved_word ) * _ * LPAREN * _ * RPAREN * linebreak
                        * function_body
  function_body       = compound_command * io_redirect^0
  BraceGroup          = LBRACE * compound_list * RBRACE
  do_group            = DO * compound_list * DONE
  SimpleCommand       = cmd_prefix * ( __ * CmdName * cmd_suffix^-1 )^-1
                      + CmdName * cmd_suffix^-1
  CmdName             = Word - reserved_word
  cmd_prefix          = ( io_redirect + Assignment ) * ( __ * cmd_prefix )^-1
  cmd_suffix          = ( __ * ( io_redirect + CmdArgument ) )^1
  CmdArgument         = Word
  io_redirect         = IORedirectFile
                      + io_here
  IORedirectFile      = io_number^-1 * io_file_op * _ * Word
  io_here             = io_number^-1 * ( DLESSDASH + DLESS ) * _ * here_end
  io_number           = C( DIGIT^1 ) / tonumber
  io_file_op          = C( GREATAND + DGREAT + CLOBBER + LESSAND + LESSGREAT + GREAT + LESS )
  here_end            = Word
  separator_op        = _ * ( AND + SEMI ) * _
  separator           = separator_op * linebreak
                      + newline_list
  sequential_sep      = _ * SEMI * linebreak
                      + newline_list
  Assignment          = Name * EQUALS * Word^-1
  Name                = C( ( ALPHA + '_' ) * ( ALPHA + DIGIT + '_' )^0 )
  Word                = ( quoted(DQUOTE)
                        + quoted(SQUOTE)
                        + Cs( -HASH * unquoted_char^1 )
                        )^1
  unquoted_char       = escaped(WSP + LF + SQUOTE + DQUOTE + operator_chars)
                      + ( ANY - WSP - LF - SQUOTE - DQUOTE - operator_chars )
  newline_list        = ( _ * Comment^-1 * LF )^1 * _
  linebreak           = _ * newline_list^-1
  Comment             = ( B(WSP) + B(LF) + B(SEMI) + B(AND) + #BOF )
                        * HASH * C( ( ANY - LF )^0 )
end


return function ()
  return build_grammar(grammar, terminals)
end
