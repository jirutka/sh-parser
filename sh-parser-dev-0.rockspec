-- vim: set ft=lua:

package = 'sh-parser'
version = 'dev-0'

source = {
  url = 'git://github.com/jirutka/sh-parser.git',
  branch = 'master',
}

description = {
  summary = 'Parser of POSIX Shell Command Language',
  detailed = [[
This is a POSIX shell parser written in LPeg that generates AST of the parsed
shell script without evaluating. It supports complete syntax of POSIX Shell
Command Language plus few common extensions implemented e.g. by Busybox ash
and dash. It does not support Bash/ZSH-specific features.]],
  homepage = 'https://github.com/jirutka/sh-parser',
  maintainer = 'Jakub Jirutka <jakub@jirutka.cz>',
  license = 'MIT',
}

dependencies = {
  'lua >= 5.1',
  'lpeg ~> 1.0',
  'fun ~> 0.1.3',
  --'PegDebug ~> 0.40' optional
}

build = {
  type = 'builtin',
  modules = {
    ['sh-parser'] = 'sh-parser/init.lua',
    ['sh-parser.ast.name_captures'] = 'sh-parser/ast/name_captures.lua',
    ['sh-parser.ast.simple'] = 'sh-parser/ast/simple.lua',
    ['sh-parser.fun_ext'] = 'sh-parser/fun_ext.lua',
    ['sh-parser.grammar'] = 'sh-parser/grammar.lua',
    ['sh-parser.location_resolver'] = 'sh-parser/location_resolver.lua',
    ['sh-parser.lpeg_sugar'] = 'sh-parser/lpeg_sugar.lua',
    ['sh-parser.parser'] = 'sh-parser/parser.lua',
    ['sh-parser.utils'] = 'sh-parser/utils.lua',
  },
}
