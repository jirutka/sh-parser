-- vim: set ft=lua:

package = 'sh-parser'
version = 'dev-0'

source = {
  url = 'git://github.com/jirutka/sh-parser.git',
  branch = 'master',
}

description = {
  summary = 'Parser of POSIX Shell Command Language',
  detailed = [[TODO]],
  homepage = 'https://github.com/jirutka/sh-parser',
  maintainer = 'Jakub Jirutka <jakub@jirutka.cz>',
  license = 'MIT',
}

dependencies = {
  'lua >= 5.1',
  'lpeg ~> 1.0',
  -- This is original "fun" module, but uploaded to LuaRocks by alloyed,
  -- because upstream don't care about LuaRocks.
  'fun-alloyed ~> 0.1.3',
}

build = {
  type = 'builtin',
  modules = {
    ['sh-parser'] = 'sh-parser/init.lua',
    ['sh-parser.fun_ext'] = 'sh-parser/fun_ext.lua',
    ['sh-parser.lpeg_sugar'] = 'sh-parser/lpeg_sugar.lua',
    ['sh-parser.utils'] = 'sh-parser/utils.lua',
  },
}
