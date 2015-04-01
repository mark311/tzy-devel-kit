import gdb
import binascii
import struct
import re

import sys
sys.path.append(".")
import opcodes

## works for lua-5.3

# basic types
LUA_TNONE             = (-1)
LUA_TNIL              = 0
LUA_TBOOLEAN          = 1
LUA_TLIGHTUSERDATA    = 2
LUA_TNUMBER           = 3
LUA_TSTRING           = 4
LUA_TTABLE            = 5
LUA_TFUNCTION         = 6
LUA_TUSERDATA         = 7
LUA_TTHREAD           = 8
LUA_NUMTAGS           = 9

# LUA_TFUNCTION variants:
LUA_TLCL       = (LUA_TFUNCTION | (0 << 4))  # Lua closure
LUA_TLCF       = (LUA_TFUNCTION | (1 << 4))  # light C function
LUA_TCCL       = (LUA_TFUNCTION | (2 << 4))  # C closure

CIST_LUA = 2

# all referenced types
union_Closure_ptr = gdb.lookup_type("union Closure").reference()
TString = gdb.lookup_type("TString")
const_char_ptr = gdb.lookup_type("char").reference()
TValue = gdb.lookup_type("TValue")


def getTValueTag(tvalue):
    inferior = gdb.inferiors()[0]
    rawmem = inferior.read_memory(tvalue["tt_"].address, 4)
    tag = struct.unpack("i", rawmem)[0]
    return (tag & 0xf, (tag >> 4) & 0x3)

def TString2cstr(ts):
    """Convert type from Lua `TString' to `const char*'."""
    return (ts.address + 1).dereference().cast(const_char_ptr).address

class LuaBacktrace(gdb.Command):
    """Print Lua call stack"""

    def __init__(self):
        super(LuaBacktrace, self).__init__("lbt", gdb.COMMAND_STACK)

    def invoke(self, arg, from_tty):
        frame = gdb.selected_frame()
        L = frame.read_var("L")
        ci = L["ci"]
        frameid = 0
        while ci != 0x0:
            t, tv = getTValueTag(ci["func"])
            tv = (tv << 4) + t
            if t != LUA_TFUNCTION:
                break

            if tv == LUA_TLCL:  # lua
                cl = ci["func"]["value_"]["gc"].dereference().cast(union_Closure_ptr)["l"].address
                proto = cl["p"]
                savedpc = ci["u"]["l"]["savedpc"]
                lineno = (proto["lineinfo"] + (savedpc - proto["code"])).dereference()

                print "#%d [LCL] ci:%s, func:%s, top:%s, cl:%s, savedpc:%s, source:%s, line:%s" % \
                    (frameid, ci, ci["func"], ci["top"], cl, savedpc,
                     proto["source"].dereference(), lineno)

            elif tv == LUA_TLCF:  # light c func
                print "#%d [LCF] ci:%s, func:%s, top:%s, cfunc:%s" %  \
                    (frameid, ci, ci["func"], ci["top"], ci["func"]["value_"]["f"])

            elif tv == LUA_TCCL:  # c closure
                cl = ci["func"]["value_"]["gc"].dereference().cast(union_Closure_ptr)["c"]
                print "#%d [CCL] ci:%s, func:%s, top:%s, cl:%s, cfunc:%s" % \
                    (frameid, ci, ci["func"], ci["top"], cl, cl["f"])

            else:
                print "t=%d, tv=%d" % (t, tv)
                assert(False)

            ci = ci["previous"]
            frameid += 1


class LuaShowStack(gdb.Command):
    """Show the stack of current lua calling frame"""

    def __init__(self):
        super(LuaShowStack, self).__init__("lsk", gdb.COMMAND_STACK)

    def invoke(self, arg, from_tty):
        frame = gdb.selected_frame()
        L = frame.read_var("L")
        ci = L["ci"]
        func = ci["func"]
        top = ci["top"]
        ltop = L["top"]
        sp = func
        while sp != top and sp != ltop:
            t, v = getTValueTag(sp)
            print sp, t, v
            sp += 1

class TStringPrinter:
    "Print a TString"

    def __init__ (self, val):
        self.val = val

    def to_string (self):
        return (self.val.address + 1).dereference().cast(const_char_ptr).address

    def display_hint (self):
        return 'string'

class TValuePrinter:
    "Print a TValue"

    def __init__ (self, val):
        self.val = val

    def to_string (self):
        t, v = getTValueTag(self.val)
        tv = (v << 4) + t
        print t, v
        if t == LUA_TSTRING:
            return self.val["value_"]["gc"].dereference().cast(TString)
        return None

def tvalue_lookup_function (val):
    lookup_tag = str(val.type)
    #print "[",lookup_tag,"]"
    if lookup_tag == "struct TString" or lookup_tag == "TString":
        return TStringPrinter(val)

    if lookup_tag == "TValue":
        return TValuePrinter(val)

    return None

# registers
LuaBacktrace()
LuaShowStack()
gdb.pretty_printers = []
gdb.pretty_printers.append(tvalue_lookup_function)
