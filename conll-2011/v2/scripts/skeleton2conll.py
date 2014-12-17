
"""
Get most current usage with:

  python skeleton2conll.py --help



"""

from __future__ import with_statement
import codecs
import sys
import os
import re
import string



WORD_COLUMN=3










def iterate_trees(string_seq):
    """

    given string_seq which is a sequence of strings, read from
    string_seq and produce strings one at a time that represent trees.

    """

    return [x for x in _iterate_trees_helper(string_seq) if x.strip()]










def _iterate_trees_helper(string_seq):

    parens = 0
    cur_parse = []

    for s in string_seq:
        if (s.startswith(";") or s.startswith("<") or s.startswith("*")) and s.endswith("\n"):
            continue # ignore comments and sgml

        for c in s:
            if c == "(" and parens == 0 and cur_parse:
                yield "".join(cur_parse)
                cur_parse = []

            cur_parse.append(c)

            if c == "(":
                parens += 1
            elif c == ")":
                parens -= 1

                if parens == 0:
                    yield "".join(cur_parse).strip()
                    cur_parse = []

    if parens != 0:
        raise Exception("Parens should have been zero at end, were %s" % parens)
    if "".join(cur_parse).strip():
        raise Exception("curparse should have been empty at end, was %s" % cur_parse)










class InvalidSexprException(Exception):
    def __init__(self, sexpr, parent=None):
        self.sexpr = sexpr
        self.parent = parent

    def __str__(self):

        ns = ""
        ns += self.sexpr
        if self.parent:
            ns += "\n\n"
            ns += str(self.parent)
        return ns










def parse_sexpr(s):
    """ turn an s-expression into a tree of lists:

    (a (b c) d) -> [a, [b, c], d]

    uses spaces and parens only -- no way to have a token with a space in it

    """
    s = s.replace("\n", " ").replace("\t"," ").strip()

    if not s.startswith("(") and not s.endswith(")"):
        return s
    elif s.startswith("(") and s.endswith(")"):
        tr = []
        cur = []
        parens = 0
        for c in s[1:-1].strip() + " ":
            if c == "(":
                parens += 1
            elif c == ")":
                parens -= 1
            elif c == " " and cur:
                if parens == 0:
                    try:
                        x = parse_sexpr("".join(cur))
                    except InvalidSexprException, e:
                        raise InvalidSexprException("Parent: %s" % s, e)

                    if x:
                        tr.append(x)
                    cur = []

            cur.append(c)

        if (cur and cur != [" "]) or parens != 0:
            raise InvalidSexprException("Invalid s-expression: " + s + " note: %s" % "".join(cur) + " parens: %s" % parens)

        return tr
    else:
        raise InvalidSexprException("Invalid s-expression: \n" + s)










def unparse_sexpr(l):
    if type(l) == type([]):
        return "(" + " ".join(unparse_sexpr(a) for a in l) + ")"
    return str(l)










def pretty_print_tree_string(a_tree_string, offset=''):

    if not a_tree_string.strip():
        return ""

    # Maximum depth we're prepared for in trees
    maxdepth=100
    maxindent=300

    # Table of indentation at tree depth
    depth_to_indent = [0 for i in xrange(maxdepth)]

    # Initialize indent_string[i] to be a string of i spaces
    indent_string = ['' for i in xrange(maxindent)]
    for i in xrange(maxindent-1):
        indent_string[i+1] = indent_string[i] + ' '

    # RE object for split that matches on a ')' followed by not a ')', but only consumes the ')'
    close_paren = re.compile(r'\)(?=\s*[^\s\)])')

    # RE object to pick up first on this line(should be only) POS tag and the word of each lexical leaf of the tree
    lexical_leaf = re.compile(r'\((?P<tag>[^\s\)\(]+)\s+(?P<word>[^\s\)\(]+)\)')

    # RE object to parse OntoNotes Normal Form tree lines:
    a_tree = a_tree_string

    pp_tree = ""

    def treeindent(depth):
        return indent_string[depth_to_indent[depth]]+offset  #Indent to appropriate point


    current_depth = 0
    for frag in  close_paren.split(a_tree):  #Split input into lines ending with a lexical item
        if frag[-1]!= '\n':
            frag=frag+')'
        else: frag=frag[0:-1]

        #Indent to appropriate point
        pp_tree += treeindent(current_depth)

        pfrag = ""
        for pfrag in (frag).split('(')[1:]:         # Split line into segments each beginning with an '('
            pfrag='('+pfrag                         # Restore deleted initial '('
            pp_tree += pfrag                      # Print each
            current_depth=current_depth+1           # Up the current depth count

            # Remember how far to indent following items at this depth
            depth_to_indent[current_depth]=depth_to_indent[current_depth-1]+len(pfrag)

        current_depth=current_depth-pfrag.count(')')    # Correct depth given closing parens
        if current_depth<=0:
            pp_tree += ''            # Separate toplevel trees with blank lines

        pp_tree += '\n'              # Print CRLF


    return re.sub("\)$", "", pp_tree)






DONT_DELETE_TREES = True










def car(sp):
    return sp[0]










def cdr(sp):
    return sp[1:]










def split_node(sp):
    return car(sp), cdr(sp)










def is_leaf(sp):
    return len(sp) == 2 and type(sp[1]) != type([])


transformations = {}










def pp(sexpr, out_text=False):
    """ pretty print the S-expr, or just spit text out if out_text is true

    out_text also skips traces

    """
    if not out_text:
        return pretty_print_tree_string(unparse_sexpr(sexpr))
    else:
        words = [word for tag, word in all_leaves(sexpr)
                 if tag != "-NONE-"] # skip traces

        return "\n".join(words)






def transforms(transformation):
    assert transformation.startswith("+") or transformation.startswith("-")
    def regfunc(func):
        transformations[transformation] = func
        return func
    return regfunc










def require(b):
    if not b:
        raise Exception("Failed Requirement")










@transforms("-edited")
def remove_edits(sp):
    """Remove subtrees tagged 'EDITED' (disfluencies) """

    return remove_tagger(sp, "EDITED")










@transforms("-trace")
def remove_edits(sp):
    """Remove traces part of speech tagged '-NONE-' """

    return remove_tagger(sp, "-NONE-")










@transforms("-phrase-tags")
def all_leaves(sp):
    """Make a tree of just the leaves

    .. code-block: scheme

        (TOP (S (NP-SBJ (NNP Zambia))
            (VP (VBD had)
                (ADVP-TMP (RB previously))
                (VP (VBD lost)
                    (NP (PRP$ its)
                        (RB away)
                        (VBD game))
                    (NP-ADV (NP (CD 0))
                            (PP (SYM -)
                                (NP (CD 1))))))
            (. .)))

    becomes

    .. code-block: scheme

        ( (NNP Zambia)
          (VBD Had)
          (RB Previously)
          (VBD lost)
          (PRP$ its)
          (RB away)
          (VBG game)
          (CD 0)
          (SYM -)
          (CD 0) )

    """

    tag, rest = split_node(sp)
    if is_leaf(sp):
        return [[tag, rest[0]]]

    tr = []
    for x in rest:
        tr.extend(all_leaves(x))
    return tr










def remove_tagger(sp, tag_to_remove):
    """ remove tag_to_remove from sp, culling empty branches """
    def callback(tag, rest):
        return tag == tag_to_remove
    return remover(sp, callback)










def remover(sp, callback):
    tag, rest = split_node(sp)
    if callback(tag, rest):
        return []
    if is_leaf(sp):
        return sp

    new_rest = [y for y in [remover(x, callback) for x in rest] if y]

    if not new_rest:
        return []
    return [tag] + new_rest










def pad_items_in_list(a_list, a_character=None):
    """
    this function will return the same list with the right amount of
    padding equal to two spaces on each side of the widest string. it
    will perform right justification.

    if the optional character is specified, then it will do a
    centering around the character in the process of padding.
    left/right justification does not work with this option.
    """

    if(a_character != None):
        for an_item in a_list:
            if(an_item.find(a_character) == -1):
                a_character = None
                break

    if(a_character != None):
        lmax=0
        rmax=0
        for an_item in a_list:
            an_item = an_item.strip()

            lf = an_item.find("*")
            if(lmax < lf):
                lmax = lf

            rf = len(an_item) - an_item.find("*")
            if(rmax < rf):
                rmax = rf



        i=0
        for i in range(0, len(a_list)):
            a_list[i] = a_list[i].strip()

            x = a_list[i].find(a_character)

            len_i=len(a_list[i])

            a_list[i] = " "*(lmax-x+2) + a_list[i]
            a_list[i] = a_list[i] + " "*(rmax-len_i+x+2)

    else:
        max=0
        for an_item in a_list:
            an_item = an_item.strip()
            x = len(an_item)
            if(max < x):
                max = x

        i=0
        for i in range(0, len(a_list)):
            a_list[i] = a_list[i].strip()

            if(a_list[i].endswith("*") or
               a_list[i].endswith("-") or
               a_list[i][-1] in string.digits ):
                a_list[i] = "%s " % (a_list[i])

            a_list[i] = a_list[i].rjust(max+2)

    return a_list










def rows2columns(matrix):
    columns = []

    for row in matrix:
        c=0
        for cell in row:
            if(c == len(columns)):
                columns.append([])

            columns[c].append(cell)
            c = c + 1

    return columns










def pretty_print_table(rows, separator=None, out_file=None):

    # cells is the matrix
    r_c_matrix = []
    for row in rows:
        r_c_matrix.append(row.split())


    c_r_matrix = rows2columns(r_c_matrix)


    for i in range(0, len(c_r_matrix)):

        if(i==5 or i>10):
            padding_character=separator
        else:
            padding_character=None

        c_r_matrix[i] = pad_items_in_list(c_r_matrix[i], padding_character)

    r_c_matrix = rows2columns(c_r_matrix)

    if(out_file == None):
        for row in r_c_matrix:
            print " ".join(row).strip()
        print

    elif(out_file == "-"):
        rows=[]
        for row in r_c_matrix:
            rows.append(" ".join(row).strip())
        return "%s\n" % ("\n".join(rows))

    else:
        raise NotImplementedError("this functionality has not yet been implemented")










def start(input_fname, conll_fname, output_fname, encoding, changes):
    """ apply changes in order to the trees in input_fname, write to output_fname """


    out_text = False
    if "--text" in changes:
        out_text = True
        changes.remove("--text")

    out = []
    with codecs.open(input_fname, "r", encoding) as inf:
        for a_tree in iterate_trees(inf):
            sexpr = parse_sexpr(a_tree)
            for change in changes:
                if not sexpr:
                    continue

                try:
                    change_func = transformations[change]
                except KeyError:
                    raise Exception("Invalid argument '%s' for change.  Allowed changes are: %s" % (change, transformations.keys()))

                try:
                    old_sexpr = sexpr[:]
                    sexpr = change_func(sexpr)
                except Exception:
                    sys.stderr.write("ERR in %s\n\nTree:\n%s\n\nInput sexpr:\n%s\n" % (change, a_tree, pp(sexpr)))
                    raise


                if not sexpr and DONT_DELETE_TREES:
                    nullp = ["XX", "nullp"]
                    if old_sexpr and old_sexpr[0] == "TOP":
                        sexpr = ["TOP", nullp]
                    else:
                        sexpr = nullp

            if sexpr:
                out.append(pp(sexpr, out_text))



    w_list = []
    for o in out:
        w_list.append(o.split("\n"))


    f=codecs.open(output_fname, "w", encoding)
    f.close()

    sentences = []
    i=0
    conll_file = codecs.open(conll_fname, "r", encoding)
    for line in conll_file:
        if(line.strip() == ""):

            assert len(sentences) == len(w_list[i]), "the example should contain the same number of words as the words in the parse"

            rows=[]
            c=0
            for columns in sentences:
                columns[WORD_COLUMN] = w_list[i][c]
                rows.append(" ".join(columns))
                c=c+1

            pretty_print_table_string = pretty_print_table(rows, out_file="-")

            if output_fname == "-":
                print pretty_print_table_string
            else:
                with codecs.open(output_fname, "a", encoding) as outf:
                    outf.write("%s\n" % (pretty_print_table_string))

            sentences = []
            i=i+1

        elif(line.startswith("#")):
            if output_fname == "-":
                print line.strip()
            else:
                with codecs.open(output_fname, "a", encoding) as outf:
                    outf.write("%s\n" % (line.strip()))
        else:
            sentences.append(line.split())










if __name__ == "__main__":

    encoding = "utf8"
    if "--gb18030" in sys.argv:
        encoding="gb18030"
        sys.argv.remove("--gb18030")

    if len(sys.argv[1:]) == 2 and sys.argv[1] in ["--help", "-h"] and sys.argv[2] in transformations:
        print
        print "  ", transformations[sys.argv[2]].__doc__
    elif not sys.argv[1:] or "--help" in sys.argv[1:] or "-h" in sys.argv[1:]:
        print
        print "-"*120
        print "Usage:  python skeleton2conll.py <ontonotes-parse-file> <input-skel-file> <conll-output-file> [transformations] ..."
        print "\nAllowed transforms:"

        max_key_len = max(len(t) for t in transformations) + 1 # +1 for colon

        for key in transformations:
            print "   %s %s" %(("%s:"%key).rjust(max_key_len),
                               transformations[key].__doc__.split("\n")[0])

        print "   %s %s" % ("--text:".rjust(max_key_len),
                            "Produce text output instead of parse trees")
        print
        print
        print "Example:"
        print "python skeleton2conll.py <ontonotes-release-directory>/data/.../bc/cnn/00/cnn_0000.parse conll-2011/dev/data/english/annotations/bc/cnn/00/cnn_0000.v0_gold_skel conll-2011/dev/data/english/annotations/bc/cnn/00/cnn_0000.v0_gold_conll -edited --text"
        print "-"*120
    else:
        input_fname, conll_fname, output_fname, changes = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4:]
        start(input_fname, conll_fname, output_fname, encoding, changes)
