#!/usr/bin/python

#  Copyright (C) Simon Wright <simon@pushface.org>

#  This package is free software; you can redistribute it and/or
#  modify it under terms of the GNU General Public License as
#  published by the Free Software Foundation; either version 2, or
#  (at your option) any later version. This package is distributed in
#  the hope that it will be useful, but WITHOUT ANY WARRANTY; without
#  even the implied warranty of MERCHANTABILITY or FITNESS FOR A
#  PARTICULAR PURPOSE. See the GNU General Public License for more
#  details. You should have received a copy of the GNU General Public
#  License distributed with this package; see file COPYING.  If not,
#  write to the Free Software Foundation, 59 Temple Place - Suite
#  330, Boston, MA 02111-1307, USA.

# $Id: nuweb.py,v 9583e6f87a23 2011/08/18 21:56:50 simonjwright $

import getopt, re, tempfile, os, sys


#-----------------------------------------------------------------------
# Notes
#-----------------------------------------------------------------------

# Makes considerable use of Python's extensions to regular
# expressions, see http://docs.python.org/library/re.html. For
# example, including (?s) in an RE is equivalent to the flag
# re.DOTALL, which allows "." to match \n (newline).

#-----------------------------------------------------------------------
# Globals
#-----------------------------------------------------------------------

# In 'document', we have the input document as a sequence of
# DocumentElements.
document = []

# In 'code_elements' we have all the CodeElements (keyed by scrap
# number).
code_elements = {}

# In 'files', the keys are the output file names, the values are the
# output streams.
files = {}

# 'hyperlinks', if True, means that cross-references are to be
# generated as hyperlinks using the hyperref package (which needs to
# be specified in your document).
hyperlinks = False

# 'need_to_rerun', if True, means we end with a message telling the
# user she needs to re-run nuweb.py after running LaTeX (because of a
# scrap number mismatch).
need_to_rerun = False

#-----------------------------------------------------------------------
# Nuweb file i/o
#-----------------------------------------------------------------------

class InputFile(file):
    """Supports iteration over an input file, eating all occurrences
    of at-percent from the occurrence's position to the first
    non-white-space character in the next line.

    Because a commented-out line can appear to have zero length,
    end-of-file is indicated by the public instance variable
    'at_end'."""
    at_percent_matcher = re.compile(r'(?m)@%.*$\s')
    non_whitespace_matcher = re.compile(r'^\s*')
    def __init__(self, path, mode='r'):
        file.__init__(self, path, mode)
        self.at_end = False
        self.line_number = 0
        self.path = path
        self.skipping_after_at_percent = False
    def readline(self):
        l = file.readline(self)
        self.at_end = len(l) == 0
        self.line_number = self.line_number + 1
        if self.skipping_after_at_percent:
            self.skipping_after_at_percent = False
            l = re.sub(InputFile.non_whitespace_matcher, '', l)
        if re.search(InputFile.at_percent_matcher, l):
            self.skipping_after_at_percent = True
        return re.sub(InputFile.at_percent_matcher, '', l)

class OutputCodeFile:
    """The contents are written to a temporary file (nw* in the
    current directory). When the file is closed, the contents are
    compared to an existing file of the desired name, if any. If there
    is no change, the existing file is left unchanged; if the file
    doesn't already exist, or has changed, the new contents are
    written to it.

    Also replaces @@ by @.

    Also ensures that trailing white space is eliminated from all code
    output files. This is really just for neatness, but in the case of
    GCC Ada, the standard style checks (-gnaty) warn about trailing
    white space (which, incidentally, includes non-zero-length blank
    lines)."""
    def __init__(self, path):
        self.path = path
        self.tempfile = tempfile.TemporaryFile(dir=".", prefix="nw")
        self.buffer = ''
    def write(self, text):
        nl = text.find("\n")
        if nl >= 0:
            self.tempfile.write\
                ((self.buffer + text[:nl -1]).replace("@@", "@").rstrip())
            self.tempfile.write("\n")
            self.buffer = ''
            self.write(text[nl + 1:])
        else:
            self.buffer = self.buffer + text
    def close(self):
        if len(self.buffer) > 0:
            self.write("\n")
        self.tempfile.seek(0)
        new_content = self.tempfile.readlines()
        self.tempfile.close()
        try:
            outfile = open(self.path, "r")
            current_content = outfile.readlines()
            outfile.close()
            if new_content == current_content:
                sys.stderr.write("output file %s unchanged.\n" % self.path)
                return
            else:
                sys.stderr.write("output file %s has changed.\n" % self.path)
        except:
            sys.stderr.write("creating output file %s.\n" % self.path);
        try:
            outfile = open(self.path, "w")
            outfile.writelines(new_content)
            outfile.close()
        except:
            sys.stderr.write("unable to create output file %s.\n" % self.path)


#-----------------------------------------------------------------------
# Fragment Name management (NOT PRESENTLY USED)
#-----------------------------------------------------------------------

class FragmentNames(dict):
    """Nuweb fragment names can be abbreviated (by ending with "..."),
    in which case there is expected to be a matching full name (not
    ending in "..."), either in the definition of a fragment or in an
    invocation.

    Fragment names are expanded fully in the LaTeX output.

    The invocation may naturally appear first in the document, but the
    processing order is to read the document (and discover the
    fragments) before processing the fragment contents to generate
    code."""
    def __missing__(self, key):
        if key[-3:] == "...":
            for k in self.keys:
                if k[:len(key)-3] == key[:-3]:
                    self[key] = k
                    return k
            raise KeyError

# In 'fragment_names' (which is a FragmentNames dictionary),
# abbreviated keys (ending in "...") are only found if there's a
# matching full definition.
fragment_names = FragmentNames()


#-----------------------------------------------------------------------
# CodeLine class and children
#-----------------------------------------------------------------------

class CodeLine():
    pass

class LiteralCodeLine(CodeLine):
    pass

class InvocatingCodeLine(CodeLine):
    pass


#-----------------------------------------------------------------------
# DocumentElement class and children
#-----------------------------------------------------------------------

class DocumentElement():
    """The abstract root of the tree of elements that make up the
    document. The document is a sequence of Text, Code and Index
    elements."""
    def generate_code(self):
        pass
    def generate_document(self, output):
        output.write(self.text)
    def matches(self, definition):
        return False

class Text(DocumentElement):
    """Contains LaTeX text from the original nuweb source file."""
    def __init__(self, text):
        self.text = re.sub(r'@@', '@', text)

class CodeElement(DocumentElement):
    """May be a File or a Fragment.

    'name' is either the file name or the definition name.  'text' is
    the code content.  'defines' is a list of the identifiers defined
    by the element.  'splittable' is True if the text is allowed to be
    split over a page boundary in the printed document (a minipage
    environment is used to prevent splitting if necessary)."""

    # The scrap sequence number, used as the index (key) to
    # code_elements.
    scrap_number = 1

    # Regexes for matching fragment invocations and any
    # parameters. Note, at this time only the old-style bracketed
    # parameterisation is handled.
    invocation_matcher = re.compile(r'(?P<start>.*)'
                                    + r'@<'
                                    + r'(?P<invocation>.*?)'
                                    + r'@>'
                                    + r'(?P<end>).*')
    parameter_matcher = re.compile(r'(?P<name>.*)'
                                   + r'(@\((?P<parameters>.*)@\))')

    def __init__(self, name, text, defines, splittable):
        self.name = name
        # remove any CRs, rely on Python to generate CRLF on output if
        # required.
        self.text = re.sub(r'\r', '', text)
        self.defines = defines
        self.splittable = splittable
        self.scrap_number = CodeElement.scrap_number
        CodeElement.scrap_number = CodeElement.scrap_number + 1
        code_elements[self.scrap_number] = self

    def write_code(self, stream, indent, parameters = []):
        """Output the code to 'stream', indenting lines after the
        first by 'indent'."""
        def substitute(l):
            """Replace each occurrence in 'l' of '@[1..9]' with the
            corresponding element of 'parameters'."""
            for j in range(len(parameters)):
                l = l.replace("@%d" % (j + 1), parameters[j])
            return l
        # Insert the indentation and split into lines.
        lines = re.sub(r'\n', "\n" + indent, self.text).split("\n")
        for l in lines[:-1]:
            self.write_code_line(stream, substitute(l), True)
        self.write_code_line(stream, substitute(lines[-1]), False)

    def write_code_line(self, stream, line, ends_with_newline):
        """Output 'line' to 'stream'."""
        m = re.match(CodeElement.invocation_matcher, line)
        if not m:
            stream.write(line)
        else:
            stream.write(m.group('start'))
            indent = re.sub(r'\S', ' ', m.group('start')).expandtabs()
            name = m.group('invocation').strip()
            parameters = []
            n = re.match(CodeElement.parameter_matcher, name)
            if n:
                name = n.group('name').strip()
                parameters = re.split(r'\s*@,\s*',
                                      n.group('parameters').strip())
            for d in document:
                if d.matches(name):
                    d.write_code(stream, indent, parameters)
            stream.write(m.group('end'))
        if ends_with_newline:
            stream.write("\n")

    def generate_document(self, output):
        output.write("\\begin{flushleft} \\small\n")
        if not self.splittable:
            output.write("\\begin{minipage}{\\linewidth}")
        output.write("\\label{scrap%d}\\raggedright\\small\n"
                     % self.scrap_number)
        self.write_title(output)
        output.write("\\vspace{-1ex}\n")
        output.write("\\begin{list}{}{} \\item\n")
        lines = self.text.split("\n")
        # Don't write the last line if it's empty (to avoid a blank
        # line above the NWsep, which is diamond if not redefined).
        if len(lines[-1]) == 0:
            lines = lines[:-1]
        for l in lines:
            self.write_latex_line(output, l)
        output.write("\\mbox{}{\NWsep}\n")
        output.write("\\end{list}\n")
        if not self.splittable:
            output.write("\\end{minipage}\\vspace{2ex}\n")
        output.write("\\end{flushleft}\n")

    def write_latex_line(self, output, line):
        # The standard substitutions
        subs = [["@<", "@$\\langle\\,$\\verb@"],
                ["@>", "@$\\rangle\\,$\\verb@"],
                ["@(", "("],
                ["@'", "'"],
                ["@,", ","],
                ["@)", ")"]]
        # Does this line invoke a fragment?
        m = re.match(CodeElement.invocation_matcher, line)
        if m:
            # We need to find all the fragments which match this
            # invocation.

            # If there are none, no action (I suppose we could output
            # a warning?).

            # Otherwise, we output a page/scrap-on-page link just
            # before the closing >.

            # If there is only one the link is to that fragment.

            # If there are more than one, the link is to the first,
            # followed by ', ...'

            # Assumption: only one invocation on a line!

            name = m.group('invocation').strip()
            # Check whether the invocation includes parameters.
            n = re.match(CodeElement.parameter_matcher, name)
            if n:
                name = n.group('name').strip()
            # Find all the elements with that name
            elements = [e for e in document if e.matches(name)]
            if len(elements) > 0:
                e = elements[0]
                try:
                    link = "%s%s" % (e.page_number, e.scrap_on_page)
                except:
                    link = "??"
                link = r'{\footnotesize \NWlink{nuweb%s}{%s}}' % (link, link)
                if len(elements) > 1:
                    link = link + ', ...'
                # Reconstitute the original line, ready for
                # substitution and output.
                line = m.group('start') \
                    + '@<' \
                    + m.group('invocation') \
                    + link \
                    + ' @>' \
                    + m.group('end')

        # Remove double-at, so that the @@ in a substring like "@@,"
        # doesn't remain live and cause the substring to get treated
        # as "@,".
        li = line.split("@@")
        r = []
        for l in li:
            # Perform the standard substitutions of '@' followd by a
            # character.
            for s in subs:
                l = l.replace(s[0], s[1])
            # Parameters like "@1" must appear as themselves.
            r.append(re.sub(r'@([1-9])', r'@\\verb|@|\\verb@\1', l))
        # Finally, reinsert any "@@"s from the original line as just
        # "@", and output.
        output.write\
            ("\\mbox{}\\verb@%s@\\\\\n" % "@\\verb|@|\\verb@".join(r))

class File(CodeElement):
    """Forms part of a named file. The whole file is composed of all
    the File objects with the same name, concatenated in document
    order."""
    def __init__(self, name, text, defines, splittable):
        """The 'name' consists of a filename (without spaces) and
        optional flags."""
        name_parts = name.split()
        CodeElement.__init__(self, name_parts[0], text, defines, splittable)
        self.flags = name_parts[1:]
    def generate_code(self):
        if not self.name in files:
            files[self.name] = OutputCodeFile(self.name)
        self.write_code(files[self.name], '')
    def write_title(self, output):
        try:
            link = "%s%s" % (self.page_number, self.scrap_on_page)
        except:
            link = "??"
        output.write("\\NWtarget{nuweb%s}{}\\verb@\"%s\"@"
                     "\\nobreak\\ {\\footnotesize{%s}}$\\equiv$\n"
                     % (link, self.name, link))

class Fragment(CodeElement):
    """Forms part of a definition. The whole definition is composed of
    all the Fragments with the same name, in document order."""
    def matches(self, invocation):
        if self.name == invocation:
            return True
        elif invocation[-3:] == '...' \
                and self.name[:len(invocation)-3] == invocation[:-3]:
            return True
        elif self.name[-3:] == '...' \
                and invocation[:len(self.name)-3] == self.name[:-3]:
            # This covers the case where the invocation is the full name.
            self.name = invocation
            return True
        else:
            return False
    def write_title(self, output):
        try:
            link = "%s%s" % (self.page_number, self.scrap_on_page)
        except:
            link = "??"
        output.write("\\NWtarget{nuweb%s}{}$\\langle\\,${\\it %s}"
                     "\\nobreak\\ {\\footnotesize{%s}}$\\,\\rangle\\equiv$\n"
                     % (link, self.name, link))

def code_element_init(segment):
    """Given a segment of the document that corresponds to a File or
    Fragment, this factory function determines the kind, name, text
    and definitions and returns an initialized CodeElement of the
    right kind."""
    matcher = re.compile(r'(?s)'
                         + r'@(?P<kind>[oOdD])'
                         + r'\s*'
                         + r'(?P<name>.*?)'
                         + r'@{(?P<text>.*?)'
                         + r'(@\|(?P<defines>.*))?'
                         + r'@}')
    m = re.match(matcher, segment)
    try:
        kind = m.group('kind')
        name = m.group('name').strip()
        text = m.group('text')
        if m.group('defines'):
            defines = m.group('defines').split()
        else:
            defines = []
    except:
        sys.stderr.write("failed code_element_init(%s)\n" % segment)
        sys.exit(1)
    if kind == 'o':
        return File(name, text, defines, False)
    elif kind == 'O':
        return File(name, text, defines, True)
    elif kind == 'd':
        return Fragment(name, text, defines, False)
    elif kind == 'D':
        return Fragment(name, text, defines, True)


#-----------------------------------------------------------------------
# Main
#-----------------------------------------------------------------------

def read_nuweb(path):
    """Reads the .w file specified at 'path' (and any other files
    included using @i), forming a sequence of DocumentElements held in
    'document'."""
    global document

    try:
        input = InputFile(path, 'r')
    except:
        sys.stderr.write("couldn't open %s for input.\n" % path)
        sys.exit(1)

    latex_text = ''
    while True:
        line = input.readline()
        if input.at_end:
            break
        if re.search(r'(?<!@)@i\s*(?P<filename>\S*)', line):
            m = re.match(r'@i\s*(?P<filename>\S*)', line)
            read_nuweb(m.group('filename'))
        elif re.search(r'(?<!@)@[oOdD]', line):
            m = re.match(r'(?P<text>.*)(?P<start>(?<!@)@[oOdD].*)', line)
            latex_text = latex_text + m.group('text')
            # Save the LaTeX text
            document.append(Text(latex_text))
            element_text = ''
            line = m.group('start')
            while not re.search(r'@}', line):
                element_text = element_text + line
                line = input.readline()
                if input.at_end:
                    sys.stderr.write("file %s ended within fragment.\n"
                                     % input.path)
                    sys.exit(1)
            n = re.match(r'(?P<fragment>.*@})(?P<text>.*)', line)
            element_text = element_text + n.group('fragment')
            document.append(code_element_init(element_text))
            latex_text = n.group('text')
        else:
            latex_text = latex_text + line
    # Save the last LaTeX text
    document.append(Text(latex_text))

    sys.stderr.write("file %s has %d lines.\n"
                     % (input.path, input.line_number))

    input.close()

def read_aux(path):
    """LaTeX generates <basename>.aux, which contains (inter alia) the
    page on which each \label{} occurs. Nuweb generates a label for
    each scrap (\label{scrapnnn}; the first time through (when no .aux
    file is found) and when the number of scraps increases, we report
    that nuweb needs to be re-run. Of course, the same will be true if
    a scrap ends up on a different page, so this is more of a hint
    than an 'if and only if' indication.

    Once we know whch page a scrap is on, and which scrap it is on
    that page, we can create the proper cross-reference."""

    global code_elements, need_to_rerun

    try:
        input = open(path, 'r')
    except:
        need_to_rerun = True
        return

    page = -1  # impossible value

    for l in input:
        m = re.match(r'\\newlabel{scrap(?P<scrap>\d+)}{{.*}{(?P<page>\d+)}', l)
        if m:
            scrap = int(m.group('scrap'))
            p = m.group('page')
            if p == page:
                scrap_on_page = chr(ord(scrap_on_page) + 1)
            else:
                page = p
                scrap_on_page = 'a'
            try:
                code_elements[scrap].page_number = page
                code_elements[scrap].scrap_on_page = scrap_on_page
            except:
                need_to_rerun = True


def main():

    global hyperlinks

    def usage():
	sys.stderr.write('%s $Revision: 9583e6f87a23 $\n' % sys.argv[0])
	sys.stderr.write('usage: nuweb.py [flags] nuweb-file\n')
	sys.stderr.write('flags:\n')
	sys.stderr.write('-h, --help:              '
			 + 'output this message\n')
	sys.stderr.write('-r, --hyperlinks:        '
			 + 'generate hyperlinks\n')

    try:
        opts, args = getopt.getopt\
	    (sys.argv[1:],
	     "hr",
	     ["help", "hyperlinks", ])
    except getopt.GetoptError:
        usage()
        sys.exit(1)

    for o, v in opts:
	if o in ("-h", "--help"):
	    usage()
	    sys.exit(0)
        elif o in ("-r", "--hyperlinks"):
            hyperlinks = True

    if len(args) != 1:
        usage()
        sys.exit(1)
    arg = args[0]

    if arg[-2:] == ".w":
        input_filename = arg
        basename = arg[:-2]
    else:
        input_filename = arg + ".w"
        basename = arg

    read_nuweb(input_filename)
    read_aux(basename + '.aux')

    # We need to resolve the abbreviated fragment references.

    # This code handles references where the full form is found in one
    # of the fragment definitions. The case where the full form
    # appears in a fragment invocation only is handled in
    # Fragment.matches(). Note that, in that case, the error of having
    # multiple possible expansions probably won't be detected.
    #
    # Personally, I'd like to remove fragment name abbreviation; it's
    # a recipe for subtle errors.
    for d in document:
        if isinstance(d, Fragment):
            if d.name[-3:] == '...':
                replacement = []
                for e in document:
                    if isinstance(e, Fragment) \
                            and e != d \
                            and e.name[-3:] != '...' \
                            and e.name[:len(d.name)-3] == d.name[:-3] :
                        if not e.name in replacement:
                            replacement.append(e.name)
                if len(replacement) > 1:
                    sys.stderr.write\
                        ("multiple expansions for definition \"%s\".\n"
                         % d.name)
                    sys.exit(1)
                elif len(replacement) == 1:
                    d.name = replacement[0]

    for d in document:
        d.generate_code()

    for f in files.values():
        f.close()

    doc = open(basename + ".tex", "w")

    def define_macro(stream, macro, definition):
        stream.write("\\newcommand{\\%s}{%s}\n" % (macro, definition))

    if hyperlinks:
        doc.write ("%s\n"
                   % "\\newcommand{\\NWtarget}[2]{\\hypertarget{#1}{#2}}")
        doc.write ("%s\n" % "\\newcommand{\\NWlink}[2]{\\hyperlink{#1}{#2}}")
    else:
        doc.write ("%s\n" % "\\newcommand{\\NWtarget}[2]{#2}")
        doc.write ("%s\n" % "\\newcommand{\\NWlink}[2]{#2}")

    define_macro (doc, "NWtxtMacroDefBy", "Fragment defined by")
    define_macro (doc, "NWtxtMacroRefIn", "Fragment referenced in")
    define_macro (doc, "NWtxtMacroNoRef", "Fragment never referenced")
    define_macro (doc, "NWtxtDefBy", "Defined by")
    define_macro (doc, "NWtxtRefIn", "Referenced in")
    define_macro (doc, "NWtxtNoRef", "Not referenced")
    define_macro (doc, "NWtxtFileDefBy", "File defined by")
    define_macro (doc, "NWtxtIdentsUsed", "Uses:")
    define_macro (doc, "NWtxtIdentsNotUsed", "Never used")
    define_macro (doc, "NWtxtIdentsDefed", "Defines:")
    define_macro (doc, "NWsep", "${\\diamond}$")
    define_macro (doc, "NWnotglobal", "(not defined globally)")
    define_macro (doc, "NWuseHyperlinks", "")

    for d in document:
        d.generate_document(doc)

    doc.close()

    if need_to_rerun:
        sys.stderr.write('Need to re-run nuweb.py after running latex.\n')


if __name__ == '__main__':
    main()
