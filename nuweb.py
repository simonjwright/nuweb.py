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

# $Id: nuweb.py,v b1be54e8ff71 2011/08/18 22:03:20 simonjwright $

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

    Also replaces @@ by @. (XXX haven't we done this already?)

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
                ((self.buffer + text[:nl]).replace("@@", "@").rstrip())
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
                #sys.stderr.write("output file %s unchanged.\n" % self.path)
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
    """A CodeLine is a line of code text from a File or Fragment,
    including any terminating \n."""

    # Regexes for matching fragment invocations and any parameters
    # while reading in. Note, at this time only the old-style
    # bracketed parameterisation is handled.
    invocation_matcher = re.compile(r'(?s)'
                                    + r'(?P<start>.*)'
                                    + r'@<'
                                    + r'(?P<invocation>.*?)'
                                    + r'@>'
                                    + r'(?P<end>.*)')
    parameter_matcher = re.compile(r'(?P<name>.*)'
                                   + r'(@\((?P<parameters>.*)@\))')

    @staticmethod
    def factory(line):
        if re.match(CodeLine.invocation_matcher, line):
            return InvocatingCodeLine(line)
        else:
            return LiteralCodeLine(line)

    @staticmethod
    def substitute_parameters(l, parameters):
        """Replace each occurrence in 'l' of '@[1..9]' with the
        corresponding element of 'parameters'."""
        for j in range(len(parameters)):
            l = l.replace("@%d" % (j + 1), parameters[j])
        return l

    @staticmethod
    def substitute_at_symbols(str):
        """Unescape @' etc for LaTeX output."""

        # Remove double-at, so that the @@ in a substring like "@@,"
        # doesn't remain live and cause the substring to get treated
        # as "@,".
        # Split at '@@' (later, we'll rejoin with '@').
        ss = str.split("@@")

        # Un-escape @' etc. Parameters like '@1' must appear as
        # themselves, which is slightly awkward because the LaTeX
        # output is embedded in \verb@...@. Any @ we want to appear in
        # the output is inserted by terminating the current \verb@
        # environment, including a \verb|@|, and restarting the verb@
        # environment.
        def subs(st):
            for s in [["@'", "'"], ["@,", ","]]:
                st = st.replace(s[0], s[1])
            return st
        ss = [re.sub(r'@([1-9])',
                     r'@\\verb|@|\\verb@\1',
                     subs(s))
              for s in ss]

        # Rejoin the string with a verbatim @.
        str = r'@\verb|@|\verb@'.join(ss)

        return str

    def write_code(self, stream, indent, parameters):
        """Writes self to 'stream' as code, indented by 'indent'. To
        be overridden."""
        pass

    def write_latex(self, stream):
        """Writes self to 'stream' as LaTeX. To be overridden."""
        pass

class LiteralCodeLine(CodeLine):
    """A line of code that contains text but no fragment invocations."""

    def __init__(self, line):
        self.text = line

    def write_code(self, stream, indent, parameters):
        """Writes self to 'stream' as code, indented by 'indent', with
        substitution of 'parameters'."""
        stream.write("%s%s"
                     % (indent,
                        CodeLine.substitute_parameters(self.text, parameters)))

    def write_latex(self, stream):
        """Writes self to 'stream' as LaTeX."""
        line = re.sub(r'\n', '', self.text)
        stream.write("\\mbox{}\\verb@%s@\\\\\n"
                     % CodeLine.substitute_at_symbols(line))

class InvocatingCodeLine(CodeLine):
    """A line of code that contains a fragment invocation."""
    # XXX only one invocation! They may be nested!

    def __init__(self, line):
        m = re.match(CodeLine.invocation_matcher, line)
        name = m.group('invocation').strip()
        n = re.match(CodeLine.parameter_matcher, name)
        if n:
            name = n.group('name').strip()
            parameters = re.split(r'\s*@,\s*',
                                  n.group('parameters').strip())
        else:
            parameters = []
        self.start = m.group('start')
        self.name = name
        self.parameters = parameters
        self.end = m.group('end')

    def write_code(self, stream, indent, parameters):
        """Writes self to 'stream' as code, indented by 'indent'."""
        stream.write(self.start)
        params = [CodeLine.substitute_parameters(p, parameters)
                  for p in self.parameters]
        # Note the indent needed for the fragments (where we are now;
        # the input indent was where we began, and we've now added the
        # characters in the self.start sequence).
        new_indent = indent +  re.sub(r'\S', ' ', self.start).expandtabs()
        fragments = [d for d in document if d.matches(self.name)]
        if len(fragments) == 0:
            sys.stderr.write("no fragments matching '%s'.\n" % self.name)
        else:
            fragments[0].write_code(stream, new_indent, params)
            for f in fragments[1:]:
                # For follow-on fragments, we have to output the new
                # indentation at the beginning of the new line. NB,
                # this assumes that if there is anything in self.end,
                # there'll only be ome matching fragment.
                stream.write(new_indent)
                f.write_code(stream, new_indent, params)
        stream.write(self.end)

    def write_latex(self, stream):
        """Writes self to 'stream' as LaTeX."""

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

        # Check whether the invocation includes parameters, and
        # form the LaTeX text accordingly.
        if len(self.parameters) > 0:
            parameters = [CodeLine.substitute_at_symbols(p)
                          for p in self.parameters]
            text = r'{\it %s}\ (\verb@%s@)' % (self.name,
                                               ", ".join(parameters))
        else:
            text = r'{\it %s}' % self.name
        # Find all the elements with the invoked name
        elements = [e for e in document if e.matches(self.name)]
        if len(elements) > 0:
            e = elements[0]
            try:
                link = '%s%s' % (e.page_number, e.scrap_on_page)
            except:
                link = '??'
            link = r'{\footnotesize \NWlink{nuweb%s}{%s}}' % (link, link)
        else:
            link = r'{\footnotesize (not found)}'
        if len(elements) > 1:
            link = link + r',\ ...'
        # Reconstitute the line, making substitutions.
        line = CodeLine.substitute_at_symbols(self.start) \
            + r'@$\langle\,$' \
            + text \
            + r'\ ' \
            + link \
            + r'\,$\rangle\,$\verb@' \
            + CodeLine.substitute_at_symbols(re.sub(r'\n', '', self.end))

        # Output the line.
        stream.write("\\mbox{}\\verb@%s@\\\\\n" % line)


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
    def defined_by(self):
        """Returns a list of other Fragments with the same name, ie
        which taken together define the whole fragment."""
        return []
    def referenced_in(self):
        """Returns a list of CodeElements which reference this
        Fragment."""
        return []
    def defines(self):
        """Returns a list of the variable definitions made by this
        CodeElement and where they're used."""
        return []
    def uses(self):
        """Returns a list of the variable definitions used by this
        CodeElement (not including any it defines itself)."""
        return []

class Text(DocumentElement):
    """Contains LaTeX text from the original nuweb source file."""
    def __init__(self, text):
        self.text = re.sub(r'@@', '@', text)

class CodeElement(DocumentElement):
    """May be a File or a Fragment.

    'name' is either the file name or the definition name.  'text' is
    the code content.  'definitions' is a list of the identifiers
    defined by the element.  'splittable' is True if the text is
    allowed to be split over a page boundary in the printed document
    (otherwise a minipage environment is used to prevent splitting)."""

    # The scrap sequence number, used as the index (key) to
    # code_elements.
    scrap_number = 1

    @staticmethod
    def factory(segment):
        """Given a segment of the document that corresponds to a File or
        Fragment, this factory function determines the kind, name, text
        and definitions and returns an initialized CodeElement of the
        right kind."""
        matcher = re.compile(r'(?s)'
                             + r'@(?P<kind>[oOdD])'
                             + r'\s*'
                             + r'(?P<name>.*?)'
                             + r'@{(?P<text>.*?)'
                             + r'(@\|(?P<definitions>.*))?'
                             + r'@}')
        m = re.match(matcher, segment)
        try:
            kind = m.group('kind')
            name = m.group('name').strip()
            text = m.group('text')
            if m.group('definitions'):
                definitions = m.group('definitions').split()
            else:
                definitions = []
        except:
            sys.stderr.write("failed CodeElement.factory(%s)\n" % segment)
            sys.exit(1)
        if kind == 'o':
            return File(name, text, definitions, False)
        elif kind == 'O':
            return File(name, text, definitions, True)
        elif kind == 'd':
            return Fragment(name, text, definitions, False)
        elif kind == 'D':
            return Fragment(name, text, definitions, True)

    @staticmethod
    def write_elements(stream, elements):
        """'elements' is a list of CodeElements whose page/scrap-on-page
        references are to be written to 'stream'."""
        # Start with an impossible page number
        page = -1
        for e in elements:
            if e.page_number != page and page != -1:
                # Insert a ', ' separator for new pages after the
                # first.
                stream.write(", ")
            # Write the link target.
            stream.write("\\NWlink{nuweb%s%s}"
                         % (e.page_number, e.scrap_on_page))
            if e.page_number != page:
                # This is a new page, so include the page number in
                # the link.
                stream.write("{%s%s}" % (e.page_number, e.scrap_on_page))
            else:
                # This is a further element on the same page, so the
                # link is just the scrap-on-page.
                stream.write("{%s}" % e.scrap_on_page)
            # Update the page number.
            page = e.page_number
        # Finish with a period.
        stream.write(".")

    def __init__(self, name, text, definitions, splittable):
        self.name = name
        # We want to split into lines, retaining the \n at the end of
        # all lines that have one already (which may not include the
        # last, or only, line).

        # We do this by making sure that all line terminators are \n\r
        # (NB, not the normal order) and splitting on \r.

        # We rely on Python to generate \r\n on output if required.
        text = re.sub(r'\r', '', text)
        # XXX needed for generate_document()
        self.text = text
        text = re.sub(r'\n', r'\n\r', text)
        # We need to keep the trailing \n, if there is one, but not to
        # get an empty line because of the split on the trailing \r.
        if text[-1] == '\r':
            text = text[:-1]
        self.lines = [CodeLine.factory(l) for l in text.split("\r")]
        self.definitions = definitions
        self.splittable = splittable
        self.scrap_number = CodeElement.scrap_number
        CodeElement.scrap_number = CodeElement.scrap_number + 1
        code_elements[self.scrap_number] = self

    def write_code(self, stream, indent, parameters = []):
        """Output the code to 'stream', indenting all lines after the
        first by 'indent', and skipping the last line if it's
        blank."""
        self.lines[0].write_code(stream, '', parameters)
        for l in self.lines[1:]:
            l.write_code(stream, indent, parameters)

    def generate_document(self, output):
        output.write("\\begin{flushleft} \\small\n")
        if not self.splittable:
            output.write("\\begin{minipage}{\\linewidth}")
        output.write("\\label{scrap%d}\\raggedright\\small\n"
                     % self.scrap_number)
        self.write_title(output)
        output.write("\\vspace{-1ex}\n")
        output.write("\\begin{list}{}{} \\item\n")
        for l in self.lines:
            l.write_latex(output)
        output.write("\\mbox{}{\NWsep}\n")
        output.write("\\end{list}\n")
        output.write("\\vspace{-1ex}\n")
        defined_by = self.defined_by()
        referenced_in = self.referenced_in()
        defines = self.defines()
        uses = self.uses()
        if len(defined_by) > 1 \
                or len(referenced_in) > 0 \
                or len(defines) > 0 \
                or len(uses) > 0:
            # We only create this list environment for the
            # crossreferences if there are any (otherwise, we'd have
            # to create an empty \item)
            output.write("\\vspace{-1ex}\n")
            output.write("\\footnotesize\n")
            output.write("\\begin{list}{}{")
            output.write("\\setlength{\\itemsep}{-\\parsep}")
            output.write("\\setlength{\\itemindent}{-\\leftmargin}")
            output.write("}\n")

            if len(defined_by) > 1:
                output.write("\\item \NWtxtMacroDefBy\ ")
                CodeElement.write_elements(output, defined_by)
                output.write("\n")

            if len(referenced_in) > 0:
                output.write("\\item \NWtxtMacroRefIn\ ")
                CodeElement.write_elements(output, referenced_in)
                output.write("\n")

            #sys.stderr.write("'%s' defined_by %d fragments.\n" % (self.name, len(defined_by)))

            output.write("\\item{}\n")

            output.write("\\end{list}\n")
        if not self.splittable:
            output.write("\\end{minipage}\n")
        output.write("\\end{flushleft}\n")

    def XXXdefines(self):
        """Returns a list of the variable definitions made by this
        CodeElement."""
        return self.definitions

class File(CodeElement):
    """Forms part of a named file. The whole file is composed of all
    the File objects with the same name, concatenated in document
    order."""

    def __init__(self, name, text, definitions, splittable):
        """The 'name' consists of a filename (without spaces) and
        optional flags."""
        name_parts = name.split()
        CodeElement.__init__(self,
                             name_parts[0],
                             text,
                             definitions,
                             splittable)
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

    def defined_by(self):
        """Returns a list of other Fragments with the same name, ie
        which taken together define the whole fragment."""
        return [d for d in document if d.matches(self.name)]


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
            document.append(CodeElement.factory(element_text))
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
	sys.stderr.write('%s $Revision: b1be54e8ff71 $\n' % sys.argv[0])
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
