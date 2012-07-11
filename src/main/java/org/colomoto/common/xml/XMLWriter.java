package org.colomoto.common.xml;

import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * A helper to write well formed XML documents.
 */
public class XMLWriter {

    private OutputStreamWriter out = null;
    private List v_stack = new ArrayList();
    private boolean inTag;
    private boolean inContent;
    private boolean indent;
    private StringBuffer buf = null;

	/**
	 * Indicates if the given string is a valid GINsim ID (contains only a-z, A-Z, 0-9, "_" or "-" characters)
	 * 
	 * @param id the string to test
	 * @return true if the given string can be used as ID
	 */
	public static boolean isValidId(String id) {
		return Pattern.compile("^[a-zA-Z0-9_-]+$").matcher(id).find();
	}

    
    /**
     * Create a XMLWriter with the path to a file.
     * 
     * @param filename
     * @param dtdFile
     * @throws IOException
     */
    public XMLWriter(String filename, String dtdFile) throws IOException {
    	this(new FileOutputStream(filename), dtdFile);
	}

    /**
     * Create a XMLWriter with an existing Writer.
     * Warning: the writer should use the right encoding, otherwise we may get into troubles.
     * 
     * @param out
     * @param dtdFile
     * @throws IOException
     */
    public XMLWriter(OutputStreamWriter out, String dtdFile) throws IOException {
        this(out,dtdFile,true);
    }
    /**
     * Create a XMLWriter with an output stream.
     * It will create a Writer for this stream, using an UTF-8 encoding.
     * 
     * @param out
     * @param dtdFile
     * @throws IOException
     */
    public XMLWriter(OutputStream out, String dtdFile) throws IOException {
        this(new OutputStreamWriter(out, "UTF-8"),dtdFile,true);
    }

    /**
     * Create a XMLWriter with an existing Writer.
     * 
     * @param out
     * @param dtdFile
     * @param indent
     * @throws IOException
     */
    private XMLWriter(OutputStreamWriter out, String dtdFile, boolean indent) throws IOException {
        this.indent = indent;
        this.out = out;
        write("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
        if (dtdFile != null) {
            write("<!DOCTYPE gxl SYSTEM \""+dtdFile+"\">\n");
        }
    }
    
	/**
     * ask to store the next calls into a string buffer.
     * Use <code>getBuffer()</code> to stop it and get the content of the buffer.
     * @throws IOException
     */
    public void toBuffer()  throws IOException {
        if (inTag) {
            write(">");
            if (indent) {
                write("\n");
            }
            inTag = false;
        }
        buf = new StringBuffer();
    }

    /**
     * If <code>toBuffer</code> has previously been called, stop saving to buffer.
     * 
     * @return the content of the buffer.
     */
    public String getBuffer() {
        if (buf == null) {
            return null;
        }
        String s = buf.toString();
        buf = null;
        return s;
    }
    
    /**
     * @param s
     * @param isAttVal
     * @throws IOException
     */
    public void writeEsc (String s, boolean isAttVal) throws IOException
    {
    	StringCharacterIterator iterator = new StringCharacterIterator(s);
    	char cur = iterator.current();
    	while (cur != CharacterIterator.DONE) {
           switch (cur) {
           case '&':
               write("&amp;");
               break;
           case '<':
               write("&lt;");
               break;
           case '>':
               write("&gt;");
               break;
           case '\"':
                if (isAttVal) {
                    write("&quot;");
                } else {
                    write('\"');
                }
                break;
           default:
        	   write(cur);
           }
           cur = iterator.next();
        }
    }

    /**
     * @param s
     * @throws IOException
     */
    public void write(String s) throws IOException {
        if (buf != null) {
            buf.append(s);
            return;
        } 
        out.write(s);
    }
    
    /**
     * @param c
     * @throws IOException
     */
    public void write(char c) throws IOException {
        if (buf != null) {
            buf.append(c);
            return;
        } 
        out.write(c);
    }
    
    /**
     * open a tag in the XML output file
     * @param name
     * @throws IOException
     */
    public void openTag(String name) throws IOException {
    	if (inTag) {
    		write(">");
    		if (indent) {
    			write("\n");
    		}
    	}
        inTag = true;
        if (indent) {
        	for (int i=0 ; i<v_stack.size() ; i++) {
        		write("  ");
        	}
        }
        write("<"+name);
        v_stack.add(name);
        inContent = false;
    }

    /**
     * add (i.e. open and close) a tag without any attributes and content.
     * @param name
     * @throws IOException
     */
    public void addTag(String name) throws IOException {
    	openTag(name);
    	closeTag();
    }
    /**
     * add (i.e. open and close) a tag with specified attributes and content.
     * @param name
     * @param attributes
     * @param content
     * @throws IOException
     */
    public void addTag(String name, String[] attributes, String content) throws IOException {
    	openTag(name, attributes);
    	addContent(content);
    	closeTag();
    }
    /**
     * open a tag and add it the specified attributes.
     * @param name
     * @param attributes
     * @throws IOException
     */
    public void openTag(String name, String[] attributes) throws IOException {
    	openTag(name);
    	for (int i=0 ; i<attributes.length ; i+=2) {
    		addAttr(attributes[i], attributes[i+1]);
    	}
    }
    /**
     * add (i.e. open and close) a tag with specified attributes and no content.
     * @param name
     * @param attributes
     * @throws IOException
     */
    public void addTag(String name, String[] attributes) throws IOException {
    	openTag(name, attributes);
    	closeTag();
    }

    /**
     * add (i.e. open and close) a tag with specified content and no attributes.
     * @param tag
     * @param content
     * @throws IOException
     */
    public void addTagWithContent(String tag, Object content) throws IOException {
    	addTagWithContent(tag, content.toString());
    }
    /**
     * add (i.e. open and close) a tag with specified content and no attributes.
     * @param tag
     * @param content
     * @throws IOException
     */
    public void addTagWithContent(String tag, String content) throws IOException {
    	openTag(tag);
    	addContent(content);
    	closeTag();
    }

    /**
     * close the currently opened tag.
     * depending on context it will use "/&gt; or "&lt;/name&gt;"
     * @throws IOException
     */
    public void closeTag() throws IOException {
        int l = v_stack.size()-1;
        if (inTag) {
            write("/>");
        } else {
            if (!inContent && indent) {
                for (int i=0 ; i<l ; i++) {
                    write("  ");
                }
            }
            write("</"+v_stack.get(l)+">");
        }
        if (indent) {
        	write("\n");
        }
        v_stack.remove(l);
        inTag = false;
        inContent = false;
    }

    /**
     * add an attribute to the opened tag.
     * If the tag is no-more really opened it will return silently without writing anything.
     * @param name
     * @param value
     * @throws IOException
     */
    public void addAttr(String name, String value) throws IOException {
        if (!inTag) {
            return;
        }
        write(" "+name+"=\"");
        if (value == null) {
        	writeEsc("", true);
        } else {
        	writeEsc(value, true);
        }
        write("\"");
    }
    
    /**
     * add a "text child"
     * @param s
     * @throws IOException 
     */
    public void addContent(String s) throws IOException {
        if (inTag) {
            write(">");
            inTag = false;
        }
        writeEsc(s, false);
        inContent = true;
    }
    /**
     * add a "text child", already formated: should _NOT_ be escaped
     * @param s
     * @param b if true, then the file might get indented
     * @throws IOException 
     */
    public void addFormatedContent(String s, boolean b) throws IOException {
    	addLongContent(s, b, false);
    }

    /**
     * add a complex "text child", it will be enclosed into CDATA markers
     * @param s
     * @param b if true, then the file might get indented
     * @throws IOException 
     */
    public void addComplexContent(String s, boolean b) throws IOException {
    	addLongContent(s, b, true);
    }

    /**
     * Common implementation for formatted and complex content.
     * 
     * @param s
     * @param b
     * @param cdata
     * @throws IOException
     */
    private void addLongContent(String s, boolean b, boolean cdata) throws IOException {
        if (inTag) {
            write(">");
            inTag = false;
            if (b && indent) {
                write("\n");
            }
        }
        if (cdata) {
        	write("<![CDATA[");
        }
        write(s);
        if (cdata) {
        	write("]]>");
        }
    }
    
    /**
     * Close the writer: all open tags and the underlying outputstream.
     * 
     * @throws IOException
     */
    public void close() throws IOException {
    	while (v_stack.size() > 0) {
    		closeTag();
    	}
    	out.close();
    }
}
