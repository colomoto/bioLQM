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
    private List<String> v_stack = new ArrayList<String>();
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
     * @param filename the path to the output file
     * @param dtdFile path to the DTD file
     * @throws IOException if the output can not be written or fails
     */
    public XMLWriter(String filename, String dtdFile) throws IOException {
    	this(new FileOutputStream(filename), dtdFile);
	}

    /**
     * Create an XML writer with indenting enabled by default.
     * @param out the output writer
     * @param dtdFile path to the DTD file
     * @throws IOException if the output can not be written or fails
     */
    public XMLWriter(OutputStreamWriter out, String dtdFile) throws IOException {
        this(out,dtdFile,true);
    }
    
    /**
     * Create an XML writer with indenting enabled by default.
     * @param out the output stream
     * @param dtdFile path to the DTD file
     * @throws IOException if the output can not be written or fails
     */
    public XMLWriter(OutputStream out, String dtdFile) throws IOException {
        this(new OutputStreamWriter(out, "UTF-8"),dtdFile, true);
    }

    /**
     * Create a XMLWriter with an existing Writer.
     * 
     * @param out the output writer
     * @param dtdFile path to the DTD file
     * @param indent enable indenting (pretty printing)
     * @throws IOException if the output can not be written or fails
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
     * @throws IOException if writing fails
     */
    public void toBuffer() throws IOException {
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
     * Write a string, using escaping as needed.
     * 
     * @param s the String to write
     * @param isAttVal specifies if this is an attribute value (uses a different escaping)
     * @throws IOException if writing fails
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
     * Write a String directly (no escaping)
     * @param s the string to write
     * @throws IOException if writing fails
     */
    public void write(String s) throws IOException {
        if (buf != null) {
            buf.append(s);
            return;
        } 
        out.write(s);
    }
    
    /**
     * Write a single character directly t the output
     * @param c the character
     * @throws IOException if writing fails
     */
    public void write(char c) throws IOException {
        if (buf != null) {
            buf.append(c);
            return;
        } 
        out.write(c);
    }
    
    /**
     * Open a tag in the XML output file
     * @param name the tag name
     * @throws IOException if writing fails
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
     * Add (i.e. open and close) a tag without any attributes and content.
     * @param name the tag name
     * @throws IOException if writing fails
     */
    public void addTag(String name) throws IOException {
    	openTag(name);
    	closeTag();
    }
    /**
     * Add (i.e. open and close) a tag with specified attributes and content.
     * @param name the tag name
     * @param attributes the list of attributes and their values
     * @param content the content text
     * @throws IOException if writing fails
     */
    public void addTag(String name, String[] attributes, String content) throws IOException {
    	openTag(name, attributes);
    	addContent(content);
    	closeTag();
    }
    /**
     * Open a tag and add it the specified attributes.
     * @param name the tag name
     * @param attributes the list of attributes and their values
     * @throws IOException if writing fails
     */
    public void openTag(String name, String[] attributes) throws IOException {
    	openTag(name);
    	for (int i=0 ; i<attributes.length ; i+=2) {
    		addAttr(attributes[i], attributes[i+1]);
    	}
    }
    /**
     * Add (i.e. open and close) a tag with specified attributes and no content.
     * @param name the tag name
     * @param attributes the list of attributes and their values
     * @throws IOException if writing fails
     */
    public void addTag(String name, String[] attributes) throws IOException {
    	openTag(name, attributes);
    	closeTag();
    }

    /**
     * Add (i.e. open and close) a tag with specified content and no attributes.
     * 
     * @param tag the tag name
     * @param content the content to add
     * @throws IOException if writing fails
     */
    public void addTag(String tag, String content) throws IOException {
    	openTag(tag);
    	addContent(content);
    	closeTag();
    }

    /**
     * Close the currently opened tag.
     * Depending on context it will use "/&gt; or "&lt;/name&gt;"
     * 
     * @throws IOException if writing fails
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
     * @param name the attribute name
     * @param value the attribute value
     * @throws IOException if writing fails
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
     * Add a "text child"
     * @param s the content to write (will be escaped as needed)
     * @throws IOException if writing fails
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
     * Add a "text child", already formated: should _NOT_ be escaped
     * @param s the formatted content
     * @param b if true, then the file might get indented
     * @throws IOException if writing fails
     */
    public void addFormatedContent(String s, boolean b) throws IOException {
    	addLongContent(s, b, false);
    }

    /**
     * Add a complex "text child", it will be enclosed into CDATA markers
     * @param s the complex content
     * @param b if true, then the file might get indented
     * @throws IOException if writing fails
     */
    public void addComplexContent(String s, boolean b) throws IOException {
    	addLongContent(s, b, true);
    }

    /**
     * Common implementation for formatted and complex content.
     * 
     * @param s the content
     * @param b if true, will use indenting
     * @param cdata if true, content will be encapsulated in CDATA markers
     * @throws IOException if writing fails
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
     * Close the writer: automatically closes all open tags and the underlying output stream.
     * 
     * @throws IOException if writing fails
     */
    public void close() throws IOException {
    	while (v_stack.size() > 0) {
    		closeTag();
    	}
    	out.close();
    }
}
