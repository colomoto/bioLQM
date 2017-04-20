package org.colomoto.biolqm.tool.implicants;

/* Copyright (c) 2013 the authors listed at the following URL, and/or
the authors of referenced articles or incorporated external code:
http://en.literateprograms.org/Quine-McCluskey_algorithm_(Java)?action=history&offset=20110925122251

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Retrieved from: http://en.literateprograms.org/Quine-McCluskey_algorithm_(Java)?oldid=17357
*/

import java.io.IOException;
import java.io.Reader;
import java.util.ArrayList;
import java.util.Arrays;


public class Term {

    public static final byte DontCare = -1;

    public Term(byte[] varVals) {
        this.varVals = varVals;
    }

    public int getNumVars() {
        return varVals.length;
    }
    
    public int getVarsValue(int i) {
        if (varVals[i] == DontCare){
            return -1;
        }
        if (varVals[i] == (byte)0) {
            return 0;
        }
        if (varVals[i] == (byte)1) {
            return 1;
        }
        
        return -2;
    }

    public String toString() {
        String result = "{";
        for(int i=0; i<varVals.length; i++) {
            if (varVals[i] == DontCare) {
                result += "X";
            } else {
                result += varVals[i];
            }
            result += " ";
        }
        result += "}";
        return result;
    }

    public Term combine(Term term) {
        int diffVarNum = -1; // The position where they differ
        for(int i=0; i<varVals.length; i++) {
            if (this.varVals[i] != term.varVals[i]) {
                if (diffVarNum == -1) {
                    diffVarNum = i;
                } else {
                    // They're different in at least two places
                    return null;
                }
            }
        }
        if (diffVarNum == -1) {
            // They're identical
            return null;
        }
        byte[] resultVars = varVals.clone();
        resultVars[diffVarNum] = DontCare;
        return new Term(resultVars);
    }

    public int countValues (byte value) {
        int result = 0;
        for(int i=0; i<varVals.length; i++) {
            if (varVals[i] == value) {
                result++;
            }
        }
        return result;
    }

    public boolean equals(Object o) {
        if (o == this) {
            return true;
        }
        if (o == null || !getClass().equals(o.getClass())) {
            return false;
        }
        Term rhs = (Term)o;
        return Arrays.equals(this.varVals, rhs.varVals);
    }

    public int hashCode() {
        return varVals.hashCode();
    }

    boolean implies(Term term) {
        for(int i=0; i<varVals.length; i++) {
            if (this.varVals[i] != DontCare &&
                this.varVals[i] != term.varVals[i]) {
                return false;
            }
        }
        return true;
    }

    public static Term read(Reader reader) throws IOException {
        int c = '\0';
        ArrayList<Byte> t = new ArrayList<Byte>();
        while (c != '\n' && c != -1) {
            c = reader.read();
            if (c == '0') {
                t.add((byte)0);
            } else if (c == '1') {
                t.add((byte)1);
            }
        }
        
        if (t.size() > 0) {
            byte[] resultBytes = new byte[t.size()];
            for(int i=0; i<t.size(); i++) {
                resultBytes[i] = (byte)t.get(i);
            }
            return new Term(resultBytes);
        }
        return null;
    }

    public static Term readintArray(int[] state) {
        if (state.length > 0) {
            byte[] resultBytes = new byte[state.length];
            for(int i=0; i<state.length; i++) {
                resultBytes[i] = (byte)state[i];
            }
            return new Term(resultBytes);
        }
        return null;
    }
    
    
    public int findNextRequiredNegation(int minidx, byte[] values) {
    	if (minidx > 0) {
    		// remove previously set value
    		values[minidx-1] = DontCare;
    	} else {
	    	// First check if this term actually needs to be negated
	    	for (int i=0 ; i<values.length ; i++) {
	    		if (values[i] != DontCare && values[i] != this.varVals[i] && varVals[i] != DontCare) {
	    			// The term is already negated, just skip it
	    			return -1;
	    		}
	    	}
    	}    	
    	// Then look for the next fixed element
    	for (int i=minidx ; i<values.length ; i++) {
    		if (varVals[i] != DontCare && values[i] == DontCare) {
    			if (varVals[i] == 0) {
    				values[i] = 1;
    			} else {
    				values[i] = 0;
    			}
    			return i;
    		}
    	}
    	
    	// No other valid negation was found
    	return -2;
    }


    private byte[] varVals;
}

