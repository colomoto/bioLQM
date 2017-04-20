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
import java.util.List;


public class Formula {

    private List<Term> termList;
    private List<Term> originalTermList;
    
    public final int[] regulators;

    public Formula(List<Term> termList) {
    	this(termList, null);
    }
    
    public Formula(List<Term> termList, int[] regulators) {
        this.termList = termList;
        this.regulators = regulators;
    }

    public int[][] toArray() {
        int[][] resultArray=new int[termList.size()][termList.get(0).getNumVars()];
        for(int i=0; i<termList.size(); i++) {
            for(int j=0; j<termList.get(0).getNumVars(); j++) {
                resultArray[i][j]=termList.get(i).getVarsValue(j);
            }
        }
        return resultArray;
    }
    
    public String toString() {
        String result = "";
        result += termList.size() + " terms, " + termList.get(0).getNumVars() + " variables\n";
        for(int i=0; i<termList.size(); i++) {
            result += termList.get(i) + "\n";
        }
        return result;
    }
    
    public Iterable<Term> getTerms() {
    	return termList;    	
    }
    
    @SuppressWarnings("unchecked")
    public void reduceToPrimeImplicants() {
        originalTermList = new ArrayList<Term>(termList);
        int numVars = termList.get(0).getNumVars();
        ArrayList<Term>[][] table = new ArrayList[numVars + 1][numVars + 1];
        for(int dontKnows=0; dontKnows <= numVars; dontKnows++) {
            for(int ones=0; ones <= numVars; ones++) {
                table[dontKnows][ones] = new ArrayList<Term>();
            }
        }
        for(int i=0; i<termList.size(); i++) {
            int dontCares = termList.get(i).countValues(Term.DontCare);
            int ones      = termList.get(i).countValues((byte)1);
            table[dontCares][ones].add(termList.get(i));
        }
        for(int dontKnows=0; dontKnows <= numVars - 1; dontKnows++) {
            for(int ones=0; ones <= numVars - 1; ones++) {
                ArrayList<Term> left   = table[dontKnows][ones];
                ArrayList<Term> right  = table[dontKnows][ones + 1];
                ArrayList<Term> out    = table[dontKnows+1][ones];
                for(int leftIdx = 0; leftIdx < left.size(); leftIdx++) {
                    for(int rightIdx = 0; rightIdx < right.size(); rightIdx++) {
                        Term combined = left.get(leftIdx).combine(right.get(rightIdx));
                        if (combined != null) {
                            if (!out.contains(combined)) {
                                out.add(combined); 
                            }
                            termList.remove(left.get(leftIdx));
                            termList.remove(right.get(rightIdx));
                            if (!termList.contains(combined)) {
                                termList.add(combined);
                            }
                        }
                    }
                }
            }
        }
    }
    
    public void reducePrimeImplicantsToSubset() {
        int numPrimeImplicants = termList.size();
        int numOriginalTerms   = originalTermList.size();
        boolean[][] table = new boolean[numPrimeImplicants][numOriginalTerms];
        for (int impl=0; impl < numPrimeImplicants; impl++) {
            for (int term=0; term < numOriginalTerms; term++) {
                table[impl][term] = termList.get(impl).implies(originalTermList.get(term));
            }
        }
        ArrayList<Term> newTermList = new ArrayList<Term>();
        boolean done = false;
        int impl;
        while (!done) {
            impl = extractEssentialImplicant(table);
            if (impl != -1) {
                newTermList.add(termList.get(impl));
            } else {
                impl = extractLargestImplicant(table);
                if (impl != -1) {
                    newTermList.add(termList.get(impl));
                } else {
                    done = true;
                }
            }
        }
        termList = newTermList;
        originalTermList = null;
    }
    
    public static Formula read(Reader reader) throws IOException {
        ArrayList<Term> terms = new ArrayList<Term>();
        Term term;
        while ((term = Term.read(reader)) != null) {
            terms.add(term);
        }
        return new Formula(terms);
    }
    
    public static Formula readintArray(int [][] states){
        ArrayList<Term> terms = new ArrayList<Term>();
        Term term;
        for(int i=0;i<states.length;i++){
            term = Term.readintArray(states[i]);
            terms.add(term);
        }
        return new Formula(terms);
    }    

    private int extractEssentialImplicant(boolean[][] table) {
        for (int term=0; term < table[0].length; term++) {
            int lastImplFound = -1;
            for (int impl=0; impl < table.length; impl++) {
                if (table[impl][term]) {
                    if (lastImplFound == -1) {
                        lastImplFound = impl;
                    } else {
                        // This term has multiple implications
                        lastImplFound = -1;
                        break;
                    }
                }
            }
            if (lastImplFound != -1) {
                extractImplicant(table, lastImplFound);
                return lastImplFound;
            }
        }
        return -1;
    }
    
    private void extractImplicant(boolean[][] table, int impl) {
        for (int term=0; term < table[0].length; term++) {
            if (table[impl][term]) {
                for (int impl2=0; impl2 < table.length; impl2++) {
                    table[impl2][term] = false;
                }
            }
        }
    }
    
    private int extractLargestImplicant(boolean[][] table) {
        int maxNumTerms = 0;
        int maxNumTermsImpl = -1;
        for (int impl=0; impl < table.length; impl++) {
            int numTerms = 0;
            for (int term=0; term < table[0].length; term++) {
                if (table[impl][term]) {
                    numTerms++;
                }
            }
            if (numTerms > maxNumTerms) {
                maxNumTerms = numTerms;
                maxNumTermsImpl = impl;
            }
        }
        if (maxNumTermsImpl != -1) {
            extractImplicant(table, maxNumTermsImpl);
            return maxNumTermsImpl;
        }
        return -1;
    }

    public Formula negatePrimes() {
    	int k = termList.size();
    	if (k < 1) {
    		List<Term> nterms = new ArrayList<Term>();
    		nterms.add( new Term(new byte[] {}) );
    		System.out.println("Negate empty formula");
    		return new Formula(nterms);
    	}
    	int[] minindices = new int[k];
    	for (int i=0 ; i<k ; i++) {
    		minindices[i] = 0;
    	}

    	int n = termList.get(0).getNumVars();
    	byte[] values = new byte[n];
    	for (int i=0 ; i<n ; i++) {
    		values[i] = Term.DontCare;
    	}
    	
    	List<Term> nterms = new ArrayList<Term>();
    	int idx = 0;
    	while (idx < k && idx>=0) {
    		int minidx = minindices[idx];
    		if (minidx < 0) {
    			// nothing more to do at this level, reset and backtrack
				minindices[idx--] = 0;
    			continue;
    		}
    		
    		Term t = termList.get(idx);
    		minidx = t.findNextRequiredNegation(minidx, values);
			minindices[idx] = minidx;
    		if (minidx >= 0) {
    			minindices[idx]++;
    		}
    		if (minidx == -2) {
    			// nothing more to do at this level, reset and backtrack
				minindices[idx--] = 0;
    			continue;
    		}
    		idx++;
    		if (idx == k) {
    			Term nt = new Term(values.clone());
    			boolean isnew = true;
    			for (int pos=0 ; pos< nterms.size() ; pos++) {
    				Term curt = nterms.get(pos);
    				if (isnew) {
	    				if (curt.equals(nt)) {
	    					isnew = false;
	    					break;
	    				}
	    				if (curt.implies(nt)) {
	    					isnew = false;
	    					break;
	    				}
	    				if (nt.implies(curt)) {
    						nterms.set(pos, nt);
        					isnew = false;
	    				}
    				} else if (nt.implies(curt)) {
    					// this term was already added, removing other covered terms
    					nterms.remove(pos);
    					pos--;
    				}
    			}
    			if (isnew) {
    				nterms.add(nt);
    			}
    			idx--;
    		}
    	}
    	Formula f = new Formula(nterms, regulators);
    	return f;
    }
    
    public boolean equals(Formula f) {
    	if (termList.size() != f.termList.size()) {
    		return false;
    	}
    	return termList.containsAll(f.termList);
    }
    
    
    public static void main(String[] args) {
    	System.out.println("Go primes");
    	int[][] states = {
    			{1,1,0},	
    			{1,1,1},	
    			{0,1,1},	
    			{1,0,1},	
    	};
    	Formula f = readintArray(states);
    	f.reduceToPrimeImplicants();
    	System.out.println(f);
    	
    	Formula nf = f.negatePrimes();
    	System.out.println(nf);
    	
    }
}

