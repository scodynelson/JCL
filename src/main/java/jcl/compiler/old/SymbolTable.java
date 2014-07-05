package jcl.compiler.old;

import jcl.symbols.SymbolStruct;

import java.util.HashMap;
import java.util.LinkedList;

public class SymbolTable {

	SymbolTable parent = null;
	LinkedList<SymbolTable> children = null; // new LinkedList<SymbolTable>();

	public SymbolTable(SymbolTable parent) {
		this.parent = parent;
	}

	// This class defines the KEY compoment of the symbol-table. It is built by
	// meeting with LAMBDA or LET that binds a variable.
	class SymbolTableKey {

		SymbolStruct theSymbol = null;
		Object[] theAttributes = null;
		SymbolTableUsage theUsage = null;
		LinkedList<SymbolTableUsage> symbolTableUsage = null;

		public SymbolTableKey(SymbolStruct theSymbol) {
			this.theSymbol = theSymbol;
			this.symbolTableUsage = new LinkedList<SymbolTableUsage>();
		}
	}

	// This creates the base KEYS hash map where newly found symbols will reside.
	HashMap<SymbolStruct, SymbolTableKey> keys = new HashMap<SymbolStruct, SymbolTableKey>();

	// This class defines the usage of a bound symbol in the local symbol-table.
	// The element holds info for where the symbol is used (such as the line nbr).
	class SymbolTableUsage {

		SymbolStruct theSymbol = null;
		Object theElement = null;

		// The parts of a symbol will be defined here
		public SymbolTableUsage(SymbolStruct theSymbol) {
			this.theSymbol = theSymbol;
		}

		public SymbolTableUsage(SymbolStruct theSymbol, SymbolTableUsage theElement) {
			this(theSymbol);
			this.theElement = theElement;
		}
	}

	// This creates the base for the list of symbols found in the code. This works
	// only for symbols found in local the symbol-table
	boolean insert(SymbolStruct sym) {
		// The base symbol that resides in the KEYS
		// If there is a sym already in the KEYS, then it makes a
		// new SymbolTableUsage and put it in the symbolTableUsage
		if (keys.get(sym) != null) {
			// if there a used Usage, hook to that. Otherwise, hook to  the SymbolTableKey
			SymbolTableKey key = keys.get(sym);
			key.theUsage = new SymbolTableUsage(sym);
			System.out.println("Added a symbol to be in the table: " + sym);
			return false;
		} else {
			keys.put(sym, new SymbolTableKey(sym));
			System.out.println("Added a key to be in the table: " + sym);
			return true;
		}
	}

	boolean locate(SymbolStruct sym) {
		return (keys.get(sym) != null);
	}

	Object getAttr(SymbolStruct sym, Object attr) {
		return null;
	}

	boolean setAttr(SymbolStruct sym, Object attr, Object value) {
		return false;
	}

//    public static void main(String[] args) {
//        SymbolTable st = new SymbolTable(null);
//        System.out.println("the foo: " + st);
//        System.exit(0);
//    }
}

