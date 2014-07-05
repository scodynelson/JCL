package jcl.compiler.old.functions;

import jcl.compiler.old.symbol.VariableOld;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SymbolStruct;

import java.math.BigInteger;

public class GensymFunction {
	public static final GensymFunction FUNCTION = new GensymFunction();

	/**
	 * @return a new gensym with string "G"
	 */
	public Object funcall() {
		SymbolStruct sym = new SymbolStruct("G" + VariableOld.GensymCounter.getValue());
		VariableOld.GensymCounter.setValue(new IntegerStruct(VariableOld.GensymCounter.getValue().getBigInteger().add(BigInteger.ONE)));
		return sym;
	}

	/**
	 * @param arg1 if integer the gensym int, else toString is gensym string
	 * @return new gensym
	 */
	public Object funcall(Object arg1) {
		SymbolStruct sym;
		if (arg1 instanceof Integer) {
			sym = new SymbolStruct("G" + arg1);
		} else {
			sym = new SymbolStruct("" + arg1 + VariableOld.GensymCounter.getValue());
			VariableOld.GensymCounter.setValue(new IntegerStruct(VariableOld.GensymCounter.getValue().getBigInteger().add(BigInteger.ONE)));
		}
		return sym;
	}
}
