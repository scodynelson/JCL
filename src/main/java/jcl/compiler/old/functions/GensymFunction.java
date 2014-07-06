package jcl.compiler.old.functions;

import jcl.arrays.StringStruct;
import jcl.compiler.old.symbol.VariableOld;
import jcl.numbers.IntegerStruct;
import jcl.symbols.SymbolStruct;

import java.math.BigInteger;

public final class GensymFunction {

	private GensymFunction() {
	}

	public static SymbolStruct<?> funcall() {
		return getGensymWithPrefix("G");
	}

	public static SymbolStruct<?> funcall(final IntegerStruct arg1) {
		return new SymbolStruct("G" + arg1.getBigInteger());
	}

	public static SymbolStruct<?> funcall(final StringStruct arg1) {
		return getGensymWithPrefix(arg1.getAsJavaString());
	}

	public static SymbolStruct<?> funcall(final String arg1) {
		return getGensymWithPrefix(arg1);
	}

	private static SymbolStruct<?> getGensymWithPrefix(final String prefix) {

		final BigInteger currentGensymCounterValue = VariableOld.GensymCounter.getValue().getBigInteger();
		final SymbolStruct<?> sym = new SymbolStruct(prefix + currentGensymCounterValue);

		final IntegerStruct newGensymCounter = new IntegerStruct(currentGensymCounterValue.add(BigInteger.ONE));
		VariableOld.GensymCounter.setValue(newGensymCounter);

		return sym;
	}
}
