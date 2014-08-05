package jcl.compiler.old.functions;

import jcl.structs.arrays.StringStruct;
import jcl.structs.numbers.IntegerStruct;
import jcl.structs.symbols.SymbolStruct;
import jcl.structs.symbols.variables.Variable;

import java.math.BigInteger;

public final class GensymFunction {

	private GensymFunction() {
	}

	public static SymbolStruct<?> funcall() {
		return getGensymWithPrefix("G");
	}

	public static SymbolStruct<?> funcall(final IntegerStruct arg1) {
		return new SymbolStruct<>("G" + arg1.getBigInteger());
	}

	public static SymbolStruct<?> funcall(final StringStruct arg1) {
		return getGensymWithPrefix(arg1.getAsJavaString());
	}

	public static SymbolStruct<?> funcall(final String arg1) {
		return getGensymWithPrefix(arg1);
	}

	private static SymbolStruct<?> getGensymWithPrefix(final String prefix) {

		final IntegerStruct currentGensymCounter = Variable.GENSYM_COUNTER.getValue();
		final BigInteger currentGensymCounterValue = currentGensymCounter.getBigInteger();
		final SymbolStruct<?> sym = new SymbolStruct<>(prefix + currentGensymCounterValue);

		final IntegerStruct newGensymCounter = new IntegerStruct(currentGensymCounterValue.add(BigInteger.ONE));
		Variable.GENSYM_COUNTER.setValue(newGensymCounter);

		return sym;
	}
}
