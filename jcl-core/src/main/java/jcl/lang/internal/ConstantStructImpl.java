package jcl.lang.internal;

import jcl.lang.LispStruct;
import jcl.lang.PackageStruct;

public class ConstantStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	private TYPE constantValue;

	private ConstantStructImpl(final String name) {
		super(name);
	}

	public static <T extends LispStruct> ConstantStructImpl<T> valueOf(final String name, final PackageStruct symbolPackage) {
		final ConstantStructImpl<T> struct = new ConstantStructImpl<>(name);
		struct.setConstant();

		symbolPackage.importSymbol(struct);
		struct.setSymbolPackage(symbolPackage);
		return struct;
	}

	public void initializeConstant(final TYPE constantValue) {
		value = constantValue;
		this.constantValue = constantValue;
	}

	public TYPE getConstantValue() {
		return constantValue;
	}
}
