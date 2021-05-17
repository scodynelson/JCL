package jcl.lang.internal;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.condition.exception.ProgramErrorException;

public class ConstantStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	private TYPE constantValue;

	protected ConstantStructImpl(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
	}

	public static <T extends LispStruct> ConstantStructImpl<T> valueOf(final String name, final PackageStruct symbolPackage) {
		return new ConstantStructImpl<T>(name, symbolPackage);
	}

	public void initializeConstant(final TYPE constantValue) {
		super.setSymbolValue(constantValue);
		this.constantValue = constantValue;
	}

	@Override
	public PackageStruct setSymbolPackage(final PackageStruct symbolPackage) {
		// TODO: temp hacks
//		throw new ProgramErrorException("Can't set package for constant " + name + '.');
		return symbolPackage;
	}

	public TYPE getConstantValue() {
		return constantValue;
	}

	@Override
	public LispStruct setSymbolValue(final LispStruct value) {
		throw new ProgramErrorException("Can't set value for constant " + name + '.');
	}

	@Override
	public LispStruct setProp(final LispStruct indicator, final LispStruct newValue) {
		throw new ProgramErrorException("Can't set properties for constant " + name + '.');
	}
}
