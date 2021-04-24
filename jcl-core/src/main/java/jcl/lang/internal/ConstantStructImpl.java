package jcl.lang.internal;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.expander.SymbolMacroExpanderInter;

public class ConstantStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	private TYPE constantValue;

	protected ConstantStructImpl(final String name, final PackageStruct symbolPackage) {
		super(name, symbolPackage);
	}

	public static <T extends LispStruct> ConstantStructImpl<T> valueOf(final String name, final PackageStruct symbolPackage) {
		return new ConstantStructImpl<T>(name, symbolPackage);
	}

	public void initializeConstant(final TYPE constantValue) {
		super.setValue(constantValue);
		this.constantValue = constantValue;
	}

	@Override
	public void setSymbolPackage(final PackageStruct symbolPackage) {
		// TODO: temp hacks
//		throw new ProgramErrorException("Can't set package for constant " + name + '.');
	}

	public TYPE getConstantValue() {
		return constantValue;
	}

	@Override
	public void setValue(final LispStruct value) {
		throw new ProgramErrorException("Can't set value for constant " + name + '.');
	}

	@Override
	public void setSymbolMacroExpander(final SymbolMacroExpanderInter symbolMacroExpander) {
		throw new ProgramErrorException("Can't set symbol macro expansion for constant " + name + '.');
	}

	@Override
	public ListStruct setProperty(final LispStruct indicator, final LispStruct newValue) {
		throw new ProgramErrorException("Can't set properties for constant " + name + '.');
	}
}
