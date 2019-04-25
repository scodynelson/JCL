package jcl.lang.internal;

import jcl.lang.FunctionStruct;
import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStruct;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.expander.SymbolMacroExpanderInter;

public class ConstantStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	private final TYPE constantValue;

	protected ConstantStructImpl(final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		super(name, symbolPackage, value, function);
		constantValue = value;
	}

	protected ConstantStructImpl(final String name, final PackageStruct symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
		constantValue = value;
	}

	public static <T extends LispStruct> ConstantStructImpl<T> valueOf(final String name, final PackageStruct symbolPackage, final T value) {
		return new ConstantStructImpl<T>(name, symbolPackage, value);
	}

	@Override
	public void setSymbolPackage(final PackageStruct symbolPackage) {
		throw new ProgramErrorException("Can't set package for constant " + name + '.');
	}

	public TYPE getConstantValue() {
		return constantValue;
	}

	@Override
	public void setValue(final LispStruct value) {
		throw new ProgramErrorException("Can't set value for constant " + name + '.');
	}

	@Override
	public void bindLexicalValue(final LispStruct value) {
		throw new ProgramErrorException("Can't bind value for constant " + name + '.');
	}

	@Override
	public void unbindLexicalValue() {
		throw new ProgramErrorException("Can't unbind value for constant " + name + '.');
	}

	@Override
	public void bindDynamicValue(final LispStruct value) {
		throw new ProgramErrorException("Can't bind value for constant " + name + '.');
	}

	@Override
	public void unbindDynamicValue() {
		throw new ProgramErrorException("Can't unbind value for constant " + name + '.');
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
