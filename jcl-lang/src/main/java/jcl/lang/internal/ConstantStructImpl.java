package jcl.lang.internal;

import jcl.lang.LispStruct;
import jcl.lang.ListStruct;
import jcl.lang.PackageStructImpl;
import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.type.LispType;

public class ConstantStructImpl<TYPE extends LispStruct> extends SymbolStructImpl {

	private final TYPE constantValue;

	protected ConstantStructImpl(final LispType lispType,
	                             final String name, final PackageStructImpl symbolPackage, final TYPE value, final FunctionStruct function) {
		super(lispType, name, symbolPackage, value, function);
		constantValue = value;
	}

	protected ConstantStructImpl(final String name, final PackageStructImpl symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
		constantValue = value;
	}

	public static <T extends LispStruct> ConstantStructImpl<T> valueOf(final String name, final PackageStructImpl symbolPackage, final T value) {
		return new ConstantStructImpl<T>(name, symbolPackage, value);
	}

	@Override
	public void setSymbolPackage(final PackageStructImpl symbolPackage) {
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
