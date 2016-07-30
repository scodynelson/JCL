package jcl.lang;

import jcl.lang.condition.exception.ProgramErrorException;
import jcl.lang.function.FunctionStruct;
import jcl.lang.function.expander.SymbolMacroExpanderInter;
import jcl.lang.list.ListStruct;
import jcl.type.LispType;

public class ConstantStruct<TYPE extends LispStruct> extends SymbolStruct {

	private final TYPE constantValue;

	protected ConstantStruct(final LispType lispType,
	                         final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		super(lispType, name, symbolPackage, value, function);
		constantValue = value;
	}

	protected ConstantStruct(final String name, final PackageStruct symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
		constantValue = value;
	}

	public static <T extends LispStruct> ConstantStruct<T> valueOf(final String name, final PackageStruct symbolPackage, final T value) {
		return new ConstantStruct<T>(name, symbolPackage, value);
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
