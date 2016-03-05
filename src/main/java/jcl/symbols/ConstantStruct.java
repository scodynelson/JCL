package jcl.symbols;

import jcl.LispStruct;
import jcl.LispType;
import jcl.conditions.exceptions.ProgramErrorException;
import jcl.functions.FunctionStruct;
import jcl.functions.expanders.SymbolMacroExpander;
import jcl.lists.ListStruct;
import jcl.packages.PackageStruct;
import jcl.types.SymbolType;

public class ConstantStruct<TYPE extends LispStruct> extends SymbolStruct {

	private final TYPE constantValue;

	protected ConstantStruct(final LispType lispType,
	                         final String name, final PackageStruct symbolPackage, final TYPE value, final FunctionStruct function) {
		super(lispType, name, symbolPackage, value, function);
		constantValue = value;
	}

	public ConstantStruct(final String name, final PackageStruct symbolPackage, final TYPE value) {
		super(name, symbolPackage, value);
		constantValue = value;
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
	public void setSymbolMacroExpander(final SymbolMacroExpander symbolMacroExpander) {
		throw new ProgramErrorException("Can't set symbol macro expansion for constant " + name + '.');
	}

	@Override
	public ListStruct setProperty(final LispStruct indicator, final LispStruct newValue) {
		throw new ProgramErrorException("Can't set properties for constant " + name + '.');
	}
}
