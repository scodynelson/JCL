package jcl.lang.function;

import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.function.parameterdsl.Parameters;

public abstract class ExtensionsBuiltInFunctionStructBase extends BuiltInFunctionStructImpl {

	protected ExtensionsBuiltInFunctionStructBase(final String documentation, final String functionName, final Parameters parameters) {
		super(documentation, functionName, parameters);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.EXTENSIONS;
		final SymbolStruct symbol = aPackage.intern(functionName).getSymbol();
		aPackage.export(symbol);
		return symbol;
	}
}