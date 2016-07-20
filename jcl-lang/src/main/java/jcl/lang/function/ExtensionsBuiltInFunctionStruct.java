package jcl.lang.function;

import jcl.lang.GlobalPackageStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.function.parameterdsl.Parameters;

public abstract class ExtensionsBuiltInFunctionStruct extends BuiltInFunctionStruct {

	protected ExtensionsBuiltInFunctionStruct(final String documentation, final String functionName, final Parameters parameters) {
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