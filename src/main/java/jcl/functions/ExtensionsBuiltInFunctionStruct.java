package jcl.functions;

import jcl.functions.parameterdsl.Parameters;
import jcl.packages.GlobalPackageStruct;
import jcl.packages.PackageStruct;
import jcl.symbols.SymbolStruct;

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
