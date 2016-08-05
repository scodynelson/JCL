package jcl.lang.function;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.PackageStructImpl;
import jcl.lang.function.parameterdsl.Parameters;

public abstract class SystemBuiltInFunctionStruct extends BuiltInFunctionStruct {

	protected SystemBuiltInFunctionStruct(final String documentation, final String functionName, final Parameters parameters) {
		super(documentation, functionName, parameters);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		final PackageStructImpl aPackage = GlobalPackageStruct.SYSTEM;
		final SymbolStruct symbol = aPackage.intern(functionName).getSymbol();
		aPackage.export(symbol);
		return symbol;
	}
}
