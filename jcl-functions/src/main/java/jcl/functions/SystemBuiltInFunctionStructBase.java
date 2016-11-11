package jcl.functions;

import jcl.lang.PackageStruct;
import jcl.lang.SymbolStruct;
import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.function.parameterdsl.Parameters;

public abstract class SystemBuiltInFunctionStructBase extends BuiltInFunctionStructImpl {

	protected SystemBuiltInFunctionStructBase(final String documentation, final String functionName, final Parameters parameters) {
		super(documentation, functionName, parameters);
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.SYSTEM;
		final SymbolStruct symbol = aPackage.intern(functionName).getSymbol();
		aPackage.export(symbol);
		return symbol;
	}
}
