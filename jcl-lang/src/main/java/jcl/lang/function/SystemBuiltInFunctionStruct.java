package jcl.lang.function;

import jcl.lang.statics.GlobalPackageStruct;
import jcl.lang.PackageStruct;
import jcl.lang.SymbolStructImpl;
import jcl.lang.function.parameterdsl.Parameters;

public abstract class SystemBuiltInFunctionStruct extends BuiltInFunctionStruct {

	protected SystemBuiltInFunctionStruct(final String documentation, final String functionName, final Parameters parameters) {
		super(documentation, functionName, parameters);
	}

	@Override
	public SymbolStructImpl getFunctionSymbol() {
		final PackageStruct aPackage = GlobalPackageStruct.SYSTEM;
		final SymbolStructImpl symbol = aPackage.intern(functionName).getSymbol();
		aPackage.export(symbol);
		return symbol;
	}
}
