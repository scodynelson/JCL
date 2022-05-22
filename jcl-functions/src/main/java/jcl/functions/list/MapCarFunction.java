package jcl.functions.list;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public class MapCarFunction extends MapperFunction {

	public MapCarFunction() {
		super("", CommonLispSymbols.MAPCAR.getName());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MAPCAR;
	}

	@Override
	protected Accumulate accumulate() {
		return Accumulate.LIST;
	}

	@Override
	protected boolean takeCar() {
		return true;
	}
}
