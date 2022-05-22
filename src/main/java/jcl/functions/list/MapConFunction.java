package jcl.functions.list;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public class MapConFunction extends MapperFunction {

	public MapConFunction() {
		super("", CommonLispSymbols.MAPCON.getName());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MAPCON;
	}

	@Override
	protected Accumulate accumulate() {
		return Accumulate.NCONC;
	}

	@Override
	protected boolean takeCar() {
		return false;
	}
}
