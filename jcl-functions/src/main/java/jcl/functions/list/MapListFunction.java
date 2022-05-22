package jcl.functions.list;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public class MapListFunction extends MapperFunction {

	public MapListFunction() {
		super("", CommonLispSymbols.MAPLIST.getName());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MAPLIST;
	}

	@Override
	protected Accumulate accumulate() {
		return Accumulate.LIST;
	}

	@Override
	protected boolean takeCar() {
		return false;
	}
}
