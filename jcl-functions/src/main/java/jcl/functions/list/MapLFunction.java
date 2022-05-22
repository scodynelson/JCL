package jcl.functions.list;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public class MapLFunction extends MapperFunction {

	public MapLFunction() {
		super("", CommonLispSymbols.MAPL.getName());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MAPL;
	}

	@Override
	protected Accumulate accumulate() {
		return Accumulate.NONE;
	}

	@Override
	protected boolean takeCar() {
		return false;
	}
}
