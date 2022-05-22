package jcl.functions.list;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public class MapCFunction extends MapperFunction {

	public MapCFunction() {
		super("", CommonLispSymbols.MAPC.getName());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MAPC;
	}

	@Override
	protected Accumulate accumulate() {
		return Accumulate.NONE;
	}

	@Override
	protected boolean takeCar() {
		return true;
	}
}
