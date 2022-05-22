package jcl.functions.list;

import jcl.lang.SymbolStruct;
import jcl.lang.statics.CommonLispSymbols;

public class MapCanFunction extends MapperFunction {

	public MapCanFunction() {
		super("", CommonLispSymbols.MAPCAN.getName());
	}

	@Override
	public SymbolStruct getFunctionSymbol() {
		return CommonLispSymbols.MAPCAN;
	}

	@Override
	protected Accumulate accumulate() {
		return Accumulate.NCONC;
	}

	@Override
	protected boolean takeCar() {
		return true;
	}
}
