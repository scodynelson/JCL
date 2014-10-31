package jcl.compiler.real.environment;

import jcl.structs.symbols.SymbolStruct;
import org.apache.commons.lang3.builder.ReflectionToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

public class ClosureBinding {

	private SymbolStruct<?> symbolStruct;
	private int position;
	private int references;

	public ClosureBinding(final SymbolStruct<?> symbolStruct, final int position, final int references) {
		this.symbolStruct = symbolStruct;
		this.position = position;
		this.references = references;
	}

	public SymbolStruct<?> getSymbolStruct() {
		return symbolStruct;
	}

	public int getPosition() {
		return position;
	}

	public int getReferences() {
		return references;
	}

	public void setSymbolStruct(final SymbolStruct<?> symbolStruct) {
		this.symbolStruct = symbolStruct;
	}

	public void setPosition(final int position) {
		this.position = position;
	}

	public void setReferences(final int references) {
		this.references = references;
	}

	@Override
	public String toString() {
		return ReflectionToStringBuilder.toString(this, ToStringStyle.MULTI_LINE_STYLE);
	}
}
